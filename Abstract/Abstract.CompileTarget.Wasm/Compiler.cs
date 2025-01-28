using System.Buffers.Binary;
using Abstract.Binutils.Abs.Bytecode;
using Abstract.Binutils.Abs.Elf;
using WebAssembly;
using WebAssembly.Instructions;
using Directory = Abstract.Binutils.Abs.Elf.Directory;
using WasmValueType = WebAssembly.WebAssemblyValueType;
using ImportFunc = WebAssembly.Import.Function;

namespace Abstract.CompileTarget.Wasm;

internal static class Compiler
{

    public static Module GenerateWasmFileFromElf(ElfProgram elf)
    {
        var module = new Module();

        var (elfFuncs, elfTypes, elfImports) = SegregateElfData(elf);
        Dictionary<uint, uint> importMap = [];

        // initialize memory
        module.Memories.Add(new Memory(1, null));
        module.Exports.Add(new Export {
            Name = "mem",
            Kind = ExternalKind.Memory,
            Index = 0
        });
        uint memoryPtr = 4;

        // compile imports
        foreach (var i in elfImports)
        {
            List<WasmValueType> returns = [];
            List<WasmValueType> parameters = [];

            var id = i.identifier;
            var refModule = id[..id.IndexOf('.')];
            var refBase = id[(id.IndexOf('.')+1)..id.IndexOf('(')];
            
            var ps = i.GetChildren("PARAM");
            foreach (var p in ps)
            {
                var t = p.GetChild("TYPE")!;
                parameters.AddRange(Struct2WasmType(t.identifier));
            }

            var ret = i.GetChild("RET");
            if (ret != null) returns.AddRange(Struct2WasmType(ret.identifier));

            var importType = new WebAssemblyType {
                Parameters = [.. parameters],
                Returns = [.. returns]
            };

            var typeid = (uint)module.Types.Count;
            module.Types.Add(importType);

            var importid = (uint)module.Imports.Count;
            module.Imports.Add(new ImportFunc {
                TypeIndex = typeid,
                Field = refBase,
                Module = refModule
            });

            importMap.Add((uint)i.index, importid);
        }

        // compile functions
        foreach (var i in elfFuncs)
        {
            var codeLump = i.GetChild("CODE", "main")!;
            var dataLump = i.GetChild("DATA", "main");

            var memoffset = memoryPtr;

            // Append data lump
            if (dataLump != null)
            {
                module.Data.Add(new Data {
                    RawData = ((MemoryStream)dataLump.content!).GetBuffer()[.. (int)dataLump.content.Length],
                    InitializerExpression = [ new Int32Constant(memoryPtr), new End() ]
                });
                memoryPtr += (uint)dataLump.content.Length;
            }

            // create type
            List<WasmValueType> returns = [];
            List<WasmValueType> parameters = [];

            var ps = i.GetChildren("PARAM");
            foreach (var p in ps)
            {
                var t = p.GetChild("TYPE")!;
                parameters.AddRange(Struct2WasmType(t.identifier));
            }

            var ret = i.GetChild("RET");
            if (ret != null) returns.AddRange(Struct2WasmType(ret.identifier));

            var funcType = new WebAssemblyType {
                Parameters = [.. returns],
                Returns = [.. parameters]
            };

            var (locals, instructions) = ParseFunctionBody(
                elf, codeLump.content!, memoffset, importMap);

            var funcBody = new FunctionBody {
                Locals = locals,
                Code = instructions
            };

            var typeid = (uint)module.Types.Count;

            module.Types.Add(funcType);
            module.Functions.Add(new Function(typeid));
            module.Codes.Add(funcBody);

            var global = i.GetChild("GLOBAL")!;

            module.Exports.Add(new Export {
                Index = typeid,
                Kind = ExternalKind.Function,
                Name = global.identifier
            });
        }

        // static memory end
        byte[] buf = new byte[4];
        BinaryPrimitives.WriteUInt32BigEndian(buf, memoryPtr);
        module.Data.Add(new Data {
            RawData = buf,
            InitializerExpression = [ new Int32Constant(0), new End() ]
        });

        return module;
    }

    private static (
        List<Directory> functions,
        List<Directory> types,
        List<Directory> imports
    ) SegregateElfData(ElfProgram elf)
    {
        List<Directory> functions = [];
        List<Directory> types = [];
        List<Directory> imports = [];

        Stack<(Directory dir, Queue<Directory> children)> _searchingStack = [];
        var projectRoot = elf.RootDirectory.GetChild("PROJECT", elf.Name)!;
        _searchingStack.Push((projectRoot, new(projectRoot.Children)));

        while (_searchingStack.Count > 0)
        {
            var (dir, children) = _searchingStack.Peek();
            if (children.Count == 0) {_searchingStack.Pop(); continue; }

            var c = children.Dequeue();

            if (c.kind == "FUNC") functions.Add(c);
            else if (c.kind == "TYPE") types.Add(c);

            if (c.ChildrenCount > 0)
                _searchingStack.Push((c, new(c.Children)));
        }

        foreach (var i in elf.RootDirectory.Children.Where(e => e.kind == "IMPORT"))
            imports.AddRange(i.Children);

        return (functions, types, imports);
    }

    private static (List<Local>, List<Instruction>) ParseFunctionBody(
        ElfProgram program, Stream bytecode, uint memoffset, Dictionary<uint, uint> importMap)
    {
        List<Local> locals = [];
        List<Instruction> instructions = [];

        VirtualStack stack = new();

        bytecode.Position = 0;
        while (bytecode.Position < bytecode.Length)
        {
            var instruction = bytecode.ReadU8();

            // TODO

            throw new NotImplementedException($"Unhandled: {instruction}");
        }

        instructions.Add(new End());

        return (locals, instructions);
    }

    // FIXME based on alliginment and not in structure itself
    private static WasmValueType[] Struct2WasmType(string structName) => structName switch {
        "Std.Types.Void" => [],

        "Std.Types.UnsignedInteger8" or "Std.Types.SignedInteger8" or
        "Std.Types.UnsignedInteger16" or "Std.Types.SignedInteger16" or
        "Std.Types.UnsignedInteger32" or "Std.Types.SignedInteger32"
        => [ WasmValueType.Int32 ],

        "Std.Types.UnsignedInteger64" or "Std.Types.SignedInteger64"
        => [ WasmValueType.Int64 ],

        "Std.Types.UnsignedInteger128" or "Std.Types.SignedInteger128"
        => [ WasmValueType.Int64, WasmValueType.Int64 ],

        "Std.Types.Single" => [ WasmValueType.Float32 ],
        "Std.Types.Double" => [ WasmValueType.Float64 ],

        _ => [WasmValueType.Int32] // ptr or simple int representable
    };

    private class VirtualStack {
        private Stack<byte> _data = [];

        public void PushU8(byte v) => _data.Push(v);
        public void PushU16(ushort v) {
            var b = BitConverter.GetBytes(v);
            _data.Push(b[1]);
            _data.Push(b[0]);
        }
        public void PushU32(uint v) {
            var b = BitConverter.GetBytes(v);
            _data.Push(b[3]);
            _data.Push(b[2]);
            _data.Push(b[1]);
            _data.Push(b[0]);
        }
        public void PushU64(ulong v) {
            var b = BitConverter.GetBytes(v);
            _data.Push(b[7]);
            _data.Push(b[6]);
            _data.Push(b[5]);
            _data.Push(b[4]);
            _data.Push(b[3]);
            _data.Push(b[2]);
            _data.Push(b[1]);
            _data.Push(b[0]);
        }

        public byte PopU8() => _data.Pop();
        public ushort PopU16() => BitConverter.ToUInt16([_data.Pop(), _data.Pop()]);
        public uint PopU32() => BitConverter.ToUInt32([_data.Pop(), _data.Pop(), _data.Pop(), _data.Pop()]);
        public ulong PopU64() => BitConverter.ToUInt64(
            [_data.Pop(), _data.Pop(), _data.Pop(), _data.Pop(),
            _data.Pop(), _data.Pop(), _data.Pop(), _data.Pop()]);


        public void PushPtr(uint v) => PushU32(v);
        public void PushPtr(int v) => PushU32((uint)v);
        public uint PopPtr() => PopU32();
    }

}
