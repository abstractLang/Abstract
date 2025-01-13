using System.Buffers.Binary;
using Abstract.Binutils.Abs.Bytecode;
using Abstract.Binutils.Abs.Elf;
using WebAssembly;
using WebAssembly.Instructions;
using Directory = Abstract.Binutils.Abs.Elf.Directory;
using WasmValueType = WebAssembly.WebAssemblyValueType;

namespace Abstract.CompileTarget.Wasm;

internal static class Compiler
{

    public static Module GenerateWasmFileFromElf(ElfProgram elf)
    {
        var module = new Module();

        var (elfFuncs, elfTypes) = SegregateElfData(elf);

        // initialize memory
        module.Memories.Add(new Memory(1, null));
        uint memoryPtr = 0;

        // compile functions
        foreach (var i in elfFuncs)
        {

            var codeLump = i.GetChild("CODE", "main")!;
            var dataLump = i.GetChild("DATA", "main")!;

            var memoffset = memoryPtr;

            module.Data.Add(new Data {
                RawData = ((MemoryStream)dataLump.content!).GetBuffer()[.. (int)dataLump.content.Length],
                InitializerExpression = [ new Int32Constant(memoryPtr), new End() ]
            });
            memoryPtr += (uint)dataLump.content.Length;

            List<WasmValueType> _params = [];
            int idx = 0;
            while(true)
            {
                var c = i.GetChild("PARAM", $"{idx++:X4}");
                if (c == null) break;

                // TODO process c here lol
            }

            var funcType = new WebAssemblyType {
                Parameters = [],
                Returns = []
            };

            var (locals, instructions) = ParseFunctionBody(elf, codeLump.content!, memoffset);

            var funcBody = new FunctionBody {
                Locals = locals,
                Code = instructions
            };

            var typeid = (uint)module.Types.Count;

            module.Types.Add(funcType);
            module.Functions.Add(new Function(typeid));
            module.Codes.Add(funcBody);

            module.Exports.Add(new Export {
                Index = typeid,
                Kind = ExternalKind.Function,
                Name = i.identifier
            });
        }
        
        return module;
    }

    private static (
        List<Directory> functions,
        List<Directory> types
    ) SegregateElfData(ElfProgram elf)
    {
        List<Directory> functions = [];
        List<Directory> types = [];

        Stack<(Directory dir, Queue<Directory> children)> _searchingStack = [];
        var projectRoot = elf.RootDirectory.GetChild("PROJECT", elf.Name)!;
        _searchingStack.Push((projectRoot, new(projectRoot.Children)));

        while (_searchingStack.Count > 0)
        {
            var (dir, children) = _searchingStack.Peek();
            if (children.Count == 0) {_searchingStack.Pop(); continue; }

            var c = children.Dequeue();

            if (c.kind == "FUNCTION") functions.Add(c);
            else if (c.kind == "TYPE") types.Add(c);

            if (c.ChildrenCount > 0)
                _searchingStack.Push((c, new(c.Children)));
        }

        return (functions, types);
    }

    private static (List<Local>, List<Instruction>) ParseFunctionBody(
        ElfProgram program, Stream bytecode, uint memoffset)
    {
        List<Local> locals = [];
        List<Instruction> instructions = [];

        VirtualStack stack = new();

        bytecode.Position = 0;
        while (bytecode.Position < bytecode.Length)
        {
            var instruction = Instructions.Get(bytecode.ReadU8());

            switch (instruction.b)
            {
                case Base.Nop: instructions.Add(new NoOperation()); break;

                case Base.Illegal
                or Base.Invalid: instructions.Add(new Unreachable()); break;


                case Base.LdConst:

                    switch (instruction.t)
                    {
                        case Types.Str:
                            instructions.Add(new Int32Constant(memoffset + bytecode.ReadU32()));
                            break;

                        default: throw new Exception();
                    }

                    break;
                case Base.LdType:
                    stack.PushU32(bytecode.ReadU32());
                    break;
                case Base.LdPType:
                    stack.PushU8(bytecode.ReadU8());
                    break;

                case Base.LdLocal:
                    instructions.Add(new LocalGet(bytecode.ReadU16()));
                    break;

                case Base.SetLocal:
                    instructions.Add(new LocalSet(bytecode.ReadU16()));
                    break;


                case Base.EnterFrame:
                    // order:
                    //  primities first
                    //  structures seccond

                    var arg1 = bytecode.ReadU16();
                    var arg2 = bytecode.ReadU16();

                    List<Types> buildinTypes = [];
                    List<uint> structTypes = [];

                    for (var i = 0; i < arg2; i++)
                        structTypes.Add(stack.PopU32());
                    for (var i = 0; i < arg1; i++)
                        buildinTypes.Add((Types)stack.PopU8());
                    
                    // POP returns things in the reverse order
                    // reversing manually here!
                    buildinTypes.Reverse();
                    structTypes.Reverse();

                    // FIXME Let's ignore structs for now hehe :3

                    foreach (var i in buildinTypes)
                    {
                        locals.Add(i switch {
                            Types.i8 or Types.u8 or
                            Types.i16 or Types.u16 or
                            Types.i32 or Types.u32 =>
                                new Local { Type = WasmValueType.Int32, Count = 1 },
                            
                            Types.i64 or Types.u64 =>
                                new Local { Type = WasmValueType.Int64, Count = 1 },

                            Types.i128 or Types.u128 =>
                                new Local { Type = WasmValueType.Int64, Count = 2 },

                            Types.f32 => new Local { Type = WasmValueType.Float32, Count = 1 },
                            Types.f64 => new Local { Type = WasmValueType.Float64, Count = 1 },

                            Types.Bool => new Local { Type = WasmValueType.Int32, Count = 1 },
                            Types.Char => new Local { Type = WasmValueType.Int32, Count = 1 },

                            Types.Str or // Pointers in general
                            Types.Arr or
                            Types.Struct => new Local { Type = WasmValueType.Int32, Count = 1 },

                            _ => throw new Exception()
                        });
                    }

                    break;
                case Base.LeaveFrame: /* TODO */ break;

                case Base.Call:
                    var funcref = bytecode.ReadU32();
                    var func = program.AllDirectories[funcref];

                    if (func.kind == "FUNCTION")
                    {
                        var pcount = 0;
                        while(func.GetChild("PARAM", $"{pcount:X4}") != null) pcount++;
                        pcount--;

                        for (var i = 0; i < pcount; i++)
                            instructions.Add(new Drop());
                    }
                    else if (func.kind == "IFUNCTIO")
                    {
                        var i = func.identifier;
                        var ps = i.Substring(i.IndexOf('(')+1, i.LastIndexOf(')') - i.IndexOf('(') - 1).Split(", ", StringSplitOptions.TrimEntries);

                        for (var j = 0; j < ps.Length; j++)
                            instructions.Add(new Drop());
                    }

                    if (instruction.t == Types.Str)
                        instructions.Add(new Int32Constant(0));

                    break;

                case Base.Ret: instructions.Add(new End()); break;

                default: throw new NotImplementedException($"Unhandled: {instruction}");
            }

        }

        return (locals, instructions);
    }
    private static WasmValueType[] Abs2WasmType(Types t) => t switch {
        Types.Void => [],
        Types.Null => [WasmValueType.Int32],

        Types.i8 or Types.i16 or Types.i32 or 
        Types.u8 or Types.u16 or Types.u32 => [WasmValueType.Int32],
        Types.i64 or Types.u64 => [WasmValueType.Int64],

        Types.i128 or Types.u128 => [WasmValueType.Int64, WasmValueType.Int64],

        Types.f32 => [WasmValueType.Float32],
        Types.f64 => [WasmValueType.Float64],

        Types.Bool => [WasmValueType.Int32],
        Types.Char => [WasmValueType.Int32],
        Types.Str => [WasmValueType.Int32],
        Types.Struct => [WasmValueType.Int32],

        _ => throw new Exception()
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
