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

        // TODO

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
