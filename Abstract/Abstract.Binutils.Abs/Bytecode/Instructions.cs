using System.Buffers.Binary;
using System.Numerics;
using Abstract.Binutils.Abs.Elf;
using Lump = Abstract.Binutils.Abs.Elf.ElfBuilder.LumpBuilder;

namespace Abstract.Binutils.Abs.Bytecode;

public static class Instructions
{
    // general utils
    public static void DirectoryReference(this Lump l, ElfBuilder.DirBuilder immref) {
        immref._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
    }

    // Instructions

}
