using Abstract.Binutils.Abs.Elf;
using Abstract.CompileTarget.Core;
using Abstract.CompileTarget.Core.Enums;

namespace Abstract.CompileTarget.Elf;

public class ElfTarget : CompileTargetExtension
{
    public override string TargetID => "elf";

    public override string TargetName => "ELF format";

    public override string TargetDescription => "Compiles the code to the abstract's Executable and Lincable format";

    public override CompilerBase CompilerInstance => new Compiler();
}

class Compiler : CompilerBase
{
    public override ExpectedELFFormat ExpectedELFFormat => ExpectedELFFormat.single;

    public override void Compile(string outPath, ElfProgram[] source)
    {
        
    }
}
