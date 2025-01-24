using Abstract.Binutils.Abs.Elf;
using Abstract.CompileTarget.Core.Enums;

namespace Abstract.CompileTarget.Core;

public abstract class CompilerBase : IDisposable
{
    public abstract ExpectedELFFormat ExpectedELFFormat { get; }

    public abstract void Compile(string outPath, ElfProgram[] source);

    public virtual void Dispose() => GC.SuppressFinalize(this);
}
