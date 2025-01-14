using Abstract.Binutils.Abs.Elf;

namespace Abstract.CompileTarget.Core;

public abstract class CompilerBase : IDisposable
{

    public abstract void Compile(string outPath, ElfProgram[] source);

    public virtual void Dispose() => GC.SuppressFinalize(this);
}
