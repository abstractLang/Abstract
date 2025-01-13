namespace Abstract.CompileTarget.Core;

public abstract class CompileTargetExtension
{
    public abstract string TargetID { get; }
    public abstract string TargetName { get; }
    public abstract string TargetDescription { get; }

    public abstract CompilerBase CompilerInstance { get; }
}
