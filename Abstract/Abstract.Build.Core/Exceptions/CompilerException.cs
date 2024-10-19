using Abstract.Build.Core.Sources;

namespace Abstract.Build.Core.Exceptions;

public abstract class CompilerException : BuildException
{
    public bool HaveLocation { get; protected set; }
    public (uint start, uint length) Range { get; private set; }
    public Script Script { get; private set; }
    
    public CompilerException() : base()
    {
        HaveLocation = false;
        Script = null!;
    }
    public CompilerException(string? message) : base(message)
    {
        HaveLocation = false;
        Script = null!;
    }
    public CompilerException(string? message, Exception innerException) : base(message, innerException)
    {
        HaveLocation = false;
        Script = null!;
    }

    public CompilerException(Script script, (uint start, uint length) range) : base()
    {
        HaveLocation = true;
        this.Range = range;
        Script = script;
    }
    public CompilerException(Script script, (uint start, uint length) range, string? message) : base(message)
    {
        HaveLocation = true;
        Range = range;
        Script = script;
    }
    public CompilerException(Script script, (uint start, uint length) range, string? message, Exception? innerException) : base(message, innerException)
    {
        HaveLocation = true;
        Range = range;
        Script = script;
    }

    public (uint start, uint end) GetLocationRange() => Range;
}
