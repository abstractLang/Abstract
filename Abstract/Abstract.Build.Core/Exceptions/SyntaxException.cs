using Abstract.Build.Core.Exceptions;
using Abstract.Build.Core.Sources;

namespace Abstract.Build.Exceptions;

public abstract class SyntaxException : BuildException
{

    public readonly (uint start, uint end) range;
    public readonly Script script;

    public SyntaxException(Script src, (uint start, uint end) range) : base()
    {
        script = src;
        this.range = range;
    }
    public SyntaxException(Script src, (uint start, uint end) range, string? message) : base(message)
    {
        script = src;
        this.range = range;
    }
    public SyntaxException(Script src, (uint start, uint end) range, string? message, Exception innerException) : base(message, innerException)
    {
        script = src;
        this.range = range;
    }

}
