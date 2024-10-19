namespace Abstract.Build.Core.Exceptions.Internal;

public abstract class InternalBuildException : BuildException
{

    public InternalBuildException() : base() { }
    public InternalBuildException(string? message) : base(message) { }
    public InternalBuildException(string? message, Exception innerException) : base(message, innerException) { }

}
