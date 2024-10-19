namespace Abstract.Build.Core.Exceptions;

public abstract class BuildException : ApplicationException
{

    public BuildException() : base() { }
    public BuildException(string? message) : base(message) { }
    public BuildException(string? message, Exception innerException) : base(message, innerException) { }

}
