namespace Abstract.Build.Core.Exceptions.Internal;

public class NotImplementedInternalBuildException : InternalBuildException
{

    public NotImplementedInternalBuildException() : base() { }
    public NotImplementedInternalBuildException(string? message) : base(message) { }
    public NotImplementedInternalBuildException(string? message, Exception innerException) : base(message, innerException) { }

}
