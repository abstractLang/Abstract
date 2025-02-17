using Abstract.InterOp;

namespace Abstract.Builder.Core;

/// <summary>
/// Build namespace.
/// Countains interoperable structures to help
/// building abstract solutions inside abstract
/// code.
/// </summary>
[AbstractNamespace("Build")]
public static class BuildNamespace
{
    /// <summary>
    /// The base for a build instance
    /// </summary>
    [AbstractStruct]
    public abstract class Builder
    {



    }

    /// <summary>
    /// A abstract build step
    /// </summary>
    [AbstractStruct]
    public abstract class Step
    {

    }


    /// <summary>
    /// Reference to a generated executable
    /// </summary>
    [AbstractStruct]
    public class Executable
    {

    }

}
