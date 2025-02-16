namespace Abstract.InterOp;

[AttributeUsage(AttributeTargets.Assembly | AttributeTargets.Module | AttributeTargets.Class | AttributeTargets.Struct, Inherited = false)]
public class AbstractNamespaceAttribute : Attribute
{

    public string? NamespaceName { get; set; } = null;

    public AbstractNamespaceAttribute() {}
    public AbstractNamespaceAttribute(string name) => NamespaceName = name;

}


[AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, Inherited = false)]
public class AbstractStructAttribute : Attribute
{

}
