﻿namespace Abstract.InterOp;

/// <summary>
/// When aplyed to a assembly, module or static class, defines it as an public abstract interoperable namespace.
/// </summary>
[AttributeUsage(AttributeTargets.Assembly | AttributeTargets.Module | AttributeTargets.Class, Inherited = false)]
public class AbstractNamespaceAttribute : Attribute
{

    public string? NamespaceName { get; set; } = null;

    public AbstractNamespaceAttribute() {}
    public AbstractNamespaceAttribute(string name) => NamespaceName = name;

}

/// <summary>
/// When aplyed to a class or structure, defines it as an public abstract interoperable structure.
/// </summary>
[AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, Inherited = false)]
public class AbstractStructAttribute : Attribute
{

}

/// <summary>
/// When aplyed to a method, defines it as an public abstract interoperable function.
/// </summary>
[AttributeUsage(AttributeTargets.Method, Inherited = false)]
public class AbstractFunctionAttribute : Attribute
{

}

/// <summary>
/// WHen aplied to a field or property, defines it as an public abstract interoperable field.
/// </summary>
[AttributeUsage(AttributeTargets.Field | AttributeTargets.Property, Inherited = false)]
public class AbstractPublicFieldAttribute : Attribute
{

}

/// <summary>
/// When aplied to a enumerator, defines it as an public abstract interoperable operator.
/// </summary>
[AttributeUsage(AttributeTargets.Enum, Inherited = false)]
public class AbstractEnumeratorAttribute : Attribute
{

}
