namespace Abstract.Parser.Core.Language.AbstractSyntaxTree;

/// <summary>
/// A reference to a defined type
/// </summary>
/// <param name="symbol"></param>
public class ReferenceTypeNode(ISymbol symbol) : TypeNode
{
    public ISymbol symbol = symbol;
    public override string ToString() => $"{symbol}";
}
/// <summary>
/// A reference to a defined type that asks for parameters
/// </summary>
public class GenericTypeNode(ISymbol symbol) : TypeNode
{
    public readonly ISymbol symbol = symbol;

    public override string ToString() => $"{symbol}:G";
}

/// <summary>
/// The base type of all type modifiers.
/// e.g.: [modifier]Type
/// </summary>
/// <param name="baseType"> The type being modified </param>
public abstract class TypeModifier(TypeNode baseType) : TypeNode
{
    public readonly TypeNode baseType = baseType;
}

/// <summary>
/// The array modifier []T
/// </summary>
/// <param name="baseType"> The type being modified </param>
public class ArrayType(TypeNode baseType) : TypeModifier(baseType)
{
    public override string ToString() => $"[]{baseType}";
}
/// <summary>
/// The reference modifier *T
/// </summary>
/// <param name="baseType"> The type being modified </param>
public class ReferenceType(TypeNode baseType) : TypeModifier(baseType)
{
    public override string ToString() => $"*{baseType}";
}
/// <summary>
/// The location reference modifier &T
/// </summary>
/// <param name="baseType"> The type being modified </param>
public class LocationRefType(TypeNode baseType) : TypeModifier(baseType)
{
    public override string ToString() => $"&{baseType}";
}