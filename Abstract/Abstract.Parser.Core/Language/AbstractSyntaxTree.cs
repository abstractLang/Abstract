using Abstract.Build.Core.Sources;
using System.Collections;
using System.Globalization;
using System.Numerics;
using System.Text;

namespace Abstract.Parser.Core.Language.AbstractSyntaxTree;


// Collections
public class CollectionExpressionNode : ValueCollectionNode<ExpressionNode>
{
    public override string ToString() => $"[{string.Join(", ", children)}]";
}

// Dynamic Values
public class MethodCallNode : ValueExpressionNode
{
    public ISymbol Symbol { get; set; } = null!;
    public ArgumentCollectionNode args = null!;

    public override string ToString() => $"{Symbol}{args}";
}
public class IdentifierNode(ISymbol symbol) : ValueExpressionNode
{
    public ISymbol symbol = symbol;
    public override string ToString() => $"{symbol}";
}

// Literal values
public class NumericLiteralNode(BigInteger value) : ValueExpressionNode
{
    public BigInteger value = value;
    public override string ToString() => $"{value}";
}
public class FloatingLiteralNode(double value) : ValueExpressionNode
{
    public double value = value;
    public override string ToString() => value.ToString(CultureInfo.InvariantCulture);
}
public class StringLiteralValueNode(string value) : ValueExpressionNode
{
    public string value = value;
    public override string ToString() => $"\"{value}\"";
}
public class BooleanLiteralValueNode(bool value) : ValueExpressionNode
{
    public bool value = value;
    public override string ToString() => $"{value}";
}
public class RangeLiteralValueNode : ValueExpressionNode
{
    public ValueExpressionNode? start = null;
    public ValueExpressionNode? end = null;
    public ValueExpressionNode? step = null;

    public override string ToString() => $"{start}..{end}" + (step != null ? $" by {step}" : "");
}
