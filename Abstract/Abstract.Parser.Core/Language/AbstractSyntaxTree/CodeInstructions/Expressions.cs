namespace Abstract.Parser.Core.Language.AbstractSyntaxTree;

// Operation expressions
public class AssignmentExpressionNode(ExpressionNode l, string op, ExpressionNode r) : ExpressionNode
{
    public ExpressionNode Left { get; private set; } = l;
    public ExpressionNode Right { get; private set; } = r;
    public string Operator { get; private set; } = op;

    public override string ToString() => $"{Left} {Operator} {Right}";
}
public class BinaryOperationExpressionNode(ExpressionNode l, string op, ExpressionNode r) : ExpressionNode
{
    public ExpressionNode Left { get; private set; } = l;
    public ExpressionNode Right { get; private set; } = r;
    public string Op { get; private set; } = op;

    public override string ToString() => $"({Left} {Op} {Right})";
}
public class UnaryOperationExpressionNode(string op, ExpressionNode ex) : ExpressionNode
{
    public ExpressionNode Expression { get; private set; } = ex;
    public string Op { get; private set; } = op;

    public override string ToString() => $"{Op}{Expression}";
}

public class CollectionExpressionNode : ValueCollectionNode<ExpressionNode>
{
    public override string ToString() => $"[{string.Join(", ", children)}]";
}
