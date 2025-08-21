namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression.TypeModifiers;

public class GenericCollectionExpressionNode : ExpressionNode
{
    public ExpressionNode[] Items => [.. _children[1..^1].Select(e => (ExpressionNode)e)];
}
