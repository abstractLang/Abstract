namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

public class ParenthesisExpressionNode : ExpressionNode
{
    public ExpressionNode Content => (ExpressionNode)_children[1];
    public override string ToString() => $"({(ExpressionNode)_children[1]})";
}
