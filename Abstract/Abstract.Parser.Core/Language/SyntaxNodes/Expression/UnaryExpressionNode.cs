namespace Abstract.Parser.Core.Language.SyntaxNodes.Expression;

public class UnaryExpressionNode : ExpressionNode
{
    public override string ToString() => $"{_children[0]}{_children[1]}";
}
