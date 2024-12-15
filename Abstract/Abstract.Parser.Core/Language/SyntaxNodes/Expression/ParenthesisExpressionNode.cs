namespace Abstract.Parser.Core.Language.SyntaxNodes.Expression;

public class ParenthesisExpressionNode : ExpressionNode
{
    public override string ToString() => $"({string.Join(" ", _children[1..^1])})";
}
