namespace Abstract.Parser.Core.Language.SyntaxNodes.Expression;

public class TypeExpressionNode : ExpressionNode
{
    public override string ToString() => string.Join("", _children);
}
