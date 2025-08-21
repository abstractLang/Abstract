using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
public class NullableTypeModifierNode : ExpressionNode
{
    public TypeExpressionNode Type => (TypeExpressionNode)_children[1];
}
