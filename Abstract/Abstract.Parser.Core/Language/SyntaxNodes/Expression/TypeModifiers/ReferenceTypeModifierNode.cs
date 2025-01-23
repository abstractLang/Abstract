using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
public class ReferenceTypeModifierNode : ExpressionNode
{
    public TypeExpressionNode Type => (TypeExpressionNode)_children[1];
}
