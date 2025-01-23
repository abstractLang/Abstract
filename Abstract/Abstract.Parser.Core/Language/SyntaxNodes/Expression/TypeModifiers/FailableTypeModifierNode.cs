using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
public class FailableTypeModifierNode : ExpressionNode
{
    public TypeExpressionNode Type => (TypeExpressionNode)_children[1];
}
