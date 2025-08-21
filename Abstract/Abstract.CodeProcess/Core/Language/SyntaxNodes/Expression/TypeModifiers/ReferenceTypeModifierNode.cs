namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression.TypeModifiers;

public class ReferenceTypeModifierNode : ExpressionNode
{
    public TypeExpressionNode Type => (TypeExpressionNode)_children[1];
}