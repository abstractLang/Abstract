using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression.TypeModifiers;

public class ArrayTypeModifierNode : ExpressionNode
{
    private int RightBraketIndex => _children.FindIndex((e) => e is TokenNode @token && token.Value == "]");

    public TypeExpressionNode Type => (TypeExpressionNode)_children[RightBraketIndex + 1];
    public override string ToString() => string.Join("", _children);
}
