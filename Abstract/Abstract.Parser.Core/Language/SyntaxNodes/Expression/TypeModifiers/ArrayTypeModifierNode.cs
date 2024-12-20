using Abstract.Parser.Core.Language.SyntaxNodes.Value;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Expression.TypeModifiers;

public class ArrayTypeModifierNode : ExpressionNode
{
    private int LeftBraketIndex => _children.FindIndex((e) => e is TokenNode @token && token.Value == "]");

    public TypeExpressionNode Type => (TypeExpressionNode)_children[LeftBraketIndex + 1];
}
