using Abstract.Parser.Core.Language.SyntaxNodes.Value;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Expression;

public class TypedIdentifierNode : ExpressionNode
{
    public TypeExpressionNode Type => (TypeExpressionNode)_children[0];
    public IdentifierNode Identifier => (IdentifierNode)_children[1];
}
