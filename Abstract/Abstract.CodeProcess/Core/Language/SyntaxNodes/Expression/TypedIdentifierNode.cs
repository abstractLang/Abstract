using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

public class TypedIdentifierNode : ExpressionNode
{
    public ExpressionNode Type => (ExpressionNode)_children[0];
    public IdentifierNode Identifier => (IdentifierNode)_children[1];
}
