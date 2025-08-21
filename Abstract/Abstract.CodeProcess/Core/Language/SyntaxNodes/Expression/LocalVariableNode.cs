using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

public class LocalVariableNode : ExpressionNode
{
    public bool IsConstant => ((TokenNode)_children[0]).token.type == TokenType.ConstKeyword;
    public TypedIdentifierNode TypedIdentifier => (TypedIdentifierNode)_children[1];
}
