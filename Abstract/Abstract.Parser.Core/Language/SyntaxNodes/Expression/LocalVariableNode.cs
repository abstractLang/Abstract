using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Statement;

public class LocalVariableNode : ExpressionNode
{
    public bool IsConstant => ((TokenNode)_children[0]).token.type == TokenType.ConstKeyword;
    public TypedIdentifierNode TypedIdentifier => (TypedIdentifierNode)_children[1];
}
