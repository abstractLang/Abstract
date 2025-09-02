using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

public class TopLevelVariableNode : ControlNode
{
    public bool IsConstant => ((TokenNode)_children[0]).token.type == TokenType.ConstKeyword;
    public IdentifierNode Identifier => ((TypedIdentifierNode)_children[1]).Identifier;

}
