using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

public class TopLevelVariableNode : ControlNode
{

    public IdentifierNode Identifier => ((TypedIdentifierNode)_children[1]).Identifier;

}
