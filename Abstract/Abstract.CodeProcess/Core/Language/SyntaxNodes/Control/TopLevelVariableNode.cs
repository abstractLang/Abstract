using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Control;

public class TopLevelVariableNode : ControlNode
{

    public IdentifierNode Identifier => ((TypedIdentifierNode)_children[1]).Identifier;

}
