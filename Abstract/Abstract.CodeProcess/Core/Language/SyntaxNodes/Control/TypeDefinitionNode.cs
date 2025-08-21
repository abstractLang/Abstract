using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

public class TypeDefinitionNode : ControlNode
{
    public IdentifierNode Identifier => (IdentifierNode)_children[1];
}
