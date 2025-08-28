using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

public class TypeDefinitionItemNode : ControlNode
{
 
    public IdentifierNode Identifier => (IdentifierNode)_children[0];
    
}
