using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

public class TypeDefinitionNamedItemNode : TypeDefinitionItemNode
{ 
    public IdentifierNode Identifier => (IdentifierNode)_children[0];
    
}
