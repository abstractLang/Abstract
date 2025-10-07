using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

public class TypeDefinitionNumericItemNode : TypeDefinitionItemNode
{
 
    public IntegerLiteralNode Value => (IntegerLiteralNode)_children[0];
    
}
