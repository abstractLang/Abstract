using Abstract.Parser.Core.Language.SyntaxNodes.Expression;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Control;

public class NamespaceNode : ControlNode
{
    // node 0 = namespaceKeyword
    public IdentifierCollectionNode Identifier => (IdentifierCollectionNode)_children[1];
    public BlockNode Body => (BlockNode)_children[2];

}
