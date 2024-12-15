using Abstract.Parser.Core.Language.SyntaxNodes.Misc;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Control;

public class StructureDeclarationNode : ControlNode
{
    // token 0 = StructKeyword
    public IdentifierNode Identifier => (IdentifierNode)_children[1];
    public bool HasGenericArguments => _children[2] is ParameterCollectionNode;
    public bool HasExtendsImplements => _children[HasGenericArguments ? 3 : 2] is ExtendsImplementsNode;
    public ExtendsImplementsNode? ExtendsImplements
    => HasExtendsImplements ? (ExtendsImplementsNode)_children[HasGenericArguments ? 3 : 2] : null;
    public BlockNode Body
    => (BlockNode)_children[
        HasGenericArguments
        ? (HasExtendsImplements ? 4 : 3)
        : (HasExtendsImplements ? 3 : 2)
    ];
}
