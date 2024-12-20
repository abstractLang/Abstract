using Abstract.Parser.Core.Language.SyntaxNodes.Control;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Misc;

namespace Abstract.Parser.Core.ProgMembers;

public sealed class MemberAttribute(AttributeNode node)
{
    public readonly AttributeNode node = node;

    public MemberIdentifier Name
        => new(((IdentifierCollectionNode)node.Children.ToArray()[1]).ToString());
    public ArgumentCollectionNode? ArgumentsNode
        => node.Children.ToArray().Length > 2
        ? (ArgumentCollectionNode)node.Children.ToArray()[2]
        : null;
    public ExpressionNode[] Arguments
        => ArgumentsNode?.Arguments ?? [];
}
