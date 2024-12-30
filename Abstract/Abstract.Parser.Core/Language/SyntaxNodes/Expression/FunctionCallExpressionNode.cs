using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Misc;
using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Statement;

public class FunctionCallExpressionNode : ExpressionNode
{
    public IdentifierCollectionNode FunctionReference => (IdentifierCollectionNode)_children[0];
    public ExpressionNode[] Arguments => ((ArgumentCollectionNode)_children[1]).Arguments;

    // post-evaluated data
    public Function FunctionTarget { get; set; } = null!;

    public override string ToString() => $"{_children[0]}{_children[1]}";
}
