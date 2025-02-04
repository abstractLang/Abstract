using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Misc;
using Abstract.Parser.Core.ProgData.FunctionExecution;
using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Statement;

public class FunctionCallExpressionNode : ExpressionNode
{
    public IdentifierCollectionNode FunctionReference => (IdentifierCollectionNode)_children[0];
    public ExpressionNode[] Arguments => EvalArguments ?? ((ArgumentCollectionNode)_children[1]).Arguments;

    // post-evaluated data
    public Function Target { get; set; } = null!;
    public ExpressionNode[] EvalArguments { get; set; } = null!;

    public override string ToString() => (Target != null && EvalArguments != null)
    ? $"{Target.GlobalReference}({string.Join(", ", (object[])EvalArguments)})"
    : $"{_children[0]}{_children[1]}";
}
