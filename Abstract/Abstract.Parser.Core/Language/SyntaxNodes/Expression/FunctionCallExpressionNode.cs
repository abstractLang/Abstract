using Abstract.Parser.Core.Language.SyntaxNodes.Expression;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Statement;

public class FunctionCallExpressionNode : ExpressionNode
{
    public override string ToString() => $"{_children[0]}{_children[1]}";
}
