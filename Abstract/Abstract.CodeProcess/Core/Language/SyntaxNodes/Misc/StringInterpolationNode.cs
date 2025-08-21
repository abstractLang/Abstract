using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Misc;

public class StringInterpolationNode : ExpressionNode
{
    public ExpressionNode Expression => (ExpressionNode)_children[1];
    public override string ToString() => $"\\{{{Expression}}}";
}
