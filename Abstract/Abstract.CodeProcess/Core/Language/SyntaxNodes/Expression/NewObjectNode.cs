using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Misc;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

public class NewObjectNode: ExpressionNode
{
    public ExpressionNode Type => (ExpressionNode)Children[1];
    public ExpressionNode[] Arguments => ((ArgumentCollectionNode)Children[2]).Arguments;
    public BlockNode? Inlined => Children.Length == 4 ? (BlockNode)Children[3] : null;
}