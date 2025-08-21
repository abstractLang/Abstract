using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Statement;

public class IfStatementNode : StatementNode
{
    public ExpressionNode Condition => (ExpressionNode)_children[1];

    public bool UsingBlock => _children[2] is BlockNode;
    public StatementNode StatementThen => (StatementNode)_children[2];
    public BlockNode BlockThen => (BlockNode)_children[2];
}
