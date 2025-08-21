using Abstract.Parser.Core.Language.SyntaxNodes.Control;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Statement;

public class IfStatementNode : StatementNode
{
    public ExpressionNode Condition => (ExpressionNode)_children[1];

    public bool UsingBlock => _children[2] is BlockNode;
    public StatementNode StatementThen => (StatementNode)_children[2];
    public BlockNode BlockThen => (BlockNode)_children[2];
}
