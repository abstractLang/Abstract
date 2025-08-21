using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Statement;

public class ElseStatementNode : StatementNode
{
    public bool UsingBlock => _children[1] is BlockNode;
    public StatementNode StatementThen => (StatementNode)_children[1];
    public BlockNode BlockThen => (BlockNode)_children[1];
}
