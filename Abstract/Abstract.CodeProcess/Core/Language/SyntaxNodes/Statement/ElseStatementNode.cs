using Abstract.Parser.Core.Language.SyntaxNodes.Base;
using Abstract.Parser.Core.Language.SyntaxNodes.Control;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Statement;

public class ElseStatementNode : StatementNode
{
    public bool UsingBlock => _children[1] is BlockNode;
    public StatementNode StatementThen => (StatementNode)_children[1];
    public BlockNode BlockThen => (BlockNode)_children[1];
}
