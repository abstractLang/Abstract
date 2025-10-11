using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Statement;

public class IfStatementNode : StatementNode
{
    public ExpressionNode Condition => (ExpressionNode)_children[1];
    public SyntaxNode Then => _children[2];
}
