using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Statement;

public class ReturnStatementNode : StatementNode
{
    public bool HasExpression => _children.Count > 1;
    public ExpressionNode Expression => (ExpressionNode)_children[1];
}
