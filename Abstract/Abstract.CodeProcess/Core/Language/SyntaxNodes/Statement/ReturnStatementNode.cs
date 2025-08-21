using Abstract.Parser.Core.Language.SyntaxNodes.Expression;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Statement;

public class ReturnStatementNode : StatementNode
{
    public bool HasExpression => _children.Count > 1;
    public ExpressionNode Expression => (ExpressionNode)_children[1];
}
