using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Statement;

public class ElseStatementNode : StatementNode
{
    public SyntaxNode Then => _children[1];
}
