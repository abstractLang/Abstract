using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Control;

public class FunctionDeclarationNode : ControlNode
{
    // token 0 = FuncKeyword
    public TypeExpressionNode ReturnType => (TypeExpressionNode)_children[1];
    public IdentifierNode Identifier => (IdentifierNode)_children[2];
    public BlockNode Body => (BlockNode)_children[3];
}
