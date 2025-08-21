using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Misc;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Control;

public class FunctionDeclarationNode : ControlNode
{
    // token 0 = FuncKeyword
    public TypeExpressionNode ReturnType => (TypeExpressionNode)_children[1];
    public IdentifierNode Identifier => (IdentifierNode)_children[2];
    public ParameterCollectionNode ParameterCollection => (ParameterCollectionNode)_children[3];
    public bool HasBody => _children.Count == 5;
    public BlockNode? Body => HasBody ? (BlockNode)_children[4] : null;
}
