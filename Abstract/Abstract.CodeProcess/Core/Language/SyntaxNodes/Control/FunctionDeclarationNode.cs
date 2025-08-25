using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Misc;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

public class FunctionDeclarationNode : ControlNode
{
    public IdentifierNode Identifier => (IdentifierNode)_children[1];
    public ParameterCollectionNode ParameterCollection => (ParameterCollectionNode)_children[2];
    public TypeExpressionNode? ReturnType => HasType ? (TypeExpressionNode)_children[3] : null;
    
    public bool HasType => _children.Count > 3 && _children[4] is not BlockNode;
    public bool HasBody => HasType ? _children.Count == 5 : _children.Count == 4;
    public BlockNode? Body => HasBody ? (BlockNode)_children[HasType ? 4 : 3] : null;
}
