using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Misc;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

public class FunctionDeclarationNode : ControlNode
{
    public IdentifierNode Identifier => (IdentifierNode)_children[1];
    public ParameterCollectionNode ParameterCollection => (ParameterCollectionNode)_children[2];

}
