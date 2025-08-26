using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData;

public class FunctionObject(string[] g, FunctionDeclarationNode synnode)
    : LangObject(g)
{
    public readonly FunctionDeclarationNode syntaxNode = synnode;
}
