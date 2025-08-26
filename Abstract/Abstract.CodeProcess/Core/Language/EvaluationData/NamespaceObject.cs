using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData;

public class NamespaceObject(string[] g, NamespaceNode synnode)
    : LangObject(g)
{
    public readonly NamespaceNode syntaxNode = synnode;
}
