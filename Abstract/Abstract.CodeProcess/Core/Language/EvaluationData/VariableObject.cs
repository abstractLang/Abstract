using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData;

public class VariableObject(string[] g, TopLevelVariableNode synnode)
    : LangObject(g)
{
    public readonly TopLevelVariableNode syntaxNode = synnode;
}
