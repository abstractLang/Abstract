using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData;

public class TypedefObject(string[] g, TypeDefinitionNode synnode)
    : LangObject(g)
{
    public readonly TypeDefinitionNode syntaxNode = synnode;
}
