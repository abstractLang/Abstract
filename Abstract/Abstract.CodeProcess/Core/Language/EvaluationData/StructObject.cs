using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData;

public class StructObject(string[] g, StructureDeclarationNode synnode)
    : LangObject(g)
{
    public readonly StructureDeclarationNode syntaxNode = synnode;
}
