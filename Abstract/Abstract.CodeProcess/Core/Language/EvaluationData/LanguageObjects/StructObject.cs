using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class StructObject(string[] g, StructureDeclarationNode synnode)
    : LangObject(g)
{
    public readonly StructureDeclarationNode syntaxNode = synnode;
}
