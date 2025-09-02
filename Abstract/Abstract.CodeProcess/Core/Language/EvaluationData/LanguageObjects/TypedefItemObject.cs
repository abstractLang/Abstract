using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class TypedefItemObject(string[] g, TypeDefinitionItemNode synnode)
    : LangObject(g)
{
    public readonly TypeDefinitionItemNode syntaxNode = synnode;

    public override string ToString() => $"{syntaxNode}";
}
