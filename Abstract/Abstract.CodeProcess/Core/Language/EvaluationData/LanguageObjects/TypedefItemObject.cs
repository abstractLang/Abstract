using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class TypedefItemObject(string[] g, string n, TypeDefinitionItemNode synnode)
    : LangObject(g, n)
{
    public readonly TypeDefinitionItemNode syntaxNode = synnode;

    public override string ToString() => $"{syntaxNode}";
}
