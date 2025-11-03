using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;



namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;

public class IRStringLiteral : IRExpression
{
    public string Data;
    public StringEncoding Encoding => ((StringTypeReference)Type!).Encoding;

    public IRStringLiteral(SyntaxNode origin, StringEncoding enconding, string data)
        : base(origin, new StringTypeReference(enconding))
    {
        Data = data;
    }
    public IRStringLiteral(SyntaxNode origin, string data)
        : base(origin, new StringTypeReference(StringEncoding.Undefined))
    {
        Data = data;
    }

    public override string ToString() => $"({((StringTypeReference)Type!).Encoding} \"{Data}\")";
}
