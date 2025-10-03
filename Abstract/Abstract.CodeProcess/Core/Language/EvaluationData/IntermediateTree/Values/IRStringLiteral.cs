using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;



namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;

public class IRStringLiteral : IRExpression
{
    public string Data;
    public StringEncoding Encoding;

    public IRStringLiteral(SyntaxNode origin, StringEncoding enconding, string data) : base(origin)
    {
        Data = data;
        Encoding = enconding;
    }
    public IRStringLiteral(SyntaxNode origin, string data) : base(origin)
    {
        Data = data;
        Encoding = StringEncoding.Undefined;
    }

    public override string ToString() => $"({Encoding} \"{Data}\")";
}
