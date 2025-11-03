using System.Text;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;

public class FunctionTypeReference(TypeReference a, TypeReference[] b) : BuiltInTypeReference
{
    public readonly TypeReference Returns = a;
    public readonly TypeReference[] Parameters = b;

    public override string ToString() => $"fn({string.Join(", ", Parameters.Select(e => e.ToString()))}) {Returns}";
}