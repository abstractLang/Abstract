namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class AliasedObject(string[] g, string n, LangObject pointsto) : LangObject(g, n)
{
    public readonly LangObject pointsTo = pointsto;

    public override string ToString()
    {
        return $"Alias  '{Name}' ('{string.Join('.', Global)}') -> '{string.Join('.', pointsTo.Global)}'";
    }
}
