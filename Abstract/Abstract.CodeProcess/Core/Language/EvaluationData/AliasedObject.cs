namespace Abstract.CodeProcess.Core.Language.EvaluationData;

public class AliasedObject(string[] g, LangObject pointsto) : LangObject(g)
{
    public readonly LangObject pointsTo = pointsto;
}
