namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;

public interface IExternModifier
{
    public (string? domain, string? symbol) Extern { get; set; }
}
