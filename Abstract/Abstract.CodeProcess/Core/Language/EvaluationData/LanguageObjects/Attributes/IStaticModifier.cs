namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public interface IPublicModifier
{
    
    public bool Public { get; set; }
    public bool Static { get; set; }
    public bool Internal { get; set; }
    public bool Abstract { get; set; }

    
}
