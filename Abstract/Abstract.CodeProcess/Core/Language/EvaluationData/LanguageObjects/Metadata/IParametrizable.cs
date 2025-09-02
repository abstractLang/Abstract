using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Metadata;

public interface IParametrizable
{
    public ParameterObject[] Parameters { get; }
}
