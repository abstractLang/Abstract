using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData;

public sealed class ProgramObject(NamespaceObject[] nmsps)
{
    public NamespaceObject[] Namespaces { get; internal set; } = nmsps;
}
