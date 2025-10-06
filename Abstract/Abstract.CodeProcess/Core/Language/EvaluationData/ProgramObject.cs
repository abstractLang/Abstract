using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData;

public sealed class ProgramObject(ModuleObject[] modules, NamespaceObject[] nmsps)
{
    public readonly ModuleObject[] Modules = modules;
    public readonly NamespaceObject[] Namespaces = nmsps;
}
