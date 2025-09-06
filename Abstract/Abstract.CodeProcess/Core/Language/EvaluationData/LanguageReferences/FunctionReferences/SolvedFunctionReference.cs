using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FunctionReferences;

public class SolvedFunctionReference(FunctionObject fun) : FunctionReference
{
    public readonly FunctionObject Function = fun;

    public override string ToString() => $"Fun<{string.Join('.', Function.Global)}>";
}