using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FunctionReferences;

public class SolvedFunctionGroupReference(FunctionGroupObject fun) : FunctionReference
{
    public readonly FunctionGroupObject FunctionGroup = fun;

    public override string ToString() => $"FGr<{string.Join('.', FunctionGroup.Global)}>";
}
