using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData;

public class IrBlockExecutionContextData(FunctionObject func)
{
    private List<LocalVariableObject?> _localVariables = [];
    public readonly FunctionObject Function = func;
    
    public void RegisterLocalVariable(LocalVariableObject localVariable)
    {
        localVariable.index = _localVariables.Count;
        _localVariables.Add(localVariable);
    }
}