using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData;

public class IrBlockExecutionContextData
{
    private List<LocalVariableObject?> _localVariables = [];

    public void RegisterLocalVariable(LocalVariableObject localVariable)
    {
        localVariable.index = _localVariables.Count;
        _localVariables.Add(localVariable);
    }
}