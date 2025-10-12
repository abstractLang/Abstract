using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData;

public class IrBlockExecutionContextData(FunctionObject func)
{
    private List<LocalVariableObject?> _localVariables = [];
    private Stack<int> _stack = [];
    public readonly FunctionObject Function = func;

    public void PushFrame() => _stack.Push(_localVariables.Count);
    public void PopFrame()
    {
        var frame = _stack.Pop();
        _localVariables.RemoveRange(frame, _localVariables.Count - frame);
    }
    
    public void RegisterLocalVariable(LocalVariableObject localVariable)
    {
        localVariable.index = _localVariables.Count;
        _localVariables.Add(localVariable);
    }
}