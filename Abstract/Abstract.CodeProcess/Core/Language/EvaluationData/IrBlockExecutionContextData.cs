using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData;

public class IrBlockExecutionContextData(LangObject obj)
{
    public List<LocalVariableObject?> LocalVariables = [];
    private Stack<int> _stack = [];
    public readonly LangObject Parent = obj;

    public void PushFrame() => _stack.Push(LocalVariables.Count);
    public void PopFrame()
    {
        var frame = _stack.Pop();
        LocalVariables.RemoveRange(frame, LocalVariables.Count - frame);
    }
    
    public void RegisterLocalVariable(LocalVariableObject localVariable)
    {
        localVariable.index = LocalVariables.Count;
        LocalVariables.Add(localVariable);
    }
}