using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData;

public class ExecutionContextData(LangObject parent, IRBlock root)
{
    private readonly LangObject _parent = parent;
    private readonly List<IRBlock> _blocks = [root];

    public LangObject Parent => _parent;
    public ParameterObject[] Parameters => _parent switch
    {
        FunctionObject @func => @func.Parameters,
        StructObject @struc => throw new NotImplementedException(),
        _ => []
    };

    public IRBlock CurrentBlock => _blocks[^1];
    
    public void PushBlock(IRBlock block) => _blocks.Add(block);
    public void PopBlock() => _blocks.RemoveAt(_blocks.Count - 1);
    
}
