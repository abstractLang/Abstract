using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FunctionReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;

public class IRNewObject(SyntaxNode origin, TypeReference t, IRExpression[] args) : IRExpression(origin)
{
    public readonly TypeReference Type = t;
    public FunctionReference? Function { get; set; } = null;
    public IRExpression[] Arguments { get; set; } = args;
    public readonly List<IRAssign> InlineAssignments = [];
    
}