using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree;

public class IRNode(SyntaxNode origin)
{
    public readonly SyntaxNode Origin = origin;
}