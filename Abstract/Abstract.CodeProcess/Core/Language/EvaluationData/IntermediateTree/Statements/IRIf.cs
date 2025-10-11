using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Statements;

public class IRIf(SyntaxNode origin, IRBlock then) : IRStatement(origin), IIfElse
{
    public IRBlock Then = then;
    public IIfElse? Else = null;
}
