using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Statements;

public class IRElse(SyntaxNode origin, IRBlock then) : IRStatement(origin), IIfElse
{
    public IRBlock Then = then;

    public override string ToString() => Then.ToString();
}
