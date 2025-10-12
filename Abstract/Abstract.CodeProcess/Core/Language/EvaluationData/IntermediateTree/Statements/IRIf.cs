using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Statements;

public class IRIf(SyntaxNode origin, IRExpression exp, IRBlock then) : IRStatement(origin), IIfElse
{
    public IRExpression Condition = exp;
    public IRBlock Then = then;
    public IIfElse? Else = null;
}
