using System.Numerics;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;

public class IRIntegerLiteral(IntegerLiteralNode origin, BigInteger val) : IRExpression(origin)
{
    public readonly BigInteger Value = val;

    public override string ToString() => $"{Value}";
}