using System.Numerics;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;

public class IRIntegerLiteral: IRExpression
{

    public readonly byte? Size;
    public readonly BigInteger Value;

    
    public IRIntegerLiteral(SyntaxNode origin, BigInteger val) : base(origin)
    {
        Value = val;
        Size = null;
    }
    public IRIntegerLiteral(SyntaxNode origin, BigInteger val, byte? size) : base(origin)
    {
        Value = val;
        Size = size;
    }
    
    public override string ToString() => $"{Value}";
}
