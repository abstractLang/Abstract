using System.Numerics;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;

public class IRIntegerLiteral: IRExpression
{

    public readonly int? Size;
    public readonly bool PtrSized;  
    public readonly BigInteger Value;

    
    public IRIntegerLiteral(SyntaxNode origin, BigInteger val) : base(origin)
    {
        Value = val;
        Size = null;
        PtrSized = false;
    }
    public IRIntegerLiteral(SyntaxNode origin, BigInteger val, int? size) : base(origin)
    {
        Value = val;
        Size = size;
        PtrSized = !size.HasValue;
    }
    
    public override string ToString() => $"{Value}";
}
