using System.Numerics;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.Integer;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;

public class IRIntegerLiteral(SyntaxNode origin, BigInteger val, IntegerTypeReference ty): IRExpression(origin, ty)
{
    public readonly BigInteger Value = val;
    public ushort? Size => (Type as RuntimeIntegerTypeReference)?.BitSize ?? 0;
    
    public override string ToString() => $"{Value}";
}
