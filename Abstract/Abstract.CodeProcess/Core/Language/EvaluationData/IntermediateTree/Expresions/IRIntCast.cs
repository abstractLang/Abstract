using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.Integer;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;

public class IRIntCast(SyntaxNode origin, IRExpression v, IntegerTypeReference ty) : IRExpression(origin, ty)
{
    public IRExpression Expression = v;

    
    public override string ToString() => $"(intcast {Expression} {Type})";
}