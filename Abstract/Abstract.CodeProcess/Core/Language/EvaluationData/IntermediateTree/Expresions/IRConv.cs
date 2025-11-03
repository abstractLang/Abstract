using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.Integer;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;

public class IrConv(SyntaxNode origin, IRExpression v, TypeReference ty) : IRExpression(origin, ty)
{
    public IRExpression Expression = v;

    public override string ToString() => $"(conv {Expression} {Type})";
}