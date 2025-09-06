using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;

public class IRSolvedReference(ExpressionNode origin, LanguageReference refe) : IRReference(origin)
{
    public readonly LanguageReference Reference = refe;
    
    public override string ToString() => Reference.ToString() ?? throw new NotImplementedException();
}
