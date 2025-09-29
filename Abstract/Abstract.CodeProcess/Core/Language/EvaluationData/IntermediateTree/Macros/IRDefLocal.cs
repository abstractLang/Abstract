using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Macros;

public class IRDefLocal(SyntaxNode origin, LocalVariableObject localVar) : IRMacro(origin)
{
    public readonly LocalVariableObject LocalVariable = localVar;
    public override string ToString() => $"$DEFINE_LOCAL {LocalVariable.Name} {LocalVariable.Type}";
}