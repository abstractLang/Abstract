using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;

public class IRUnaryExp(SyntaxNode origin, IRUnaryExp.UnaryOperation op, IRExpression value) : IRExpression(origin)
{
    public UnaryOperation Operation = op;
    public IRExpression Value = value;
    
    public TypeReference? ResultType { get; set; } = null;
    
    public enum UnaryOperation
    {
        Plus,
        Minus,
        Not,
        
        Reference,
        
        PreIncrement, PostIncrement,
        PreDecrement, PostDecrement,
    }

    public override string ToString() => Operation switch
    {
        UnaryOperation.Plus => $"+{value}",
        UnaryOperation.Minus => $"-{value}",
        UnaryOperation.Not => $"!{value}",
        
        UnaryOperation.Reference => $"&{value}",
        
        UnaryOperation.PreIncrement => $"++{value}",
        UnaryOperation.PostIncrement => $"{value}++",
        
        UnaryOperation.PreDecrement => $"--{value}",
        UnaryOperation.PostDecrement => $"{value}--",
        
        _ => throw new ArgumentOutOfRangeException()
    };
}