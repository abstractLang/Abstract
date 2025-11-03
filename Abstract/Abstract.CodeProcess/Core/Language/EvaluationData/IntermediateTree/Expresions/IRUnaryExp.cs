using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;

public class IRUnaryExp(SyntaxNode origin, IRUnaryExp.UnaryOperation op, IRExpression value) : IRExpression(origin, value.Type)
{
    public UnaryOperation Operation = op;
    public IRExpression Value = value;
    
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
        UnaryOperation.Plus => $"+{Value}",
        UnaryOperation.Minus => $"-{Value}",
        UnaryOperation.Not => $"!{Value}",
        
        UnaryOperation.Reference => $"&{Value}",
        
        UnaryOperation.PreIncrement => $"++{Value}",
        UnaryOperation.PostIncrement => $"{Value}++",
        
        UnaryOperation.PreDecrement => $"--{Value}",
        UnaryOperation.PostDecrement => $"{Value}--",
        
        _ => throw new ArgumentOutOfRangeException()
    };
}