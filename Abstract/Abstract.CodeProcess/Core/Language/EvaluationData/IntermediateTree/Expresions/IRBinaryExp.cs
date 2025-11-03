using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;

public class IRBinaryExp(
    BinaryExpressionNode origin,
    IRBinaryExp.Operators ope,
    IRExpression left,
    IRExpression right) : IRExpression(origin, null)
{

    public Operators Operator { get; set; } = ope;
    public IRExpression Left { get; set; } = left;
    public IRExpression Right { get; set; } = right;
    
    public override string ToString()
    {
        var sb = new StringBuilder();

        sb.AppendLine($"({Operator.ToString().ToLower()}");
        sb.AppendLine(Left.ToString().TabAll());
        sb.Append(Right.ToString().TabAll());
        sb.Append(')');
        
        return sb.ToString();
    }
    
    public enum Operators
    {
        Add,
        AddWarpAround,
        AddOnBounds,
        Subtract,
        SubtractWarpAround,
        SubtractOnBounds,
        Multiply,
        Divide,
        DivideFloor,
        DivideCeil,
        Reminder,
        
        Bitwise_And,
        Bitwise_Or,
        Bitwise_Xor,
        Left_Shift,
        Right_Shift,
        
        Logical_And,
        Logical_Or,
    }
}
