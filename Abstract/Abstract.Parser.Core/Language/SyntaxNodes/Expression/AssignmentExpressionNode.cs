namespace Abstract.Parser.Core.Language.SyntaxNodes.Expression;

public class AssignmentExpressionNode : BinaryExpressionNode
{

    public override string ToFancyString()
    {
        if (!evaluated) return base.ToString();
        
        var l = Left;
        var r = ConvertRight != null ? $"{ConvertRight.identifier}({Right.ToFancyString()})" : Right.ToFancyString();

        return $"{l} {Operator} {r}";
    }

}
