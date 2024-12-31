using Abstract.Parser.Core.Language.SyntaxNodes.Value;
using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Expression;

public class BinaryExpressionNode : ExpressionNode
{
    public ExpressionNode Left => (ExpressionNode)_children[0];
    public string Operator => ((TokenNode)_children[1]).Value;
    public ExpressionNode Right => (ExpressionNode)_children[2];

    public Function? ConvertLeft { get; set; } = null;
    public Function? ConvertRight { get; set; } = null;
    public Function Operate { get; set; } = null!;

    public override string ToFancyString()
    {
        if (Operate == null) return base.ToFancyString();

        var l = ConvertLeft == null ? Left.ToFancyString() : $"{ConvertLeft.identifier}({Left.ToFancyString()})";
        var r = ConvertRight == null ? Right.ToFancyString() : $"{ConvertRight.identifier}({Right.ToFancyString()})";

        return $"{Operate.identifier}({l}, {r})";
    }
}
