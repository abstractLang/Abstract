using Abstract.Parser.Core.Language.SyntaxNodes.Base;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Value;

public abstract class ValueNode : ExpressionNode
{
    public ValueNode(Token token)
    {
        this.token = token;
        _children = null!;
    }

    public Token token;

    public override (uint start, uint end) Range => token.Range;

    public override string ToString() => "Unhandled value node";
    public override string ToFancyString() => ToString();
    public override string ToTree() => ToString();
}
