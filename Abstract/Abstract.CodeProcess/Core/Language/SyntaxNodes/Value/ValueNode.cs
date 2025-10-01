using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

public abstract class ValueNode : ExpressionNode
{
    public ValueNode(Token token)
    {
        this.token = token;
        _children = null!;
    }

    public Token token;

    public override (uint line_start, uint line_end, uint start, uint end) Range
        => (token.Range.line, token.Range.line, token.Range.start, token.Range.end);


    public abstract override string ToString();

    public override string ToTree() => ToString();
}
