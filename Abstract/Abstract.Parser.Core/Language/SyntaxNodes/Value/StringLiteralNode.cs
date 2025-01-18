using Abstract.Parser.Core.Language.SyntaxNodes.Base;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Misc;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Value;

public class StringLiteralNode() : ExpressionNode()
{
    public SyntaxNode[] Content => [.. _children[1..^1]];

    public bool isSinple => !_children[1..^1].Any(e => e is StringInterpolationNode);
    public string RawContent => string.Join("", (object[])Children[1..^1]);

    public override string ToString() => $"\"{RawContent}\"";
}
