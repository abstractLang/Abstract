using System.Text;
using Abstract.Parser.Core.Language.SyntaxNodes.Base;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Misc;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Value;

public class StringLiteralNode() : ExpressionNode()
{
    public SyntaxNode[] Content => [.. _children[1..^1]];

    public bool isSimple => !_children[1..^1].Any(e => e is StringInterpolationNode);
    public string RawContent => string.Join("", (object[])Children[1..^1]);

    public override string ToString() => $"\"{RawContent}\"";
    public string BuildStringContent()
    {
        var str = new StringBuilder();

        foreach (var i in _children[1..^1])
        {
            if (i is StringSectionNode @sec) str.Append(sec.Value);
            else if (i is CharacterLiteralNode @charr) str.Append(charr.BuildCharacter());
            else throw new InvalidOperationException(i.ToString());
        }

        return str.ToString();
    }
}
