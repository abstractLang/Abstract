using System.Globalization;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Value;

public class NullLiteralNode(Token t) : ValueNode(t)
{
    public override string ToString() => "null";
}
