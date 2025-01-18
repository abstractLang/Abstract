using Abstract.Parser.Core.Language.SyntaxNodes.Value;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Misc;

public class StringSectionNode(Token t) : ValueNode(t)
{
    public string Value => t.value;
    public override string ToString() => Value;
}
