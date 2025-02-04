using Abstract.Parser.Core.Language.SyntaxNodes.Value;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Misc;

public class StringSectionNode(Token tkn) : ValueNode(tkn)
{
    public string Value => token.value;
    public override string ToString() => Value;
}
