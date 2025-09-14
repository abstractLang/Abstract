using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Misc;

public class StringSectionNode(Token tkn) : ValueNode(tkn)
{
    public string Value => token.value.ToString();
    public override string ToString() => Value;
}
