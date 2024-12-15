namespace Abstract.Parser.Core.Language.SyntaxNodes.Value;

public class TokenNode(Token token) : ValueNode(token)
{

    public string Value => token.value;

    public override string ToString() => $"{Value}";
}
