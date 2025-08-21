namespace Abstract.Parser.Core.Language.SyntaxNodes.Value;

public class IdentifierNode(Token token) : ValueNode(token)
{

    public string Value => token.value;

    public override string ToString() => $"{Value}";
}
