namespace Abstract.Parser.Core.Language.SyntaxNodes.Value;

public class StringLiteralNode(Token token) : ValueNode(token)
{
    public string Value => token.value;

    public override string ToString() => $"\"{Value}\"";
}
