namespace Abstract.Parser.Core.Language.SyntaxNodes.Value;

public class CharacterLiteralNode(Token token, bool insideString = false) : ValueNode(token)
{

    public string Value => token.value;
    public bool insideString = insideString;
    public override string ToString() => insideString ? Value : $"'{Value}'";

}
