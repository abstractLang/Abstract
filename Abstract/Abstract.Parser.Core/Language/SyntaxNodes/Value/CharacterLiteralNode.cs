namespace Abstract.Parser.Core.Language.SyntaxNodes.Value;

public class CharacterLiteralNode(Token token, bool insideString = false) : ValueNode(token)
{

    public string Value => token.value;
    public bool insideString = insideString;
    public override string ToString() => insideString ? Value : $"'{Value}'";


    public string BuildCharacter()
    {
        return Value switch
        {
            "\\t" => "\t",
            "\\n" => "\n",
            "\\r" => "\r",
            "\\0" => char.ConvertFromUtf32(0x00),

            _ => throw new NotImplementedException()
        };
    }
}
