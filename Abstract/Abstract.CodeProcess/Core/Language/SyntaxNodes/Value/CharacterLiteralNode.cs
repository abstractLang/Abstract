namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

public class CharacterLiteralNode(Token token, bool insideString = false) : ValueNode(token)
{

    public string Value => token.value.ToString();
    public bool insideString = insideString;
    public override string ToString() => insideString ? Value : $"'{Value}'";

    public string BuildCharacter()
    {
        return Value switch
        {
            // Control
            "\\t" => "\t",
            "\\n" => "\n",
            "\\r" => "\r",
            "\\0" => "\0",

            // Characters
            "\\\"" => "\"",
            "\\\'" => "\'",
            "\\\\" => "\\",

            _ => Value
        };
    }
}
