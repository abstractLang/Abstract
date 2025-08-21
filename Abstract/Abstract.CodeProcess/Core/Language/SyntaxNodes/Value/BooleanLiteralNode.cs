namespace Abstract.Parser.Core.Language.SyntaxNodes.Value;

public class BooleanLiteralNode(Token token) : ValueNode(token)
{

    public bool Value => token.value == "true";

    public override string ToString() => $"{Value}";
}
