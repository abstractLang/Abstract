namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

public class BooleanLiteralNode(Token token) : ValueNode(token)
{

    public bool Value => token.value.ToString() == "true";

    public override string ToString() => $"{Value}";
}
