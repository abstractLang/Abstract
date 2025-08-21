namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

public class NullLiteralNode(Token t) : ValueNode(t)
{
    public override string ToString() => "null";
}
