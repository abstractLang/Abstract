using System.Numerics;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

public class IntegerLiteralNode(Token token) : ValueNode(token)
{

    public BigInteger Value => BigInteger.Parse(token.value);

    public override string ToString() => $"{Value}";
}
