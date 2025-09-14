using System.Globalization;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

public class FloatingLiteralNode(Token token) : ValueNode(token)
{

    public double Value => double.Parse(token.value.ToString(), CultureInfo.InvariantCulture);

    public override string ToString() => Value.ToString(CultureInfo.InvariantCulture);
}
