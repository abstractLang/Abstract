using Abstract.Parser.Core.Language.SyntaxNodes.Expression;

namespace Abstract.Parser.Core.ProgData;

public struct ExpressionReference(ExpressionNode node) : IGenericExpression
{
    public readonly ExpressionNode node = node;
}
