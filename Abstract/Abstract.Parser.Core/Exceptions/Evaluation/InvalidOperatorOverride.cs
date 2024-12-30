using Abstract.Build.Exceptions;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;

namespace Abstract.Parser.Core.Exceptions.Evaluation;

public class InvalidOperatorOverride(
    BinaryExpressionNode call,
    string typeLeft,
    string typeRight
)
: SyntaxException(
    call.GetSourceScript(),
    call.Range,
    $"operator {call.Operator} in base {typeLeft} cannot be used with type {typeRight} used in \"{call}\""
)
{
}
