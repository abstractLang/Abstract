using Abstract.Build.Exceptions;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;

namespace Abstract.Parser.Core.Exceptions.Evaluation;

public class InvalidOperatorForTypeException(BinaryExpressionNode call, string type)
: SyntaxException(
    call.GetSourceScript(),
    call.Range,
    $"type {type} does not have the operator {call.Operator} used in \"{call}\""
)
{
}
