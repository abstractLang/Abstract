using Abstract.Build.Exceptions;
using Abstract.Parser.Core.Language.SyntaxNodes.Statement;

namespace Abstract.Parser.Core.Exceptions.Evaluation;

public class LocalAlreadyDeclaratedException(LocalVariableNode local, string identifier)
: SyntaxException(
    local.GetSourceScript(),
    local.Range,
    $"Local variable {local} cannot be declared because a reference named \"{identifier}\" is already declarated in this scope"
)
{
}