using Abstract.Build.Core.Sources;
using Abstract.Build.Exceptions;
using Abstract.Parser.Core.Language;

namespace Abstract.Parser.Core.Exceptions.Syntax;

public class UnexpectedTokenException(Script s, Token t) : SyntaxException(s, t.Range)
{

    private Token _unexpectedToken = t;

    public override string Message
        => $"Unexpected token {_unexpectedToken}";

}
