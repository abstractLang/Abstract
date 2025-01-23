using Abstract.Build.Core.Sources;
using Abstract.Build.Exceptions;
using Abstract.Parser.Core.Language;

namespace Abstract.Parser.Core.Exceptions.Syntax;

public class UnexpectedTokenException : SyntaxException
{

    private Token _unexpectedToken;
    private string? expecting = null;


    public UnexpectedTokenException(Script s, Token t) : base(s, t.Range)
    {
        _unexpectedToken = t;
    }

    public UnexpectedTokenException(Script s, Token t, string expecting) : base(s, t.Range)
    {
        _unexpectedToken = t;
        this.expecting = expecting;
    }

    public override string Message
        => $"Unexpected token {_unexpectedToken}" + (expecting != null ? $". Expecting {expecting}" : "");

}
