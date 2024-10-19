using Abstract.Build.Core.Exceptions;
using Abstract.Parser.Core.Language;

namespace Abstract.Parser.Core.Exeptions.Syntax;

public class InvalidIdentifierTokenException(Token token) : CompilerException(token.scriptRef, token.Range)
{
    private Token _token = token;
    public override string Message => $"{_token.type} is not valid as a identifier!";

}
