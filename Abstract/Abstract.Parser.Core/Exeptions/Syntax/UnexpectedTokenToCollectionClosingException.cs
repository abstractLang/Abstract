using Abstract.Build.Core.Exceptions;
using Abstract.Parser.Core.Language;

namespace Abstract.Parser.Core.Exeptions.Syntax;

public class UnexpectedTokenToCollectionClosingException(Token token)
    : CompilerException(token.scriptRef, token.Range)
{

    private Token _token = token;

    public override string Message
        => $"Collection Expression should be closed with ] (found \"{_token.value}\")!";

}
