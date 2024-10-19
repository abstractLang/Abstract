using Abstract.Build.Core.Exceptions;
using Abstract.Parser.Core.Language;

namespace Abstract.Parser.Core.Exeptions.Syntax;

public class UnexpectedTokenException (Token token, string? message) : CompilerException(token.scriptRef, token.Range, message)
{
}
