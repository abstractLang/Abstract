using Abstract.Build.Core.Exceptions;
using Abstract.Parser.Core.Language;

namespace Abstract.Parser.Core.Exeptions.Syntax;

internal class IdentifierRequiredException(Token token) : CompilerException(token.scriptRef, token.Range)
{
}
