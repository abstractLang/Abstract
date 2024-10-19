using Abstract.Build.Core.Exceptions;
using Abstract.Parser.Core.Language;

namespace Abstract.Parser.Core.Exeptions.Syntax;

public class InvalidCompoundIdentifierException(Token token) : CompilerException(token.scriptRef, token.Range)
{
}
