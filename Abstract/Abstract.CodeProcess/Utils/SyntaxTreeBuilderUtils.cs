using Abstract.Build.Core.Exceptions;
using Abstract.Parser.Core.Exeptions.Syntax;
using Abstract.Parser.Core.Language;

namespace Abstract.Parser;

public partial class SyntaxTreeBuilder
{

    private Queue<Token> tkns = null!;

    public delegate CompilerException Callback(Token token);

    private Token Eat() => tkns.Dequeue();
    private bool TryEat(TokenType type)
    {
        if (tkns.Peek().type == type)
        {
            tkns.Dequeue();
            return true;
        }
        return false;
    }
    private bool TryEat(TokenType type, out Token tkn)
    {
        if (tkns.Peek().type == type)
        {
            tkn = tkns.Dequeue();
            return true;
        }
        tkn = new();
        return false;
    }
    private Token Diet(TokenType type, Callback? err = null)
    {
        var tkn = tkns.Dequeue();
        if (tkn.type != type)
        {
            throw err?.Invoke(tkn) ??
            new UnexpectedTokenException(tkn, $"Unexpected token \"{tkn.value}\" of type {tkn.type} (expecting {type})");
        }
        return tkn;
    }
    private void Reject(TokenType type, Callback? err = null)
    {
        var tkn = Bite();
        if (tkn.type == type)
        {
            throw err?.Invoke(tkn) ??
            new UnexpectedTokenException(tkn, $"Unexpected token \"{tkn.value}\" of type {tkn.type}!");
        }
    }
    private Token Bite() => tkns.Peek();
    private bool NextIs(params TokenType[] type) => type.Contains(tkns.Peek().type);

    private bool IsEOF() => tkns.Peek().type == TokenType.EOFChar;
    private bool IsEndOfStatement() => tkns.Peek().type == TokenType.EOFChar || tkns.Peek().type == TokenType.LineFeedChar;
}
