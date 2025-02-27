using Abstract.Core.Language;
using TokenKind = Abstract.Core.Language.Token.Kind;

namespace Abstract.Parsing;

public static class Lexer
{
    private static readonly Dictionary<string, TokenKind> _tokenKindMap = new()
    {

    };

    public static Token[] LexText(ReadOnlyMemory<char> text)
    {
        var textSpan = text.Span;
        int index = 0;

        List<Token> tokens = [];

        int triviaBegin = 0;
        int triviaEnd = 0;

        while (index < text.Length)
        {
            // DoubleCharacterToken:
            if (text.Length - index <= 2) goto SingleCharacterToken;
            var c2 = textSpan.Slice(index, index + 2);

                 if (c2 == "\r\n") AppendToken(tokens, text, triviaBegin, triviaEnd, index, ++index, TokenKind.StatementEnd); // Windows standard
            else goto SingleCharacterToken;

            index += 2; continue;

            // ------------------------------------------------------------------------------- //
            SingleCharacterToken:
            var c1 = textSpan[index];

                 if (c1 == '\n')   AppendToken(tokens, text, triviaBegin, triviaEnd, index,          TokenKind.StatementEnd); // Linux standard
            else if (c1 == '\r')   AppendToken(tokens, text, triviaBegin, triviaEnd, index,          TokenKind.StatementEnd); // MacOS standard
            else goto StringToken;

            index += 1; continue;

            // ------------------------------------------------------------------------------- //
            StringToken:
            // TODO

            throw new NotImplementedException(textSpan[index..].ToString());
        }

        for (int i = 0; i < 100; i++)
            Console.WriteLine(tokens.Count);

        return [];
    }

    private static void AppendToken(
        List<Token> tknlist,
        ReadOnlyMemory<char> src,
        int triviabegin, int triviaend,
        int tokenbegin,  int tokenend,
        Token.Kind kind
    )
    {
        var tkn = new Token(
            new(src.Slice(triviabegin, triviaend - triviabegin), triviabegin),
            new(src.Slice(tokenbegin, tokenend - tokenbegin),
            tokenbegin), kind);
        
        tknlist[^1].after = tkn;
        tkn.before = tknlist[^1];
        tknlist.Add(tkn);
    }

    private static void AppendToken(
        List<Token> tknlist,
        ReadOnlyMemory<char> src,
        int triviabegin, int triviaend,
        int tokenbegin,
        Token.Kind kind
    )
    {
        var tkn = new Token(
            new(src.Slice(triviabegin, triviaend - triviabegin), triviabegin),
            new(src.Slice(tokenbegin, 1),
            tokenbegin), kind);

        if (tknlist.Count > 0)
        {
            tknlist[^1].after = tkn;
            tkn.before = tknlist[^1];
        }
        tknlist.Add(tkn);
    }

}
