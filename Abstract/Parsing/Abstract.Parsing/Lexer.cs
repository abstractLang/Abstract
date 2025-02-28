using System.Text;
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

        while (index < text.Length)
        {
            // DoubleCharacterToken:
            if (text.Length - index < 2) goto SingleCharacterToken;
            var c2 = textSpan.Slice(index, 2).ToString();

            if (c2 == "\r\n") AppendToken(tokens, text, triviaBegin, index, index + 2, TokenKind.statement_end); // Windows standard
            else goto SingleCharacterToken;

            index += 2; triviaBegin = index; continue;

            // ------------------------------------------------------------------------------- //
            SingleCharacterToken:
            var c1 = textSpan[index];

                 if (c1 == '\n')   AppendToken(tokens, text, triviaBegin, index, TokenKind.statement_end); // Linux standard
            else if (c1 == '\r')   AppendToken(tokens, text, triviaBegin, index, TokenKind.statement_end); // MacOS standard
            else if (c1 == ';')    AppendToken(tokens, text, triviaBegin, index, TokenKind.statement_end); // Statement break

            else if (c1 == '.')    AppendToken(tokens, text, triviaBegin, index, TokenKind.char_dot);

            // operators
            else if (c1 == '+')    AppendToken(tokens, text, triviaBegin, index, TokenKind.char_cross);
            else if (c1 == '-')    AppendToken(tokens, text, triviaBegin, index, TokenKind.char_dash);
            else if (c1 == '/')    AppendToken(tokens, text, triviaBegin, index, TokenKind.char_slash);
            else if (c1 == '*')    AppendToken(tokens, text, triviaBegin, index, TokenKind.char_star);

            // opens and closings
            else if (c1 == '(') AppendToken(tokens, text, triviaBegin, index, TokenKind.char_open_parenthesis);
            else if (c1 == ')') AppendToken(tokens, text, triviaBegin, index, TokenKind.char_close_parenthesis);

            else goto StringToken;

            index += 1; triviaBegin = index; continue;

            // ------------------------------------------------------------------------------- //
            StringToken:
            var i = 0;
            
            // Check if string
            if (textSpan[index] == '"') // TODO string interpolation
            {
                for (i = index + 1; i < text.Length; i++) if (textSpan[i] == '"') { i += 1;  break; }
                AppendToken(tokens, text, triviaBegin, index, i, TokenKind.literal_string);
                index = i; triviaBegin = i; continue;
            }

            // Check if char
            if (textSpan[index] == '\'')
            {
                for (i = index + 1; i < text.Length; i++) if (textSpan[i] == '\'') { i += 1; break; }
                AppendToken(tokens, text, triviaBegin, index, i, TokenKind.literal_char);
                index = i; triviaBegin = i; continue;
            }

            // Check if comment
            if (textSpan[index] == '#')
            {
                if (text.Length - index > 3 && textSpan[index..(index+3)].ToString() == "###")
                {
                    for (i = index + 1; i+3 < text.Length; i++) if (textSpan[i..(i+3)].ToString() == "###") break;
                }
                else for (i = index + 1; i < text.Length; i++)
                    if ((text.Length - i > 2 && textSpan[i..(i+1)] == "\r\n") || textSpan[i] == '\n' || textSpan[i] == '\r') break;

                index = i; continue; // included on trivia
            }

            // Try parse identifier
            if (!char.IsAsciiLetterOrDigit(textSpan[index]) && textSpan[index] != '_') goto Skip;
            for (i = index + 1; i < text.Length; i++) if (!char.IsAsciiLetterOrDigit(textSpan[i]) && textSpan[i] != '_') break;
            AppendToken(tokens, text, triviaBegin, index, i, TokenKind.identifier);

            index = i; triviaBegin = index; continue;

            Skip:
            index++; continue;
        }


        var str = new StringBuilder();
        foreach (var i in tokens)
        {
            var t = i.trivia.ToString().Replace("\n", "\\n").Replace("\r", "\\r").Replace("\t", "\\t");
            var v = i.value.ToString().Replace("\n", "\\n").Replace("\r", "\\r").Replace("\t", "\\t");

            if (t.Length > 50) t = t[..45] + " ...";

            str.AppendLine($"|{t}|{new string('.', Math.Max(0, 49 - t.Length))}|{v}|{new string('.', 49 - v.Length)}| {i.kind}");
        }

        File.WriteAllText("tokens.txt", str.ToString());

        return [];
    }

    private static void AppendToken(List<Token> tknlist, ReadOnlyMemory<char> src, int triviabegin, int tokenbegin,  int tokenend, Token.Kind kind)
    {
        var tkn = new Token(
            new(src[triviabegin .. tokenbegin], triviabegin),
            new(src[tokenbegin .. tokenend],
            tokenbegin), kind);

        if (tknlist.Count > 0)
        {
            tknlist[^1].after = tkn;
            tkn.before = tknlist[^1];
        }
        tknlist.Add(tkn);
    }
    private static void AppendToken(List<Token> tknlist, ReadOnlyMemory<char> src, int triviabegin, int tokenbegin, Token.Kind kind)
        => AppendToken(tknlist, src, triviabegin, tokenbegin, tokenbegin + 1, kind);

}
