using Abstract.Compiler.Build.Core.Resources;
using Abstract.Compiler.Parser.Core;

namespace Abstract.Compiler.Parser;

public static class Tokenizer
{
    
    private static readonly Dictionary<string, TokenKind[]> LineJunctions = new()
    {
        { "justLeft",  [
            TokenKind.LeftBracketChar,
            TokenKind.RightParenthesisChar
        ] },
        { "justRight", [
            TokenKind.LeftPerenthesisChar,
            TokenKind.AtSiginChar,
        ] },
        { "bothSides", [
            TokenKind.DotChar,
            TokenKind.CrossChar, TokenKind.MinusChar, TokenKind.PowerOperator,
            TokenKind.StarChar, TokenKind.StarChar, TokenKind.PercentChar,
            TokenKind.EqualsChar,

            TokenKind.RightArrowOperator, TokenKind.LessEqualsOperator,
            TokenKind.EqualOperator, TokenKind.UnEqualOperator,
            TokenKind.LeftAngleChar, TokenKind.RightAngleChar,
            TokenKind.LessEqualsOperator, TokenKind.GreatEqualsOperator,

            TokenKind.AddAssign, TokenKind.MulAssign,
            TokenKind.SubAssign, TokenKind.DivAssign
        ] },
    };

    private static readonly Dictionary<string, TokenKind> Keyword2TokenMap = new()
    {
        // keywords
        { "namespace", TokenKind.NamespaceKeyword },
        { "import", TokenKind.ImportKeyword },
        { "from", TokenKind.FromKeyword },

        { "let", TokenKind.LetKeyword },
        { "const", TokenKind.ConstKeyword },
        { "func", TokenKind.FuncKeyword },
        { "struct", TokenKind.StructKeyword },
        { "extends", TokenKind.ExtendsKeyword },
        { "packet", TokenKind.PacketKeyword },
        { "enum", TokenKind.EnumKeyword },

        { "switch", TokenKind.SwitchKeyword },
        { "match", TokenKind.MatchKeyword },
        { "if", TokenKind.IfKeyword },
        { "elif", TokenKind.ElifKeyword },
        { "else", TokenKind.ElseKeyword },

        { "while", TokenKind.WhileKeyword },
        { "for", TokenKind.ForKeyword },
        { "do", TokenKind.DoKeyword },
        { "in", TokenKind.InKeyword },
        { "break", TokenKind.BreakKeyword },

        { "return", TokenKind.ReturnKeyword },

        { "as", TokenKind.AsKeyword },

        // values
        { "null", TokenKind.NullKeyword },
        { "true", TokenKind.TrueKeyword },
        { "false", TokenKind.FalseKeyword },

        // operators
        { "and", TokenKind.AndOperator },
        { "or", TokenKind.OrOperator },

    };

    private static Token Tokenize(string value, TokenKind type, int start, int end)
        => new() { type = type, value = value, start = (uint)start, end = (uint)end };

    private static Token Tokenize(char value, TokenKind type, int start)
        => Tokenize("" + value, type, start, start+1);

    
    public static Token[] ParseScript(Script source)
    {
        List<Token> result = [];
        var sourceCode = source.Read();
        
        ParseTokens(result, sourceCode);
        PostProcess(result);
        
        return [.. result];
    }
    
    private static void ParseTokens(List<Token> tokens, string source)
    {

        for (var i = 0; i < source.Length; i++)
        {
            var c = source[i];
            var c2 = source.Length > i + 1 ? source[i + 1] : '\0';

            // Check if it's skippable
            if (c == '\t') { continue; }

            // Check if it's a multiline character
            if (c.IsLanguageSymbol() && c2.IsLanguageSymbol())
            {
                var cc = $"{c}{c2}";

                var type = cc switch
                {

                    "=>" => TokenKind.RightArrowOperator,
                    "==" => TokenKind.EqualOperator,
                    "!=" => TokenKind.UnEqualOperator,

                    "<=" => TokenKind.LessEqualsOperator,
                    ">=" => TokenKind.GreatEqualsOperator,

                    "**" => TokenKind.PowerOperator,

                    "+=" => TokenKind.AddAssign,
                    "-=" => TokenKind.SubAssign,
                    "*=" => TokenKind.MulAssign,
                    "/=" => TokenKind.DivAssign,
                    "%=" => TokenKind.RestAssign,

                    _ => TokenKind.Undefined
                };

                if (type != TokenKind.Undefined)
                {
                    tokens.Add(Tokenize(cc, type, i, i + 1));

                    i++;
                    continue;
                }
            }

            switch (c)
            {
                // Check single characters
                case '\n' or '\r':
                {
                    var windowsLf = c == '\r' && source.Length > i + 1 && source[i + 1] == '\n';
                    tokens.Add(Tokenize("\\n", TokenKind.LineFeedChar, i, (windowsLf) ? ++i : i));
                    break;
                }
                case ' ':
                {
                    tokens.Add(Tokenize(" ", TokenKind.SpaceChar, i, -1));
                    break;
                }
                case '(':
                    tokens.Add(Tokenize(c, TokenKind.LeftPerenthesisChar, i));
                    break;
                case ')':
                    tokens.Add(Tokenize(c, TokenKind.RightParenthesisChar, i));
                    break;
                case '{':
                    tokens.Add(Tokenize(c, TokenKind.LeftBracketChar, i));
                    break;
                case '}':
                    tokens.Add(Tokenize(c, TokenKind.RightBracketChar, i));
                    break;
                case '[':
                    tokens.Add(Tokenize(c, TokenKind.LeftSquareBracketChar, i));
                    break;
                case ']':
                    tokens.Add(Tokenize(c, TokenKind.RightSquareBracketChar, i));
                    break;
                case '<':
                    tokens.Add(Tokenize(c, TokenKind.LeftAngleChar, i));
                    break;
                case '>':
                    tokens.Add(Tokenize(c, TokenKind.RightAngleChar, i));
                    break;
                case '+':
                    tokens.Add(Tokenize(c, TokenKind.CrossChar, i));
                    break;
                case '-':
                    tokens.Add(Tokenize(c, TokenKind.MinusChar, i));
                    break;
                case '*':
                    tokens.Add(Tokenize(c, TokenKind.StarChar, i));
                    break;
                case '/':
                    tokens.Add(Tokenize(c, TokenKind.SlashChar, i));
                    break;
                case '%':
                    tokens.Add(Tokenize(c, TokenKind.PercentChar, i));
                    break;
                case '=':
                    tokens.Add(Tokenize(c, TokenKind.EqualsChar, i));
                    break;
                case '&':
                    tokens.Add(Tokenize(c, TokenKind.AmpersandChar, i));
                    break;
                case '?':
                    tokens.Add(Tokenize(c, TokenKind.QuestionChar, i));
                    break;
                case '!':
                    tokens.Add(Tokenize(c, TokenKind.BangChar, i));
                    break;
                case '@':
                    tokens.Add(Tokenize(c, TokenKind.AtSiginChar, i));
                    break;
                case ',':
                    tokens.Add(Tokenize(c, TokenKind.CommaChar, i));
                    break;
                case '.':
                    tokens.Add(Tokenize(c, TokenKind.DotChar, i));
                    break;
                default:
                {
                    // Build number token
                    if (char.IsDigit(c))
                    {

                        var num = "";
                        byte numBase = 10;

                        var isFloating = false;

                        if (c == '0') // verify different bases
                        {
                            switch (char.ToLower(source[i + 1]))
                            {
                                case 'x':
                                    numBase = 16;
                                    i += 2;
                                    break;
                            
                                case 'b':
                                    numBase = 2;
                                    i += 2;
                                    break;
                            
                            }

                        }

                        var j = i;
                        for (; source.Length > j; j++)
                        {
                            var cc = source[j];

                            if (cc == '.')
                            {
                                if (numBase != 10 || isFloating) break;

                                isFloating = true;
                                num += cc;
                                continue;
                            }

                            if (numBase == 10 && !char.IsDigit(cc)) break;
                            else if (numBase == 16 && !char.IsAsciiHexDigit(cc)) break;
                            else if (numBase == 2) break;

                            num += cc;
                        }

                        tokens.Add(Tokenize(num, !isFloating
                                ? TokenKind.IntegerNumberLiteral
                                : TokenKind.FloatingNumberLiteral,
                            i,j));

                        i = j - 1;

                    }

                    // Build identifier token
                    else if (c.IsValidOnIdentifierStarter())
                    {

                        var token = "";

                        var j = i;
                        for (; source.Length > j && source[j].IsValidOnIdentifier(); j++)
                            token += source[j];

                        tokens.Add(Keyword2TokenMap.TryGetValue(token, out var type)
                            ? Tokenize(token, type, i, j)
                            : Tokenize(token, TokenKind.Identifier, i, j));

                        i = j - 1;

                    }

                    else switch (c)
                    {
                        // Build string tokens
                        case '"':
                        {
                            tokens.Add(Tokenize("\"", TokenKind.DoubleQuotes, i, -1));

                            var curr = "";
                            var currstart = i + 1;

                            var j = currstart;
                            for (; source.Length > j; j++)
                            {
                                // test escape
                                if (source[j] == '\\')
                                {
                                    if (!string.IsNullOrEmpty(curr))
                                    {
                                        tokens.Add(Tokenize(curr, TokenKind.StringLiteral, currstart, j));
                                        curr = "";
                                    }

                                    if (source.Length < j + 1) break;
                                    var escapeCode = source[j + 1];

                                    switch (escapeCode)
                                    {
                                        // interpolation
                                        case '{':
                                            // FIXME interpolation
                                            // interpolation content detection is a little
                                            // raw implemented :p

                                            throw new NotImplementedException();

                                        //tokens.Add(Tokenize("\\{", TokenKind.EscapedLeftBracketChar, i, j));

                                        //string interpolation = "";
                                        //int subscopes = 0;
                                        //bool inString = false;

                                        //while (source.Length > j && !(subscopes == 0 && source[j] == '}'))
                                        //{
                                        //    if (source[j] == '"') inString = !inString;
                                        //    else if (source[j] == '{' && inString == false) subscopes++;
                                        //    else if (source[j] == '}' && inString == false) subscopes--;

                                        //    interpolation += source[j];
                                        //}

                                        //tokens.AddRange(ParseTokens(interpolation));

                                        //if (source[j] == '}')
                                        //    tokens.Add(Tokenize("}", TokenKind.RightBracketChar, i, j));
                                        //continue;
                                        // hexadecimal
                                        case 'x':
                                        {
                                            var k = j + 2;
                                            var hex = "";
                                            while (source.Length > k && source[k] != '\\')
                                                hex += source[k++];

                                            tokens.Add(Tokenize($@"\x{hex}\", TokenKind.CharacterLiteral, i, j));
                                            break;
                                        }
                                        default:
                                            tokens.Add(Tokenize($"\\{escapeCode}", TokenKind.CharacterLiteral, j, ++j));
                                            break;
                                    }

                                    currstart = j+1;
                                    continue;
                                }

                                // test end of string
                                else if (source[j] == '"')
                                {
                                    if (!string.IsNullOrEmpty(curr)) tokens.Add(Tokenize(curr, TokenKind.StringLiteral, currstart, j));
                                    tokens.Add(Tokenize("\"", TokenKind.DoubleQuotes, i, -1));
                                    break;
                                }

                                else curr += source[j];
                            }

                            i = j;
                            break;
                        }
                        // Ignore comments
                        case '#' when source.Length > i + 3 && source[i..(i + 3)] == "###":
                        {
                            i += 3;
                            while (source.Length > i + 3 && source[i..(i + 3)] != "###") i++;
                            i += 3;
                            break;
                        }
                        case '#':
                        {
                            i++;
                            while (source.Length > i + 1 && source[i] != '\n') i++;
                            if (source[i] == '#') i++;
                            break;
                        }
                        // unrecognized character
                        default:
                            // FIXME implement error handlers
                            // try { throw new UnrecognizedCharacterException(c, i); }
                            // catch (SyntaxException e) { currentSrc.ThrowError(e); }
                            break;
                    }

                    break;
                }
            }

        }
        source = "";

        tokens.Add(Tokenize("\\EOF", TokenKind.EofChar, source.Length, -1));
        
    }
    private static void PostProcess(List<Token> tokensList)
    {
        
        var index = 0;

        List<int> indexesToRemove = [];

        while (index < tokensList.Count)
        {
            var currToken = tokensList[index];

            if ((LineJunctions["justLeft"].Contains(currToken.type)
                 || LineJunctions["bothSides"].Contains(currToken.type)) && index > 0)
            {
                for (var i = index - 1; i >= 0; i--)
                {
                    if (tokensList[i].type == TokenKind.LineFeedChar)
                    {
                        indexesToRemove.Add(i);
                    }
                    else break;
                }
            }
            if (LineJunctions["justRight"].Contains(currToken.type)
                || LineJunctions["bothSides"].Contains(currToken.type))
            {
                for (var i = index + 1; i <= tokensList.Count; i++)
                {
                    if (tokensList[i].type == TokenKind.LineFeedChar)
                    {
                        indexesToRemove.Add(i);
                        index++;
                    }
                    else break;
                }
            }

            if (index > 0 && tokensList[index-1].type == TokenKind.SpaceChar)
            {
                var oldtkn = tokensList[index];
                oldtkn.spaceBefore = true;
                tokensList[index] = oldtkn;
            }
            if (index+1 < tokensList.Count && tokensList[index+1].type == TokenKind.SpaceChar)
            {
                var oldtkn = tokensList[index];
                oldtkn.spaceAfter = true;
                tokensList[index] = oldtkn;
            }

            index++;

        }

        indexesToRemove = [.. indexesToRemove.Distinct().OrderByDescending(e => e)];

        foreach (var i in indexesToRemove) tokensList.RemoveAt(i);
        tokensList.RemoveAll(e => e.type == TokenKind.SpaceChar);

    }
}
