using Abstract.Build.Core.Sources;
using Abstract.Parser.Core.Language;

namespace Abstract.CodeProcess;

public class Lexer
{
    private Script currentSrc = null!;


    private readonly Dictionary<string, TokenType[]> lineJunctions = new()
    {
        { "justLeft",  [
            TokenType.LeftBracketChar,
            TokenType.RightParenthesisChar
        ] },
        { "justRight", [
            TokenType.LeftPerenthesisChar,
            TokenType.AtSiginChar,
        ] },
        { "bothSides", [
            TokenType.CrossChar, TokenType.MinusChar, TokenType.PowerOperator,
            TokenType.StarChar, TokenType.StarChar, TokenType.PercentChar,
            TokenType.EqualsChar,

            TokenType.RightArrowOperator, TokenType.LessEqualsOperator,
            TokenType.EqualOperator, TokenType.UnEqualOperator,
            TokenType.LeftAngleChar, TokenType.RightAngleChar,
            TokenType.LessEqualsOperator, TokenType.GreatEqualsOperator,

            TokenType.AddAssigin, TokenType.MulAssigin,
            TokenType.SubAssigin, TokenType.DivAssigin
        ] },
    };

    private Dictionary<string, TokenType> _keyword2TokenMap = new()
    {
        // keywords
        { "namespace", TokenType.NamespaceKeyword },
        { "import", TokenType.ImportKeyword },

        { "let", TokenType.LetKeyword },
        { "const", TokenType.ConstKeyword },
        { "func", TokenType.FuncKeyword },
        { "struct", TokenType.StructKeyword },
        { "extends", TokenType.ExtendsKeyword },
        { "packet", TokenType.PacketKeyword },
        { "enum", TokenType.EnumKeyword },

        { "switch", TokenType.SwitchKeyword },
        { "match", TokenType.MatchKeyword },
        { "if", TokenType.IfKeyword },
        { "elif", TokenType.ElifKeyword },
        { "else", TokenType.ElseKeyword },

        { "while", TokenType.WhileKeyword },
        { "for", TokenType.ForKeyword },
        { "do", TokenType.DoKeyword },
        { "in", TokenType.InKeyword },
        { "break", TokenType.BreakKeyword },

        { "return", TokenType.ReturnKeyword },

        { "as", TokenType.AsKeyword },

        // values
        { "null", TokenType.NullKeyword },
        { "true", TokenType.TrueKeyword },
        { "false", TokenType.FalseKeyword },

        // operators
        { "and", TokenType.AndOperator },
        { "or", TokenType.OrOperator },

        // types
        { "void", TokenType.TypeKeyword },
        { "noreturn", TokenType.TypeKeyword },
        { "type", TokenType.TypeKeyword },

                                            { "byte", TokenType.TypeKeyword },
        { "i8", TokenType.TypeKeyword },    { "u8", TokenType.TypeKeyword },
        { "i16", TokenType.TypeKeyword },   { "u16", TokenType.TypeKeyword },
        { "i32", TokenType.TypeKeyword },   { "u32", TokenType.TypeKeyword },
        { "i64", TokenType.TypeKeyword },   { "u64", TokenType.TypeKeyword },
        { "i128", TokenType.TypeKeyword },  { "u128", TokenType.TypeKeyword },
        { "iptr", TokenType.TypeKeyword },  { "uptr", TokenType.TypeKeyword },

        { "f32", TokenType.TypeKeyword },   { "float", TokenType.TypeKeyword },
        { "f64", TokenType.TypeKeyword },   { "double", TokenType.TypeKeyword },

        { "bool", TokenType.TypeKeyword },
        { "char", TokenType.TypeKeyword },
        { "string", TokenType.TypeKeyword },

    };

    private Token Tokenize(string value, TokenType type, int start, int end)
        => new() { type = type, value = value, start = (uint)start, end = (uint)end, scriptRef = currentSrc };

    private Token Tokenize(char value, TokenType type, int start)
        => Tokenize("" + value, type, start, start+1);

    public Token[] Parse(Script source)
    {
        currentSrc = source;

        var sourceCode = source.GetContent();
        List<Token> tokens = [];

        for (int i = 0; i < sourceCode.Length; i++)
        {
            char c = sourceCode[i];
            char c2 = sourceCode.Length > i + 1 ? sourceCode[i + 1] : '\0';

            // Check if it's skipable
            if (c == ' ' | c == '\r' | c == '\t') { continue; }

            // Check if it's a multiline character
            if (c.IsLanguageSymbol() && c2.IsLanguageSymbol())
            {
                string cc = $"{c}{c2}";

                TokenType type = cc switch
                {

                    "=>" => TokenType.RightArrowOperator,
                    "==" => TokenType.EqualOperator,
                    "!=" => TokenType.UnEqualOperator,

                    "<=" => TokenType.LessEqualsOperator,
                    ">=" => TokenType.GreatEqualsOperator,

                    "**" => TokenType.PowerOperator,

                    "+=" => TokenType.AddAssigin,
                    "-=" => TokenType.SubAssigin,
                    "*=" => TokenType.MulAssigin,
                    "/=" => TokenType.DivAssigin,
                    "%=" => TokenType.RestAssigin,

                    _ => TokenType.Undefined
                };

                if (type != TokenType.Undefined)
                {
                    tokens.Add(Tokenize(cc, type, i, i + 1));

                    i++;
                    continue;
                }
            }

            // Check single characters
            if (c == '\n') {
                if (tokens.Count > 0 && tokens[^1].type != TokenType.LineFeedChar)
                tokens.Add(Tokenize("\\n", TokenType.LineFeedChar, i, -1));
            }
            else if (c == '(')
                tokens.Add(Tokenize(c, TokenType.LeftPerenthesisChar, i));
            else if (c == ')')
                tokens.Add(Tokenize(c, TokenType.RightParenthesisChar, i));
            else if (c == '{')
                tokens.Add(Tokenize(c, TokenType.LeftBracketChar, i));
            else if (c == '}')
                tokens.Add(Tokenize(c, TokenType.RightBracketChar, i));
            else if (c == '[')
                tokens.Add(Tokenize(c, TokenType.LeftSquareBracketChar, i));
            else if (c == ']')
                tokens.Add(Tokenize(c, TokenType.RightSquareBracketChar, i));

            else if (c == '<')
                tokens.Add(Tokenize(c, TokenType.LeftAngleChar, i));
            else if (c == '>')
                tokens.Add(Tokenize(c, TokenType.RightAngleChar, i));

            else if (c == '+')
                tokens.Add(Tokenize(c, TokenType.CrossChar, i));
            else if (c == '-')
                tokens.Add(Tokenize(c, TokenType.MinusChar, i));
            else if (c == '*')
                tokens.Add(Tokenize(c, TokenType.StarChar, i));
            else if (c == '/')
                tokens.Add(Tokenize(c, TokenType.SlashChar, i));
            else if (c == '%')
                tokens.Add(Tokenize(c, TokenType.PercentChar, i));
            else if (c == '=')
                tokens.Add(Tokenize(c, TokenType.EqualsChar, i));

            else if (c == '&')
                tokens.Add(Tokenize(c, TokenType.AmpersandChar, i));
            else if (c == '?')
                tokens.Add(Tokenize(c, TokenType.QuestionChar, i));
            else if (c == '@')
                tokens.Add(Tokenize(c, TokenType.AtSiginChar, i));

            else if (c == ',')
                tokens.Add(Tokenize(c, TokenType.CommaChar, i));
            else if (c == '.')
                tokens.Add(Tokenize(c, TokenType.DotChar, i));

            else
            {

                // Build number token
                if (char.IsDigit(c))
                {

                    string num = "";
                    byte numBase = 10;

                    bool isFloating = false;

                    if (c == '0') // verify different bases
                    {
                        if (char.ToLower(sourceCode[i + 1]) == 'x')
                        {
                            numBase = 16;
                            i += 2;
                        }
                        if (char.ToLower(sourceCode[i + 1]) == 'b')
                        {
                            numBase = 2;
                            i += 2;
                        }
                    }

                    int j = i;
                    for (; sourceCode.Length > j; j++)
                    {
                        char cc = sourceCode[j];

                        if (cc == '.')
                        {
                            if (numBase != 10 || isFloating) break;

                            isFloating = true;
                            num += cc;
                            continue;
                        }

                        if (numBase == 10 && !char.IsDigit(cc)) break;
                        else if (numBase == 16 && !char.IsAsciiHexDigit(cc)) break;
                        else if (numBase == 2 && (cc != '0' || cc != '1')) break;

                        num += cc;
                    }

                    tokens.Add(Tokenize(num, !isFloating ? TokenType.IntegerNumberLiteral : TokenType.FloatingNumberLiteral, i, j));

                    i = j - 1;

                }

                // Build identifier token
                else if (c.IsValidOnIdentifierStarter())
                {

                    string token = "";

                    int j = i;
                    for (; sourceCode.Length > j && sourceCode[j].IsValidOnIdentifier(); j++)
                        token += sourceCode[j];

                    if (_keyword2TokenMap.TryGetValue(token, out var type))
                        tokens.Add(Tokenize(token, type, i, j));

                    else tokens.Add(Tokenize(token, TokenType.Identifier, i, j));

                    i = j - 1;

                }

                // Build string token
                else if (c == '"')
                {
                    string token = "";

                    int j = i + 1;
                    for (; sourceCode.Length > j && sourceCode[j] != '"'; j++)
                        token += sourceCode[j];

                    tokens.Add(Tokenize(token, TokenType.StringLiteral, i, j));

                    i = j;
                }

                // Igonore comments
                else if (c == '#')
                {
                    if (sourceCode.Length > i + 3 && sourceCode[i..(i + 3)] == "###")
                    {
                        i += 3;
                        while (sourceCode.Length > i + 3 && sourceCode[i..(i + 3)] != "###") i++;
                        i += 3;
                    }
                    else
                    {
                        i++;
                        while (sourceCode.Length > i + 1 && sourceCode[i] != '#' && sourceCode[i] != '\n') i++;
                        if (sourceCode[i] == '#') i++;
                    }
                }

                // unrecognized character
                else
                {
                    // FIXME implement error handlers
                    // try { throw new UnrecognizedCharacterException(c, i); }
                    // catch (SyntaxException e) { currentSrc.ThrowError(e); }
                }

            }

        }
        sourceCode = "";

        tokens.Add(Tokenize("\\EOF", TokenType.EOFChar, sourceCode.Length, -1));

        VerifyEndOfStatements(tokens);

        currentSrc = null!;

        return [.. tokens];
    }

    public void VerifyEndOfStatements(List<Token> tokensList)
    {

        List<Token> tokensToEvaluate = [.. tokensList];
        int index = 0;

        List<int> indexesToRemove = [];

        while (tokensToEvaluate.Count > 0)
        {
            var currToken = tokensToEvaluate[0];

            if ((lineJunctions["justLeft"].Contains(currToken.type)
            || lineJunctions["bothSides"].Contains(currToken.type)) && index > 0)
            {
                for (int i = index - 1; i >= 0; i--)
                {
                    if (tokensList[i].type == TokenType.LineFeedChar)
                    {
                        indexesToRemove.Add(i);
                    }
                    else break;
                }
            }
            if (lineJunctions["justRight"].Contains(currToken.type)
            || lineJunctions["bothSides"].Contains(currToken.type))
            {
                for (int i = index + 1; i <= tokensList.Count; i++)
                {
                    if (tokensList[i].type == TokenType.LineFeedChar)
                    {
                        indexesToRemove.Add(i);
                        tokensToEvaluate.RemoveAt(1);
                        index++;
                    }
                    else break;
                }
            }

            tokensToEvaluate.RemoveAt(0);
            index++;

        }

        indexesToRemove.Sort();
        indexesToRemove.Reverse();

        foreach (var i in indexesToRemove.Distinct())
            tokensList.RemoveAt(i);

    }

}
