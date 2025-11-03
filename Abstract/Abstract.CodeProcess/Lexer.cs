using System.Diagnostics;
using Abstract.CodeProcess.Core.Language;

namespace Abstract.CodeProcess;

public class Lexer
{

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
            TokenType.DotChar,
            TokenType.CrossChar, TokenType.MinusChar, TokenType.PowerOperator,
            TokenType.StarChar, TokenType.StarChar, TokenType.PercentChar,
            TokenType.EqualsChar,
            
            TokenType.LeftBracketChar, //TokenType.RightBracketChar,

            TokenType.RightArrowOperator, TokenType.LessEqualsOperator,
            TokenType.EqualOperator, TokenType.UnequalOperator,
            TokenType.LeftAngleChar, TokenType.RightAngleChar,
            TokenType.LessEqualsOperator, TokenType.GreatEqualsOperator,
            TokenType.ExactEqualOperator, TokenType.ExactUnequalOperator,

            TokenType.AddAssigin, TokenType.MulAssigin,
            TokenType.SubAssigin, TokenType.DivAssigin,
            
            TokenType.CircumflexChar, TokenType.PipeChar,
            TokenType.BitShiftLeftOperator, TokenType.BitShiftRightOperator,
            TokenType.BitwiseXorAssign, TokenType.BitwiseAndAssign, TokenType.BitwiseOrAssign,
        ] },
    };

    private Dictionary<string, TokenType> _keyword2TokenMap = new()
    {
        // keywords
        { "from", TokenType.FromKeyword },
        { "import", TokenType.ImportKeyword },

        { "let", TokenType.LetKeyword },
        { "const", TokenType.ConstKeyword },
        { "func", TokenType.FuncKeyword },
        { "struct", TokenType.StructKeyword },
        { "extends", TokenType.ExtendsKeyword },
        { "packet", TokenType.PacketKeyword },
        { "typedef", TokenType.TypedefKeyword },

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
        { "new", TokenType.NewKeyword },

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
        { "f32", TokenType.TypeKeyword },   { "float", TokenType.TypeKeyword },
        { "f64", TokenType.TypeKeyword },   { "double", TokenType.TypeKeyword },

        { "bool", TokenType.TypeKeyword },
        { "char", TokenType.TypeKeyword },
        { "string", TokenType.TypeKeyword },

    };

    private Token Tokenize(TokenType type, (ReadOnlyMemory<char> val, uint line, uint start, uint end) range)
        => new() { type = type, value = range.val, line = range.line, start = range.start,  end = range.end};
    
    public Token[] Lex(string source)
    {
        List<Token> tokens = [];
        
        LexRange(source, tokens);
        VerifyEndOfStatements(tokens);

        return [.. tokens];
    }
    
    private void LexRange(string source, List<Token> tokens)
    {
        SourceWindow src = new(source);

        while (!src.IsEof())
        {
            char c = src.Next();
            
            // Check if it's skipable
            if (c == ' ' | c == '\r' | c == '\t')
            {
                src.Discard();
                continue;
            }


            switch (c)
            {
                // Check single characters
                case '\n': tokens.Add(Tokenize(TokenType.LineFeedChar, src.GetSlice())); break;

                case '(': tokens.Add(Tokenize(TokenType.LeftPerenthesisChar, src.GetSlice())); break;
                case ')': tokens.Add(Tokenize(TokenType.RightParenthesisChar, src.GetSlice())); break;
                case '{': tokens.Add(Tokenize(TokenType.LeftBracketChar, src.GetSlice())); break;
                case '}': tokens.Add(Tokenize(TokenType.RightBracketChar, src.GetSlice())); break;
                case '[': tokens.Add(Tokenize(TokenType.LeftSquareBracketChar, src.GetSlice())); break;
                case ']': tokens.Add(Tokenize(TokenType.RightSquareBracketChar, src.GetSlice())); break;
                case '?': tokens.Add(Tokenize(TokenType.QuestionChar, src.GetSlice())); break;
                case '!': tokens.Add(Tokenize(TokenType.BangChar, src.GetSlice())); break;
                case '@': tokens.Add(Tokenize(TokenType.AtSiginChar, src.GetSlice())); break;
                case ',': tokens.Add(Tokenize(TokenType.CommaChar, src.GetSlice())); break;
                case '.': tokens.Add(Tokenize(TokenType.DotChar, src.GetSlice())); break;
                case ':': tokens.Add(Tokenize(TokenType.ColonChar, src.GetSlice())); break;

                case '<':
                {
                    if (src.NextIs('=')) tokens.Add(Tokenize(TokenType.LessEqualsOperator, src.GetSlice()));
                    else if (src.NextIs('<')) tokens.Add(src.NextIs('=')
                            ? Tokenize(TokenType.BitShiftLeftAssign, src.GetSlice())
                            : Tokenize(TokenType.BitShiftLeftOperator, src.GetSlice()));
                    else tokens.Add(Tokenize(TokenType.LeftAngleChar, src.GetSlice()));
                } break;
                
                case '>':
                {
                    if (src.NextIs('=')) tokens.Add(Tokenize(TokenType.GreatEqualsOperator, src.GetSlice()));
                    else if (src.NextIs('>')) tokens.Add(src.NextIs('=')
                        ? Tokenize(TokenType.BitShiftRightAssign, src.GetSlice())
                        : Tokenize(TokenType.BitShiftLeftOperator, src.GetSlice()));
                    else tokens.Add(Tokenize(TokenType.RightAngleChar, src.GetSlice()));
                } break;
                
                case '+': tokens.Add(Tokenize(
                          src.NextIs('=') ? TokenType.AddAssigin
                        : src.NextIs('+') ? TokenType.IncrementOperator
                        : src.NextIs('%') ? TokenType.AddWarpOperator
                        : src.NextIs('|') ? TokenType.SubOnBoundsOperator
                        : TokenType.CrossChar
                    , src.GetSlice())); break;
                
                case '-': tokens.Add(Tokenize(
                          src.NextIs('=') ? TokenType.SubAssigin
                        : src.NextIs('-') ? TokenType.DecrementOperator
                        : src.NextIs('%') ? TokenType.SubWarpOperator
                        : src.NextIs('|') ? TokenType.SubOnBoundsOperator
                        : TokenType.MinusChar
                    , src.GetSlice())); break;
                
                case '*': tokens.Add(src.NextIs('=')
                    ? Tokenize(TokenType.MulAssigin, src.GetSlice())
                    : Tokenize(TokenType.StarChar, src.GetSlice())); break;
                
                case '/': tokens.Add(Tokenize(
                          src.NextIs('=') ? TokenType.DivAssigin
                        : src.NextIs('^') ? TokenType.DivideCeilOperator
                        : src.NextIs('_') ? TokenType.DivideFloorOperator
                        : TokenType.SlashChar
                    , src.GetSlice())); break;
                
                case '%': tokens.Add(src.NextIs('=')
                    ? Tokenize(TokenType.RestAssigin, src.GetSlice())
                    : Tokenize(TokenType.PercentChar, src.GetSlice())); break;
                
                case '=': tokens.Add(src.NextIs('=')
                    ? Tokenize(TokenType.EqualOperator, src.GetSlice())
                    : src.NextIs('>') 
                        ? Tokenize(TokenType.RightArrowOperator, src.GetSlice())
                        : Tokenize(TokenType.EqualsChar, src.GetSlice())); break;
                
                case '^': tokens.Add(src.NextIs('=')
                    ? Tokenize(TokenType.BitwiseXorAssign, src.GetSlice())
                    : Tokenize(TokenType.CircumflexChar, src.GetSlice())); break;
                
                case '|': tokens.Add(src.NextIs('=')
                    ? Tokenize(TokenType.BitwiseOrAssign, src.GetSlice())
                    : Tokenize(TokenType.PipeChar, src.GetSlice())); break;
                
                case '&': tokens.Add(src.NextIs('=')
                    ? Tokenize(TokenType.BitwiseAndAssign, src.GetSlice())
                    : Tokenize(TokenType.AmpersandChar, src.GetSlice())); break;

                // ignore comments
                case '#':
                {
                    if (src.NextStringIs("##"))
                    {
                        src.Next();
                        src.Next();
                        while (!src.NextStringIs("###")) src.Next();
                        src.Discard();
                    }
                    else
                    {
                        while (src.Peek() != '\n') src.Next();
                        src.Discard();
                    }
                } break;
                
                default:
                {
                    // Build number token
                    if (char.IsDigit(c))
                    {
                        var numBase = 10;
                        var isFloating = false;
                    
                        if (c == '0' && !src.IsEof()) // verify different bases
                        {
                            if (char.ToLower(src.Peek()) == 'x')
                            {
                                numBase = 16;
                                src.Next();
                            }
                            if (char.ToLower(src.Peek()) == 'b')
                            {
                                numBase = 2;
                                src.Next();
                            }
                        }

                        while (!src.IsEof())
                        {
                            char cc = src.Peek();
                            if (cc == '.')
                            {
                                if (numBase != 10 || isFloating) break;

                                isFloating = true;
                                _ = src.Next();
                                continue;
                            }

                            if (numBase == 10 && !char.IsDigit(src.Peek())) break;
                            if (numBase == 16 && !char.IsAsciiHexDigit(src.Peek())) break;
                            if (numBase == 2  && src.Peek() is not ('0' or '1')) break;
                            
                            src.Next();
                        }

                        tokens.Add(Tokenize(
                            isFloating
                                ? TokenType.FloatingNumberLiteral
                                : TokenType.IntegerNumberLiteral,
                            src.GetSlice()));

                    }

                    // Build identifier token
                    else if (c.IsValidOnIdentifierStarter())
                    {
                        while (src.Peek().IsValidOnIdentifier()) src.Next();

                        var value = src.GetSlice();

                        tokens.Add(_keyword2TokenMap.TryGetValue(value.Item1.ToString(), out var type)
                            ? Tokenize(type, value)
                            : Tokenize(TokenType.Identifier, value));
                    }

                    // Build string tokens
                    else if (c == '"')
                    {
                        tokens.Add(Tokenize(TokenType.DoubleQuotes, src.GetSlice()));

                        while (!src.IsEof())
                        {
                            // test escape
                            if (src.Peek() == '\\')
                            {
                                if (src.Dirty) tokens.Add(Tokenize(TokenType.StringLiteral, src.GetSlice()));
                            
                                if (src.IsEof()) break;
                                char escapeCode = src.Next();
                            
                                // interpolation
                                if (escapeCode == '{') throw new NotImplementedException();
                            
                                // hexadecimal
                                else if (escapeCode == 'x')
                                {
                                    while (src.Peek() != '\\') src.Next();
                                    tokens.Add(Tokenize(TokenType.CharacterLiteral, src.GetSlice()));
                                }
                                else
                                {
                                    src.Next();
                                    tokens.Add(Tokenize(TokenType.CharacterLiteral, src.GetSlice()));
                                }
                            }

                            // test end of string
                            else if (src.Peek() == '"')
                            {
                                if (src.Dirty) tokens.Add(Tokenize(TokenType.StringLiteral, src.GetSlice()));
                                src.Next();
                                tokens.Add(Tokenize(TokenType.DoubleQuotes, src.GetSlice()));
;                               break;
                            }
                            else src.Next();
                        }
                    }
                    
                    // unrecognized character
                    else
                    {
                        throw new NotImplementedException();
                        // FIXME implement error handlers
                        // try { throw new UnrecognizedCharacterException(c, i); }
                        // catch (SyntaxException e) { currentSrc.ThrowError(e); }
                    }

                    break;
                }
            }
        }
        
        tokens.Add(Tokenize(TokenType.EofChar, src.GetSlice()));

        VerifyEndOfStatements(tokens);

    }

    private void VerifyEndOfStatements(List<Token> tokensList)
    {
        while (tokensList[0].type == TokenType.LineFeedChar) tokensList.RemoveAt(0);
        
        var indexesToRemove = new List<int>();
        
        for (int index = 0; index < tokensList.Count; index++)
        {
            var currToken = tokensList[index];
            
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
                for (int i = index + 1; i < tokensList.Count; i++)
                {
                    if (tokensList[i].type == TokenType.LineFeedChar)
                    {
                        indexesToRemove.Add(i);
                    }
                    else break;
                }
            }
        }
        
        foreach (var i in indexesToRemove.Distinct().OrderByDescending(x => x))
            tokensList.RemoveAt(i);
        
        for (int i = tokensList.Count - 2; i >= 0; i--)
        {
            if (tokensList[i].type == TokenType.LineFeedChar &&
                tokensList[i + 1].type == TokenType.LineFeedChar)
            {
                tokensList.RemoveAt(i + 1);
            }
        }
    }

    
    private class SourceWindow(string source)
    {
        private string source = source;
        
        private uint line = 0;
        private uint startColumn = 0;
        private uint endColumn = 0;
        
        private int index_start = 0;
        private int index_end = 0;

        public bool Dirty => index_end != index_start;

        private char Advance()
        {
            if (IsEof()) return (char)0x1A;
            
            var c = source[index_end++];
            endColumn++;

            switch (c)
            {
                case '\n': NextLine(); break;
            }
            
            return c;
        }
        
        private void NextLine()
        {
            line++;
            startColumn = 0;
            endColumn = 0;
            _ = CloseBounds();
        }

        public bool IsEof() => index_end >= source.Length;
        public char Peek()
        {
            if (IsEof()) return (char)0x1A;
            return source[index_end];
        }
        public char Next() => Advance();

        public bool NextIs(char c)
        {
            if (Peek() != c) return false;
            _ = Advance();
            return true;

        }
        public bool NextIfTest(Func<char, bool> test)
        {
            if (test(Peek())) return false;
            _ = Advance();
            return true;
        }

        public bool NextStringIs(string s)
        {
            if (index_end + s.Length >= source.Length) return false;
            var res = source[index_end .. (index_end + s.Length)] == s;
            if (!res) return false;

            for (var i = 0; i < s.Length; i++) Advance();
            return true;
        }
        
        public void Discard() => _ = CloseBounds();
        public (uint, uint, uint) CloseBounds()
        {
            var ret = (line, startColumn, endColumn);
            startColumn = endColumn;
            index_start = index_end;
            return ret;
        }
        public (ReadOnlyMemory<char>, uint, uint, uint) GetSlice()
        {
            var str = source.AsMemory(index_start, index_end - index_start);
            var bounds = CloseBounds();
            return (str, bounds.Item1, bounds.Item2, bounds.Item3); 
        }
        
        public override string ToString()
            => $"({line+1}:{startColumn}-{endColumn})\n{source[0..index_end]}{{{{!index here!}}}}{source[index_end..]}";
    }
}
