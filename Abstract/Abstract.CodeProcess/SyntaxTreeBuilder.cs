using Abstract.Build;
using Abstract.Build.Core.Exceptions;
using Abstract.Build.Core.Sources;
using Abstract.Build.Exceptions;
using Abstract.Parser.Core.Exceptions.Syntax;
using Abstract.Parser.Core.Language;
using Abstract.Parser.Core.Language.SyntaxNodes.Base;
using Abstract.Parser.Core.Language.SyntaxNodes.Control;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression.TypeModifiers;
using Abstract.Parser.Core.Language.SyntaxNodes.Misc;
using Abstract.Parser.Core.Language.SyntaxNodes.Statement;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;

namespace Abstract.Parser;

public class SyntaxTreeBuilder(ErrorHandler errHandler)
{

    private ErrorHandler _errHandler = errHandler;
    private Script _currentScript = null!;
    private Queue<Token> _tokens = null!;


    public ScriptReferenceNode ParseScriptTokens(Script src, Token[] tkns)
    {
        _tokens = new(tkns);
        _currentScript = src;

        ScriptReferenceNode res = null!;

        res = ParseScript(src);

        _tokens = null!;
        _currentScript = null!;

        return res;
    }

    private ScriptReferenceNode ParseScript(Script src)
    {

        var scriptRef = new ScriptReferenceNode(src);

        while(!IsEOF())
        {
            try
            {
                scriptRef.AppendChild(ParseRoot());
            }
            catch (SyntaxException ex) { _errHandler.RegisterError(ex); }
            catch (BuildException ex) { _errHandler.RegisterError(_currentScript, ex); }
        }

        return scriptRef;

    }

    private ControlNode ParseRoot()
    {
        ControlNode node;
       
        switch (Bite().type)
        {
            case TokenType.NamespaceKeyword:
                try {

                node = new NamespaceNode();
                node.AppendChild(EatAsNode()); // namespace
                node.AppendChild(ParseIdentfier()); // <identifier.*>
                TryEndLine();
                node.AppendChild(ParseBlock((BlockNode block, ref bool _) => {
                    block.AppendChild(ParseRoot()); // {...}
                }));

                } catch { DiscardLine(); throw; }
                break;

            case TokenType.StructKeyword:
                try {

                node = new StructureDeclarationNode();
                node.AppendChild(EatAsNode()); // struct
                node.AppendChild(ParseSingleIdentfier()); // <identifier>

                if (Taste(TokenType.LeftPerenthesisChar)) node.AppendChild(ParseParameterCollection()); // generic parameters
                
                bool useExtendsImplements = false;
                var extendsImplements = new ExtendsImplementsNode();
                
                if (TryEatAsNode(TokenType.ExtendsKeyword, out var extends)) // extends
                {
                    extendsImplements.AppendChild(extends);
                    extendsImplements.AppendChild(ParseIdentfier());
                    useExtendsImplements = true;
                }

                if (useExtendsImplements) node.AppendChild(extendsImplements);

                TryEndLine();
                node.AppendChild(ParseBlock((BlockNode n, ref bool _)
                    => n.AppendChild(ParseRoot()))); // {...}
            
                } catch { DiscardLine(); throw; }
                break;

            case TokenType.FuncKeyword:
                try {

                node = new FunctionDeclarationNode();
                node.AppendChild(EatAsNode()); // func
                node.AppendChild(ParseType()); // <type>
                node.AppendChild(ParseSingleIdentfier()); // <identifier>
                node.AppendChild(ParseParameterCollection()); // (..., ...)
                TryEndLine();
                if (Taste(TokenType.LeftBracketChar))
                    node.AppendChild(ParseBlock((BlockNode n, ref bool _)
                        => n.AppendChild(ParseFunctionBody()))); // {...}
            
                } catch { DiscardLine(); throw; }
                break;

            case TokenType.PacketKeyword:
                try{

                node = new PacketDeclarationNode();
                node.AppendChild(EatAsNode()); // packet
                node.AppendChild(ParseSingleIdentfier()); // <identifier>
                TryEndLine();
                node.AppendChild(ParseBlock((BlockNode n, ref bool _)
                    => n.AppendChild(ParsePacketBody()))); // {...}

                } catch { DiscardLine(); throw; }
                break;

            case TokenType.LetKeyword or
            TokenType.ConstKeyword:
                try{

                node = new TopLevelVariableNode();
                node.AppendChild(EatAsNode()); // let / const
                node.AppendChild(ParseTypedIdentifier()); // <type> <identifier>

                if (TryEatAsNode(TokenType.EqualsChar, out var equals)) // =
                {
                    node.AppendChild(equals);
                    node.AppendChild(ParseExpression()); // <value>
                }
                
                EndLine();

                } catch { DiscardLine(); throw; }
                break;

            case TokenType.EnumKeyword:
                node = new EnumDeclarationNode();
                node.AppendChild(EatAsNode()); // enum
                node.AppendChild(ParseSingleIdentfier()); // identifier
                if (Taste(TokenType.LeftPerenthesisChar))
                    node.AppendChild(ParseArgumentCollection()); // (T)
                
                node.AppendChild(ParseBlock((BlockNode block, ref bool _break) => { // {...}
                    var i = new EnumeratorItemNode();

                    i.AppendChild(ParseSingleIdentfier()); // <ident>
                    if (TryEatAsNode(TokenType.EqualsChar, out var node))
                    {
                        i.AppendChild(node); // =
                        i.AppendChild(ParseExpression()); // <exp>
                    }

                    if (!TryEat(TokenType.CommaChar, out _)) _break = true;
                    TryEndLine();

                    block.AppendChild(i);
                }));

                break;

            case TokenType.ImportKeyword:
                node = ParseImport();
                break;

            case TokenType.AtSiginChar:
                try{

                node = new AttributeNode();
                node.AppendChild(EatAsNode());
                node.AppendChild(ParseIdentfier());

                if (Taste(TokenType.LeftPerenthesisChar))
                    node.AppendChild(ParseArgumentCollection());

                TryEndLine();

                } catch { DiscardLine(); throw; }
                break;

            // TODO constructors and destructors

            default: throw new UnexpectedTokenException(_currentScript, Eat(), "func, let, const, namespace, struct or @");
        }

        TryEndLine();
        return node;
    }

    private SyntaxNode ParseFunctionBody()
    {
        return ParseStatement(true);
    }

    private SyntaxNode ParseStatement(bool endLine = false)
    {
        SyntaxNode node;

        switch (Bite().type)
        {
            // Blocks
            case TokenType.LeftBracketChar:
                try {
                node = ParseBlock((BlockNode n, ref bool _)
                    => n.AppendChild(ParseFunctionBody()));
                } catch { DiscardUntil(TokenType.RightBracketChar); throw; }
                break;

            // Conditionals
            case TokenType.IfKeyword:
                try {

                node = new IfStatementNode();
                node.AppendChild(EatAsNode());  // if
                node.AppendChild(ParseExpression()); // <condition>
                TryEndLine();
                node.AppendChild(ParseStatement()); // <statement>

                } catch { DiscardLine(); throw; }
                break;
            case TokenType.ElifKeyword:
                try {

                node = new ElifStatementNode();
                node.AppendChild(EatAsNode()); // elif
                node.AppendChild(ParseExpression()); // <condition>
                TryEndLine();
                node.AppendChild(ParseStatement()); // <statement>

                } catch { DiscardLine(); throw; }
                break;
            case TokenType.ElseKeyword:
                try {

                node = new ElseStatementNode();
                node.AppendChild(EatAsNode()); // else
                TryEndLine();
                node.AppendChild(ParseStatement()); // <statement>

                } catch { DiscardLine(); throw; }
                break;

            // Loops
            case TokenType.WhileKeyword:
                try {

                node = new WhileStatementNode();
                node.AppendChild(EatAsNode()); // while
                node.AppendChild(ParseExpression()); // <condition>
                node.AppendChild(DietAsNode(TokenType.RightArrowOperator, (t)  // =>
                    => throw new UnexpectedTokenException(_currentScript, t)));
                node.AppendChild(ParseStatement()); // <statement>

                } catch { DiscardLine(); throw; }
                break;
            case TokenType.ForKeyword:
                try {

                node = new ForStatementNode();
                node.AppendChild(EatAsNode()); // for
                node.AppendChild(ParseRangeExpression()); // <range>
                node.AppendChild(DietAsNode(TokenType.RightArrowOperator, (t)  // =>
                    => throw new UnexpectedTokenException(_currentScript, t)));
                node.AppendChild(ParseStatement()); // <statement>

                } catch { DiscardLine(); throw; }
                break;

            // Return
            case TokenType.ReturnKeyword:
                try {

                node = new ReturnStatementNode();
                node.AppendChild(EatAsNode()); // return
                if (!Taste(TokenType.LineFeedChar))
                    node.AppendChild(ParseExpression()); // <value>
                
                } catch { DiscardLine(); throw; }
                break;

            default:
                node = ParseExpression(false);
                break;
        }

        if (endLine) EndLine();
        return node;
    }

    private ExpressionNode ParseExpression(bool canFallTrough = true)
    {
        var node = ParseAssignmentExpression();

        if (
            !canFallTrough
            && node is not AssignmentExpressionNode
            && node is not LocalVariableNode
            && node is not FunctionCallExpressionNode
        ) throw new InvalidOnFunctionRoot(_currentScript, node);

        return node;
    }

    #region Recursive expression parsing

    private ExpressionNode ParseAssignmentExpression()
    {
        var node = ParseBooleanOperationExpression();

        if(Taste(
            TokenType.EqualsChar, // =
            TokenType.AddAssigin, // +=
            TokenType.SubAssigin, // -=
            TokenType.MulAssigin, // *=
            TokenType.DivAssigin, // /=
            TokenType.RestAssigin // %=
        ))
        {
            var n = new AssignmentExpressionNode();
            n.AppendChild(node);
            n.AppendChild(EatAsNode());
            n.AppendChild(ParseAssignmentExpression());
            node = n;
        }

        return node;
    }

    private ExpressionNode ParseBooleanOperationExpression()
    {
        var node = ParseMultiplicativeExpression();

        if(Taste(
            TokenType.EqualOperator,       // ==
            TokenType.UnEqualOperator,     // !=

            TokenType.LeftAngleChar,       // <
            TokenType.RightAngleChar,      // >
            TokenType.LessEqualsOperator,  // <=
            TokenType.GreatEqualsOperator, // >=

            TokenType.AndOperator,  // and
            TokenType.OrOperator    // or
        ))
        {
            var n = new BooleanOperationExpressionNode();
            n.AppendChild(node);
            n.AppendChild(EatAsNode());
            n.AppendChild(ParseBooleanOperationExpression());
            node = n;
        }

        return node;
    }

    private ExpressionNode ParseMultiplicativeExpression()
    {
        var node = ParseAdditiveExpression();

        if(Taste(
            TokenType.StarChar,     // *
            TokenType.SlashChar,    // /
            TokenType.PercentChar,  // %
            TokenType.PowerOperator // **
        ))
        {
            var n = new BinaryExpressionNode();
            n.AppendChild(node);
            n.AppendChild(EatAsNode());
            n.AppendChild(ParseMultiplicativeExpression());
            node = n;
        }

        return node;
    }

    private ExpressionNode ParseAdditiveExpression()
    {
        var node = ParseUnaryExpression();

        if(Taste(
            TokenType.CrossChar, // +
            TokenType.MinusChar  // -
        ))
        {
            var n = new BinaryExpressionNode();
            n.AppendChild(node);
            n.AppendChild(EatAsNode());
            n.AppendChild(ParseAdditiveExpression());
            node = n;
        }

        return node;
    }

    private ExpressionNode ParseUnaryExpression()
    {
        ExpressionNode node;

        if(Taste(
            TokenType.CrossChar, // +
            TokenType.MinusChar  // -
        ))
        {
            node = new UnaryExpressionNode();
            node.AppendChild(EatAsNode());
            node.AppendChild(ParseValue());
        }
        else node = ParseValue();

        return node;
    }

    #endregion

    private ExpressionNode ParseValue()
    {
        ExpressionNode node;

        switch (Bite().type)
        {
            // Identifiers & function calls
            case TokenType.Identifier or
            TokenType.DotChar:
                try {
                
                var identifier = ParseIdentfier();

                if (Taste(TokenType.LeftPerenthesisChar))
                {
                    node = new FunctionCallExpressionNode();
                    node.AppendChild(identifier);
                    node.AppendChild(ParseArgumentCollection());
                }
                else node = identifier;

                } catch { DiscardLine(); throw; }
                break;

            // To make things works right, local variables needs
            // to be parsed here
            case TokenType.LetKeyword or
            TokenType.ConstKeyword:
                try{

                node = new LocalVariableNode();
                node.AppendChild(EatAsNode()); // let / const
                node.AppendChild(ParseTypedIdentifier()); // <type> <identifier>

                } catch { DiscardLine(); throw; }
                break;

            // parenthesis enclosed expression
            case TokenType.LeftPerenthesisChar:
                try {

                node = new ParenthesisExpressionNode();

                node.AppendChild(DietAsNode(TokenType.LeftPerenthesisChar, (t) => throw new UnexpectedTokenException(_currentScript, t)));
                node.AppendChild(ParseExpression());
                node.AppendChild(DietAsNode(TokenType.RightParenthesisChar, (t) => throw new UnexpectedTokenException(_currentScript, t)));

                } catch { DiscardUntil(TokenType.RightParenthesisChar); throw; }
                break;

            // Numbers
            case TokenType.IntegerNumberLiteral:
                node = new IntegerLiteralNode(Eat());
                break;
            case TokenType.FloatingNumberLiteral:
                node = new FloatingLiteralNode(Eat());
                break;
            
            // Booleans
            case TokenType.TrueKeyword
            or TokenType.FalseKeyword:
                node = new BooleanLiteralNode(Eat());
                break;
            
            // String
            case TokenType.DoubleQuotes:
                node = new StringLiteralNode();

                node.AppendChild(EatAsNode());
                while (!IsEOF())
                {
                    if (Taste(TokenType.StringLiteral))
                        node.AppendChild(new StringSectionNode(Eat()));

                    else if (Taste(TokenType.EscapedLeftBracketChar))
                    {
                        var interpNode = new StringInterpolationNode();
                        interpNode.AppendChild(EatAsNode());
                        interpNode.AppendChild(ParseExpression());
                        interpNode.AppendChild(DietAsNode(TokenType.RightBracketChar, (t)
                            => throw new UnexpectedTokenException(_currentScript, t)));
                        node.AppendChild(interpNode);
                    }

                    else if (Taste(TokenType.CharacterLiteral))
                        node.AppendChild(new CharacterLiteralNode(Eat()));

                    else if (Taste(TokenType.DoubleQuotes)) break;

                    else throw new UnexpectedTokenException(_currentScript, Eat());
                }
                node.AppendChild(DietAsNode(TokenType.DoubleQuotes, (e)
                    => throw new UnexpectedTokenException(_currentScript, e)));

                break;

            // collections (or type arrays)
            case TokenType.LeftSquareBracketChar:
                int index = 0;
                while(!TasteMore(index++, TokenType.RightSquareBracketChar));

                if (TasteMore(index,
                TokenType.TypeKeyword,
                TokenType.Identifier,
                TokenType.StarChar
                ))
                    node = ParseType();

                node = ParseGenericCollection();
                break;
            
            // types
            case TokenType.TypeKeyword
            or TokenType.StarChar:
                return ParseType();

            // null
            case TokenType.NullKeyword:
                return new NullLiteralNode(Eat());

            default: throw new UnexpectedTokenException(_currentScript, Eat());
        }

        return node;
    }

    private GenericCollectionExpressionNode ParseGenericCollection()
    {
        var collection = new GenericCollectionExpressionNode();
        collection.AppendChild(DietAsNode(TokenType.LeftSquareBracketChar,
        (e) => throw new UnexpectedTokenException(_currentScript, e)));

        if (!Taste(TokenType.RightSquareBracketChar))
        do collection.AppendChild(ParseExpression());
        while(TryEat(TokenType.CommaChar, out _));

        collection.AppendChild(DietAsNode(TokenType.RightSquareBracketChar,
        (e) => throw new UnexpectedTokenException(_currentScript, e)));
        return collection;
    }

    private RangeExpressionNode ParseRangeExpression()
    {
        var range = new RangeExpressionNode();

        range.AppendChild(ParseIdentfier()); // <identifier>
        range.AppendChild(DietAsNode(TokenType.InKeyword, (t) => throw new UnexpectedTokenException(_currentScript, t))); // in

        range.AppendChild(ParseExpression());  // <value>

        return range;
    }


    private SyntaxNode ParsePacketBody()
    {
        throw new UnexpectedTokenException(_currentScript, Eat());
    }

    private ImportFromNode ParseImport()
    {
        var nodebase = new ImportFromNode();

        nodebase.AppendChild(DietAsNode(TokenType.ImportKeyword, (t)
            => throw new UnexpectedTokenException(_currentScript, t)));

        if (Taste(TokenType.LeftBracketChar))
        {
            var collection = new ImportCollectionNode();

            collection.AppendChild(DietAsNode(TokenType.LeftBracketChar, (t)
                => throw new UnexpectedTokenException(_currentScript, t)));

            while (!Taste(TokenType.RightBracketChar))
            {
                var item = new ImportItemNode();
                
                item.AppendChild(ParseIdentfier());
                if (TryEatAsNode(TokenType.AsKeyword, out var asNode)) {
                    item.AppendChild(asNode);
                    item.AppendChild(ParseIdentfier());
                }

                collection.AppendChild(item);
                if (!Taste(TokenType.CommaChar)) break;
            }

            collection.AppendChild(DietAsNode(TokenType.RightBracketChar, (t)
                => throw new UnexpectedTokenException(_currentScript, t)));

            nodebase.AppendChild(collection);
        }

        nodebase.AppendChild(DietAsNode(TokenType.FromKeyword, (t)
            => throw new UnexpectedTokenException(_currentScript, t)));

        nodebase.AppendChild(ParseIdentfier());

        return nodebase;
    }

    private ParameterCollectionNode ParseParameterCollection()
    {
        var collection = new ParameterCollectionNode();
        collection.AppendChild(DietAsNode(TokenType.LeftPerenthesisChar,
            (t) => throw new UnexpectedTokenException(_currentScript, t)));
        TryEndLine();

        if (!IsEOF() && Bite().type != TokenType.RightParenthesisChar)
        {
            do
            {
                try { collection.AppendChild(ParseTypedIdentifier()); }
                catch (SyntaxException ex) { _errHandler.RegisterError(ex); }
            }
            while(TryEat(TokenType.CommaChar, out _));
        }

        collection.AppendChild(DietAsNode(TokenType.RightParenthesisChar,
            (t) => throw new UnexpectedTokenException(_currentScript, t)));

        return collection;
    }
    private ArgumentCollectionNode ParseArgumentCollection()
    {
        var collection = new ArgumentCollectionNode();
        collection.AppendChild(DietAsNode(TokenType.LeftPerenthesisChar,
            (t) => throw new UnexpectedTokenException(_currentScript, t)));
        TryEndLine();

        if (!IsEOF() && Bite().type != TokenType.RightParenthesisChar)
        {
            do
            {
                try { collection.AppendChild(ParseExpression()); }
                catch (SyntaxException ex) { _errHandler.RegisterError(ex); }
            }
            while(TryEat(TokenType.CommaChar, out _));
        }

        collection.AppendChild(DietAsNode(TokenType.RightParenthesisChar,
            (t) => throw new UnexpectedTokenException(_currentScript, t)));

        return collection;
    }


    private TypeExpressionNode ParseType()
    {
        var type = new TypeExpressionNode();

        if (Taste(TokenType.TypeKeyword))
            type.AppendChild(new IdentifierNode(Eat()));

        else if (TryEatAsNode(TokenType.LeftSquareBracketChar, out var leftBrac))
        {
            var arrayMod = new ArrayTypeModifierNode();

            arrayMod.AppendChild(leftBrac);
            if (!Taste(TokenType.RightSquareBracketChar))
            {
                throw new NotImplementedException();
            }
            arrayMod.AppendChild(DietAsNode(TokenType.RightSquareBracketChar,
            (t) => throw new UnexpectedTokenException(_currentScript, t)));

            arrayMod.AppendChild(ParseType());
            type.AppendChild(arrayMod);
        }

        else if (TryEatAsNode(TokenType.QuestionChar, out var question))
        {
            var nullableMod = new NullableTypeModifierNode();

            nullableMod.AppendChild(question);
            nullableMod.AppendChild(ParseType());
            type.AppendChild(nullableMod);
        }

        else if (TryEatAsNode(TokenType.BangChar, out var bang))
        {
            var failableMod = new FailableTypeModifierNode();

            failableMod.AppendChild(question);
            failableMod.AppendChild(ParseType());
            type.AppendChild(failableMod);
        }

        else if (TryEatAsNode(TokenType.StarChar, out var star))
        {
            var refMod = new ReferenceTypeModifierNode();

            refMod.AppendChild(question);
            refMod.AppendChild(ParseType());
            type.AppendChild(refMod);
        }

        else type.AppendChild(ParseExpression());
        
        return type;
    }
    private TypedIdentifierNode ParseTypedIdentifier()
    {
        var ti = new TypedIdentifierNode();
        ti.AppendChild(ParseType());
        ti.AppendChild(ParseSingleIdentfier());
        return ti;
    }

    private IdentifierCollectionNode ParseIdentfier()
    {
        var collection = new IdentifierCollectionNode(TryEat(TokenType.DotChar, out _));

        collection.AppendChild(ParseSingleIdentfier());

        while (TryEatAsNode(TokenType.DotChar, out _))
        collection.AppendChild(new IdentifierNode(Eat()));

        return collection;
    }
    private IdentifierNode ParseSingleIdentfier()
    {
        return new IdentifierNode(Diet(TokenType.Identifier, (e) => {}));
    }

    private delegate void ParseBlockProcess(BlockNode node, ref bool _break);
    private BlockNode ParseBlock(ParseBlockProcess processContent)
    {
        var block = new BlockNode();
        block.AppendChild(DietAsNode(TokenType.LeftBracketChar,
            (t) => throw new UnexpectedTokenException(_currentScript, t)));
        TryEndLine();

        bool _break = false;
        while (!IsEOF() && !Taste(TokenType.RightBracketChar))
        {
            try {
                processContent(block, ref _break);
                if (_break) break;
            }
            catch (SyntaxException ex) { _errHandler.RegisterError(ex); }
        }

        block.AppendChild(DietAsNode(TokenType.RightBracketChar,
            (t) => throw new UnexpectedTokenException(_currentScript, t)));

        return block;
    }


    #region Utilities
    private Token Bite() => _tokens.Peek();
    private bool Taste(TokenType t) => _tokens.Count > 0 ? _tokens.Peek().type == t : t == TokenType.EOFChar;
    private bool Taste(params TokenType[] t)
        => _tokens.Count > 0 ? t.Contains(_tokens.Peek().type) : t.Contains(TokenType.EOFChar);

    private bool TasteMore(int index, TokenType t)
    {
        var qal = _tokens.ToArray();
        return qal[index].type == t;
    }
    private bool TasteMore(int index, params TokenType[] t)
    {
        var qal = _tokens.ToArray();
        return t.Contains(qal[index].type);
    }

    private Token Eat()
    {
        if (_tokens.Count > 0) return _tokens.Dequeue();
        else return new() {type = TokenType.EOFChar, value = "\\EOF"};
    }
    private TokenNode EatAsNode() => new(Eat());

    private bool TryEat(TokenType t, out Token tkn)
    {
        if (Bite().type == t)
        {
            tkn = Eat();
            return true;
        }
        tkn = new() {type = TokenType.EOFChar, value = "\\EOF"};
        return false;
    }
    private bool TryEatAsNode(TokenType t, out TokenNode node)
    {
        var r = TryEat(t, out var a);
        node = r ? new (a) : null!;
        return r;
    }

    private Token Diet(TokenType t, Action<Token>? errorCallback)
    {
        var c = Taste(t);
        if (!c) errorCallback?.Invoke(Eat());
        else return Eat();
        return default;
    }
    private TokenNode DietAsNode(TokenType t, Action<Token>? errorCallback)
    {
        return new(Diet(t, errorCallback));
    }

    private bool IsEOF() => _tokens.Count == 0 || _tokens.Peek().type == TokenType.EOFChar;
    private bool IsEndOfLine() => _tokens.Count == 0 || _tokens.Peek().type == TokenType.LineFeedChar;

    private void TryEndLine() => TryEat(TokenType.LineFeedChar, out _);
    private void EndLine()
    {
        if (IsEndOfLine()) Eat();
        else throw new UnexpectedTokenException(_currentScript, Eat());
    }
    private void DiscardLine()
    {
        while (!IsEndOfLine()) Eat();
        Eat();
    }
    private void DiscardUntil(TokenType t)
    {
        while (!TryEat(t, out _)) ;
    }
    #endregion

}
