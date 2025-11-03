using System.Diagnostics;
using Abstract.CodeProcess.Core;
using Abstract.CodeProcess.Core.Language;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression.TypeModifiers;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Misc;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Statement;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess;

public class Parser(ErrorHandler errHandler)
{

    private ErrorHandler _errHandler = errHandler;
    
    private Token[] _tokens = null!;
    private uint _tokens_cursor = 0;

    public SyntaxTree Parse(Token[] tkns)
    {
        _tokens = tkns;
        _tokens_cursor = 0;

        var tree = new SyntaxTree();

        while(!IsEOF())
        {
            try { tree.AppendChild(ParseRoot()); }
            catch (Exception ex) { Eat(); _errHandler.RegisterError(ex); }
        }

        return tree;

    }

    private ControlNode ParseRoot()
    {
        ControlNode node;
       
        switch (Bite().type)
        {
            case TokenType.StructKeyword:
            try {

                node = new StructureDeclarationNode();
                node.AppendChild(EatAsNode()); // struct
                node.AppendChild(ParseSingleIdentifier()); // <identifier>

                if (Taste(TokenType.LeftPerenthesisChar)) node.AppendChild(ParseParameterCollection()); // generic parameters
                
                bool useExtendsImplements = false;
                var extendsImplements = new ExtendsImplementsNode();
                
                if (TryEatAsNode(TokenType.ExtendsKeyword, out var extends)) // extends
                {
                    extendsImplements.AppendChild(extends);
                    extendsImplements.AppendChild(ParseIdentifierAccess());
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
                node.AppendChild(ParseSingleIdentifier()); // <identifier>
                node.AppendChild(ParseParameterCollection()); // (..., ...)
                node.AppendChild(ParseType()); // <type>
                
                TryEndLine();
                if (Taste(TokenType.LeftBracketChar))
                {
                    node.AppendChild(ParseBlock((BlockNode n, ref bool _)
                        => n.AppendChild(ParseFunctionBody()))); // {...}
                }

            }
            catch { DiscardLine(); throw; }
            break;

            case TokenType.PacketKeyword:
            try{

                node = new PacketDeclarationNode();
                node.AppendChild(EatAsNode()); // packet
                node.AppendChild(ParseSingleIdentifier()); // <identifier>
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

            case TokenType.TypedefKeyword:
            {
                node = new TypeDefinitionNode();
                node.AppendChild(EatAsNode()); // typedef
                node.AppendChild(ParseSingleIdentifier()); // identifier
                if (Taste(TokenType.LeftPerenthesisChar))
                    node.AppendChild(ParseArgumentCollection()); // (T)

                node.AppendChild(ParseBlock((BlockNode block, ref bool _break) =>
                {
                    var value = Bite();
                    switch (value.type)
                    {
                        case TokenType.RangeOperator:
                        case TokenType.IntegerNumberLiteral when TasteMore(1, TokenType.RangeOperator):
                            throw new NotImplementedException();

                        case TokenType.IntegerNumberLiteral:
                        {
                            var n = new TypeDefinitionNumericItemNode();
                            n.AppendChild(ParseValue(false));
                            block.AppendChild(n);
                        } break;
                        
                        default: throw new NotImplementedException();
                    }
                    
                    // var i = new TypeDefinitionNamedItemNode();
                    //
                    // i.AppendChild(ParseSingleIdentifier()); // <ident>
                    // if (TryEatAsNode(TokenType.EqualsChar, out var node))
                    // {
                    //     i.AppendChild(node); // =
                    //     i.AppendChild(ParseExpression()); // <exp>
                    // }

                    if (!TryEat(TokenType.CommaChar, out _)) _break = true;
                    TryEndLine();
                }));

            } break;

            case TokenType.FromKeyword:
                node = ParseImport();
                break;

            case TokenType.AtSiginChar:
            try{

                node = new AttributeNode();
                node.AppendChild(EatAsNode());
                node.AppendChild(ParseIdentifierAccess());

                if (Taste(TokenType.LeftPerenthesisChar))
                    node.AppendChild(ParseArgumentCollection());

                TryEndLine();

            } catch { DiscardLine(); throw; }
            break;

            // TODO constructors and destructors

            default: throw new Exception(
                $"Unexpected token {Bite()} at position {_tokens_cursor}");
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
                if (Taste(TokenType.ElifKeyword, TokenType.ElseKeyword)) endLine = false;

            } catch { DiscardLine(); throw; }
            break;
            
            case TokenType.ElifKeyword:
            try {

                node = new ElifStatementNode();
                node.AppendChild(EatAsNode()); // elif
                node.AppendChild(ParseExpression()); // <condition>
                TryEndLine();
                node.AppendChild(ParseStatement()); // <statement>
                if (Taste(TokenType.ElifKeyword, TokenType.ElseKeyword)) endLine = false;

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
                node.AppendChild(ParseExpression());
                if (TryEatAsNode(TokenType.ColonChar, out var c1))
                {
                    node.AppendChild(c1);
                    node.AppendChild(ParseExpression());
                }
                if (TryEatAsNode(TokenType.ColonChar, out var c2))
                {
                    node.AppendChild(c2);
                    node.AppendChild(ParseExpression());
                }
                node.AppendChild(DietAsNode(TokenType.DoKeyword, (t)
                    => throw new Exception($"Unexpected token '{Bite()}'")));
                node.AppendChild(ParseStatement()); // <statement>

            } catch { DiscardLine(); throw; }
            break;
            case TokenType.ForKeyword:
            try {

                node = new ForStatementNode();
                node.AppendChild(EatAsNode()); // for
                node.AppendChild(ParseRangeExpression()); // <range>
                node.AppendChild(DietAsNode(TokenType.RightArrowOperator, (t)  // =>
                    => throw new Exception($"Unexpected token '{Bite()}'")));
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
                node = ParseExpression();
                break;
        }

        if (endLine) EndLine();
        return node;
    }

    private ExpressionNode ParseExpression(bool recursive = false)
    {
        var t = _tokens[_tokens_cursor];
        var node = ParseAssignmentExpression(recursive);
        return node;
    }

    #region Recursive expression parsing
    
    private ExpressionNode ParseAssignmentExpression(bool recursive)
    {
        var node = ParseAdditiveExpression(recursive);

        if (!Taste(
                TokenType.EqualsChar, // =
                TokenType.AddAssigin, // +=
                TokenType.SubAssigin, // -=
                TokenType.MulAssigin, // *=
                TokenType.DivAssigin, // /=
                TokenType.RestAssigin // %=
            )) return node;
        
        var n = new AssignmentExpressionNode();
        n.AppendChild(node);
        n.AppendChild(EatAsNode());
        n.AppendChild(ParseAssignmentExpression(recursive));
        node = n;

        return node;
    }

    
    private ExpressionNode ParseAdditiveExpression(bool recursive)
    {
        var node = ParseMultiplicativeExpression(recursive);

        if (!Taste(
                TokenType.CrossChar, // +
                TokenType.AddOnBoundsOperator, // +|
                TokenType.AddWarpOperator, // +%
                TokenType.MinusChar, // -
                TokenType.SubOnBoundsOperator, // -|
                TokenType.SubWarpOperator // -%
            )) return node;
        
        var n = new BinaryExpressionNode();
        n.AppendChild(node);
        n.AppendChild(EatAsNode());
        n.AppendChild(ParseAdditiveExpression(recursive));
        node = n;

        return node;
    }
    
    private ExpressionNode ParseMultiplicativeExpression(bool recursive)
    {
        var node = ParseBooleanOperationExpression(recursive);

        if (!Taste(
                TokenType.StarChar, // *
                TokenType.SlashChar, // /
                TokenType.DivideCeilOperator, // /^
                TokenType.DivideFloorOperator, // /_
                TokenType.PercentChar, // %
                TokenType.PowerOperator // **
            )) return node;
        
        var n = new BinaryExpressionNode();
        n.AppendChild(node);
        n.AppendChild(EatAsNode());
        n.AppendChild(ParseMultiplicativeExpression(recursive));
        node = n;

        return node;
    }

    private ExpressionNode ParseBooleanOperationExpression(bool recursive)
    {
        var node = ParseBitwiseOperationExpression(recursive);

        if (!Taste(
            TokenType.EqualOperator, // ==
            TokenType.UnequalOperator, // !=
            
            TokenType.ExactEqualOperator, // ===
            TokenType.ExactUnequalOperator, // !==
            
            TokenType.LeftAngleChar, // <
            TokenType.RightAngleChar, // >
            
            TokenType.LessEqualsOperator, // <=
            TokenType.GreatEqualsOperator, // >=
            
            TokenType.AndOperator, // and
            TokenType.OrOperator // or
        )) return node;
        
        var n = new BooleanOperationExpressionNode();
        n.AppendChild(node);
        n.AppendChild(EatAsNode());
        n.AppendChild(ParseBooleanOperationExpression(recursive));
        node = n;

        return node;
    }

    private ExpressionNode ParseBitwiseOperationExpression(bool recursive)
    {
        var node = ParseCastingExpression(recursive);

        if (!Taste(
                TokenType.PipeChar, // |
                TokenType.AmpersandChar, // &
                TokenType.CircumflexChar, // ^
            
                TokenType.BitShiftLeftOperator, // <<
                TokenType.BitShiftRightOperator // >>
            )) return node;
        
        var n = new BinaryExpressionNode();
        n.AppendChild(node);
        n.AppendChild(EatAsNode());
        n.AppendChild(ParseBooleanOperationExpression(recursive));
        node = n;

        return node;
    }
    
    private ExpressionNode ParseCastingExpression(bool recursive)
    {
        var node = ParseUnaryExpression(recursive);

        if (!Taste(TokenType.AsKeyword)) return node;

        var n = new TypeCastNode();
        n.AppendChild(node);
        n.AppendChild(EatAsNode());
        n.AppendChild(ParseType());
        
        return n;
    }
    private ExpressionNode ParseUnaryExpression(bool recursive)
    {
        ExpressionNode node;

        if(Taste(
               TokenType.CrossChar, // +
               TokenType.MinusChar,  // -
               TokenType.AmpersandChar,  // &
               TokenType.IncrementOperator, // ++
               TokenType.DecrementOperator // --
           ))
        {
            node = new UnaryPrefixExpressionNode();
            node.AppendChild(EatAsNode());
            node.AppendChild(ParseValue(recursive));
        }
        else node = ParseValue(recursive);

        if (!Taste(
                TokenType.IncrementOperator, // ++,
                TokenType.DecrementOperator // --
            )) return node;
        
        var n = new UnaryPostfixExpressionNode();
        n.AppendChild(node);
        n.AppendChild(EatAsNode());
        node = n;

        return node;
    }    
    
    #endregion

    private ExpressionNode ParseValue(bool recursive)
    {
        ExpressionNode node;

        switch (Bite().type)
        {
            // Identifiers & function calls
            case TokenType.Identifier or
            TokenType.DotChar:
            try
            {
               
                var identifier = recursive
                    ? ParseSingleIdentifier()
                    : ParseIdentifierAccess();

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

            // Constructor invoke
            case TokenType.NewKeyword:
            {
                
                // Possible constructor syntax
                //  new <Type>(args...)
                //  new <Type(generic...)>(args...)
                
                node = new NewObjectNode();
                node.AppendChild(EatAsNode()); // new
                
                // ParseType will parse it as a generic.
                // After it the compiler must verify
                // if there is a following argument block.
                // If not, it must dissect the returned
                // type expression.
                
                var type = ParseType();

                if (Taste(TokenType.LeftPerenthesisChar))
                {
                    node.AppendChild(type);
                    node.AppendChild(ParseArgumentCollection());
                }
                else
                {
                    if (type is FunctionCallExpressionNode)
                    {
                        var typeExp = type.Children[0];
                        var argsExp = type.Children[1];

                        type.RemoveChild(typeExp);
                        type.RemoveChild(argsExp);
                        
                        node.AppendChild(typeExp);
                        node.AppendChild(argsExp);
                    }
                }

                if (TryEatAsNode(TokenType.LeftBracketChar, out var n))
                {
                    var block = new BlockNode();
                    block.AppendChild(n);

                    while (!IsEOF() && !Taste(TokenType.RightBracketChar))
                    {
                        var i = new AssignmentExpressionNode();

                        i.AppendChild(ParseIdentifierAccess());
                        i.AppendChild(DietAsNode(TokenType.EqualsChar,
                            t => throw new Exception("Expected assignment operator")));
                        i.AppendChild(ParseExpression());
                        EndLine();
                        
                        block.AppendChild(i);
                    }
                    
                    block.AppendChild(DietAsNode(TokenType.RightBracketChar,
                        (t) => throw new Exception($"Expected closing bracket, found {t}")));
                    node.AppendChild(block);
                }
                
            } break;
            
            // Parenthesis enclosed expression
            case TokenType.LeftPerenthesisChar:
            try {

                node = new ParenthesisExpressionNode();

                node.AppendChild(DietAsNode(TokenType.LeftPerenthesisChar, (t) => throw new Exception($"Unexpected token '{Bite()}'")));
                node.AppendChild(ParseExpression());
                node.AppendChild(DietAsNode(TokenType.RightParenthesisChar, (t) => throw new Exception($"Unexpected token '{Bite()}'")));

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

                    else if (Taste(TokenType.EscapedLeftBracket))
                    {
                        var interpNode = new StringInterpolationNode();
                        interpNode.AppendChild(EatAsNode());
                        interpNode.AppendChild(ParseExpression());
                        interpNode.AppendChild(DietAsNode(TokenType.RightBracketChar, (t)
                            => throw new Exception($"Unexpected token '{Bite()}'")));
                        node.AppendChild(interpNode);
                    }

                    else if (Taste(TokenType.CharacterLiteral))
                        node.AppendChild(new CharacterLiteralNode(Eat()) { insideString = true });

                    else if (Taste(TokenType.DoubleQuotes)) break;

                    else throw new Exception($"Unexpected token '{Bite()}'");
                }
                node.AppendChild(DietAsNode(TokenType.DoubleQuotes, (e)
                    => throw new Exception($"Unexpected token '{Bite()}'")));

                break;

            // collections (or type arrays)
            case TokenType.LeftSquareBracketChar:
                int index = 0;
                while(!TasteMore(index++, TokenType.RightSquareBracketChar));

                if (TasteMore(index,
                    TokenType.TypeKeyword,
                    TokenType.Identifier,
                    TokenType.StarChar
                )) node = ParseType();

                node = ParseGenericCollection();
                break;
            
            // types
            case TokenType.TypeKeyword
            or TokenType.StarChar
            or TokenType.BangChar
            or TokenType.QuestionChar:
                return ParseType();

            // null
            case TokenType.NullKeyword:
                return new NullLiteralNode(Eat());

            default: throw new Exception($"Unexpected token {Bite()}");
        }

        while (TryEatAsNode(TokenType.DotChar, out var n))
        {
            var newnode = new AccessNode();
            newnode.AppendChild(node);
            newnode.AppendChild(n);
            newnode.AppendChild(ParseExpression());
            node = newnode;
        }
        
        return node;
    }

    private GenericCollectionExpressionNode ParseGenericCollection()
    {
        var collection = new GenericCollectionExpressionNode();
        collection.AppendChild(DietAsNode(TokenType.LeftSquareBracketChar,
            (e) => throw new Exception($"Unexpected token '{Bite()}'")));

        if (!Taste(TokenType.RightSquareBracketChar))
            do collection.AppendChild(ParseExpression());
            while(TryEat(TokenType.CommaChar, out _));

        collection.AppendChild(DietAsNode(TokenType.RightSquareBracketChar,
            (e) => throw new Exception($"Unexpected token '{Bite()}'")));
        return collection;
    }

    private RangeExpressionNode ParseRangeExpression()
    {
        var range = new RangeExpressionNode();

        range.AppendChild(ParseIdentifierAccess()); // <identifier>
        range.AppendChild(DietAsNode(TokenType.InKeyword, (t)
            => throw new Exception($"Unexpected token '{Bite()}'"))); // in

        range.AppendChild(ParseExpression());  // <value>

        return range;
    }
    
    private SyntaxNode ParsePacketBody()
    {
        throw new Exception($"TODO");
    }

    private FromImportNode ParseImport()
    {
        var nodebase = new FromImportNode();
        
        nodebase.AppendChild(DietAsNode(TokenType.FromKeyword, (t)
            => throw new Exception($"Unexpected token '{Bite()}'")));

        nodebase.AppendChild(ParseIdentifierAccess());

        nodebase.AppendChild(DietAsNode(TokenType.ImportKeyword, (t)
            => throw new Exception($"Unexpected token '{Bite()}'")));

        if (!Taste(TokenType.LeftBracketChar)) return nodebase;
        
        {
            var collection = new ImportCollectionNode();

            collection.AppendChild(DietAsNode(TokenType.LeftBracketChar, (t)
                => throw new Exception($"Unexpected token '{Bite()}'")));

            while (!Taste(TokenType.RightBracketChar))
            {
                var item = new ImportItemNode();
                
                item.AppendChild(ParseIdentifierAccess());
                if (TryEatAsNode(TokenType.AsKeyword, out var asNode)) {
                    item.AppendChild(asNode);
                    item.AppendChild(ParseIdentifierAccess());
                }

                collection.AppendChild(item);
                if (!Taste(TokenType.CommaChar)) break;
            }

            collection.AppendChild(DietAsNode(TokenType.RightBracketChar, (t)
                => throw new Exception($"Unexpected token '{Bite()}'")));

            nodebase.AppendChild(collection);
        }
        
        return nodebase;
    }

    private ParameterCollectionNode ParseParameterCollection()
    {
        var collection = new ParameterCollectionNode();
        collection.AppendChild(DietAsNode(TokenType.LeftPerenthesisChar,
            (t) => throw new Exception($"Unexpected token '{Bite()}'")));
        TryEndLine();

        if (!IsEOF() && Bite().type != TokenType.RightParenthesisChar)
        {
            do
            {
                try { collection.AppendChild(ParseTypedIdentifier()); }
                catch (Exception ex) { _errHandler.RegisterError(ex); }
            }
            while(TryEat(TokenType.CommaChar, out _));
        }

        collection.AppendChild(DietAsNode(TokenType.RightParenthesisChar,
            (t) => throw new Exception($"Unexpected token '{Bite()}'")));

        return collection;
    }
    private ArgumentCollectionNode ParseArgumentCollection()
    {
        var collection = new ArgumentCollectionNode();
        collection.AppendChild(DietAsNode(TokenType.LeftPerenthesisChar,
            (t) => throw new Exception($"Unexpected token {Bite()}")));
        TryEndLine();

        if (!IsEOF() && Bite().type != TokenType.RightParenthesisChar)
        {
            do
            {
                try { collection.AppendChild(ParseExpression()); }
                catch (Exception ex) { _errHandler.RegisterError(ex); }
            }
            while(TryEat(TokenType.CommaChar, out _));
        }

        collection.AppendChild(DietAsNode(TokenType.RightParenthesisChar,
            (t) => throw new Exception($"Expected token ')', found {t}")));

        return collection;
    }


    private ExpressionNode ParseType()
    {
        if (Taste(TokenType.TypeKeyword)) return new IdentifierNode(Eat());

        else if (TryEatAsNode(TokenType.LeftSquareBracketChar, out var leftBrac))
        {
            var arrayMod = new ArrayTypeModifierNode();

            arrayMod.AppendChild(leftBrac);
            if (!Taste(TokenType.RightSquareBracketChar))
            {
                throw new NotImplementedException();
            }
            arrayMod.AppendChild(DietAsNode(TokenType.RightSquareBracketChar,
            (t) => throw new Exception($"Unexpected token '{Bite()}'")));

            arrayMod.AppendChild(ParseType());
            return arrayMod;
        }

        else if (TryEatAsNode(TokenType.QuestionChar, out var question))
        {
            var nullableMod = new NullableTypeModifierNode();

            nullableMod.AppendChild(question);
            nullableMod.AppendChild(ParseType());
            return nullableMod;
        }

        else if (TryEatAsNode(TokenType.BangChar, out var bang))
        {
            var failableMod = new FailableTypeModifierNode();

            failableMod.AppendChild(bang);
            failableMod.AppendChild(ParseType());
            return failableMod;
        }

        else if (TryEatAsNode(TokenType.StarChar, out var star))
        {
            var refMod = new ReferenceTypeModifierNode();

            refMod.AppendChild(star);
            refMod.AppendChild(ParseType());
            return refMod;
        }

        return ParseExpression();
    }
    private TypedIdentifierNode ParseTypedIdentifier()
    {
        var ti = new TypedIdentifierNode();
        ti.AppendChild(ParseType());
        ti.AppendChild(ParseSingleIdentifier());
        return ti;
    }

    private ExpressionNode ParseIdentifierAccess()
    {
        ExpressionNode node;

        if (TryEatAsNode(TokenType.DotChar, out var n))
        {
            node = new AccessNode();
            node.AppendChild(new ImplicitAccessNode());
            node.AppendChild(n);
            node.AppendChild(ParseExpression());
        }
        else node = ParseSingleIdentifier();


        while (true)
        {
            switch (Bite().type)
            {
                case TokenType.DotChar:
                {
                    var newnode = new AccessNode();
                    newnode.AppendChild(node);
                    newnode.AppendChild(EatAsNode());
                    newnode.AppendChild(ParseSingleIdentifier());
                    node = newnode;
                } break;
                
                default: goto loopbreak;
            }
        }
        loopbreak:

        return node;
    }
    private IdentifierNode ParseSingleIdentifier()
    {
        return new IdentifierNode(Diet(TokenType.Identifier, (e) => {}));
    }

    private delegate void ParseBlockProcess(BlockNode node, ref bool _break);
    private BlockNode ParseBlock(ParseBlockProcess processContent)
    {
        var block = new BlockNode();
        block.AppendChild(DietAsNode(TokenType.LeftBracketChar,
            (t) => throw new Exception($"Unexpected token '{Bite()}'")));
        TryEndLine();

        bool _break = false;
        while (!IsEOF() && !Taste(TokenType.RightBracketChar))
        {
            try {
                processContent(block, ref _break);
                if (_break) break;
            }
            catch (Exception ex) { _errHandler.RegisterError(ex); }
        }

        block.AppendChild(DietAsNode(TokenType.RightBracketChar,
            (t) => throw new Exception($"Unexpected token {Bite()}")));

        return block;
    }


    #region Utilities
    private Token Bite() => _tokens[_tokens_cursor];
    private bool Taste(TokenType t) => _tokens_cursor < _tokens.Length
        ? _tokens[_tokens_cursor].type == t
        : t == TokenType.EofChar;
    private bool Taste(params TokenType[] t) => _tokens_cursor < _tokens.Length
            ? t.Contains(_tokens[_tokens_cursor].type)
            : t.Contains(TokenType.EofChar);
    
    private bool TasteMore(int index, params TokenType[] t)
    {
        return t.Contains(_tokens[_tokens_cursor + index].type);
    }

    private Token Eat()
    {
        return _tokens.Length >= _tokens_cursor
            ? _tokens[_tokens_cursor++]
            : new Token {type = TokenType.EofChar, value = "\\EOF".AsMemory()};
    }
    private TokenNode EatAsNode() => new(Eat());

    private bool TryEat(TokenType t, out Token tkn)
    {
        if (Bite().type == t)
        {
            tkn = Eat();
            return true;
        }
        tkn = new Token {type = TokenType.EofChar, value = "\\EOF".AsMemory()};
        return false;
    }
    private bool TryEatAsNode(TokenType t, out TokenNode node)
    {
        var r = TryEat(t, out var a);
        node = r ? new TokenNode(a) : null!;
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
        return new TokenNode(Diet(t, errorCallback));
    }

    private bool IsEOF() => _tokens_cursor >= _tokens.Length || _tokens[_tokens_cursor].type == TokenType.EofChar;
    private bool IsEndOfLine() => _tokens_cursor >= _tokens.Length || _tokens[_tokens_cursor].type == TokenType.LineFeedChar;

    private void TryEndLine() => TryEat(TokenType.LineFeedChar, out _);
    private void EndLine()
    {
        if (IsEndOfLine()) Eat();
        else throw new Exception($"Unexpected token {Bite()}. Expected end of line");
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
