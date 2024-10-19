using Abstract.Build;
using Abstract.Build.Core.Exceptions;
using Abstract.Build.Core.Sources;
using Abstract.Parser.Core.Exeptions.Syntax;
using Abstract.Parser.Core.Language;
using Abstract.Parser.Core.Language.AbstractSyntaxTree;
using System.Numerics;

namespace Abstract.Parser;

public partial class SyntaxTreeBuilder (ErrorHandler err)
{
    private readonly ErrorHandler err = err;

    public ScriptRoot Parse(Script script, Token[] tokens)
    {
        var root = new ScriptRoot(script);
        tkns = new(tokens);

        while (!IsEOF())
        {
            try { root.AppendChild(ProcessToken()); }
            catch (CompilerException ex) { err.RegisterError(ex.Script, ex); }
        }

        tkns = null!;
        return root;
    }

    private SyntaxNode ProcessToken()
    {
        var token = Bite();

        SyntaxNode res = null!;
        bool isStatementEnd = true;

        switch (token.type)
        {
            case TokenType.LineFeedChar: Eat();
                /* ignore */ throw new UnexpectedTokenException(token, $"Unexpected end of line (\\n)");

            case TokenType.NamespaceKeyword:
                Eat();
                var namespaceNode = new NamespaceNode();

                var symbol = ParseSymbol();
                namespaceNode.Symbol = new(symbol.tokens, namespaceNode);

                namespaceNode.AppendChildren(ParseScope());

                res = namespaceNode;
                break;

            case TokenType.AtSiginChar:
                Eat();
                var attributeNode = new AttributeNode();

                attributeNode.Symbol = ParseSymbol();
                if (Bite().type == TokenType.LeftPerenthesisChar)
                    attributeNode.Arguments = ParseArgs();

                TryEat(TokenType.LineFeedChar);

                res = attributeNode;
                isStatementEnd = false;
                break;

            case TokenType.FuncKeyword:
                Eat();
                var func = new FunctionNode();

                func.ReturnType = ParseType();
                func.Symbol = new(ParseSymbol(true).tokens, func);

                func.Parameters = ParseParams();

                if (NextIs(TokenType.LeftBracketChar))
                    func.AppendChildren(ParseScope());

                res = func;
                break;

            case TokenType.StructKeyword:
                Eat();
                var struc = new StructureNode();

                struc.Symbol = new(ParseSymbol().tokens, struc);

                if (Bite().type == TokenType.LeftPerenthesisChar)
                    struc.GenericParameters = ParseParams();

                if (TryEat(TokenType.ExtendsKeyword))
                    struc.ExtendsType = ParseType();

                struc.AppendChildren(ParseScope());
                res = struc;
                break;

            case TokenType.LetKeyword or
            TokenType.ConstKeyword:
                bool isConst = Eat().type == TokenType.ConstKeyword;

                var variable = new VariableDeclarationNode(isConst);

                variable.ReturnType = ParseType();
                variable.Symbol = new(ParseSymbol().tokens, variable);

                if (TryEat(TokenType.EqualsChar))
                    variable.Value = ParseExpression();

                res = variable;
                break;

            default:
                res = ParseExpression();
                isStatementEnd = true;
                break;
        }

        if (isStatementEnd) EndStatement();
        return res;
    }

    private SyntaxNode[] ParseScope()
    {
        try
        {
            List<SyntaxNode> result = [];

            Diet(TokenType.LeftBracketChar, (t) => new UnexpectedTokenException(t, $"Scope Expected!"));
            TryEat(TokenType.LineFeedChar);
            while (!IsEOF() && !NextIs(TokenType.RightBracketChar))
            {
                try { result.Add(ProcessToken()); }
                catch (CompilerException ex) { err.RegisterError(ex.Script, ex); }
            }
            Diet(TokenType.RightBracketChar, (t) => new UnexpectedTokenException(t, $"}} Expected to close the scope!"));

            return [.. result];
        }
        catch (CompilerException ex) { err.RegisterError(ex.Script, ex); }
        return [];
    }

    private ParameterCollectionNode ParseParams()
    {
        var _params = new ParameterCollectionNode();

        try
        {
            Diet(TokenType.LeftPerenthesisChar, (t) => new UnexpectedTokenException(t, $"Expected argument collection!"));
            if (!NextIs(TokenType.RightParenthesisChar))
            {
                do
                {
                    var paramItem = new ParameterNode();

                    paramItem.ReturnType = ParseType();
                    paramItem.Symbol = new MasterSymbol(ParseSymbol().tokens, paramItem);

                    _params.AppendChild(paramItem);
                }
                while (!IsEOF() && !NextIs(TokenType.RightParenthesisChar) && TryEat(TokenType.CommaChar));
            }
            Diet(TokenType.RightParenthesisChar, (t) => new UnexpectedTokenException(t, $"Expected closing parenthesis!"));
        }
        catch (CompilerException ex) { err.RegisterError(ex.Script, ex); }

        return _params;
    }
    private ArgumentCollectionNode ParseArgs()
    {
        var args = new ArgumentCollectionNode();

        try
        {
            Diet(TokenType.LeftPerenthesisChar, (t) => new UnexpectedTokenException(t, $"Expected argument collection!"));
            if (!NextIs(TokenType.RightParenthesisChar))
            {
                do { args.AppendChild(ParseExpression()); }
                while (!IsEOF() && TryEat(TokenType.CommaChar));
            }
            Diet(TokenType.RightParenthesisChar, (t) => new UnexpectedTokenException(t, $"Expected closing parenthesis!"));
        }
        catch (CompilerException ex)
        {
            err.RegisterError(ex.Script, ex);
            SkipStatement();
        }

        return args;
    }

    private ExpressionNode ParseExpression()
    {
        return ParseAssignmentExpression();
    }

    private ExpressionNode ParseAssignmentExpression()
    {
        ExpressionNode baseExp = ParseBooleanExpression();

        //while (EatWithCondition(TokenType.EqualsChar))
        //    baseExp = new AssignmentExpression(baseExp, ParseExpression());

        return baseExp;
    }

    private ExpressionNode ParseBooleanExpression()
    {
        ExpressionNode baseExp = ParseAdditiveExpression();

        //while (CurrentIs(
        //    TokenType.EqualOperator, TokenType.UnEqualOperator,
        //    TokenType.LeftAngleChar, TokenType.RightAngleChar,
        //    TokenType.LessEqualsOperator, TokenType.GreatEqualsOperator
        //))
        //    baseExp = new BinaryExpression(Eat().value, baseExp, ParseBooleanExpression());

        return baseExp;
    }

    private ExpressionNode ParseAdditiveExpression()
    {
        ExpressionNode baseExp = ParseMultiplicativeExpression();

        while (NextIs(TokenType.CrossChar, TokenType.MinusChar))
            baseExp = new BinaryOperationExpressionNode(baseExp, Eat().value, ParseMultiplicativeExpression());

        return baseExp;
    }

    private ExpressionNode ParseMultiplicativeExpression()
    {
        ExpressionNode baseExp = ParseUnaryExpression();

        while (NextIs(TokenType.StarChar, TokenType.SlashChar, TokenType.PercentChar))
            baseExp = new BinaryOperationExpressionNode(baseExp, Eat().value, ParseMultiplicativeExpression());

        return baseExp;
    }

    private ExpressionNode ParseUnaryExpression()
    {
        //if (CurrentIs(TokenType.MinusChar, TokenType.CrossChar))
        //{
        //    string unaryOp = Eat().value;
        //    return new UnaryExpression(unaryOp, ParseTypeCasting());
        
        //}
        //else
            return ParseTypeCasting();
    }

    private ExpressionNode ParseTypeCasting()
    {
        ExpressionNode baseExp = VerifyForParenthesis();

        //while (EatWithCondition(TokenType.AsKeyword))
        //    return new TypeCastingExpression(ParseType(), baseExp);

        return baseExp;
    }

    public ExpressionNode VerifyForParenthesis()
    {
        if (TryEat(TokenType.LeftPerenthesisChar))
        {
            var exp = ParseExpression();
            Diet(TokenType.RightParenthesisChar, (t) => new UnexpectedTokenException(t, "Expected closing parenthesis!"));
            return exp;
        }
        else return ParseValue();
    }

    public ValueExpressionNode ParseValue()
    {

        return Bite().type switch
        {
            TokenType.StringLiteral => new StringLiteralValueNode(Eat().value),
            TokenType.IntegerNumberLiteral => new NumericLiteralNode(BigInteger.Parse(Eat().value)),
            TokenType.FloatingNumberLiteral => new FloatingLiteralNode(double.Parse(Eat().value)),

            TokenType.TrueKeyword => new BooleanLiteralValueNode(true),
            TokenType.FalseKeyword => new BooleanLiteralValueNode(false),

            TokenType.Identifier => ParseValueFromIdentifier(),

            TokenType.LeftSquareBracketChar => ParseCollection(),
            _ => throw new UnexpectedTokenException(Bite(), $"Unexpected token {Eat()}"),
        };
    }
    public ValueExpressionNode ParseValueFromIdentifier()
    {
        var symbol = ParseSymbol();

        if (NextIs(TokenType.LeftPerenthesisChar))
        {
            return new MethodCallNode
            {
                Symbol = symbol,
                args = ParseArgs()
            };
        }
        else return new IdentifierNode(symbol);
    }
    public CollectionExpressionNode ParseCollection()
    {
        var collection = new CollectionExpressionNode();

        Diet(TokenType.LeftSquareBracketChar, (t) => new UnexpectedTokenToCollectionInitializationException(t));

        if (!NextIs(TokenType.RightParenthesisChar))
        {
            do { collection.AppendChild(ParseExpression()); }
            while (!IsEOF() && TryEat(TokenType.CommaChar));
        }

        Diet(TokenType.RightSquareBracketChar, (t) => new UnexpectedTokenToCollectionClosingException(t));

        return collection;
    }

    private TypeNode ParseType()
    {
        if (NextIs(TokenType.TypeKeyword))
            return new PrimitiveTypeNode(Eat().value);

        else if (NextIs(TokenType.Identifier))
            return new ReferenceTypeNode(ParseSymbol());

        else if (TryEat(TokenType.LeftSquareBracketChar))
        {
            Diet(TokenType.RightSquareBracketChar, (t)
                => new UnexpectedTokenException(t, "Expecting ] to close the array declaration!"));

            return new ArrayType(ParseType());
        }

        else throw new NotImplementedException(Eat().ToString());
    }
    private TempSymbol ParseSymbol(bool compound = true)
    {
        List<string> symbols = [];

        do symbols.Add(Diet(TokenType.Identifier, (t) => { return new InvalidIdentifierTokenException(t); }).value);
        while (compound && TryEat(TokenType.DotChar));

        Reject(TokenType.DotChar, (t) => new InvalidCompoundIdentifierException(t));

        return new(symbols);
    }


    private void SkipStatement()
    {
        while (!IsEndOfStatement()) Eat();
        while (TryEat(TokenType.LineFeedChar));
    }
    private void EndStatement()
    {
        Diet(TokenType.LineFeedChar);
        while (TryEat(TokenType.LineFeedChar));
    }
}
