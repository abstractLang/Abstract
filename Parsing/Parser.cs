using System.Diagnostics;
using System.Text;
using Abstract.Core.Language;

namespace Abstract.Parsing;

public static class Parser
{

    public static SyntaxTree BuildTree(Token[] tokens)
    {
        SyntaxTree tree = new();
        List<Token> tokensList = [.. tokens];

        while(tokensList.Count > 0)
        {
            try { ParseRoot(tree.root, tokensList); } catch {}
        }

        OutputGraph(tree);
        return tree;
    }

    public static void ParseRoot(SyntaxNode parent, List<Token> tokens)
    {
        try
        {
            if (tokens[0].kind == Token.Kind.statement_end) tokens.RemoveAt(0);

            else if (tokens[0].kind == Token.Kind.char_at)
            {
                var attribute_node = new SyntaxNode(NodeKind.Attribute);

                attribute_node.AppendChild(new TokenNode(tokens.Pop()));
                attribute_node.AppendChild(ParseIdentifier(tokens));

                if (tokens[0].kind == Token.Kind.char_open_parenthesis)
                    attribute_node.AppendChild(ParseArguments(tokens));

                parent.AppendChild(attribute_node);
            }

            else if (tokens[0].kind == Token.Kind.keyword_from)
            {
                var import_node = new SyntaxNode(NodeKind.FromImport);

                import_node.AppendChild(new TokenNode(tokens.Pop()));
                import_node.AppendChild(ParseIdentifier(tokens));
                import_node.AppendChild(new TokenNode(tokens.Pop()));

                parent.AppendChild(import_node);
            }

            else if (tokens[0].kind == Token.Kind.keyword_func)
            {
                var func_declaration = new SyntaxNode(NodeKind.FunctionDeclaration);
                func_declaration.AppendChild(new TokenNode(tokens.Pop()));
                func_declaration.AppendChild(ParseSingleIdentifier(tokens));
                func_declaration.AppendChild(ParseParameters(tokens));

                if (tokens[0].kind != Token.Kind.char_open_curlyBracket)
                    func_declaration.AppendChild(ParseType(tokens));

                func_declaration.AppendChild(ParseScope(tokens));

                parent.AppendChild(func_declaration);
            }

            else if (tokens[0].kind is Token.Kind.keyword_let or Token.Kind.keyword_const)
            {
                var variable = new SyntaxNode(NodeKind.Variable);

                variable.AppendChild(new TokenNode(tokens.Pop()));

                // variable identifiers counts only as one tokes, so
                // in the case of
                // >    let foo
                // it will have at least 2 tokens, the variable
                // indicator and the name. the same verification
                // can be still valid if the 3rd token is an equals

                if (tokens[1].kind is not Token.Kind.statement_end and not Token.Kind.char_equals)
                    variable.AppendChild(ParseType(tokens));
                variable.AppendChild(ParseSingleIdentifier(tokens));

                if (tokens[0].kind == Token.Kind.char_equals)
                {
                    variable.AppendChild(new TokenNode(tokens.Pop()));
                    variable.AppendChild(ParseExpression(tokens));
                }

                parent.AppendChild(variable);
            }

            else parent.AppendChild(ParseExpression(tokens));
        }
        catch { /* TODO handle error */ }
    }


    public static ISyntaxNode ParseExpression(List<Token> tokens)
    {
        return ParseArithmeticAddition(tokens);
    }
    public static ISyntaxNode ParseScope(List<Token> tokens)
    {
        if (tokens[0].kind != Token.Kind.char_open_curlyBracket)
            throw new Exception("WHERES THE FUCKING CURLY BRACKET???????????");

        var node = new SyntaxNode(NodeKind.Scope);
        node.AppendChild(new TokenNode(tokens.Pop()));

        while(tokens[0].kind != Token.Kind.char_close_curlyBracket) {
            ParseRoot(node, tokens);
        }

        if (tokens[0].kind != Token.Kind.char_close_curlyBracket)
            throw new Exception("WHERES THE FUCKING CURLY BRACKET???????????");
        node.AppendChild(new TokenNode(tokens.Pop()));

        return node;
    }


    #region Recursive parsing
    // Recursive operation prerecession being procced here
    public static ISyntaxNode ParseAssign(List<Token> tokens)
    {
        var val = ParseValue(tokens);
        // TODO logic
        return val;
    }
    public static ISyntaxNode ParseTypeCasting(List<Token> tokens)
    {
        var val = ParseAssign(tokens);
        // TODO logic
        return val;
    }

    #region bitwise ops
    public static ISyntaxNode ParseBitwise(List<Token> tokens)
    {
        throw new NotImplementedException();
    }
    public static ISyntaxNode ParseBitshift(List<Token> tokens)
    {
        throw new NotImplementedException();
    }
    #endregion
    #region arithmetic ops
    public static ISyntaxNode ParseArithmeticAddition(List<Token> tokens)
    {
        var val = ParseArithmeticMultiplication(tokens);

        while (tokens[0].kind == Token.Kind.char_cross ||
            tokens[0].kind == Token.Kind.char_dash
        )
        {
            var exp = new SyntaxNode(NodeKind.BinaryExpression);
            exp.AppendChild(val);
            exp.AppendChild(new TokenNode(tokens.Pop()));
            exp.AppendChild(ParseArithmeticAddition(tokens));
            val = exp;
        }

        return val;
    }
    public static ISyntaxNode ParseArithmeticMultiplication(List<Token> tokens)
    {
        var val = ParseArithmeticExponentiation(tokens);

        while (tokens[0].kind == Token.Kind.char_star ||
            tokens[0].kind == Token.Kind.char_slash ||
            tokens[0].kind == Token.Kind.char_percent
        )
        {
            var exp = new SyntaxNode(NodeKind.BinaryExpression);
            exp.AppendChild(val);
            exp.AppendChild(new TokenNode(tokens.Pop()));
            exp.AppendChild(ParseArithmeticMultiplication(tokens));
            val = exp;
        }

        return val;
    }
    public static ISyntaxNode ParseArithmeticExponentiation(List<Token> tokens)
    {
        var val = ParseTypeCasting(tokens);
        // TODO logic
        return val;
    }
    #endregion


    public static ISyntaxNode ParseValue(List<Token> tokens)
    {
        if (tokens[0].kind == Token.Kind.identifier)
        {
            var identifier = ParseIdentifier(tokens);

            if (tokens[0].kind == Token.Kind.char_open_parenthesis) // function call
            {
                var call = new SyntaxNode(NodeKind.FunctionCall);
                call.AppendChild(identifier);
                call.AppendChild(ParseArguments(tokens));
                return call;
            }

            return identifier;
        }
        
        else if (tokens[0].kind == Token.Kind.literal_string)
        {
            var str = new SyntaxNode(NodeKind.StringLiteral);
            str.AppendChild(new TokenNode(tokens.Pop()));
            return str;
        }
        
        else if (tokens[0].kind == Token.Kind.literal_integer)
        {
            var inte = new SyntaxNode(NodeKind.IntegerLiteral);
            inte.AppendChild(new TokenNode(tokens.Pop()));
            return inte;
        }

        else throw new Exception($"{tokens[0]} ({tokens.Pop().kind}) unhandled");
    }

    #endregion

    public static ISyntaxNode ParseIdentifier(List<Token> tokens)
    {
        List<TokenNode> onhold = [];

        while(true) {
            if (tokens[0].kind == Token.Kind.char_dot) {
                if (!string.IsNullOrEmpty(tokens[0].trivia.ToString()) || !string.IsNullOrEmpty(tokens[0].after.trivia.ToString()))
                    throw new Exception("dot operator with whitespace");
                
                onhold.Add(new(tokens.Pop()));
            }
            else if (onhold.Count > 0) break;
            
            if (tokens[0].kind == Token.Kind.identifier)
                onhold.Add(new(tokens.Pop()));
            
            else throw new Exception($"{tokens[0].kind} cannot be used in a identifier");
        }

        var identifierNode = new SyntaxNode(NodeKind.Identifier);
        identifierNode.AppendChildren(onhold);

        return identifierNode;
    }
    public static ISyntaxNode ParseSingleIdentifier(List<Token> tokens)
    {
        if (tokens[0].kind != Token.Kind.identifier)
            throw new Exception("WHERE'S THE FUCKING IDENTIFIER??????");
        
        var identifierNode = new SyntaxNode(NodeKind.Identifier);
        identifierNode.AppendChild(new TokenNode(tokens.Pop()));
        return identifierNode;
    }
    public static ISyntaxNode ParseType(List<Token> tokens)
    {
        var typenode = new SyntaxNode(NodeKind.Type);

        if (tokens[0].kind == Token.Kind.char_open_squareBracket)
        {
            typenode.AppendChild(new TokenNode(tokens.Pop()));
            if (tokens[0].kind != Token.Kind.char_close_squareBracket) throw new Exception("WHERE IS THE FUCKING SQUARE BRACKET?????");
            typenode.AppendChild(new TokenNode(tokens.Pop()));
            typenode.AppendChild(ParseType(tokens));
        }

        else if (tokens[0].kind == Token.Kind.char_bang)
        {
            typenode.AppendChild(new TokenNode(tokens.Pop()));
            typenode.AppendChild(ParseType(tokens));
        }

        else if (tokens[0].kind == Token.Kind.identifier)
            typenode.AppendChild(ParseIdentifier(tokens));

        return typenode;
    }


    public static ISyntaxNode ParseArguments(List<Token> tokens)
    {
        var parent = new SyntaxNode(NodeKind.ArgumentsList);

        if (tokens[0].kind != Token.Kind.char_open_parenthesis) throw new Exception("WHERE'S THE FUCKING PARENTHESIS???");
        parent.AppendChild(new TokenNode(tokens.Pop()));

        if (tokens[0].kind != Token.Kind.char_close_parenthesis) while(true) {
            parent.AppendChild(ParseExpression(tokens));

            if (tokens[0].kind != Token.Kind.char_comma) break;
            parent.AppendChild(new TokenNode(tokens.Pop()));
        }

        if (tokens[0].kind != Token.Kind.char_close_parenthesis) throw new Exception("WHERE'S THE FUCKING PARENTHESIS???");
        parent.AppendChild(new TokenNode(tokens.Pop()));

        return parent;
    }
    public static ISyntaxNode ParseParameters(List<Token> tokens)
    {
        var parent = new SyntaxNode(NodeKind.ParametersList);

        if (tokens[0].kind != Token.Kind.char_open_parenthesis) throw new Exception("WHERE'S THE FUCKING PARENTHESIS???");
        parent.AppendChild(new TokenNode(tokens.Pop()));

        if (tokens[0].kind != Token.Kind.char_close_parenthesis) while(true) {

            var item = new SyntaxNode(NodeKind.TypedIdentifier);
            item.AppendChild(ParseType(tokens));
            item.AppendChild(ParseSingleIdentifier(tokens));
            parent.AppendChild(item);

            if (tokens[0].kind != Token.Kind.char_comma) break;
            parent.AppendChild(new TokenNode(tokens.Pop()));
        }

        if (tokens[0].kind != Token.Kind.char_close_parenthesis) throw new Exception("WHERE'S THE FUCKING PARENTHESIS???");
        parent.AppendChild(new TokenNode(tokens.Pop()));

        return parent;
    }


    private static void OutputGraph(SyntaxTree tree)
    {
        StringBuilder nodes = new();
        StringBuilder conns = new();
        int nodecount = 1;

        Queue<(SyntaxNode node, int parentid, int parentidx)> queue = [];

        foreach (var i in tree.root.Children) queue.Enqueue(((SyntaxNode)i, 0, 0));

        while (queue.Count > 0)
        {
            var (node, parentid, parentidx) = queue.Dequeue();
            var iid = nodecount++;

            nodes.Append($"  {iid} [label=\"{{ {node.Kind} | {{");
            if (parentid != 0) conns.AppendLine($"  {parentid}:{parentidx} -- {iid}");

            for (var j = 0; j < node.Children.Length; j++)
            {
                var c = node.Children[j];

                if (c is SyntaxNode @sn)
                {
                    nodes.Append($"<{j}>[{sn.Kind}] | ");
                    queue.Enqueue((sn, iid, j));

                }
                else if (c is TokenNode @tn)
                {
                    nodes.Append($"{
                        tn.Value.ToString()
                            .Replace("\\", "\\\\")
                            .Replace("\"", "\\\"")
                            .Replace("{", "\\{")
                            .Replace("}", "\\}")
                        } | ");
                }
            }

            nodes.Length -= 3;
            nodes.AppendLine($"}}}}\"]");
        }

        StringBuilder merge = new();

        merge.AppendLine("graph {");
        merge.AppendLine($"  node [shape=\"Mrecord\"]");
        merge.Append(nodes);
        merge.Append(conns);
        merge.AppendLine("}");
        File.WriteAllText("ast.dot", merge.ToString());

        var psi = new ProcessStartInfo() {
            FileName = "dot",
            Arguments = "-Tpng ast.dot -o ast.png",
            UseShellExecute = false,
            CreateNoWindow = false
        };

        using Process proc = new() { StartInfo = psi };
        proc.Start();

        proc.WaitForExit();

        //File.Delete("ast.dot");
    }
}

static class ListExtensions
{
    public static Token Pop(this List<Token> l)
    {
        var t = l[0];
        l.RemoveAt(0);
        return t;
    }
    public static void PushLeft(this List<Token> l, Token t)
    {
        l.Insert(0, t);
    }
}
