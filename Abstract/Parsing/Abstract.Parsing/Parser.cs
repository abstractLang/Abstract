﻿using System.Diagnostics;
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
        if (tokens[0].kind == Token.Kind.statement_end) tokens.RemoveAt(0);

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
            func_declaration.AppendChild(ParseType(tokens));
            func_declaration.AppendChild(ParseScope(tokens));

            parent.AppendChild(func_declaration);
        }

        else ParseExpression(parent, tokens);
    }


    public static void ParseExpression(SyntaxNode parent, List<Token> tokens)
    {
        var exp = ParseAssign(tokens);
        parent.AppendChild(exp);
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
        var val = ParseTypeCasting(tokens);
        // TODO logic
        return val;
    }
    public static ISyntaxNode ParseTypeCasting(List<Token> tokens)
    {
        var val = ParseArithmeticExponentation(tokens);
        // TODO logic
        return val;
    }

    #region arithmetic ops
    public static ISyntaxNode ParseArithmeticExponentation(List<Token> tokens)
    {
        var val = ParseArithmeticMultiplication(tokens);
        // TODO logic
        return val;
    }
    public static ISyntaxNode ParseArithmeticMultiplication(List<Token> tokens)
    {
        var val = ParseArithmeticAddition(tokens);
        // TODO logic
        return val;
    }
    public static ISyntaxNode ParseArithmeticAddition(List<Token> tokens)
    {
        var val = ParseValue(tokens);
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
                throw new Exception("Do fucking function call rn");
            }

            return identifier;
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
        return ParseIdentifier(tokens);
    }

    public static ISyntaxNode ParseArguments(List<Token> tokens)
    {
        return null!;
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
                    nodes.Append($"{tn.Value} | ");
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
