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

        try {
            while(tokensList.Count > 0)
                ParseRoot(tree.root, tokensList);

        } catch(Exception ex) {
            Console.WriteLine(ex);
            Console.ReadLine();
        }

        OutputGraph(tree);
        return tree;
    }

    public static void ParseRoot(SyntaxNode parent, List<Token> tokens)
    {
        if (tokens[0].kind == Token.Kind.statement_end) tokens.RemoveAt(0);

        if (tokens[0].kind == Token.Kind.keyword_from)
        {
            var import_node = new SyntaxNode(NodeKind.FromImport);

            import_node.AppendChild(new TokenNode(tokens.Pop()));
            ParseIdentifier(import_node, tokens);
            import_node.AppendChild(new TokenNode(tokens.Pop()));

            parent.AppendChild(import_node);
        }

        else throw new Exception($"{tokens[0].kind} unhandled");
    }

    public static void ParseIdentifier(SyntaxNode parent, List<Token> tokens)
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
        parent.AppendChild(identifierNode);
    }


    private static void OutputGraph(SyntaxTree tree)
    {
        StringBuilder str = new();
        int nodes = 0;

        Stack<(ISyntaxNode parent, int parentid, Queue<ISyntaxNode> children)> stack = [];
        stack.Push((tree.root, 0, new(tree.root.Children)));

        str.AppendLine("graph {");
        str.AppendLine($"\t{nodes++} [label=\"root\" shape=\"rectangle\"]");

        while (stack.Count > 0)
        {
            var (parent, parentid, children) = stack.Peek();

            while (children.Count > 0)
            {
                var i = children.Dequeue();
                var iid = nodes++;

                str.AppendLine($"\t{iid} [label=\"{i.Kind}\" shape=\"rectangle\"]");
                str.AppendLine($"\t{parentid} -> {iid}");

                if (i is SyntaxNode @sn)
                    stack.Push((sn, iid, new(sn.Children)));
            }

            stack.Pop();
        }

        str.AppendLine("}");
        File.WriteAllText("ast.dot", str.ToString());

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
