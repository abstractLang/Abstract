using System.Diagnostics;
using System.Text;
using Abstract.Core.Language;

namespace Abstract.Parsing;

public static class Parser
{

    public static SyntaxTree BuildTree(Token[] tokens)
    {
        SyntaxTree tree = new();

        
        OutputGraph(tree);
        return tree;
    }

    public static void ParseRoot(SyntaxNode parent, List<Token> tokens)
    {

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

                str.AppendLine($"\t{parentid} -- {iid} [label=\"{i.Kind}\" shape=\"rectangle\"]");

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
