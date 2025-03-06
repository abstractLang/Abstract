using Abstract.Core.Language;

namespace Abstract.Parsing;

public static class Parser
{

    public static SyntaxTree BuildTree(Token[] tokens)
    {
        SyntaxTree tree = new();

        

        return tree;
    }

    public static void ParseRoot(SyntaxNode parent, List<Token> tokens)
    {

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
