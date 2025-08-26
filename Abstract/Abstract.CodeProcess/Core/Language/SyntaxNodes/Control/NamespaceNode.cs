using System.Text;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

public class NamespaceNode(string identifier)
{
    public readonly string[] Identifier = identifier.Split('.');
    private readonly List<SyntaxTree> _trees = [];

    public SyntaxTree[] Trees => [.. _trees];
    
    public void AddTree(SyntaxTree tree) => _trees.Add(tree);


    public override string ToString() => identifier;

    public string ContentToString()
    {
        var sb = new StringBuilder();

        foreach (var t in _trees) sb.AppendLine(t.ToString());
        
        return sb.ToString();
    }
}
