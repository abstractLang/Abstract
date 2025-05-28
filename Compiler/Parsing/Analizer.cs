using Abstract.Core.Language;
using Abstract.Core.Src;

namespace Abstract.Parsing;

public class Analizer
{

    private Dictionary<string[], ProgramMember> _nametable = [];
    private List<Piece> _pieces = [];

    public void IncludeTrees(IEnumerable<SyntaxTree> trees)
    {
        foreach (var tree in trees) IncludeTree(tree);
    }

    public void IncludeTree(SyntaxTree tree)
    {
        // This function should append the tree data
        // as a single piece, listing all references
        // to build everything as a single thing after

        // Checking if name is valid
        string[] parent_namespace = ["Program", .. tree.src.FileNamespace];
        if (_nametable.TryGetValue(parent_namespace, out var member))
        {
            if (member is not ProgramMember_Namespace) throw new Exception($"two entries with the same name \"{PMNamespace.Combine(parent_namespace)}\"!");
        }
        else _nametable.Add(parent_namespace, new ProgramMember_Namespace(parent_namespace, PMNamespace.Combine(tree.src.FileNamespace)));

        var newPiece = new Piece(["Program", .. tree.src.FileNamespace], tree);
        _pieces.Add(newPiece);

        ParseNodeRecursive(newPiece, null!, tree.root);
    }

    private void ParseNodeRecursive(Piece piece, ProgramMember? parent, ISyntaxNode node)
    {
        SyntaxNode snode;

        switch (node.Kind)
        {
            case NodeKind.Root:
                snode = (SyntaxNode)node;
                foreach (var i in snode.Children) ParseNodeRecursive(piece, null!, i);
                break;

            case NodeKind.FromImport:
                Console.WriteLine(node);
                break;

            case NodeKind.FunctionDeclaration:
                snode = (SyntaxNode)node;
                ParseFunction(piece, parent, snode);
                break;

            case NodeKind.StructureDeclaration:
                snode = (SyntaxNode)node;
                Console.WriteLine($"structure: \"{snode.Children[1]}\"");
                break;

            default:
                Console.WriteLine($"AST node not being analized: {node.Kind}");
                break;
        }
    }
    private void ParseFunction(Piece piece, ProgramMember? parent, SyntaxNode function)
    {
         Console.WriteLine($"function: \"{function.Children[1]}\"");
    }


    public void Compile()
    {

        Console.WriteLine("Nametable:");
        foreach (var i in _nametable)
        {
            Console.WriteLine($"- {PMNamespace.Combine(i.Key),-20}: {i.Value.GetType().Name}");
        }

    }


    class Piece(string[] nmsp, SyntaxTree tree)
    {
        public readonly string[] piece_namespace = nmsp;
        public Script Src => tree.src;
        public SyntaxTree tree = tree;
    }


    // Program members
    abstract class ProgramMember(string[] member_namespace, string identifier)
    {
        public readonly string[] member_namespace = member_namespace;
        public readonly string identifier = identifier;
    }

    class ProgramMember_Namespace(string[] member_namespace, string identifier) : ProgramMember(member_namespace, identifier)
    {

    }
    class ProgramMember_Structure(string[] member_namespace, string identifier) : ProgramMember(member_namespace, identifier)
    {

    }
    class ProgramMember_FunctionGroup(string[] member_namespace, string identifier) : ProgramMember(member_namespace, identifier)
    {

    }

}
