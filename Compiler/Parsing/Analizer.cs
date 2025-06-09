using System.Collections;
using System.Diagnostics.CodeAnalysis;
using Abstract.Core.Language;
using Abstract.Core.Src;

namespace Abstract.Parsing;

public class Analizer
{

    private Dictionary<string[], ProgramMember> _nametable = new(new StringListComparer());
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
        string[] module_namespace = ["Program"];
        string[] member_name = [..module_namespace, ..tree.src.FileNamespace];

        if (!_nametable.TryGetValue(member_name, out var member))
            member = new ProgramMember_Namespace(module_namespace, PMNamespace.Combine(tree.src.FileNamespace));

        if (member is not ProgramMember_Namespace)
            throw new Exception($"two entries with the same name \"{PMNamespace.Combine(member_name)}\"!");

        _nametable.Add(member_name, member);

        var newPiece = new Piece(member_name, tree);
        _pieces.Add(newPiece);

        ParseNodeRecursive(newPiece, member, tree.root);
    }

    private void ParseNodeRecursive(Piece piece, ProgramMember? parent, ISyntaxNode node)
    {
        SyntaxNode snode;

        switch (node.Kind)
        {
            case NodeKind.Root:
                snode = (SyntaxNode)node;
                foreach (var i in snode.Children) ParseNodeRecursive(piece, parent, i);
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
                ParseStructure(piece, parent, snode);
                break;

            default:
                Console.WriteLine($"AST node not being analized: {node.Kind}");
                break;
        }
    }
    private void ParseFunction(Piece piece, ProgramMember? parent, SyntaxNode function)
    {
        var id = GetIdentifierAsStringArray((SyntaxNode)function.Children[1])[0];
        Console.WriteLine($"function: \"{id}\"");

        string[] name = [.. parent?.Fully_qualified_namespace ?? [], id];

        if (!_nametable.TryGetValue(name, out var pmember))
        {
            pmember = new ProgramMember_FunctionGroup(parent?.member_namespace, id);
            _nametable.Add(name, pmember);
        }

        if (pmember is not ProgramMember_FunctionGroup @fgroup)
            throw new Exception($"identifier {string.Join('.', name)} already used by {pmember.GetType().Name}");

        fgroup.functions.Add(new());
    }
    private void ParseStructure(Piece piece, ProgramMember? parent, SyntaxNode structure)
    {
        var id = GetIdentifierAsStringArray((SyntaxNode)structure.Children[1])[0];
        Console.WriteLine($"structure: \"{id}\"");

        string[] name = [.. parent?.Fully_qualified_namespace ?? [], id];

        if (!_nametable.TryGetValue(name, out var pmember))
        {
            pmember = new ProgramMember_Structure(parent?.member_namespace, id);
            _nametable.Add(name, pmember);
        }
        else throw new Exception($"identifier {string.Join('.', name)} already used by {pmember.GetType().Name}");
    }

    private string[] GetIdentifierAsStringArray(ISyntaxNode identifier)
    {
        List<string> parts = [];

        if (identifier is SyntaxNode @node && node.Kind == NodeKind.Identifier)
        {
            foreach (var i in node.Children) parts.Add(((TokenNode)i).Value.ToString());
        }
        else if (identifier is TokenNode @tkn) return [tkn.Value.ToString()];

        return [.. parts];
    }

    public void Compile()
    {

        Console.WriteLine("Nametable:");
        foreach (var i in _nametable)
        {
            Console.WriteLine($"- {PMNamespace.Combine(i.Key),-50}: {i.Value.GetType().Name}");
        }

    }


    class Piece(string[] nmsp, SyntaxTree tree)
    {
        public readonly string[] piece_namespace = nmsp;
        public Script Src => tree.src;
        public SyntaxTree tree = tree;
    }


    // Program members
    abstract class ProgramMember(string[]? member_namespace, string identifier)
    {
        public readonly string[] member_namespace = member_namespace ?? [];
        public readonly string identifier = identifier;
        public string[] Fully_qualified_namespace => [.. member_namespace, identifier];
    }

    class ProgramMember_Namespace(string[]? member_namespace, string identifier) : ProgramMember(member_namespace, identifier)
    {

    }
    class ProgramMember_Structure(string[]? member_namespace, string identifier) : ProgramMember(member_namespace, identifier)
    {

    }
    class ProgramMember_FunctionGroup(string[]? member_namespace, string identifier) : ProgramMember(member_namespace, identifier)
    {
        public List<SubProgramMember_Function> functions = [];
    }

    // Unispecific
    class SubProgramMember_Function()
    {

    }

    class StringListComparer : IEqualityComparer<string[]>
    {
        public bool Equals(string[]? x, string[]? y)
        {
            if (x == null || y == null) return x == y;
            return x.SequenceEqual(y);
        }

        public int GetHashCode([DisallowNull] string[] obj)
        {
            var hash = new HashCode();
            foreach (var i in obj) hash.Add(i);
            return hash.ToHashCode();
        }
    }


}
