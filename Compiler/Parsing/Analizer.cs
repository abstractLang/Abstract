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

        ParseScopeRecursive(newPiece, member, tree.root.Children);
    }

    private void ParseScopeRecursive(Piece piece, ProgramMember? parent, ISyntaxNode[] scope)
    {
        List<Attribute> attributes_on_hold = [];
        SyntaxNode snode;

        foreach (var i in scope)
        {
            switch (i.Kind)
            {
                case NodeKind.FromImport:
                    Console.WriteLine(i);
                    break;


                case NodeKind.Attribute:
                    snode = (SyntaxNode)i;
                    attributes_on_hold.Add(ParseAttribute(piece, parent, snode));
                    break;


                case NodeKind.FunctionDeclaration:
                    snode = (SyntaxNode)i;
                    var spmf = ParseFunction(piece, parent, snode);
                    spmf.attributes.AddRange(attributes_on_hold);
                    attributes_on_hold.Clear();
                    break;

                case NodeKind.StructureDeclaration:
                    snode = (SyntaxNode)i;
                    var pms = ParseStructure(piece, parent, snode);
                    pms.attributes.AddRange(attributes_on_hold);
                    attributes_on_hold.Clear();
                    break;

                case NodeKind.Variable:
                    snode = (SyntaxNode)i;
                    var pmf = ParseField(piece, parent, snode);
                    pmf.attributes.AddRange(attributes_on_hold);
                    attributes_on_hold.Clear();
                    break;


                default:
                    Console.WriteLine($"AST node not being analized: {i.Kind}\n{i}");
                    break;
            }
        }

        if (attributes_on_hold.Count > 0) throw new Exception("Unused attributes!");
    }

    private SubProgramMember_Function ParseFunction(Piece piece, ProgramMember? parent, SyntaxNode function)
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

        var f = new SubProgramMember_Function();
        fgroup.functions.Add(f);

        return f;
    }
    private ProgramMember_Structure ParseStructure(Piece piece, ProgramMember? parent, SyntaxNode structure)
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

        // Parse inner scope
        ParseScopeRecursive(piece, pmember, ((SyntaxNode)structure.Children[2]).Children[1..^1]);

        return (ProgramMember_Structure)pmember;
    }
    private ProgramMember_Field ParseField(Piece piece, ProgramMember? parent, SyntaxNode field)
    {
        string id;
        
        if (field.Children.Length == 2) id = GetIdentifierAsStringArray((SyntaxNode)field.Children[1])[0];
        else if (field.Children.Length > 2) id = GetIdentifierAsStringArray((SyntaxNode)field.Children[2])[0];
        else throw new NotImplementedException();

        Console.WriteLine($"field: \"{id}\"");

        string[] name = [.. parent?.Fully_qualified_namespace ?? [], id];

        if (!_nametable.TryGetValue(name, out var pmember))
        {
            pmember = new ProgramMember_Field(parent?.member_namespace, id);
            _nametable.Add(name, pmember);
        }
        else throw new Exception($"identifier {string.Join('.', name)} already used by {pmember.GetType().Name}");

        return (ProgramMember_Field)pmember;
    }

    private Attribute ParseAttribute(Piece piece, ProgramMember? parent, SyntaxNode attrib)
    {
        var newattrb = new Attribute();
        newattrb.identifier = GetIdentifierAsStringArray(attrib.Children[1]);
        if (attrib.Children.Length > 3)
        {
            List<ISyntaxNode> args = [];
            for (var i = 2; i < attrib.Children.Length - 1; i++)
            {
                args.Add(attrib.Children[i]);
            }
            newattrb.args = [.. args];
        }
        return newattrb;
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

            Console.WriteLine($"- {PMNamespace.Combine(i.Key),-35}: {i.Value.GetType().Name,-38} - {string.Join(", ", i.Value.attributes.Select(e => PMNamespace.Combine(e.identifier)))}");

            if (i.Value is ProgramMember_FunctionGroup @funcg)
            {
                for (var j = 0; j < funcg.functions.Count; j++)
                {
                    var f = funcg.functions[j];
                    Console.WriteLine($"\t- {j,-28}: {f.GetType().Name,-38} - {string.Join(", ", f.attributes.Select(e => PMNamespace.Combine(e.identifier)))}");
                }
            }

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
        public List<Attribute> attributes = [];
        public readonly string[] member_namespace = member_namespace ?? [];
        public readonly string identifier = identifier;
        public string[] Fully_qualified_namespace => [.. member_namespace, identifier];
    }

    class ProgramMember_Namespace(string[]? member_namespace, string identifier) : ProgramMember(member_namespace, identifier)
    {

    }
    class ProgramMember_Structure(string[]? member_namespace, string identifier) : ProgramMember(member_namespace, identifier)
    {
        public List<ProgramMember_Field> instanceFields = [];
    }
    class ProgramMember_FunctionGroup(string[]? member_namespace, string identifier) : ProgramMember(member_namespace, identifier)
    {
        public List<SubProgramMember_Function> functions = [];
    }
    class ProgramMember_Field(string[]? member_namespace, string identifier) : ProgramMember(member_namespace, identifier)
    {
        
    }

    // Unispecific
    class SubProgramMember_Function() : ProgramMember(["subfunc"], "")
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

    class Attribute
    {
        public string[] identifier = null!;
        public ISyntaxNode[] args = [];
    }
}
