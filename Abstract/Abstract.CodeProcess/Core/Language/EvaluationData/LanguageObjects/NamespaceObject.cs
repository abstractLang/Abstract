using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class NamespaceObject(string[] g, string n, NamespaceNode synnode)
    : LangObject(g, n), IStaticModifier
{
    public bool Static { get => true; set { } }
    public Dictionary<string[], (List<NamespaceImport>? specific, NamespaceObject? nmsp)> Imports = new(new IdentifierComparer());

    public readonly NamespaceNode syntaxNode = synnode;
    public override NamespaceObject Namespace => this;

    public void AddImport(string[] nmsp, string[] member, string? alias)
    {
        if (Imports.TryGetValue(nmsp, out var namespaceItem))
        {
            namespaceItem = ([], null);
            Imports.Add(nmsp, namespaceItem);
        }
        if (namespaceItem.specific.Any(e => IdentifierComparer.IsEquals(e.member, member))) return;
        namespaceItem.specific.Add(new NamespaceImport(member, alias));
    }
    public void AddImportAll(string[] nmsp)
    {
        if (!Imports.TryAdd(nmsp, (null, null))) Imports[nmsp] = (null, Imports[nmsp].nmsp);
    }
    
    public override string ToString()
    {
        var sb = new StringBuilder();
        sb.AppendLine($"Namespace '{Name}' ('{string.Join('.', Global)}'):");

        foreach (var i in Imports)
        {
            sb.Append($"from \"{string.Join('.', i.Key)}\" import ");
            if (i.Value.specific == null) sb.AppendLine("*");
            else sb.AppendLine("{" + string.Join(", ", string.Join(", ", i.Value.specific)));
        }
        
        foreach (var c in Children)
        {
            var lines = c.ToString()!.Split(Environment.NewLine);
            foreach (var l in lines) sb.AppendLine($"\t{l}");
        }
        
        return sb.ToString();
    }
    
    public class NamespaceImport(string[] m, string? a)
    {
        public string[] member = m;
        public string? alias = a;
        public LangObject? reference = null;

        public override string ToString() => string.Join('.', member) + (alias == null ? "" : $"as {alias}");
    }
}
