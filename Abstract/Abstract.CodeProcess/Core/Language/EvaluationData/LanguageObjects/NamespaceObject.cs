using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class NamespaceObject(string[] g, string n, NamespaceNode synnode)
    : LangObject(g, n), IStaticModifier
{
    public bool Static { get => true; set { } }

    public readonly NamespaceNode syntaxNode = synnode;
    public override NamespaceObject Namespace => this;
    
    public List<ImportObject> Imports = [];
    
    public override string ToString()
    {
        var sb = new StringBuilder();
        sb.AppendLine($"Namespace '{Name}' ('{string.Join('.', Global)}'):");

        foreach (var c in Children)
        {
            var lines = c.ToString()!.Split(Environment.NewLine);
            foreach (var l in lines) sb.AppendLine($"\t{l}");
        }
        
        return sb.ToString();
    }
    
}
