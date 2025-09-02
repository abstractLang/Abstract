using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class NamespaceObject(string[] g, NamespaceNode synnode)
    : LangObject(g),
        IStaticModifier
{
    public bool Static { get => true; set { } }

    public readonly NamespaceNode syntaxNode = synnode;
    
    public override string ToString()
    {
        var sb = new StringBuilder();
        sb.AppendLine($"Namespace '{string.Join('.', Global)}':");

        foreach (var c in Children)
        {
            var lines = c.ToString()!.Split(Environment.NewLine);
            foreach (var l in lines) sb.AppendLine($"\t{l}");
        }
        
        return sb.ToString();
    }
}
