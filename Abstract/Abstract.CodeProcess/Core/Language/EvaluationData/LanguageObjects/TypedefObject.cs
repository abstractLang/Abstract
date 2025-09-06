using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class TypedefObject(string[] g, string n, TypeDefinitionNode synnode)
    : LangObject(g, n),
        IPublicModifier,
        IStaticModifier,
        IInternalModifier,
        IAbstractModifier
{
    bool IPublicModifier.Public { get; set; } = false;
    bool IStaticModifier.Static { get; set; } = false;
    bool IInternalModifier.Internal { get; set; } = false;
    bool IAbstractModifier.Abstract { get; set; } = false;
    
    public bool Flags { get; set; } = false;
    
    public readonly TypeDefinitionNode syntaxNode = synnode;
    
    
    public override string ToString()
    {
        var sb = new StringBuilder();
        sb.AppendLine($"Typedef '{Name}' ('{string.Join('.', Global)}'):");

        foreach (var c in Children)
        {
            var lines = c.ToString()!.Split(Environment.NewLine);
            foreach (var l in lines) sb.AppendLine($"\t{l}");
        }
        
        return sb.ToString();
    }
}
