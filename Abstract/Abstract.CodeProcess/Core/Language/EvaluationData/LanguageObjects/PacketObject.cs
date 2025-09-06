using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class PacketObject(string[] g, string n, PacketDeclarationNode synnode)
    : LangObject(g, n),
        IPublicModifier,
        IInternalModifier
{
    public bool Public { get; set; } = false;
    public bool Internal { get; set; } = false;
    
    
    public readonly PacketDeclarationNode syntaxNode = synnode;
    
    public override string ToString()
    {
        var sb = new StringBuilder();
        
        sb.Append(Public ? "public " : "private ");
        if (Internal) sb.Append("internal ");

        sb.AppendLine($"Packet '{Name}' ('{string.Join('.', Global)}')");
        
        return sb.ToString();
    }
}
