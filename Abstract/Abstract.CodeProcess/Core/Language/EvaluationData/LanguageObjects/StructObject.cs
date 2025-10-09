using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class StructObject(string[] g, string n, StructureDeclarationNode synnode)
    : LangObject(g, n),
        IPublicModifier,
        IStaticModifier,
        IInternalModifier,
        IAbstractModifier
{
    public bool Public { get; set; } = false;
    public bool Static { get; set; } = false;
    public bool Internal { get; set; } = false;
    public bool Abstract { get; set; } = false;
    public bool Interface { get; set; } =  false;
    public bool Final { get; set; } =  false;
    
    public TypeReference? Extends { get; set; }
    public (FunctionObject parent, FunctionObject? overrided, bool isSealed)[] VirtualTable { get; set; }
    
    public readonly StructureDeclarationNode syntaxNode = synnode;
    
    public override string ToString()
    {
        var sb = new StringBuilder();
        
        sb.Append(Public ? "public " : "private ");
        sb.Append(Static ? "static " : "instance ");
        if (Internal) sb.Append("internal ");
        sb.Append(Abstract ? "abstract " : "concrete ");

        sb.Append($"Structure '{Name}' ('{string.Join('.', Global)}')");
        if (Extends != null) sb.Append($" extends {Extends}");
        sb.AppendLine(":");
        
        foreach (var c in Children)
        {
            var lines = c.ToString()!.Split(Environment.NewLine);
            foreach (var l in lines) sb.AppendLine($"\t{l}");
        }
        
        return sb.ToString();
    }
}
