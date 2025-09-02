using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class VariableObject(string[] g, TopLevelVariableNode synnode)
    : LangObject(g),
        IPublicModifier,
        IStaticModifier,
        IInternalModifier,
        IAbstractModifier
{
    public bool Constant { get; set; } = false;
    public bool Public { get; set; } = false;
    public bool Static { get; set; } = false;
    public bool Internal { get; set; } = false;
    public bool Abstract { get; set; } = false;
    
    public readonly TopLevelVariableNode syntaxNode = synnode;
    
    public override string ToString()
    {
        var sb = new StringBuilder();
        
        sb.Append(Public ? "public " : "private ");
        sb.Append(Static ? "static " : "instance ");
        if (Internal) sb.Append("internal ");
        sb.Append(Abstract ? "abstract " : "concrete ");

        sb.Append(Constant ? "Constant " : "Variable ");
        sb.AppendLine($"'{string.Join('.', Global)}'");
        
        return sb.ToString();
    }
}
