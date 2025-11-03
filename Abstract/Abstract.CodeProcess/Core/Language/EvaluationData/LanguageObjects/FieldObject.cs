using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class FieldObject(string[] g, string n, TopLevelVariableNode synnode, TypeReference t)
    : LangObject(g, n),
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
    
    public TypeReference Type { get; set; } = t;
    
    public readonly TopLevelVariableNode syntaxNode = synnode;


    public IRExpression? Value = null;
    
    public override string ToString()
    {
        var sb = new StringBuilder();
        
        sb.Append(Public ? "public " : "private ");
        sb.Append(Static ? "static " : "instance ");
        if (Internal) sb.Append("internal ");
        sb.Append(Abstract ? "abstract " : "concrete ");

        sb.Append(Constant ? "Constant " : "Variable ");
        sb.AppendLine($"'{Name}' ('{string.Join('.', Global)}') : {Type}");

        if (Value != null)
        {
            sb.Append($" = {Value}");
        }
        
        return sb.ToString();
    }
}
