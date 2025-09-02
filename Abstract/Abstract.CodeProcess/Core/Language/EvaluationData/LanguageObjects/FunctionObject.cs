using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Metadata;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class FunctionObject(string[] g, FunctionDeclarationNode synnode)
    : LangObject(g),
        IPublicModifier,
        IStaticModifier,
        IInternalModifier,
        IAbstractModifier,
        IVirtualModifier,
        IOverrideAttribute,
        IExternModifier,
        
        IParametrizable
{
    public bool Public { get; set; } = false;
    public bool Static { get; set; } = false;
    public bool Internal { get; set; } = false;
    public bool Abstract { get; set; } = false;
    public bool Virtual { get; set; } = false;
    public bool Override { get; set; } = false;
    public bool Extern { get; set; } = false;
    
    public ParameterObject[] Parameters => [.. _parameters];
    

    public readonly FunctionDeclarationNode syntaxNode = synnode;
    private List<ParameterObject> _parameters = [];

    public void AddParameter(ParameterObject parameter) => _parameters.Add(parameter);

    public override string ToString()
    {
        var sb = new StringBuilder();
        sb.Append(Public ? "public " : "private ");
        sb.Append(Static ? "static " : "instance ");
        if (Internal) sb.Append("internal ");
        sb.Append(Abstract ? "abstract " : "concrete ");
        if (Virtual) sb.Append("virtual ");
        if (Override) sb.Append("override ");
        if (Extern) sb.Append("extern ");

        sb.AppendLine($"Function:");

        foreach (var p in _parameters)
        {
            sb.AppendLine($"\t{p}");
        }
        
        return sb.ToString();
    }
    
}
