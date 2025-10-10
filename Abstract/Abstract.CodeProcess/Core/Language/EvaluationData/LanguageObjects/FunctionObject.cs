using System.Reflection.Metadata;
using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Metadata;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;
using TypeReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.TypeReference;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class FunctionObject(string[] g, string n, FunctionDeclarationNode synnode)
    : LangObject(g, n),
        IPublicModifier,
        IStaticModifier,
        IInternalModifier,
        IAbstractModifier,
        IVirtualModifier,
        IOverrideAttribute,
        IExternModifier,
        IExportModifier,
        IParametrizable
{
    public bool Public { get; set; } = false;
    public bool Static { get; set; } = false;
    public bool Internal { get; set; } = false;
    public bool Abstract { get; set; } = false;
    public bool Virtual { get; set; } = false;
    public bool Override { get; set; } = false;
    public (string? domain, string? symbol) Extern { get; set; } = (null, null);
    public string? Export { get; set; } = null;
    public bool Generic { get; set; } = false;
    public bool ConstExp { get; set; } = false;

    
    public uint VirtualIndex = 0;

    public TypeReference ReturnType = null!;
    public ParameterObject[] Parameters => [.. _parameters];
    public IRBlock? Body = null;
    

    public readonly FunctionDeclarationNode syntaxNode = synnode;
    private List<ParameterObject> _parameters = [];

    public void AddParameter(params ParameterObject[] parameter)
    {
        var lastidx = _parameters.Count;
        foreach (var p in parameter)
        {
            _parameters.Add(p);
            p.index = lastidx++;
        }
    }

    public override string ToString()
    {
        var sb = new StringBuilder();
        sb.Append(Public ? "public " : "private ");
        sb.Append(Static ? "static " : "instance ");
        if (Internal) sb.Append("internal ");
        sb.Append(Abstract ? "abstract " : "concrete ");
        if (Virtual) sb.Append("virtual ");
        if (Override) sb.Append("override ");
        if (Extern.domain == null && Extern.symbol != null) sb.Append($"import_extern(\"{Extern.symbol}\") ");
        else if (Extern.domain != null && Extern.symbol != null) sb.Append($"import_extern(\"{Extern.domain}\" \"{Extern.symbol}\") ");
        if (Generic) sb.Append("generic ");

        sb.AppendLine($"Function {Name} -> {ReturnType}:");

        foreach (var p in _parameters)
            sb.AppendLine($"\t{p}");

        if (Body == null) sb.AppendLine("\t[Bodyless]");
        else
        {
            var lines = Body.ToString().Split(Environment.NewLine);
            foreach (var l in lines) sb.AppendLine($"\t{l}");
        }
        
        return sb.ToString();
    }
    
}
