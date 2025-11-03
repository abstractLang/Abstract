using System.Text;
using Abstract.CodeProcess.Core;
using Abstract.CodeProcess.Core.Language;
using Abstract.CodeProcess.Core.Language.EvaluationData;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.AttributeReferences;

namespace Abstract.CodeProcess;

public partial class Analyzer(ErrorHandler handler)
{
    private readonly ErrorHandler _errorHandler = handler;

    private readonly List<ModuleObject> _modules = [];
    private readonly List<NamespaceObject> _namespaces = [];
    private readonly Dictionary<string[], LangObject> _globalReferenceTable = new(new IdentifierComparer());
    private readonly Stack<List<AttributeReference>> _onHoldAttributes = [];
    
    
    public ProgramObject Analyze(
        Module[] modules,
        bool dumpGlobalTable = false,
        bool dumpEvaluatedData = false)
    {
        // Stage 1
        SearchReferences(modules);
        
        if (dumpEvaluatedData) DumpEvaluatedData();
        if (dumpGlobalTable) DumpGlobalTable();
        
        // Stage 2
        ScanHeadersMetadata();
        
        if (dumpEvaluatedData) DumpEvaluatedData();
        if (dumpGlobalTable) DumpGlobalTable();
        
        // Stage 3
        ScanObjectHeaders();
        ScanObjectBodies();
        
        if (dumpEvaluatedData) DumpEvaluatedData();
        if (dumpGlobalTable) DumpGlobalTable();
        
        // Stage 4
        DoSemanticAnalysis();
        
        // Debug shit
        if (dumpEvaluatedData) DumpEvaluatedData();
        if (dumpGlobalTable) DumpGlobalTable();

        return new ProgramObject([.. _modules], [.. _namespaces]);
    }
    
    
    private void DumpGlobalTable()
    {
        var sb = new StringBuilder();

        foreach (var i in _globalReferenceTable)
        {
            var kind = i.Value switch
            {
                ModuleObject => "Modl",
                NamespaceObject => "Nmsp",
                FunctionGroupObject => "FnGp",
                FunctionObject => "Func",
                StructObject => "Type",
                PacketObject => "Pack",
                TypedefObject => "TDef",
                TypedefItemObject => "DefV",
                FieldObject @fld => fld.Static ? "SFld" : "LFld",
                AliasedObject => "Alia",
                _ => throw new NotImplementedException()
            };
            sb.AppendLine($"{kind}\t{string.Join('.', i.Key)}");
        }
        
        File.WriteAllText(".abs-cache/debug/reftable.txt", sb.ToString());
    }

    private void DumpEvaluatedData()
    {
        var sb = new StringBuilder();

        foreach (var i in _modules)
            sb.AppendLine(i.ToString());
        
        File.WriteAllTextAsync(".abs-cache/debug/eval.txt", sb.ToString());
    }
    
    
}
