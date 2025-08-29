using System.Text;
using Abstract.CodeProcess.Core;
using Abstract.CodeProcess.Core.Language;
using Abstract.CodeProcess.Core.Language.EvaluationData;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.AttributeReferences;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess;

public class Analizer(ErrorHandler handler)
{
    private ErrorHandler _errorHandler = handler;

    private Dictionary<string[], LangObject> _globalReferenceTable = new(new IdentifierComparer());
    private List<AttributeReference> onHoldAttributes = [];
    
    
    public void Analize(Module[] modules)
    {
        SearchReferences(modules);
        
        DumpGlobalReferenceTable();
    }

    #region Stage One
    
    private void SearchReferences(Module[] modules)
    {
        foreach (var m in modules)
        {
            foreach (var n in m.Namespaces)
            {
                List<string> name = [m.name];
                if (n.Identifier.Length > 0 && !string.IsNullOrEmpty(n.Identifier[0]))
                    name.AddRange(n.Identifier);

                string[] g = [.. name];
                var obj = new NamespaceObject(g, n);
                
                _globalReferenceTable.Add(g, obj);
                SearchNamespaceRecursive(obj);
            }
        }
    }

    private void SearchNamespaceRecursive(NamespaceObject nmsp)
    {
        foreach (var t in nmsp.syntaxNode.Trees)
        {
            foreach (var n in t.Children) SearchGenericScopeRecursive(nmsp, (ControlNode)n);
        }  
    }
    private void SearchGenericScopeRecursive(LangObject parent, ControlNode node)
    {
        if (node is AttributeNode @attr)
        {
            onHoldAttributes.Add(EvaluateAttribute(attr));
            return;
        }

        LangObject obj = node switch
        {
            FunctionDeclarationNode @funcnode => RegisterFunction(parent, funcnode),
            StructureDeclarationNode @structnode => RegisterStructure(parent, structnode),
            TypeDefinitionNode @structnode => RegisterTypedef(parent, structnode),
            TopLevelVariableNode @structnode => RegisterVariable(parent, structnode),
            
            _ => throw new NotImplementedException()
        };

        if (onHoldAttributes.Count <= 0) return;
        obj.AppendAttributes([.. onHoldAttributes]);
        onHoldAttributes.Clear();

    }
    private void SearchTypedefScopeRecursive(LangObject parent, ControlNode node)
    {
        if (node is AttributeNode @attr)
        {
            onHoldAttributes.Add(EvaluateAttribute(attr));
            return;
        }

        LangObject obj = node switch
        {
            FunctionDeclarationNode @funcnode => RegisterFunction(parent, funcnode),
            TypeDefinitionItemNode @typedefitem => RegisterTypedefItem(parent, typedefitem),
            
            _ => throw new NotImplementedException()
        };

        if (onHoldAttributes.Count <= 0) return;
        obj.AppendAttributes([.. onHoldAttributes]);
        onHoldAttributes.Clear();

    }
    
    
    private FunctionObject RegisterFunction(LangObject parent, FunctionDeclarationNode funcnode)
    {
        string[] g = [..parent.Global, funcnode.Identifier.Value];
        FunctionGroupObject funcg = null;
        if (!_globalReferenceTable.TryGetValue(g, out var a))
        {
            funcg = new FunctionGroupObject(g);
            parent.AppendChild(funcg);
            _globalReferenceTable.Add(g, funcg);
        }

        funcg ??= (FunctionGroupObject)a!;

        FunctionObject f = new(g, funcnode);
        funcg.AddOverload(f);
            
        return f;
    }
    private StructObject RegisterStructure(LangObject parent, StructureDeclarationNode structnode)
    {
        string[] g = [..parent.Global, structnode.Identifier.Value];
        StructObject struc = new(g, structnode);
        parent.AppendChild(struc);
        _globalReferenceTable.Add(g, struc);

        foreach (var i in structnode.Body.Content)
            SearchGenericScopeRecursive(struc, (ControlNode)i);

        return struc;
    }
    private TypedefObject RegisterTypedef(LangObject parent, TypeDefinitionNode typedef)
    {
        string[] g = [..parent.Global, typedef.Identifier.Value];
        TypedefObject typd = new(g, typedef);
        parent.AppendChild(typd);
        _globalReferenceTable.Add(g, typd);
                
        foreach (var i in typedef.Body.Content)
            SearchTypedefScopeRecursive(typd, (ControlNode)i);

        return typd;
    }
    private TypedefItemObject RegisterTypedefItem(LangObject parent, TypeDefinitionItemNode typedefitem)
    {
        string[] g = [..parent.Global, typedefitem.Identifier.Value];
        TypedefItemObject typdi = new(g, typedefitem);
        parent.AppendChild(typdi);
        _globalReferenceTable.Add(g, typdi);
        
        return typdi;
    }
    private VariableObject RegisterVariable(LangObject parent, TopLevelVariableNode variable)
    {
        string[] g = [..parent.Global, variable.Identifier.Value];
        VariableObject vari = new(g, variable);
        parent.AppendChild(vari);
        _globalReferenceTable.Add(g, vari);

        return vari;
    }
    
    private static AttributeReference EvaluateAttribute(AttributeNode node)
    {
        var identifierNode = (node.Children[1] as IdentifierCollectionNode)!;
        if (identifierNode.incomplete) goto _default; // All buildins are complete!
        if (identifierNode.Children.Length > 1) goto _default; // All buildins are single-world!
        var attribname = (identifierNode.Children[0] as IdentifierNode)!.Value;

        var builtin = attribname switch
        {
            "static" => BuiltinAttributes.Static,
            "defineGlobal" => BuiltinAttributes.DefineGlobal,
            "align" => BuiltinAttributes.Align,
            "constExp" => BuiltinAttributes.ConstExp,

            "public" => BuiltinAttributes.Public,
            "private" => BuiltinAttributes.Private,
            "internal" => BuiltinAttributes.Internal,
            "final" => BuiltinAttributes.Final,
            "abstract" => BuiltinAttributes.Abstract,
            "interface" => BuiltinAttributes.Interface,
            "extern" => BuiltinAttributes.Extern,
            "virtual" => BuiltinAttributes.Virtual,
            "override" => BuiltinAttributes.Override,
            "allowAccessTo" => BuiltinAttributes.AllowAccessTo,
            "denyAccessTo" => BuiltinAttributes.DenyAccessTo,

            "inline" => BuiltinAttributes.Inline,
            "noinline" => BuiltinAttributes.Noinline,
            "comptime" => BuiltinAttributes.Comptime,
            "runtime" => BuiltinAttributes.Runtime,
            "callconv" => BuiltinAttributes.CallConv,

            "getter" => BuiltinAttributes.Getter,
            "setter" => BuiltinAttributes.Setter,

            "explicitConvert" => BuiltinAttributes.ExplicitConvert,
            "implicitConvert" => BuiltinAttributes.ImplicitConvert,
            "overrideOperator" => BuiltinAttributes.OverrideOperator,
            "indexerGetter" => BuiltinAttributes.IndexerGetter,
            "indexerSetter" => BuiltinAttributes.IndexerSetter,

            _ => BuiltinAttributes._undefined
        };
        if (builtin == BuiltinAttributes._undefined) goto _default;
        return new BuiltInAttributeReference(node, builtin);
        
        _default:
        return new UnsolvedAttributeReference(node);
    }
    
    #endregion
    #region Stage Two
    #endregion
 
    private void DumpGlobalReferenceTable()
    {
        var sb = new StringBuilder();

        foreach (var i in _globalReferenceTable)
        {
            var kind = i.Value switch
            {
                NamespaceObject => "Nmsp",
                FunctionGroupObject => "FnGp",
                FunctionObject => "Func",
                StructObject => "Type",
                TypedefObject => "TDef",
                TypedefItemObject => "DefV",
                VariableObject => "TVar",
                
            };
            sb.AppendLine($"{kind}\t{string.Join('.', i.Key)}");
        }
        
        File.WriteAllText(".abs-cache/debug/reftable.txt", sb.ToString());
    }
    
    private class IdentifierComparer : IEqualityComparer<string[]>
    {
        public bool Equals(string[]? x, string[]? y)
        {
            if (x is null || y is null) return false;
            return x.SequenceEqual(y);
        }

        public int GetHashCode(string[] key)
        {
            var val = string.Join('.', key);
            return HashCode.Combine(val);
        }
    }
    
}
