using System.Collections;
using System.Text;
using Abstract.CodeProcess.Core;
using Abstract.CodeProcess.Core.Language;
using Abstract.CodeProcess.Core.Language.EvaluationData;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess;

public class Analizer(ErrorHandler handler)
{
    private ErrorHandler _errorHandler = handler;

    private Dictionary<string[], LangObject> _globalReferenceTable = new(new IdentifierComparer());
    private List<AttributeNode> onHoldAttributes = [];
    
    public void Analize(Module[] modules)
    {
        SearchReferences(modules);

        DumpGlobalReferenceTable();
    }

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
                SearchNamespace(obj);
            }
        }
    }

    private void SearchNamespace(NamespaceObject nmsp)
    {
        foreach (var t in nmsp.syntaxNode.Trees)
        {
            foreach (var n in t.Children) SearchHeadersRecursive(nmsp, (ControlNode)n);
        }  
    }
    
    private void SearchHeadersRecursive(LangObject parent, ControlNode treeroot)
    {
        if (treeroot is AttributeNode @attr)
        {
            onHoldAttributes.Add(@attr);
            return;
        }

        LangObject obj;
        switch (treeroot)
        {
            case FunctionDeclarationNode @funcnode:
            {
                string[] g = [..parent.Global, funcnode.Identifier.Value];
                FunctionGroupObject funcg = null;
                if (!_globalReferenceTable.TryGetValue(g, out var a))
                {
                    funcg = new FunctionGroupObject(g);
                    _globalReferenceTable.Add(g, funcg);
                }

                funcg ??= (FunctionGroupObject)a!;

                FunctionObject f = new(g, funcnode);
                funcg.AddOverload(f);
            
                obj = f;
                break;
            }
            case StructureDeclarationNode @structnode:
            {
                string[] g = [..parent.Global, structnode.Identifier.Value];
                StructObject struc = new(g, structnode);
                _globalReferenceTable.Add(g, struc);
                obj = struc;
                break;
            }
            case TypeDefinitionNode @typedef:
            {
                string[] g = [..parent.Global, typedef.Identifier.Value];
                TypedefObject typd = new(g, typedef);
                _globalReferenceTable.Add(g, typd);
                obj = typd;
                break;
            }
            case TopLevelVariableNode @variable:
            {
                string[] g = [..parent.Global, variable.Identifier.Value];
                VariableObject vari = new(g, variable);
                _globalReferenceTable.Add(g, vari);
                obj = vari;
                break;
            }
            default:
                throw new NotImplementedException();
        }

        if (onHoldAttributes.Count <= 0) return;
        obj.AppendAttributes([.. onHoldAttributes]);
        onHoldAttributes.Clear();

    }

 
    private void DumpGlobalReferenceTable()
    {
        var sb = new StringBuilder();

        foreach (var i in _globalReferenceTable)
        {
            var kind = i.Value switch
            {
                NamespaceObject => "Nmsp",
                FunctionGroupObject => "FnGp",
                StructObject => "Type",
                TypedefObject => "ypdf",
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
