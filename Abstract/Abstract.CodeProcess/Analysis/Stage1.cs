using System.Diagnostics;
using Abstract.CodeProcess.Core.Language;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.AttributeReferences;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess;

/*
 * Stage One:
 *  Iterates though the syntatic tree and
 *  collects the headers for general metadata
 *  generation. All generated data is organized
 *  in a tree and dumped into `_globalReferenceTable`
 */

public partial class Analyzer
{
    private void SearchReferences(Module[] modules)
    {
        _modules.Clear();
        _namespaces.Clear();
        _globalReferenceTable.Clear();
        _onHoldAttributes.Clear();
        
        foreach (var m in modules)
        {
            var module = new ModuleObject(m.name);
            foreach (var n in m.Namespaces)
            {
                List<string> name = [m.name];
                if (n.Identifier.Length > 0 && !string.IsNullOrEmpty(string.Join('.', n.Identifier)))
                    name.AddRange(n.Identifier);

                string[] g = [.. name];
                var obj = new NamespaceObject(g, n.Identifier[0], n);
                
                module.AppendChild(obj);
                _globalReferenceTable.Add(g, obj);
                _namespaces.Add(obj);
                SearchNamespaceRecursive(obj);
            }
            _modules.Add(module);
        }

        _modules.TrimExcess();
        _namespaces.TrimExcess();
        _globalReferenceTable.TrimExcess();
        _onHoldAttributes.Clear();
    }

    private void SearchNamespaceRecursive(NamespaceObject nmsp)
    {
        _onHoldAttributes.Push([]);
        foreach (var t in nmsp.syntaxNode.Trees)
        {
            foreach (var n in t.Children) SearchGenericScopeRecursive(nmsp, (ControlNode)n);
        }

        var poppedList = _onHoldAttributes.Pop();
        if (poppedList.Count == 0) return;
        
        foreach (var unbinded in poppedList)
        {
            try { throw new Exception($"Attribute {unbinded} not assigned to any member"); }
            catch (Exception e) { _errorHandler.RegisterError(e); }
        }
    }
    private void SearchGenericScopeRecursive(LangObject parent, ControlNode node)
    {
        switch (node)
        {
            case AttributeNode @attr:
                _onHoldAttributes.Peek().Add(EvaluateAttribute(attr));
                return;
            case FromImportNode @fromimport:
            {
                if (parent is not NamespaceObject @nmsp)
                    throw new Exception("Imports can only be made inside a namespace root");
                RegisterImport(nmsp, fromimport);
                return;
            }
        }

        LangObject obj = node switch
        {
            FunctionDeclarationNode @funcnode => RegisterFunction(parent, funcnode),
            PacketDeclarationNode @packetnode => RegisterPacket(parent, packetnode),
            StructureDeclarationNode @structnode => RegisterStructure(parent, structnode),
            TypeDefinitionNode @typedefnode => RegisterTypedef(parent, typedefnode),
            TopLevelVariableNode @fieldnode => RegisterField(parent, fieldnode),
            
            _ => throw new NotImplementedException()
        };

        if (_onHoldAttributes.Count <= 0) return;
        obj.AppendAttributes([.. _onHoldAttributes.Peek()]);
        _onHoldAttributes.Peek().Clear();

    }
    private void SearchTypedefScopeRecursive(LangObject parent, ControlNode node)
    {
        if (node is AttributeNode @attr)
        {
            _onHoldAttributes.Peek().Add(EvaluateAttribute(attr));
            return;
        }

        LangObject obj = node switch
        {
            FunctionDeclarationNode @funcnode => RegisterFunction(parent, funcnode),
            TypeDefinitionItemNode @item => RegisterTypedefItem(parent, item),
            
            _ => throw new NotImplementedException()
        };

        if (_onHoldAttributes.Count <= 0) return;
        obj.AppendAttributes([.. _onHoldAttributes.Peek()]);
        _onHoldAttributes.Peek().Clear();

    }


    private void RegisterImport(NamespaceObject parent, FromImportNode fromImport)
    {

        if (fromImport.Children.Length < 4)
        {
            var namespaceParts = ((IdentifierCollectionNode)fromImport.Children[1]).Values;
            parent.AddImportAll(namespaceParts);
        }
        else throw new UnreachableException();
    }
    
    private FunctionObject RegisterFunction(LangObject? parent, FunctionDeclarationNode funcnode)
    {
        string[] g = parent != null
            ? [..parent.Global, funcnode.Identifier.Value]
            : [funcnode.Identifier.Value];
        
        FunctionGroupObject? funcg = null;
        if (!_globalReferenceTable.TryGetValue(g, out var a))
        {
            funcg = new FunctionGroupObject(g, funcnode.Identifier.Value);
            parent?.AppendChild(funcg);
            _globalReferenceTable.Add(g, funcg);
        }
        var peepoop = funcg ?? (FunctionGroupObject)a!;

        FunctionObject f = new(g, funcnode.Identifier.Value, funcnode);
        peepoop.AddOverload(f);
        
        return f;
    }
    private StructObject RegisterStructure(LangObject? parent, StructureDeclarationNode structnode)
    {
        string[] g = parent != null
            ? [..parent.Global, structnode.Identifier.Value]
            : [structnode.Identifier.Value];
        
        StructObject struc = new(g, structnode.Identifier.Value, structnode);
        parent?.AppendChild(struc);
        _globalReferenceTable.Add(g, struc);

        do
        {
            _onHoldAttributes.Push([]);
            
            foreach (var i in structnode.Body.Content)
                SearchGenericScopeRecursive(struc, (ControlNode)i);
            
            var poppedList = _onHoldAttributes.Pop();
            if (poppedList.Count == 0) break;
        
            foreach (var unbinded in poppedList)
            {
                try { throw new Exception($"Attribute {unbinded} not assigned to any member"); }
                catch (Exception e) { _errorHandler.RegisterError(e); }
            }
        } while (false);

        return struc;
    }
    private PacketObject RegisterPacket(LangObject? parent, PacketDeclarationNode packetnode)
    {
        string[] g = parent != null
            ? [..parent.Global, packetnode.Identifier.Value]
            : [packetnode.Identifier.Value];
        
        PacketObject packet = new(g, packetnode.Identifier.Value, packetnode);
        parent?.AppendChild(packet);
        _globalReferenceTable.Add(g, packet);

        do
        {
            _onHoldAttributes.Push([]);
            
            foreach (var i in packetnode.Body.Content)
                SearchGenericScopeRecursive(packet, (ControlNode)i);
            
            var poppedList = _onHoldAttributes.Pop();
            if (poppedList.Count == 0) break;
        
            foreach (var unbinded in poppedList)
            {
                try { throw new Exception($"Attribute {unbinded} not assigned to any member"); }
                catch (Exception e) { _errorHandler.RegisterError(e); }
            }
        } while (false);

        return packet;
    }
    private TypedefObject RegisterTypedef(LangObject? parent, TypeDefinitionNode typedef)
    {
        string[] g = parent != null
            ? [..parent.Global, typedef.Identifier.Value]
            : [typedef.Identifier.Value];
        
        TypedefObject typd = new(g, typedef.Identifier.Value, typedef);
        parent?.AppendChild(typd);
        _globalReferenceTable.Add(g, typd);

        do
        {
            _onHoldAttributes.Push([]);
            foreach (var i in typedef.Body.Content)
                SearchTypedefScopeRecursive(typd, (ControlNode)i);

            var poppedList = _onHoldAttributes.Pop();
            if (poppedList.Count == 0) break;

            foreach (var unbinded in poppedList)
            {
                try { throw new Exception($"Attribute {unbinded} not assigned to any member"); }
                catch (Exception e) { _errorHandler.RegisterError(e); }
            }
        } while (false);

        return typd;
    }
    private TypedefItemObject RegisterTypedefItem(LangObject? parent, TypeDefinitionItemNode typedefitem)
    {
        switch (typedefitem)
        {
            case TypeDefinitionNumericItemNode @num:
            {
                var number = num.Value.Value;
                TypedefItemObject tydi = new(null!, number.ToString(), num);
                parent?.AppendChild(tydi);

                return tydi;
            } 

            case TypeDefinitionNamedItemNode @named:
            {
                string[] g = parent != null
                    ? [..parent.Global, named.Identifier.Value]
                    : [named.Identifier.Value];
        
                TypedefItemObject typdi = new(g, named.Identifier.Value, named);
                parent?.AppendChild(typdi);
                _globalReferenceTable.Add(g, typdi);
        
                return typdi;       
            }
            default: throw new UnreachableException();
        }
    }
    private FieldObject RegisterField(LangObject? parent, TopLevelVariableNode variable)
    {
        string[] g = parent != null
            ? [..parent.Global, variable.Identifier.Value]
            : [variable.Identifier.Value];
        
        FieldObject vari = new(g, variable.Identifier.Value, variable, SolveShallowType(variable.Type));
        vari.Constant = variable.IsConstant;
        parent?.AppendChild(vari);
        _globalReferenceTable.Add(g, vari);

        return vari;
    }

    
    private void RegisterAlias(LangObject? parent, LangObject target, string alias)
    {
        string[] g = parent != null ? [..parent.Global, alias] : [alias];
        
        AliasedObject aliased = new(g, alias, target);
        parent?.AppendChild(aliased);
        _globalReferenceTable.Add(g, aliased);
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
            "virtual" => BuiltinAttributes.Virtual,
            "override" => BuiltinAttributes.Override,
            "allowAccessTo" => BuiltinAttributes.AllowAccessTo,
            "denyAccessTo" => BuiltinAttributes.DenyAccessTo,

            "extern" => BuiltinAttributes.Extern,
            "export" => BuiltinAttributes.Export,
            
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
}