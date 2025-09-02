using System.Diagnostics;
using System.Text;
using Abstract.CodeProcess.Core;
using Abstract.CodeProcess.Core.Language;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.AttributeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression.TypeModifiers;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Misc;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess;

public class Analizer(ErrorHandler handler)
{
    private ErrorHandler _errorHandler = handler;

    private List<NamespaceObject> _namespaces = [];
    private Dictionary<string[], LangObject> _globalReferenceTable = new(new IdentifierComparer());
    private List<AttributeReference> onHoldAttributes = [];
    
    
    public void Analize(Module[] modules)
    {
        SearchReferences(modules);
        ScanHeadersMetadata();
        
        // Debug shit
        DumpGlobalTable();
        DumpEvaluatedData();
    }

    #region Stage One
    
    /*
     * Stage One:
     *  Iterates though the syntatic tree and
     *  collects the headers for general metadata
     *  generation. All generated data is organized
     *  in a tree and dumped into `_globalReferenceTable`
     */
    
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
                _namespaces.Add(obj);
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
            PacketDeclarationNode @packetnode => RegisterPacket(parent, packetnode),
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
    
    
    private FunctionObject RegisterFunction(LangObject? parent, FunctionDeclarationNode funcnode)
    {
        string[] g = parent != null
            ? [..parent.Global, funcnode.Identifier.Value]
            : [funcnode.Identifier.Value];
        
        FunctionGroupObject? funcg = null;
        if (!_globalReferenceTable.TryGetValue(g, out var a))
        {
            funcg = new FunctionGroupObject(g);
            parent?.AppendChild(funcg);
            _globalReferenceTable.Add(g, funcg);
        }

        var peepoop = funcg ?? (FunctionGroupObject)a!;

        FunctionObject f = new(g, funcnode);
        peepoop.AddOverload(f);
        
        return f;
    }
    private StructObject RegisterStructure(LangObject? parent, StructureDeclarationNode structnode)
    {
        string[] g = parent != null
            ? [..parent.Global, structnode.Identifier.Value]
            : [structnode.Identifier.Value];
        
        StructObject struc = new(g, structnode);
        parent?.AppendChild(struc);
        _globalReferenceTable.Add(g, struc);

        foreach (var i in structnode.Body.Content)
            SearchGenericScopeRecursive(struc, (ControlNode)i);

        return struc;
    }
    private PacketObject RegisterPacket(LangObject? parent, PacketDeclarationNode packetnode)
    {
        string[] g = parent != null
            ? [..parent.Global, packetnode.Identifier.Value]
            : [packetnode.Identifier.Value];
        
        PacketObject packet = new(g, packetnode);
        parent?.AppendChild(packet);
        _globalReferenceTable.Add(g, packet);

        foreach (var i in packetnode.Body.Content)
            SearchGenericScopeRecursive(packet, (ControlNode)i);

        return packet;
    }
    private TypedefObject RegisterTypedef(LangObject? parent, TypeDefinitionNode typedef)
    {
        string[] g = parent != null
            ? [..parent.Global, typedef.Identifier.Value]
            : [typedef.Identifier.Value];
        
        TypedefObject typd = new(g, typedef);
        parent?.AppendChild(typd);
        _globalReferenceTable.Add(g, typd);
                
        foreach (var i in typedef.Body.Content)
            SearchTypedefScopeRecursive(typd, (ControlNode)i);

        return typd;
    }
    private TypedefItemObject RegisterTypedefItem(LangObject? parent, TypeDefinitionItemNode typedefitem)
    {
        string[] g = parent != null
            ? [..parent.Global, typedefitem.Identifier.Value]
            : [typedefitem.Identifier.Value];
        
        TypedefItemObject typdi = new(g, typedefitem);
        parent?.AppendChild(typdi);
        _globalReferenceTable.Add(g, typdi);
        
        return typdi;
    }
    private VariableObject RegisterVariable(LangObject? parent, TopLevelVariableNode variable)
    {
        string[] g = parent != null
            ? [..parent.Global, variable.Identifier.Value]
            : [variable.Identifier.Value];
        
        VariableObject vari = new(g, variable);
        vari.Constant = variable.IsConstant;
        parent?.AppendChild(vari);
        _globalReferenceTable.Add(g, vari);

        return vari;
    }

    private void RegisterAlias(LangObject? parent, LangObject target, string alias)
    {
        string[] g = parent != null ? [..parent.Global, alias] : [alias];
        
        AliasedObject aliased = new(g, target);
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

    /*
     * Stage Two:
     *  Scans all the headers, unwraps the build-in
     *  attributes and evaluate header-level references.
     *  This step should be done early as it may dump
     *  more shit into `_globalReferenceTable`.
     */
    
    private void ScanHeadersMetadata()
    {
        foreach (var reference in _globalReferenceTable.ToArray())
        {
            var langObj = reference.Value;

            if (langObj is FunctionGroupObject @funcg)
            {
                foreach (var o in funcg.Overloads)
                    ProcessHeader(o, langObj.Parent);
            }
            else ProcessHeader(langObj, langObj.Parent);
        }
    }
    private void ProcessHeader(LangObject reference, LangObject parent)
    {
        // Prefer `parent` instead of `reference.Parent` as the first
        // indicates the logical parent, that may not be the same as
        // the actual parent node

        switch (reference)
        {
            case FunctionObject @a: UnwrapFunctionMeta(a); break;
            case StructObject @a: UnwrapStructureMeta(a); break;
        }
        
        // Handling quick inheritance
        if (reference is IStaticModifier @refStatic and not StructObject
            && parent is IStaticModifier @parentStatic)
        {
            refStatic.Static = parentStatic.Static;
        }
        
        // Handling builtin attributes
        foreach (var attr in reference.Attributes)
        {
            if (attr is not BuiltInAttributeReference @builtInAttribute) continue;

            switch (builtInAttribute.Attribute)
            {
                case BuiltinAttributes.Static:
                    if (reference is IStaticModifier @s) s.Static = true; break;
                case BuiltinAttributes.Public:
                    if (reference is IPublicModifier @p) p.Public = true; break;
                case BuiltinAttributes.Private:
                    if (reference is IPublicModifier @p2) p2.Public = false; break;
                case BuiltinAttributes.Internal:
                    if (reference is IInternalModifier @i) i.Internal = true; break;
                case BuiltinAttributes.Final:
                    if (reference is StructObject @f) f.Final = true; break;
                case BuiltinAttributes.Abstract:
                    if (reference is IAbstractModifier @a) a.Abstract = true; break;
                case BuiltinAttributes.Interface:
                    if (reference is StructObject @i2) i2.Interface = true; break;
                case BuiltinAttributes.Virtual:
                    if (reference is IVirtualModifier @v) v.Virtual = true; break;
                case BuiltinAttributes.Override:
                    if (reference is IOverrideAttribute @o) o.Override = true; break;
                case BuiltinAttributes.Extern:
                    if (reference is IExternModifier @e) e.Extern = true; break;

                case BuiltinAttributes.DefineGlobal:
                {
                    var node = builtInAttribute.syntaxNode;
                    // attribute node structure:
                    // <@/> <identifier/> <(> <args.../> <)/>
                    
                    if (node.Children.Length != 3) throw new Exception("'DefineGlobal' expected arguments");
                    var args = (node.Children[2] as ArgumentCollectionNode)!.Arguments;
                    
                    foreach (var arg in args)
                    {
                        if (arg is not StringLiteralNode @strlit)
                            throw new Exception("'DefineGlobal' expected Strings");
                        
                        RegisterAlias(null, reference, strlit.RawContent);
                    }
                } break;
                    
                case BuiltinAttributes.Align:
                case BuiltinAttributes.ConstExp:
                case BuiltinAttributes.AllowAccessTo:
                case BuiltinAttributes.DenyAccessTo:
                case BuiltinAttributes.Inline:
                case BuiltinAttributes.Noinline:
                case BuiltinAttributes.Comptime:
                case BuiltinAttributes.Runtime:
                case BuiltinAttributes.CallConv:
                case BuiltinAttributes.Getter:
                case BuiltinAttributes.Setter:
                case BuiltinAttributes.IndexerGetter:
                case BuiltinAttributes.IndexerSetter:
                case BuiltinAttributes.ExplicitConvert:
                case BuiltinAttributes.ImplicitConvert:
                case BuiltinAttributes.OverrideOperator:
                    break;

                case BuiltinAttributes._undefined:
                default: throw new NotImplementedException();
            }
        }

    }

    private void UnwrapFunctionMeta(FunctionObject function)
    {
        var node = function.syntaxNode;
        var paramc = node.ParameterCollection;

        foreach (var i in paramc.Items)
        {
            var typeref = SolveShallowType(i.Type);
            var name = i.Identifier.Value;
            
            function.AddParameter(new ParameterObject(typeref, name));
        }

    }
    private void UnwrapStructureMeta(StructObject structure)
    {
    }

    /// <summary>
    /// Try to solve constant type forms (arrays, pointers, builtin types, etc.)
    /// and returns `UnsolvedTypeReference` if evaluation-dependent.
    /// </summary>
    /// <param name="node">The type representation</param>
    /// <returns>The evaluation result</returns>
    private TypeReference SolveShallowType(SyntaxNode node)
    {
        while (true)
        {
            switch (node)
            {
                case TypeExpressionNode @texp:
                    if (texp.Children.Length > 1) throw new UnreachableException("Wtf i didn't even knew this was possible");
                    node = texp.Children[0];
                    continue;

                case IdentifierCollectionNode @idc:
                    if (idc.Children.Length != 1) return new UnsolvedTypeReference(idc);
                    node = idc.Children[0];
                    continue;
                
                case IdentifierNode @id:
                    var value = id.Value;
                    switch (value)
                    {
                        case "iptr": return new IntegerTypeReference(true);
                        case "uptr": return new IntegerTypeReference(false);
                        case "void": return new VoidTypeReference();
                        case "type": return new TypeTypeReference();
                        case "anytype": return new AnytypeTypeReference();
;                    }

                    if (value[0] is 'i' or 'u' && value[1..].All(char.IsNumber))
                    {
                        return new IntegerTypeReference(
                            value[0] == 'i',
                            byte.Parse(value[1..]));
                    }

                    return new UnsolvedTypeReference(id);
                
                case ArrayTypeModifierNode @ar:
                    return new SliceTypeReference(SolveShallowType(ar.Type));
                
                case ReferenceTypeModifierNode @rf:
                    return new ReferenceTypeReference(SolveShallowType(rf.Type));
                
                default: throw new NotImplementedException();
            }

            break;
        }
    }

    #endregion
    #region Stage Three
    
    /*
     * Stage Three:
     *  Processes every funtion body into a intermediate
     *  representation that will be used for data storage,
     *  compile time execution, runtime evaluation and
     *  high-level optimizations.
     */
    
    #endregion
    
    private void DumpGlobalTable()
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
                PacketObject => "Pack",
                TypedefObject => "TDef",
                TypedefItemObject => "DefV",
                VariableObject => "TVar",
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

        foreach (var i in _namespaces)
            sb.AppendLine(i.ToString());
        
        File.WriteAllText(".abs-cache/debug/eval.txt", sb.ToString());
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

