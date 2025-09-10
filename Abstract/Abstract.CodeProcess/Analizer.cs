using System.Diagnostics;
using System.Text;
using Abstract.CodeProcess.Core;
using Abstract.CodeProcess.Core.Language;
using Abstract.CodeProcess.Core.Language.EvaluationData;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.AttributeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.CodeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FunctionReferences;
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
    
    
    public ProgramObject Analize(Module[] modules)
    {
        // Stage 1
        SearchReferences(modules);
        // Stage 2
        ScanHeadersMetadata();
        // Stage 3
        ScanExecutionBodies();
        // Stage 4
        DoSemanticAnalysis();
        
        // Debug shit
        DumpGlobalTable();
        DumpEvaluatedData();

        return new ProgramObject([.. _namespaces]);
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
                var obj = new NamespaceObject(g, n.Identifier[0], n);
                
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

        foreach (var i in structnode.Body.Content)
            SearchGenericScopeRecursive(struc, (ControlNode)i);

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

        foreach (var i in packetnode.Body.Content)
            SearchGenericScopeRecursive(packet, (ControlNode)i);

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
                
        foreach (var i in typedef.Body.Content)
            SearchTypedefScopeRecursive(typd, (ControlNode)i);

        return typd;
    }
    private TypedefItemObject RegisterTypedefItem(LangObject? parent, TypeDefinitionItemNode typedefitem)
    {
        string[] g = parent != null
            ? [..parent.Global, typedefitem.Identifier.Value]
            : [typedefitem.Identifier.Value];
        
        TypedefItemObject typdi = new(g, typedefitem.Identifier.Value, typedefitem);
        parent?.AppendChild(typdi);
        _globalReferenceTable.Add(g, typdi);
        
        return typdi;
    }
    private VariableObject RegisterVariable(LangObject? parent, TopLevelVariableNode variable)
    {
        string[] g = parent != null
            ? [..parent.Global, variable.Identifier.Value]
            : [variable.Identifier.Value];
        
        VariableObject vari = new(g, variable.Identifier.Value, variable);
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
                    ProcessHeader(o);
            }
            else ProcessHeader(langObj);
        }
    }
    private void ProcessHeader(LangObject reference)
    {
        switch (reference)
        {
            case FunctionObject @a: UnwrapFunctionMeta(a); break;
            case StructObject @a: UnwrapStructureMeta(a); break;
        }
        
        // Handling quick inheritance
        if (reference is IStaticModifier @refStatic and not StructObject
            && reference.Parent is IStaticModifier @parentStatic)
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

            if (typeref is
                TypeTypeReference or
                AnytypeTypeReference) function.Generic = true;
            
            function.AddParameter(new ParameterObject(typeref, name));
        }

    }
    private void UnwrapStructureMeta(StructObject structure)
    {
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

    private void ScanExecutionBodies()
    {
        foreach (var i in _globalReferenceTable)
        {
            switch (i.Value)
            {
                case FunctionObject @funcobj:
                    ScanFunctionExecutionBody(funcobj);
                    break;
                
                case FunctionGroupObject @funcgroup:
                {
                    foreach (var i2 in funcgroup.Overloads)
                        ScanFunctionExecutionBody(i2);

                    break;
                }
            }
        }
    }

    private void ScanFunctionExecutionBody(FunctionObject function)
    {
        var body = GetFunctionBody(function.syntaxNode);
        if (body == null) return;

        function.Body = UnwrapExecutionContext_Block(function, body);
    }

    private static BlockNode? GetFunctionBody(FunctionDeclarationNode functionNode)
    {
        // Function body options ([..] means constant):
        //  [func <ident> <params>] <type> <body>      (len 5, type: 3, body: 4)
        //  [func <ident> <params>] <type>             (len 4, type: 3, body:  )
        //  [func <ident> <params>] <body>             (len 4, type:  , body: 3)
        //  [func <ident> <params>]                    (len 3, type:  , body:  )
        
        var funContent = functionNode.Children;
        var funLen = funContent.Length;

        if (funLen >= 4) return funContent[funLen-1] as BlockNode;
        return null;

    }

    private IRBlock UnwrapExecutionContext_Block(LangObject parent, BlockNode block)
    {
        var rootBlock = new IRBlock(block);
        var execctx = new ExecutionContextData(parent, rootBlock);

        foreach (var i in block.Content)
        {
            var res = UnwrapExecutionContext_Statement(i, execctx);
            if (res != null) rootBlock.Content.Add(res);
        }

        return rootBlock;
    }
    private IRNode? UnwrapExecutionContext_Statement(SyntaxNode node, ExecutionContextData ctx)
    {
        switch (node)
        {
            case LocalVariableNode @localvar:
            {
                // LocalVariableNode is also handled in the expression function,
                // this is because if it is just a declaration, we need to return
                // null to ignore it, but if it is used inside a expression, we
                // must return a reference to the local variable!
                
                var typenode = localvar.TypedIdentifier.Type;
                var identnode = localvar.TypedIdentifier.Identifier;
                    
                ctx.AddLocal(new LocalVariableObject(
                    SolveTypeLazy(localvar.TypedIdentifier.Type, ctx),
                    identnode.Value));
                return null;
            }

            case AssignmentExpressionNode @assign:
            {
                var left = UnwrapExecutionContext_Expression(assign.Left, ctx);
                var right = UnwrapExecutionContext_Expression(assign.Right, ctx);
                IRBinaryExp.Operators? op = null;
                
                switch (assign.Operator)
                { 
                    case "=": break;

                    case "+=": op = IRBinaryExp.Operators.Add; break;
                    case "-=": op = IRBinaryExp.Operators.Subtract; break;
                    case "*=": op = IRBinaryExp.Operators.Multiply; break;
                    case "/=": op = IRBinaryExp.Operators.Divide; break;
                    case "%=": op = IRBinaryExp.Operators.Reminder; break;
                    
                    default: throw new NotImplementedException();
                }
                
                if (op != null) right = new IRBinaryExp(assign, op.Value, left, right);
                return new IRAssign(assign, left, right);
            }
            
            default: return UnwrapExecutionContext_Expression(node, ctx);
        }
    }

    private IRExpression UnwrapExecutionContext_Expression(SyntaxNode node, ExecutionContextData ctx)
    {
        switch (node)
        {
            case LocalVariableNode @localvar:
            {
                var newLocal = new LocalVariableObject(
                    SolveTypeLazy(localvar.TypedIdentifier.Type, ctx),
                    localvar.TypedIdentifier.Identifier.Value);
                
                ctx.AddLocal(newLocal);
                return new IRSolvedReference(
                    localvar.TypedIdentifier.Identifier,
                    new LocalReference(newLocal));
            }

            case FunctionCallExpressionNode @funccal:
            {
                return new IRInvoke(funccal,
                    UnwrapExecutionContext_Expression(funccal.FunctionReference, ctx),
                    funccal.Arguments.Select(i
                        => UnwrapExecutionContext_Expression(i, ctx)).ToArray());
            }

            case BinaryExpressionNode @bexp:
            {
                return new IRBinaryExp(bexp,
                    bexp.Operator switch
                    {
                        "+" => IRBinaryExp.Operators.Add,
                        "-" => IRBinaryExp.Operators.Subtract,
                        "*" => IRBinaryExp.Operators.Multiply,
                        "/" => IRBinaryExp.Operators.Divide,
                        "%" => IRBinaryExp.Operators.Reminder,
                        
                        "&" => IRBinaryExp.Operators.Bitwise_And,
                        "|" => IRBinaryExp.Operators.Bitwise_Or,
                        "^" => IRBinaryExp.Operators.Bitwise_Xor,
                        
                        "<<" => IRBinaryExp.Operators.Left_Shift,
                        ">>" => IRBinaryExp.Operators.Right_Shift,
                        
                        "or" => IRBinaryExp.Operators.Logical_Or,
                        "and" => IRBinaryExp.Operators.Logical_And,
                        
                        _ => throw new NotImplementedException(),
                    },
                    UnwrapExecutionContext_Expression(bexp.Left, ctx),
                    UnwrapExecutionContext_Expression(bexp.Right, ctx));
            }
            
            
            case IdentifierCollectionNode @identc: return SearchReference(identc, ctx);
            case IdentifierNode @ident: return SearchReference(ident, ctx);
                
            case IntegerLiteralNode @intlit: return new IRIntegerLiteral(intlit, intlit.Value);
            
            default: throw new NotImplementedException();
        };
    }

    private IRReference SearchReference(ExpressionNode node, ExecutionContextData ctx)
    {
        if (node is TypeExpressionNode) node = (ExpressionNode)node.Children[0];
        
        string[] reference = node switch
        {
            IdentifierCollectionNode @idc => idc.Values,
            IdentifierNode @idn => [idn.Value],
            _ => throw new NotImplementedException(),
        };
        
        LanguageReference? foundReference = null;
        
        if (reference.Length == 0) goto end;
        if (reference.Length == 1)
        {
            var local = ctx.Locals.FirstOrDefault(e => e.Name == reference[0]);
            if (local != null)
            {
                foundReference = new LocalReference(local);
                goto end;
            }
            var param = ctx.Parameters.FirstOrDefault(e => e.Name == reference[0]);
            if (param != null)
            {
                foundReference = new ParameterReference(param);
                goto end;
            }
        }

        { // Check siblings
            var siblings = ctx.Parent.Parent.Children;
            var sibling = siblings
                .FirstOrDefault(e => IdentifierComparer.IsEquals([e.Name], reference));
            if (sibling != null)
            {
                foundReference = GetObjectReference(sibling);
                goto end;
            }
        }
        { // Check global references
            var global = _globalReferenceTable.Values
                .FirstOrDefault(e => IdentifierComparer.IsEquals(e.Global, reference));
            if (global != null)
            {
                foundReference = GetObjectReference(global);
                goto end;
            }
        }

        end:
        if (foundReference is SolvedFunctionGroupReference g)
            foundReference = GetObjectReference(g.FunctionGroup.Overloads[0]);
        
        return foundReference != null
            ? new IRSolvedReference(node, foundReference)
            : new IRUnknownReference(node);
    }

    private TypeReference SolveTypeLazy(ExpressionNode node, ExecutionContextData ctx)
    {
        var shallow = SolveShallowType(node);
        if (shallow is not UnsolvedTypeReference) return shallow;

        var reef = SearchReference(node, ctx);
        if (reef is not IRSolvedReference @solv) return new UnsolvedTypeReference(node);
        return (solv.Reference as TypeReference) ?? new UnsolvedTypeReference(node);
    }
    
    #endregion
    #region Stage Four

    /*
     * Stage Four:
     *  Semantic analysis, solving automatic type inference, type conversion,
     *  operator overloading, function overloading, etc.
     */

    private void DoSemanticAnalysis()
    {
        foreach (var fung in _globalReferenceTable
                     .Select(e => e.Value)
                     .OfType<FunctionGroupObject>())
        {
            foreach (var fun in fung.Overloads)
            {
                if (fun.Body != null)
                    BlockSemaAnal(fun.Body);                
            }
        }
    } 

    private void BlockSemaAnal(IRBlock block)
    {
        for (var i = 0; i < block.Content.Count; i++)
            block.Content[i] = NodeSemaAnal(block.Content[i]);
    }

    private IRNode NodeSemaAnal(IRNode node)
    {
        return node switch
        {
            IRInvoke @iv => NodeSemaAnal_Invoke(iv),
            IRAssign @ass => NodeSemaAnal_Assign(ass),
            IRBinaryExp @be => NodeSemaAnal_BinExp(be),
            
            IRSolvedReference 
                or IRIntegerLiteral => node,
            
            _ => throw new NotImplementedException(),
        };
    }

    private IRNode NodeSemaAnal_Invoke(IRInvoke node)
    {
        // TODO handle invoke sema
        return node;
    }

    private IRNode NodeSemaAnal_Assign(IRAssign node)
    {
        // TODO handle assignment

        node.Target = (IRExpression)NodeSemaAnal(node.Target);
        node.Value = (IRExpression)NodeSemaAnal(node.Value);
        
        return node;
    }

    private IRNode NodeSemaAnal_BinExp(IRBinaryExp node)
    {
        if (node is { Left: IRIntegerLiteral @leftInt, Right: IRIntegerLiteral @rightInt })
        {
            return new IRIntegerLiteral(node.Origin, node.Operator switch {
                
                IRBinaryExp.Operators.Add => leftInt.Value + rightInt.Value,
                IRBinaryExp.Operators.Subtract => leftInt.Value - rightInt.Value,
                IRBinaryExp.Operators.Multiply => leftInt.Value * rightInt.Value,
                IRBinaryExp.Operators.Divide => leftInt.Value / rightInt.Value,
                IRBinaryExp.Operators.Reminder => leftInt.Value % rightInt.Value,
                
                IRBinaryExp.Operators.Bitwise_And => leftInt.Value & rightInt.Value,
                IRBinaryExp.Operators.Bitwise_Or => leftInt.Value | rightInt.Value,
                IRBinaryExp.Operators.Bitwise_Xor => leftInt.Value ^ rightInt.Value,
                IRBinaryExp.Operators.Left_Shift => leftInt.Value << (int)rightInt.Value,
                IRBinaryExp.Operators.Right_Shift => leftInt.Value >> (int)rightInt.Value,
                
                IRBinaryExp.Operators.Logical_And or
                IRBinaryExp.Operators.Logical_Or or
                _ => throw new NotImplementedException(),
            });
        }
        
        // TODO handle idk
        return node;
    }
    
    #endregion
    
    
    #region Multi Stage
    
    /// <summary>
    /// Try to solve constant type forms (arrays, pointers, builtin types, etc.)
    /// and returns `UnsolvedTypeReference` if evaluation-dependent.
    /// </summary>
    /// <param name="node">The type representation</param>
    /// <returns>The evaluation result</returns>
    private static TypeReference SolveShallowType(SyntaxNode node)
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
        }
    }

    private static LanguageReference GetObjectReference(LangObject obj)
    {
        return obj switch
        {
            FunctionObject @f => new SolvedFunctionReference(f),
            FunctionGroupObject @fg => new SolvedFunctionGroupReference(fg),
            
            StructObject @s => new SolvedStructTypeReference(s),
            
            _ => throw new NotImplementedException(),
        };
    }
    
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
        public static bool IsEquals(string[]? x, string[]? y)
        {
            if (x is null || y is null) return false;
            return x.SequenceEqual(y);
        }
        public bool Equals(string[]? x, string[]? y) => IdentifierComparer.IsEquals(x, y);

        public int GetHashCode(string[] key)
        {
            var val = string.Join('.', key);
            return HashCode.Combine(val);
        }
    }
    
}

