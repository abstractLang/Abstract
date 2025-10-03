using System.Diagnostics;
using System.Numerics;
using System.Text;
using Abstract.CodeProcess.Core;
using Abstract.CodeProcess.Core.Language;
using Abstract.CodeProcess.Core.Language.EvaluationData;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Macros;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Statements;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.AttributeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.CodeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FieldReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FunctionReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.Integer;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression.TypeModifiers;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Misc;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Statement;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess;

public class Analizer(ErrorHandler handler)
{
    private ErrorHandler _errorHandler = handler;

    private List<NamespaceObject> _namespaces = [];
    private Dictionary<string[], LangObject> _globalReferenceTable = new(new IdentifierComparer());
    private Stack<List<AttributeReference>> _onHoldAttributes = null!;
    
    
    public ProgramObject Analize(
        Module[] modules,
        bool dumpGlobalTable = false,
        bool dumpEvaluatedData = false)
    {
        // Stage 1
        SearchReferences(modules);
        
        // Stage 2
        ScanHeadersMetadata();
        
        // Stage 3
        LazyScanObjectHeaders();
        ScanObjectBodies();
        
        // Stage 4
        DoSemanticAnalysis();
        
        // Debug shit
        if (dumpEvaluatedData) DumpEvaluatedData();
        if (dumpGlobalTable) DumpGlobalTable();

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
        _namespaces = [];
        _globalReferenceTable = new(new IdentifierComparer());
        _onHoldAttributes = [];
        
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
        
        _namespaces.TrimExcess();
        _globalReferenceTable.TrimExcess();
        _onHoldAttributes = null!;
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
        if (node is AttributeNode @attr)
        {
            _onHoldAttributes.Peek().Add(EvaluateAttribute(attr));
            return;
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
            TypeDefinitionItemNode @typedefitem => RegisterTypedefItem(parent, typedefitem),
            
            _ => throw new NotImplementedException()
        };

        if (_onHoldAttributes.Count <= 0) return;
        obj.AppendAttributes([.. _onHoldAttributes.Peek()]);
        _onHoldAttributes.Peek().Clear();

    }
    
    
    private FunctionObject RegisterFunction(LangObject? parent, FunctionDeclarationNode funcnode)
    {
        string[] g = parent != null
            ? [..parent.Global, funcnode.Identifier.Value]
            : [funcnode.Identifier.Value];

        var aaa = string.Join('.', g);
        
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
        string[] g = parent != null
            ? [..parent.Global, typedefitem.Identifier.Value]
            : [typedefitem.Identifier.Value];
        
        TypedefItemObject typdi = new(g, typedefitem.Identifier.Value, typedefitem);
        parent?.AppendChild(typdi);
        _globalReferenceTable.Add(g, typdi);
        
        return typdi;
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
                case BuiltinAttributes.ConstExp:
                    if (reference is FunctionObject @c) c.ConstExp = true; break;
                
                case BuiltinAttributes.DefineGlobal:
                {
                    var node = builtInAttribute.syntaxNode;
                    // attribute node structure:
                    // <@/> <identifier/> <(> <args.../> <)/>
                    
                    if (node.Children.Length != 3) throw new Exception("'DefineGlobal' expected 1+ arguments");
                    var args = (node.Children[2] as ArgumentCollectionNode)!.Arguments;
                    
                    for (var argi = 0; argi < args.Length; argi++)
                    {
                        if (args[argi] is not StringLiteralNode @strlit)
                            throw new Exception($"'DefineGlobal' expected argument {argi} as ComptimeString");
                        
                        RegisterAlias(null, reference, strlit.RawContent);
                    }
                } break;

                case BuiltinAttributes.Extern:
                {
                    var node = builtInAttribute.syntaxNode;
                    
                    if (reference is not IExternModifier @externModifier)
                        throw new Exception($"Attribute {attr} is not suitable to {reference.GetType().Name}");
                    
                    if (node.Children.Length != 3) throw new Exception("'Extern' expected 1 arguments");
                    var args = (node.Children[2] as ArgumentCollectionNode)!.Arguments;


                    switch (args.Length)
                    {
                        case 1:
                        {
                            if (args[0] is not StringLiteralNode @strlit)
                                throw new Exception("'Extern' expected argument 0 as ComptimeString");

                            externModifier.Extern = (null, strlit.RawContent);
                            break;
                        }
                        case 2:
                        {
                            if (args[0] is not StringLiteralNode @strlit1)
                                throw new Exception("'Extern' expected argument 0 as ComptimeString");
                            if (args[1] is not StringLiteralNode @strlit2)
                                throw new Exception("'Extern' expected argument 1 as ComptimeString");
                        
                            externModifier.Extern = (strlit1.RawContent, strlit2.RawContent);
                            break;
                        }
                        default:
                            throw new Exception($"'Extern' expected 1 or 2 arguments, found {args.Length}");
                    }
                } break;

                // TODO builtin attributes
                case BuiltinAttributes.Align:
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
     *  Processes every function body into a intermediate
     *  representation that will be used for data storage,
     *  compile time execution, runtime evaluation and
     *  high-level optimizations.
     */

    private void LazyScanObjectHeaders()
    {
        foreach (var i in _globalReferenceTable)
        {
            switch (i.Value)
            {
                case FunctionObject @funcobj: LazyScanFunctionMeta(funcobj); break;
                case StructObject @structobj: LazyScanStructureMeta(structobj); break;
                
                case FunctionGroupObject @funcgroup:
                    foreach (var i2 in funcgroup.Overloads) LazyScanFunctionMeta(i2);
                    break;
            }
        }
    }
    
    
    private void LazyScanStructureMeta(StructObject structure)
    {
        foreach (var i in structure.Children)
        {
            switch (i)
            {
                case FieldObject field:
                    
                    if (field.Type is UnsolvedTypeReference @unsolved)
                        field.Type = SolveTypeLazy(unsolved.syntaxNode, structure);
                    
                    break;
                
                case FunctionObject function: break; // Not handled here!
                default: throw new UnreachableException();
            }
        }
    }
    private void LazyScanFunctionMeta(FunctionObject function)
    {
        foreach (var t in function.Parameters)
        {
            if (t.Type is UnsolvedTypeReference @unsolved)
                t.Type = SolveTypeLazy(unsolved.syntaxNode, function);
        }
    }

    
    private void ScanObjectBodies()
    {
        foreach (var i in _globalReferenceTable)
        {
            switch (i.Value)
            {
                case FunctionObject @funcobj: ScanFunctionExecutionBody(funcobj); break;
                case FunctionGroupObject @funcgroup:
                {
                    foreach (var i2 in funcgroup.Overloads) ScanFunctionExecutionBody(i2);
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

                var type = SolveTypeLazy(localvar.TypedIdentifier.Type, ctx);
                return new IRDefLocal(localvar, new(type, identnode.Value));
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
            
            case ReturnStatementNode @ret:
            {
                var exp = ret.HasExpression
                    ? UnwrapExecutionContext_Expression(ret.Expression, ctx)
                    : null;
                
                return new IRReturn(ret, exp);
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
                
                ctx.CurrentBlock.Content.Add(new IRDefLocal(localvar, newLocal));
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
                        
                        _ => throw new UnreachableException(),
                    },
                    UnwrapExecutionContext_Expression(bexp.Left, ctx),
                    UnwrapExecutionContext_Expression(bexp.Right, ctx));
            }
            case UnaryExpressionNode @uexp:
            {
                return new IRUnaryExp(uexp, uexp.Operator switch
                {
                    "+" => IRUnaryExp.UnaryPrefix.Plus,
                    "-" => IRUnaryExp.UnaryPrefix.Minus,
                    "!" => IRUnaryExp.UnaryPrefix.Not,
                    
                    _ => throw new UnreachableException(),
                },
                    UnwrapExecutionContext_Expression(uexp.Expression, ctx));
            }

            case TypeCastNode @tcast:
            {
                return new IrConv(tcast,
                    UnwrapExecutionContext_Expression(tcast.Value, ctx),
                    SolveTypeLazy(tcast.TargetType, ctx));
            }
            
            case IdentifierCollectionNode @identc: return SearchReference(identc, ctx);
            case IdentifierNode @ident: return SearchReference(ident, ctx);
                
            case IntegerLiteralNode @intlit: return new IRIntegerLiteral(intlit, intlit.Value);
            case StringLiteralNode @strlit:
            {
                if (strlit.isSimple) return new IRStringLiteral(strlit, ((StringSectionNode)strlit.Content[0]).Value);
                else throw new NotImplementedException();
            }
            
            case NewObjectNode @newobj:
            {
                List<IRAssign> asisgns = [];

                var typer = SolveTypeLazy(newobj.Type, ctx) as SolvedStructTypeReference
                            ?? throw new Exception($"Undefined symbol {newobj.Type}");

                if (newobj.Inlined != null)
                {
                    foreach (var i in newobj.Inlined.Content)
                    {
                        if (i is not AssignmentExpressionNode @ass) throw new UnreachableException();
                        asisgns.Add(new IRAssign(ass,
                            SearchReferenceStrictlyInside(ass.Left, typer.Struct),
                            UnwrapExecutionContext_Expression(ass.Right, ctx)));
                    }
                }

                var ctor = new IRNewObject(newobj, typer,
                    newobj.Arguments.Select(i => UnwrapExecutionContext_Expression(i, ctx)).ToArray(),
                    [..asisgns]);
                
                return ctor;
            } break;
            
            case ParenthesisExpressionNode @pa: return UnwrapExecutionContext_Expression(pa.Content, ctx);
            
            default: throw new NotImplementedException();
        };
    }

    
    private IRReference SearchReference(ExpressionNode node, ExecutionContextData ctx)
    {
        string[] reference = node switch
        {
            IdentifierCollectionNode @idc => idc.Values,
            IdentifierNode @idn => [idn.Value],
            _ => throw new NotImplementedException(),
        };
        
        List<LanguageReference> referenceChain = [];
        LangObject? langobj = null;
        
        if (reference.Length == 0) goto end;

        do
        {
            // capturing root
            var res1 = SearchReference_ExecCtx(reference[0], ctx);
            if (res1.HasValue)
            {
                referenceChain.Add(res1.Value.Item1);
                switch (res1.Value.Item2)
                {
                    case RuntimeIntegerTypeReference:
                    case StringTypeReference:
                        goto end; break;
                        
                    case SolvedStructTypeReference @sst: langobj = sst.Struct; break; 
                    default: throw new NotImplementedException();
                }
                break;
            }

            var res2 = SearchReference_ChildrenOf(reference[0], ctx.Parent.Parent);
            if (res2.HasValue)
            {
                referenceChain.Add(res2.Value.Item1);
                langobj = res2.Value.Item2;
                break;
            }

            var res3 = SearchReference_Global(reference);
            if (res3 != null) referenceChain.Add(res3);
            goto end;
            
        } while (false);

        if (reference.Length == 1) goto end;
        foreach (var i in reference[1..])
        {
            var res = SearchReference_ChildrenOf(i, langobj);
            if (res.HasValue)
            {
                referenceChain.Add(res.Value.Item1);
                var children = res.Value.Item2;
                
                var tref = children switch
                {
                    FieldObject f => f.Type,
                    _ => throw new UnreachableException()
                };
                langobj = tref switch
                {
                    SolvedStructTypeReference st => st.Struct,
                    IntegerTypeReference => null,
                    _ => throw new UnreachableException()
                };
            }
            else
            {
                referenceChain.Clear();
                break;
            }
        }
        
        end:

        switch (referenceChain.Count)
        {
            case 0: return new IRUnknownReference(node);
            case 1: return new IRSolvedReference(node, referenceChain[0]);
            default:
            {
                var cnode = ((IdentifierCollectionNode)node)!.Children;

                var first = new IRSolvedReference(cnode[^1], referenceChain[^1]);
                IRReference last = first;
            
                for (var i = referenceChain.Count - 2; i >= 0; i--)
                {
                    var e = referenceChain[i];
                    last = new IRReferenceAccess(cnode[i], referenceChain[i], last);
                }

                return last;
            }
        }
    }
    private IRReference SearchReference(ExpressionNode node, LangObject obj)
    {
        string[] reference = node switch
        {
            IdentifierCollectionNode @idc => idc.Values,
            IdentifierNode @idn => [idn.Value],
            _ => throw new NotImplementedException(),
        };
        
        List<LanguageReference> referenceChain = [];
        LangObject? langobj = null;
        
        if (reference.Length == 0) goto end;

        do
        {
            // capturing root
            var res1 = SearchReference_ChildrenOf(reference[0], obj.Parent);
            if (res1.HasValue)
            {
                referenceChain.Add(res1.Value.Item1);
                break;
            }

            var res2 = SearchReference_Global(reference);
            if (res2 != null) referenceChain.Add(res2);
            goto end;
            
        } while (false);

        if (reference.Length == 1) goto end;
        foreach (var i in reference[1..])
        {
            var res = SearchReference_ChildrenOf(i, langobj);
            if (res.HasValue)
            {
                referenceChain.Add(res.Value.Item1);
                var children = res.Value.Item2;
                
                var tref = children switch
                {
                    FieldObject f => f.Type,
                    _ => throw new UnreachableException()
                };
                langobj = tref switch
                {
                    SolvedStructTypeReference st => st.Struct,
                    IntegerTypeReference => null,
                    _ => throw new UnreachableException()
                };
            }
            else
            {
                referenceChain.Clear();
                break;
            }
        }
        
        end:

        switch (referenceChain.Count)
        {
            case 0: return new IRUnknownReference(node);
            case 1: return new IRSolvedReference(node, referenceChain[0]);
            default:
            {
                var cnode = ((IdentifierCollectionNode)node)!.Children;

                var first = new IRSolvedReference(cnode[^1], referenceChain[^1]);
                IRReference last = first;
            
                for (var i = referenceChain.Count - 1; i >= 1; i--)
                {
                
                    var e = referenceChain[i];
                    last = new IRReferenceAccess(cnode[i], referenceChain[i], last);
                }

                return first;
            }
        }
    }
    
    private (LanguageReference, LangObject)? SearchReference_ChildrenOf(string part, LangObject parent)
    {
        var siblings = parent.Children;
        var found = siblings.FirstOrDefault(e => IdentifierComparer.IsEquals([e.Name], [part]));
        if (found == null) return null;

        return (GetObjectReference(found), found);
        
    }
    private (LanguageReference, TypeReference)? SearchReference_ExecCtx(string part, ExecutionContextData ctx)
    {
        var local = ctx.CurrentBlock.Content
            .OfType<IRDefLocal>()
            .FirstOrDefault(e => e.LocalVariable.Name == part);
        if (local != null) return (new LocalReference(local.LocalVariable), local.LocalVariable.Type);
        
        var param = ctx.Parameters.FirstOrDefault(e => e.Name == part);
        if (param != null) return (new ParameterReference(param), param.Type);

        return null;
    }

    private LanguageReference? SearchReference_Global(string[] parts)
    {
        var member = _globalReferenceTable.Values
            .FirstOrDefault(e => IdentifierComparer.IsEquals(e.Global, parts));
        return (member == null) ? null : GetObjectReference(member);
    }
    
    
    private IRReference SearchReferenceStrictlyInside(ExpressionNode node, LangObject container)
    {
        string[] reference = node switch
        {
            IdentifierCollectionNode @idc => idc.Values,
            IdentifierNode @idn => [idn.Value],
            _ => throw new NotImplementedException(),
        };
        
        LanguageReference? foundReference = null;
        
        switch (reference.Length)
        {
            case 0: goto end;
            case 1:
            {
                var child = container.Children.FirstOrDefault(e => e.Name == reference[0]);
                if (child != null) foundReference = GetObjectReference(child);
                goto end;
            }
        }

        end:
        return foundReference != null
            ? new IRSolvedReference(node, foundReference)
            : new IRUnknownReference(node);
    }
    
    
    private TypeReference SolveTypeLazy(ExpressionNode node, ExecutionContextData? ctx)
    {
        var shallow = SolveShallowType(node);
        if (shallow is not UnsolvedTypeReference) return shallow;

        if (ctx == null) return new UnsolvedTypeReference(node);
        
        var reef = SearchReference(node, ctx);
        if (reef is not IRSolvedReference @solv) return new UnsolvedTypeReference(node);
        return (solv.Reference as TypeReference) ?? new UnsolvedTypeReference(node);
    }
    private TypeReference SolveTypeLazy(ExpressionNode node, LangObject? obj)
    {
        var shallow = SolveShallowType(node);
        if (shallow is not UnsolvedTypeReference) return shallow;

        if (obj == null) return new UnsolvedTypeReference(node);
        
        var reef = SearchReference(node, obj);
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
        var funclist = _globalReferenceTable
            .Select(e => e.Value)
            .OfType<FunctionGroupObject>().SelectMany(e => e.Overloads)
            .ToArray();
        
        // Header analysis
        foreach (var obj in _globalReferenceTable.Values)
        {
            switch (obj)
            {
                case FunctionGroupObject @functionGroup:
                    foreach (var fun in @functionGroup.Overloads) FunctionSemaAnal(fun);
                    break;
                case FunctionObject @fun:
                    FunctionSemaAnal(fun);
                    break;
                case FieldObject @field:
                    FieldSemaAnal(field);
                    break;
            }
            
        }
        
        // Excution analysis
        foreach (var fun in funclist)
        {
            if (fun.Body != null) BlockSemaAnal(fun.Body);
        }
    } 
    
    private void FunctionSemaAnal(FunctionObject function)
    {
        foreach (var i in function.Parameters)
        {
            if (i.Type is not UnsolvedTypeReference @unsolv) continue;
            i.Type = SolveTypeLazy(unsolv.syntaxNode, function);
        }
    }

    private void FieldSemaAnal(FieldObject field)
    {
        if (field.Type is not UnsolvedTypeReference @unsolv) return;
        field.Type = SolveTypeLazy(unsolv.syntaxNode, field);
    }

    private void BlockSemaAnal(IRBlock block)
    {
        IrBlockExecutionContextData ctx = new IrBlockExecutionContextData();
        for (var i = 0; i < block.Content.Count; i++)
            block.Content[i] = NodeSemaAnal(block.Content[i], ctx);
    }

    private IRNode NodeSemaAnal(IRNode node, IrBlockExecutionContextData ctx)
    {
        return node switch
        {
            IRDefLocal @dl => NodeSemaAnal_Macro_DefLocal(dl, ctx),
            
            IRInvoke @iv => NodeSemaAnal_Invoke(iv, ctx),
            IRAssign @ass => NodeSemaAnal_Assign(ass, ctx),
            IRBinaryExp @be => NodeSemaAnal_BinExp(be, ctx),
            IRUnaryExp @ue => NodeSemaAnal_UnExp(ue, ctx),
            IrConv @tc =>NodeSemaAnal_Conv(tc, ctx),
            IRNewObject @no => NodeSemaAnal_NewObj(no, ctx),
            IRReturn @re => NodeSemaAnal_Return(re, ctx),
            
            IRReferenceAccess
                or IRSolvedReference
                or IRStringLiteral
                or IRIntegerLiteral => node,
            
            IRUnknownReference => throw new UnreachableException("All references must already been handled"),
            
            _ => throw new NotImplementedException(),
        };
    }

    private IRNode NodeSemaAnal_Macro_DefLocal(IRDefLocal deflocal, IrBlockExecutionContextData ctx)
    {
        ctx.RegisterLocalVariable(deflocal.LocalVariable);
        return deflocal;
    }
    
    private IRNode NodeSemaAnal_Invoke(IRInvoke node, IrBlockExecutionContextData ctx)
    {
        if (node.Target is IRSolvedReference @solvedref)
        {

            for (var i = 0; i < node.Arguments.Length; i++)
                node.Arguments[i] = (IRExpression)NodeSemaAnal(node.Arguments[i], ctx);
            
            if (solvedref.Reference is SolvedFunctionGroupReference @fgroupref)
            {
                // Node is a function group and must be analysed to point
                // to the correct or most optimal overload

                var overloads = fgroupref.FunctionGroup.Overloads;
                var arguments = node.Arguments;
                FunctionObject? betterFound = null;
                var betterFoundSum = 0;
                
                foreach (var ov in overloads)
                {
                    if (ov.Parameters.Length != arguments.Length) continue;
                    
                    var parameters = ov.Parameters;
                    var suitability = new int[parameters.Length];

                    foreach (var (i, e) in parameters.Index())
                    {
                        var s = (int)CalculateTypeSuitability(e.Type, arguments[i], true);
                        if (s == 0) goto NoSuitability;
                        suitability[i] = s;
                    }
                    
                    var sum = (suitability.Sum() * 100) / parameters.Length;
                    if (sum <= betterFoundSum) continue;
                    betterFound = ov;
                    betterFoundSum = sum;
                    
                    NoSuitability:
                    continue;
                }

                if (betterFound == null)  throw new Exception($"CompError:" + 
                                                             $"No overload that matches function call {node.Origin}");
                node.Target = new IRSolvedReference(solvedref.Origin, new SolvedFunctionReference(betterFound));

                
                for (var i = 0; i < node.Arguments.Length; i++)
                    node.Arguments[i] = SolveTypeCast(betterFound.Parameters[i].Type, node.Arguments[i]);
            }
            
            // I SUPPOSE it is impossible that the analizer stages before here
            // are able to assign anything other than a FunctionGroup ref...
            // i may be wrong...
            else throw new NotImplementedException();
        }
        else throw new NotImplementedException();
        
        return node;
    }
    private IRNode NodeSemaAnal_Assign(IRAssign node, IrBlockExecutionContextData ctx)
    {
        node.Target = (IRExpression)NodeSemaAnal(node.Target, ctx);
        node.Value = (IRExpression)NodeSemaAnal(node.Value, ctx);

        // TODO type target inference
        
        node.Value = SolveTypeCast(GetEffectiveTypeReference(node.Target), node.Value);
        
        return node;
    }
    private IRNode NodeSemaAnal_BinExp(IRBinaryExp node, IrBlockExecutionContextData ctx)
    {
        node.Left = (IRExpression)NodeSemaAnal(node.Left, ctx);
        var leftTypeRef = GetEffectiveTypeReference(node.Left);
        node.Right = SolveTypeCast(leftTypeRef, (IRExpression)NodeSemaAnal(node.Right, ctx));

        // Operate literals at comptime
        switch (node)
        {
            case { Left: IRIntegerLiteral @leftInt, Right: IRIntegerLiteral @rightInt }:
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
            
            case { Left: IRStringLiteral @leftStr, Right: IRStringLiteral @rightStr }:
                return new IRStringLiteral(node.Origin, node.Operator switch
                    {
                        IRBinaryExp.Operators.Add => leftStr.Data + rightStr.Data,
                        _ => throw new UnreachableException()
                    }
                );
        }

        // TODO solve operator overloading

        var ltype = GetEffectiveTypeReference(node.Left);
        var rtype = GetEffectiveTypeReference(node.Right);
        
        if (ltype is RuntimeIntegerTypeReference @left &&
            rtype is RuntimeIntegerTypeReference @right)
        {
            if (left.BitSize >= right.BitSize) node.ResultType = left;
            else if (left.BitSize < right.BitSize) node.ResultType = right;
        }
        
        else if (ltype is RuntimeIntegerTypeReference @left2 &&
                 rtype is ComptimeIntegerTypeReference)
        {
            node.ResultType = left2;
            node.Right = new IRIntegerLiteral(node.Right.Origin, ((IRIntegerLiteral)node.Right).Value, left2.BitSize);
        }
        
        else if (ltype is ComptimeIntegerTypeReference @left3 &&
                 rtype is RuntimeIntegerTypeReference @right3)
        {
            node.Left = new IRIntegerLiteral(node.Left.Origin, ((IRIntegerLiteral)node.Left).Value, right3.BitSize);
            node.ResultType = right3;
        }
        
        else throw new NotImplementedException();
        
        return node;
    }
    private IRNode NodeSemaAnal_UnExp(IRUnaryExp node, IrBlockExecutionContextData ctx)
    {
        node.Value = (IRExpression)NodeSemaAnal(node.Value, ctx);

        if (node is { Value: IRIntegerLiteral @valInt })
        {
            return new IRIntegerLiteral(node.Origin, node.Prefix switch
            {
                IRUnaryExp.UnaryPrefix.Plus => valInt.Value,
                IRUnaryExp.UnaryPrefix.Minus => BigInteger.Negate(valInt.Value),
                IRUnaryExp.UnaryPrefix.Not => ~valInt.Value,
                _ => throw new UnreachableException(),
            });
        }

        return node;
    }

    private IRNode NodeSemaAnal_Conv(IrConv node, IrBlockExecutionContextData ctx)
    {
        node.Expression = (IRExpression)NodeSemaAnal(node.Expression, ctx);
        return SolveTypeCast(node.TargetType, node.Expression, true);
    }
    private IRNode NodeSemaAnal_NewObj(IRNewObject node, IrBlockExecutionContextData ctx)
    {
        // TODO i prefer handle constructor overloading when
        //  we have actual constructable structures working

        for (var i = 0; i < node.InlineAssignments.Length; i++)
        {
            var v = node.InlineAssignments[i];

            v.Target = (IRExpression)NodeSemaAnal(v.Target, ctx);
            v.Value = (IRExpression)NodeSemaAnal(v.Value, ctx);
            
            v.Value = SolveTypeCast(GetEffectiveTypeReference(v.Target), v.Value);
            
            node.InlineAssignments[i] = v;
        }

        return node;
    }
    private IRNode NodeSemaAnal_Return(IRReturn node, IrBlockExecutionContextData ctx)
    {
        if (node.Value != null) node.Value = (IRExpression)NodeSemaAnal(node.Value, ctx);
        return node;
    }
    
    
    /// <summary>
    /// With a desired type and a value node,
    /// returns a node that explicitly solves
    /// any applicable casting.
    /// Value must already have been evaluated!
    /// </summary>
    /// <param name="typeTo"> Target type </param>
    /// <param name="value"> Value to cast </param>
    /// <returns></returns>
    private IRExpression SolveTypeCast(TypeReference typeTo, IRExpression value, bool @explicit = false)
    {
        if (typeTo is RuntimeIntegerTypeReference typetoRi)
        {
            if (value is IRIntegerLiteral @lit)
                    return new IRIntegerLiteral(lit.Origin, lit.Value, typetoRi.PtrSized? null : typetoRi.BitSize);

            var valType = GetEffectiveTypeReference(value);
            if (valType is RuntimeIntegerTypeReference valueRi)
            {
                // If same type, do nothing
                if (typetoRi.BitSize == valueRi.BitSize
                    && typetoRi.Signed == valueRi.Signed) {}
                
                // If pointer sized, delegate check for backend
                else if (valueRi.PtrSized || typetoRi.PtrSized)
                    return new IRIntCast(value.Origin, value, typetoRi);

                var val = valueRi;
                var tar = typetoRi;
                var o = value.Origin;
                
                if (val.Signed == tar.Signed)
                {
                    if (val.BitSize == tar.BitSize) return value;
                    if (val.BitSize < tar.BitSize) return new IRIntExtend(o, value, tar);
                    if (val.BitSize > tar.BitSize && @explicit) return new IRIntTrunc(o, value, tar);
                }
                else if (!val.Signed && tar.Signed)
                {
                    
                    if (val.BitSize == tar.BitSize && @explicit) return new IRIntCast(o, value, tar);
                    if (val.BitSize < tar.BitSize) return new IRIntExtend(o, value, tar);
                    if (val.BitSize > tar.BitSize && @explicit) return new IRIntTrunc(o, value, tar);
                }
                else
                {
                    if (val.BitSize == tar.BitSize && @explicit) return new IRIntCast(o, value, tar);
                    if (val.BitSize < tar.BitSize && @explicit) return new IRIntExtend(o, value, tar);
                    if (val.BitSize > tar.BitSize && @explicit) return new IRIntTrunc(o, value, tar);
                }
                    
                throw new Exception($"Cannot convert type {val} to {tar} in {value.Origin:pos}");
            }
        }
        
        return value;
    }
    
    private Suitability CalculateTypeSuitability(TypeReference typeTo, IRExpression value, bool allowImplicit)
    {
        var typeVl = GetEffectiveTypeReference(value);
        switch (typeTo)
        { 
            
            case RuntimeIntegerTypeReference intParam:
                if (value is IRIntegerLiteral) return Suitability.Perfect;
                if (typeVl is RuntimeIntegerTypeReference intArg)
                {
                    if (intParam.PtrSized && intArg.PtrSized)
                    {
                        if (intParam.Signed == intArg.Signed) return Suitability.Perfect;
                        if (allowImplicit) return Suitability.NeedsSoftCast;
                    }
                    
                    if (intParam.PtrSized || intArg.PtrSized) return Suitability.NeedsSoftCast;
                    
                    if (intParam.BitSize == intArg.BitSize
                        && intParam.Signed == intArg.Signed) return Suitability.Perfect;

                    var val = intArg;
                    var tar = intParam;
                
                    if (val.Signed == tar.Signed)
                    {
                        if (val.BitSize == tar.BitSize) return Suitability.Perfect;
                        if (val.BitSize < tar.BitSize) return Suitability.NeedsSoftCast;
                        if (val.BitSize > tar.BitSize && @allowImplicit) return Suitability.NeedsSoftCast;
                        return 0;
                    }
                    if (!val.Signed && tar.Signed)
                    {

                        if (val.BitSize == tar.BitSize && @allowImplicit) return Suitability.NeedsHardCast;
                        if (val.BitSize < tar.BitSize) return Suitability.NeedsHardCast;
                        if (val.BitSize > tar.BitSize && @allowImplicit) return Suitability.NeedsHardCast;
                         return 0;
                    }
                    return allowImplicit
                        ? Suitability.NeedsHardCast
                        : Suitability.None;
                }

                return 0;

            case StringTypeReference stringParam:
                if (typeVl is StringTypeReference @strArg
                    && (strArg.Encoding == StringEncoding.Undefined
                        || strArg.Encoding == stringParam.Encoding)) return Suitability.Perfect;
                return Suitability.None;

            case SolvedStructTypeReference @solvedstruct:
                if (typeVl is SolvedStructTypeReference @solvedstructarg)
                    return (Suitability)solvedstruct.CalculateSuitability(solvedstructarg);
                return Suitability.None;
            
        }
        throw new UnreachableException();
    }

    private enum Suitability
    {
        None = 0,
        NeedsHardCast = 1,
        NeedsSoftCast = 2,
        Perfect = 3
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
                        case "iptr": return new RuntimeIntegerTypeReference(true);
                        case "uptr": return new RuntimeIntegerTypeReference(false);
                        case "void": return new VoidTypeReference();
                        case "type": return new TypeTypeReference();
                        case "string": return new StringTypeReference(StringEncoding.Undefined);
                        case "anytype": return new AnytypeTypeReference();
;                    }

                    if (value[0] is 'i' or 'u' && value[1..].All(char.IsNumber))
                        return new RuntimeIntegerTypeReference(value[0] == 'i', byte.Parse(value[1..]));
                    

                    return new UnsolvedTypeReference(id);
                
                case ArrayTypeModifierNode @ar:
                    return new SliceTypeReference(SolveShallowType(ar.Type));
                
                case ReferenceTypeModifierNode @rf:
                    return new ReferenceTypeReference(SolveShallowType(rf.Type));
                
                case BinaryExpressionNode @b: return new UnsolvedTypeReference(b);
                
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
            
            FieldObject @v => new SolvedFieldReference(v),
            
            _ => throw new NotImplementedException(),
        };
    }
    
    private static TypeReference GetEffectiveTypeReference(IRExpression expr)
    {
        switch (expr)
        {
            case IRIntegerLiteral: return new ComptimeIntegerTypeReference();
            case IRStringLiteral: return new StringTypeReference(StringEncoding.Undefined);
            
            case IRSolvedReference @solvedFuck:
                return solvedFuck.Reference switch
                {
                    
                    IntegerTypeReference @intt => intt,
                    SolvedStructTypeReference @structt => structt,
                    SolvedFieldReference @field => field.Field.Type,
                    
                    LocalReference @local => local.Local.Type,
                    ParameterReference @param => param.Parameter.Type,
                    
                    _ => throw new NotImplementedException()
                };
            
            case IRReferenceAccess @access: return GetEffectiveTypeReference(access.B);

            
            case IRBinaryExp @exp:
                return exp.ResultType ?? throw new UnreachableException(
                    "This function should not be called when this value is null");

            
            case IrConv @conv: return conv.TargetType;
            case IRIntCast @tcast: return tcast.TargetType;
            case IRIntExtend @icast: return icast.toType;
            case IRIntTrunc @itrunc: return itrunc.toType;
            
            default: throw new NotImplementedException();
        }
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
                FieldObject => "TVar",
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
        
        File.WriteAllTextAsync(".abs-cache/debug/eval.txt", sb.ToString());
    }
    
    private class IdentifierComparer : IEqualityComparer<string[]>
    {
        public static bool IsEquals(string[]? x, string[]? y)
        {
            if (x is null || y is null) return false;
            return x.SequenceEqual(y);
        }
        public bool Equals(string[]? x, string[]? y) => IsEquals(x, y);


        public int GetHashCode(string[] key)
        {
            var val = string.Join('.', key);
            return string.GetHashCode(val, StringComparison.Ordinal);
        }
    }
    
}

