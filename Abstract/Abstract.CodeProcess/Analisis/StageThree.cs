using System.Diagnostics;
using Abstract.CodeProcess.Core;
using Abstract.CodeProcess.Core.Language.EvaluationData;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Macros;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Statements;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.CodeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.Integer;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Statement;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess;


/*
 * Stage Three:
 *  Processes every function body into a intermediate
 *  representation that will be used for data storage,
 *  compile time execution, runtime evaluation and
 *  high-level optimizations.
 */

public partial class Analyzer
{
    private void LazyScanObjectHeaders()
    {
        foreach (var i in _globalReferenceTable)
        {
            switch (i.Value)
            {
                case NamespaceObject @nmsp: LazyScanNamespaceMeta(nmsp); break;
                case FunctionObject @funcobj: LazyScanFunctionMeta(funcobj); break;
                case StructObject @structobj: LazyScanStructureMeta(structobj); break;
                
                case FunctionGroupObject @funcgroup:
                    foreach (var i2 in funcgroup.Overloads) LazyScanFunctionMeta(i2);
                    break;
            }
        }
    }
    
    
    private void LazyScanNamespaceMeta(NamespaceObject nmsp)
    {
        foreach (var n in nmsp.Imports)
        {
            var member = _globalReferenceTable.Values
                .FirstOrDefault(e => IdentifierComparer.IsEquals(e.Global, n.Key));
            
            if (member is not NamespaceObject @nmspObj)
                throw new Exception($"\"{string.Join('.', n.Key)}\" is not a namespace");

            nmsp.Imports[n.Key] = (n.Value.specific, nmspObj);

            if (n.Value.specific != null) throw new UnreachableException();
        }
    }
    private void LazyScanStructureMeta(StructObject structure)
    {
        foreach (var i in structure.Children)
        {
            switch (i)
            {
                case FieldObject field:
                    if (!IsSolved(field.Type)) field.Type = SolveTypeLazy(field.Type, structure);
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
            if (!IsSolved(t.Type)) t.Type = SolveTypeLazy(t.Type, function);
        }
        if (!IsSolved(function.ReturnType))
            function.ReturnType = SolveTypeLazy(function.ReturnType, function);
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

                var type = SolveTypeLazy(new UnsolvedTypeReference(localvar.TypedIdentifier.Type), ctx);
                return new IRDefLocal(localvar, new LocalVariableObject(type, identnode.Value));
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
                    SolveTypeLazy(new UnsolvedTypeReference(localvar.TypedIdentifier.Type), ctx),
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
                    
                    "&" => IRUnaryExp.UnaryPrefix.Reference,
                    
                    _ => throw new UnreachableException(),
                },
                    UnwrapExecutionContext_Expression(uexp.Expression, ctx));
            }

            case TypeCastNode @tcast:
            {
                return new IrConv(tcast,
                    UnwrapExecutionContext_Expression(tcast.Value, ctx),
                    SolveTypeLazy(new UnsolvedTypeReference(tcast.TargetType), ctx));
            }
            
            case IdentifierCollectionNode @identc: return SearchReference(identc, ctx);
            case IdentifierNode @ident: return SearchReference(ident, ctx);
                
            case IntegerLiteralNode @intlit: return new IRIntegerLiteral(intlit, intlit.Value);
            case StringLiteralNode @strlit:
            {
                if (strlit.IsSimple) return new IRStringLiteral(strlit, strlit.RawContent);
                else throw new NotImplementedException();
            }
            
            case NewObjectNode @newobj:
            {
                List<IRAssign> asisgns = [];

                var typer = SolveTypeLazy(new UnsolvedTypeReference(newobj.Type), ctx)
                                as SolvedStructTypeReference
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

        do // capturing root
        {
            // Search in exec context
            var res1 = SearchReference_ExecCtx(reference[0], ctx);
            if (res1.HasValue)
            {
                referenceChain.Add(res1.Value.Item1);
                var v2 = res1.Value.Item2;
                while (true)
                {
                    switch (v2)
                    {
                        case RuntimeIntegerTypeReference:
                        case StringTypeReference:
                            goto end; break;

                        case ReferenceTypeReference @refe:
                            v2 = refe.InternalType;
                            continue;
                        
                        case SolvedStructTypeReference @sst: langobj = sst.Struct; break;
                        default: throw new NotImplementedException();
                    }
                    break;
                }
                break;
            }

            // Search in sibilings
            var res2 = SearchReference_ChildrenOf(reference[0], ctx.Parent.Parent);
            if (res2.HasValue)
            {
                referenceChain.Add(res2.Value.Item1);
                langobj = res2.Value.Item2;
                break;
            }

            // Search in imports
            var res3 = SearchReference_Import(reference[0], ctx.Parent.Namespace);
            if (res3.HasValue)
            {
                referenceChain.Add(res3.Value.Item1);
                langobj = res3.Value.Item2;
                break;
            }

            // Search in globals
            var res4 = SearchReference_Global(reference);
            if (res4 != null) referenceChain.Add(res4);
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

        do // capturing root
        {
            // Vhecking if is a sibling
            var res1 = SearchReference_ChildrenOf(reference[0], obj.Parent);
            if (res1.HasValue)
            {
                referenceChain.Add(res1.Value.Item1);
                break;
            }
            
            // Checking if is a import
            var res2 = SearchReference_Import(reference[0], obj.Namespace);
            if (res2.HasValue)
            {
                referenceChain.Add(res2.Value.Item1);
                break;
            }
            
            // Checking if is a global
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
    private (LanguageReference, LangObject)? SearchReference_Import(string part, NamespaceObject parentNmsp)
    {
        foreach (var i in parentNmsp.Imports)
        {
            if (i.Value.specific == null)
            {
                var res = SearchReference_ChildrenOf(part, i.Value.nmsp!);
                if (res != null) return res;
            }
            else throw new UnreachableException();
        }
        return null;
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
    
    
    private TypeReference SolveTypeLazy(TypeReference typeref, ExecutionContextData? ctx)
    {
        switch (typeref)
        {
            case UnsolvedTypeReference @unsolved:
            {
                var node = unsolved.syntaxNode;
                var shallow = SolveShallowType(node);
                if (IsSolved(shallow)) return shallow;

                if (ctx == null) return new UnsolvedTypeReference(node);

                var reef = SearchReference(node, ctx);
                if (reef is not IRSolvedReference @solv) return new UnsolvedTypeReference(node);
                return (solv.Reference as TypeReference) ?? new UnsolvedTypeReference(node);
            }
            case SliceTypeReference @slice: slice.InternalType = SolveTypeLazy(@slice.InternalType, ctx); break;
            case ReferenceTypeReference @refer: refer.InternalType = SolveTypeLazy(@refer.InternalType, ctx); break;
        }
        return typeref;
    }
    private TypeReference SolveTypeLazy(TypeReference typeref, LangObject? obj)
    {
        switch (typeref)
        {
            case UnsolvedTypeReference @unsolved:
            {
                var node = unsolved.syntaxNode;
                var shallow = SolveShallowType(node);
                if (shallow is not UnsolvedTypeReference) return shallow;

                if (obj == null) return new UnsolvedTypeReference(node);

                var reef = SearchReference(node, obj);
                if (reef is not IRSolvedReference @solv) return new UnsolvedTypeReference(node);
                return (solv.Reference as TypeReference) ?? new UnsolvedTypeReference(node);
            }
            case SliceTypeReference @slice: slice.InternalType = SolveTypeLazy(@slice.InternalType, obj); break;
            case ReferenceTypeReference @refer: refer.InternalType = SolveTypeLazy(@refer.InternalType, obj); break;
        }
        return typeref;
    }
}