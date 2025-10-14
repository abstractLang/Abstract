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

using IntegerTypeReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.Integer.IntegerTypeReference;
using ReferenceTypeReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.ReferenceTypeReference;
using SliceTypeReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.SliceTypeReference;
using TypeReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.TypeReference;

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
    private void ScanObjectHeaders()
    {
        foreach (var i in _globalReferenceTable)
        {
            switch (i.Value)
            {
                case NamespaceObject @nmsp: ScanNamespaceMeta(nmsp); break;
                case FunctionObject @funcobj: ScanFunctionMeta(funcobj); break;
                case StructObject @structobj: ScanStructureMeta(structobj); break;
                
                case FunctionGroupObject @funcgroup:
                    foreach (var i2 in funcgroup.Overloads) ScanFunctionMeta(i2);
                    break;
            }
        }

        var structsSortedList = TopologicalSort(_globalReferenceTable.Values.OfType<StructObject>());
        foreach (var structs in structsSortedList) LazyScanStructureMeta(structs);
    }
    
    
    private void ScanNamespaceMeta(NamespaceObject nmsp)
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
    private void ScanStructureMeta(StructObject structure)
    {
        if (structure.Extends != null)
        {
            structure.Extends = SolveTypeLazy(structure.Extends, structure);
            if (structure.Extends is UnsolvedTypeReference)
                throw new NotImplementedException();
            if (structure.Extends is not SolvedStructTypeReference)
                throw new Exception("Trying to extend a non-struct type");
        }
        
        foreach (var i in structure.Children)
        {
            switch (i)
            {
                case FieldObject field:
                    if (!IsSolved(field.Type)) field.Type = SolveTypeLazy(field.Type, structure);
                    break;
                
                case FunctionGroupObject group:
                case FunctionObject function:
                    break; // Not handled here!

                default: throw new UnreachableException();
            }
        }
    }
    private void ScanFunctionMeta(FunctionObject function)
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

        var ctx = new ExecutionContextData(function);
        function.Body = UnwrapExecutionContext_Block(ctx, body);
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

    
    private IRBlock UnwrapExecutionContext_Block(ExecutionContextData ctx, BlockNode block)
    {
        var rootBlock = new IRBlock(block);

        ctx.PushBlock(rootBlock);
        foreach (var i in block.Content)
        {
            var res = UnwrapExecutionContext_Statement(i, ctx);
            ctx.Last = res.node;
            
            if (res is { node: not null, include: true }) rootBlock.Content.Add(res.node);
        }
        ctx.PopBlock();

        return rootBlock;
    }
    private (IRNode? node, bool include) UnwrapExecutionContext_Statement(SyntaxNode node, ExecutionContextData ctx)
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
                var name = localvar.TypedIdentifier.Identifier.Value;
                var type = SolveTypeLazy(new UnsolvedTypeReference(localvar.TypedIdentifier.Type), ctx);
                
                if (ctx.Locals.Any(e => e.LocalVariable.Name == name))
                    throw new Exception($"{localvar:pos} shadows \'{name}\' declaration");
                
                var deflocal = new IRDefLocal(localvar, new LocalVariableObject(type, name));
                ctx.CurrentBlock.Content.Add(deflocal);
                return (deflocal, true);
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
                return (new IRAssign(assign, left, right), true);
            }

            case IfStatementNode @if:
            {
                return (ParseIfElif(@if, @if.Then, @if.Condition), true);
            }
            case ElifStatementNode @elif:
            {
                if (ctx.Last is not IRIf @irif)
                    throw new Exception("elif blocks only allowed after if or elif statements");
                
                var a = ParseIfElif(elif, elif.Then, elif.Condition);
                irif.Else = a;
                return (a, false);
            }
            case ElseStatementNode @_else:
            {
                if (ctx.Last is not IRIf @irif)
                    throw new Exception("else blocks only allowed after if or elif statements");
                
                IRBlock then = new IRBlock(_else.Then);
                ctx.PushBlock(then);

                if (_else.Then is BlockNode @block) then = UnwrapExecutionContext_Block(ctx, block);
                else
                {
                    var (res, _) = UnwrapExecutionContext_Statement(_else.Then, ctx);
                    if (res != null) then.Content.Add(res);
                }
                ctx.PopBlock();

                var a = new IRElse(_else, then);
                irif.Else = a;
                return (a, false);
            }

            case WhileStatementNode @while:
            {
                var clen = @while.Children.Length;
                
                IRBlock? def = null;
                IRBlock? step = null;
                IRBlock then;
                
                var defidx  = clen switch
                {
                    4 => -1,
                    6 => -1,
                    _ => 1
                };
                var conidx = clen switch
                {
                    4 => 1,
                    6 => 1,
                    _ => 3
                };
                var stepidx = clen switch
                {
                    4 => -1,
                    6 => 3,
                    _ => 5
                };
                var bodyidx = clen switch
                {
                    4 => 3,
                    6 => 5,
                    _ => 7
                };
                
                if (defidx != -1)
                {
                    def = new IRBlock(@while.Children[defidx]);
                    ctx.PushBlock(def);
                    var res = UnwrapExecutionContext_Statement(@while.Children[defidx], ctx);
                    if (res.include && res.node != null) def.Content.Add(res.node);
                }
                if (stepidx != -1)
                {
                    step = new IRBlock(@while.Children[stepidx]);
                    step.Content.Add(UnwrapExecutionContext_Expression(@while.Children[stepidx], ctx));
                }
                
                var condition = UnwrapExecutionContext_Expression(@while.Children[conidx], ctx);
                
                var content = @while.Children[bodyidx];
                if (content is BlockNode @bn) then = UnwrapExecutionContext_Block(ctx, bn);
                else
                {
                    then = new IRBlock(content);
                    var n = UnwrapExecutionContext_Statement(content, ctx);
                    if (n is { include: true, node: not null }) then.Content.Add(n.node);
                }

                if (def != null) ctx.PopBlock();
                return (new IRWhile(@while, def, condition, step, then), true);
            } break;
            
            case ReturnStatementNode @ret:
            {
                var exp = ret.HasExpression ? UnwrapExecutionContext_Expression(ret.Expression, ctx) : null;
                return (new IRReturn(ret, exp), true);
            }
            
            default: return (UnwrapExecutionContext_Expression(node, ctx), true);
        }
        
        IRIf ParseIfElif(SyntaxNode origin, SyntaxNode origin_then, SyntaxNode cond)
        {
            var condition = UnwrapExecutionContext_Expression(cond, ctx);
            IRBlock then = new IRBlock(origin_then);
            
            ctx.PushBlock(then);
            if (origin_then is BlockNode @block) then = UnwrapExecutionContext_Block(ctx, block);
            else
            {
                var (res, _) = UnwrapExecutionContext_Statement(origin_then, ctx);
                if (res != null) then.Content.Add(res);
            }
            ctx.PopBlock();
                
            return new IRIf(@origin, condition, then);
        }
    }
    private IRExpression UnwrapExecutionContext_Expression(SyntaxNode node, ExecutionContextData ctx)
    {
        switch (node)
        {
            case LocalVariableNode @localvar:
            {
                var name = localvar.TypedIdentifier.Identifier.Value;
                
                if (ctx.Locals.Any(e => e.LocalVariable.Name == name))
                    throw new Exception($"{localvar:pos} shadows \'{name}\' declaration");
                
                var newLocal = new LocalVariableObject(
                    SolveTypeLazy(new UnsolvedTypeReference(localvar.TypedIdentifier.Type), ctx), name);
                
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
                if (bexp.Operator is ">" or "<" or ">=" or "<=") return new IRCompareExp(bexp,
                    bexp.Operator switch
                    {
                        ">" => IRCompareExp.Operators.GreaterThan,
                        ">=" => IRCompareExp.Operators.GreaterThanOrEqual,
                        "<" => IRCompareExp.Operators.LessThan,
                        "<=" => IRCompareExp.Operators.LessThanOrEqual,
                        _ => throw new UnreachableException(),
                    },
                    UnwrapExecutionContext_Expression(bexp.Left, ctx),
                    UnwrapExecutionContext_Expression(bexp.Right, ctx));

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
            case UnaryPrefixExpressionNode @uexp:
            {
                return new IRUnaryExp(uexp, uexp.Operator switch
                {
                    "+" => IRUnaryExp.UnaryOperation.Plus,
                    "-" => IRUnaryExp.UnaryOperation.Minus,
                    "!" => IRUnaryExp.UnaryOperation.Not,
                    
                    "&" => IRUnaryExp.UnaryOperation.Reference,
                    
                    "++" => IRUnaryExp.UnaryOperation.PreIncrement,
                    "--" => IRUnaryExp.UnaryOperation.PreDecrement,
                    
                    _ => throw new UnreachableException(),
                },
                    UnwrapExecutionContext_Expression(uexp.Expression, ctx));
            }
            case UnaryPostfixExpressionNode @uexp:
            {
                return new IRUnaryExp(uexp, uexp.Operator switch
                    {
                        "++" => IRUnaryExp.UnaryOperation.PostIncrement,
                        "--" => IRUnaryExp.UnaryOperation.PostDecrement,
                    
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
            case BooleanLiteralNode @boollit: return new IRIntegerLiteral(boollit, boollit.Value ? 1 : 0, 1);
            
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



    private void LazyScanStructureMeta(StructObject structure)
    {
        // This functions ensures that the structure's dependency tree
        // was already scanned!

        var parent = (structure.Extends as SolvedStructTypeReference)?.Struct;
        var virtualCount = EnumerateFunctions(structure.Children).Count(e => e.Abstract || e.Virtual);
        virtualCount += parent?.VirtualTable.Length ?? 0;

        structure.VirtualTable = new (FunctionObject, FunctionObject?, bool)[virtualCount];
        if (parent != null) foreach (var (idx, e) in parent.VirtualTable.Index())
            structure.VirtualTable[idx].parent = e.overrided ?? e.parent;
        
        var virtualStartAt = parent?.VirtualTable.Length ?? 0;

        uint i = 0;
        foreach (var func in EnumerateFunctions(structure.Children))
        {
            // Checking if it is virtual, so a new entries
            // Should be allocated in the vtable
            if (func.Abstract || func.Virtual)
            {
                structure.VirtualTable[i].parent = func;
                structure.VirtualTable[i].overrided = func;
                func.VirtualIndex = i;
                i++;
            }
            
            // Solving a override function
            if (func.Override) SolveOverridingFunction(func, structure);
        }

    }

    private void SolveOverridingFunction(FunctionObject func, StructObject parent)
    {
        foreach (var (i, e) in parent.VirtualTable.Index())
        {
            var basefunc = e.parent;
            
            // I Suppose it is impossible to override a already
            // overrided function in the same structure, so skipping
            // here will be quicker
            if (e.overrided != null) continue;
            if (func.Name != basefunc.Name) continue;
            if (func.Parameters.Length != basefunc.Parameters.Length) continue;

            for (var j = 0; j < func.Parameters.Length; j++)
            {
                if (CalculateTypeSuitability(func.Parameters[j].Type, basefunc.Parameters[j].Type, false)
                    != Suitability.Perfect) continue;
            }
            
            parent.VirtualTable[i].overrided = basefunc;
            func.VirtualIndex = (uint)i;
            return;
        }

        throw new Exception("No virtual function to override");
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
                        case BooleanTypeReference:
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
            
            // Search in namespace
            var res3 = SearchReference_ChildrenOf(reference[0], ctx.Parent.Namespace);
            if (res3.HasValue)
            {
                referenceChain.Add(res3.Value.Item1);
                langobj = res3.Value.Item2;
                break;
            }

            // Search in imports
            var res4 = SearchReference_Import(reference[0], ctx.Parent.Namespace);
            if (res4.HasValue)
            {
                referenceChain.Add(res4.Value.Item1);
                langobj = res4.Value.Item2;
                break;
            }

            // Search in globals
            var res5 = SearchReference_Global(reference);
            if (res5 != null) referenceChain.Add(res5);
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
                
                switch (children)
                {
                    case FieldObject f:
                        langobj = f.Type switch
                        {
                            SolvedStructTypeReference st => st.Struct,
                            IntegerTypeReference => null,
                            _ => throw new UnreachableException()
                        };
                        break;
                    
                    case FunctionGroupObject g: langobj = null; break;
                    default: throw new UnreachableException();
                }
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
            // Checking if is a sibling
            var res1 = SearchReference_ChildrenOf(reference[0], obj.Parent);
            if (res1.HasValue)
            {
                referenceChain.Add(res1.Value.Item1);
                break;
            }
            
            // Checking if is inside namespace
            var res2 = SearchReference_ChildrenOf(reference[0], obj.Namespace);
            if (res2.HasValue)
            {
                referenceChain.Add(res2.Value.Item1);
                break;
            }
            
            // Checking if is a import
            var res3 = SearchReference_Import(reference[0], obj.Namespace);
            if (res3.HasValue)
            {
                referenceChain.Add(res3.Value.Item1);
                break;
            }
            
            // Checking if is a global
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
        var local = ctx.Locals.FirstOrDefault(e => e.LocalVariable.Name == part);
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
        var i = 0;
        while (true)
        {
            i++;
            switch (typeref)
            {
                case UnsolvedTypeReference @unsolved:
                {
                    var node = unsolved.syntaxNode;
                    if (i == 1)
                    {
                        typeref = SolveShallowType(node);
                        if (IsSolved(typeref)) return typeref;
                        if (ctx == null) return new UnsolvedTypeReference(node);
                        continue;
                    }
                    else
                    {
                        var reef = SearchReference(node, ctx);
                        if (reef is not IRSolvedReference @solv) return new UnsolvedTypeReference(node);
                        return (solv.Reference as TypeReference) ?? new UnsolvedTypeReference(node);   
                    }
                }
                case SliceTypeReference @slice: slice.InternalType = SolveTypeLazy(@slice.InternalType, ctx); break;
                case ReferenceTypeReference @refer: refer.InternalType = SolveTypeLazy(@refer.InternalType, ctx); break;
            }

            return typeref;
        }
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

    private List<StructObject> TopologicalSort(IEnumerable<StructObject> structs)
    {
        var visited = new HashSet<StructObject>();
        var visiting = new HashSet<StructObject>();
        var ordered = new List<StructObject>();

        foreach (var s in structs) Visit(s);
        return ordered;

        void Visit(StructObject s)
        {
            if (visited.Contains(s))
                return;

            if (!visiting.Add(s))
                throw new Exception($"Cyclic dependency detected at struct '{string.Join('.', s.Global)}'");

            var parent = (s.Extends as SolvedStructTypeReference)?.Struct;
            if (parent != null)
                Visit(parent);

            visiting.Remove(s);
            visited.Add(s);
            ordered.Add(s);
        }
    }

}
