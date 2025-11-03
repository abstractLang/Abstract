using System.Diagnostics;
using Abstract.CodeProcess.Core;
using Abstract.CodeProcess.Core.Language;
using Abstract.CodeProcess.Core.Language.EvaluationData;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Macros;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Statements;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.CodeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.Integer;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Statement;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

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
    }
    private void ScanStructureMeta(StructObject structure)
    {
        if (structure.Extends != null)
        {
            structure.Extends = SolveTypeLazy(structure.Extends, null, structure);
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
                    if (!IsSolved(field.Type)) field.Type = SolveTypeLazy(field.Type, null, structure);
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
            if (!IsSolved(t.Type)) t.Type = SolveTypeLazy(t.Type, null, function);
        }
        if (!IsSolved(function.ReturnType))
            function.ReturnType = SolveTypeLazy(function.ReturnType, null, function);
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
                case FieldObject @fld: ScanFieldValueBody(fld); break;
            }
        }
    }

    private void ScanFieldValueBody(FieldObject field)
    {
        var nodes = field.syntaxNode.Children;

        if (nodes is not [_, _, TokenNode { token.type: TokenType.EqualsChar }, ExpressionNode @value]) return;
        
        var ctx = new ExecutionContextData(field);
        field.Value = UnwrapExecutionContext_Expression(value, ctx);
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
                var type = SolveTypeLazy(new UnsolvedTypeReference(localvar.TypedIdentifier.Type), ctx, null);
                
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
                    SolveTypeLazy(new UnsolvedTypeReference(localvar.TypedIdentifier.Type), ctx, null), name);
                
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
                        "+%" => IRBinaryExp.Operators.AddWarpAround,
                        "+|" => IRBinaryExp.Operators.AddOnBounds,
                        
                        "-" => IRBinaryExp.Operators.Subtract,
                        "-%" =>  IRBinaryExp.Operators.SubtractWarpAround,
                        "-|" => IRBinaryExp.Operators.SubtractOnBounds,
                        
                        "*" => IRBinaryExp.Operators.Multiply,
                        
                        "/" => IRBinaryExp.Operators.Divide,
                        "/_" => IRBinaryExp.Operators.DivideFloor,
                        "/^" => IRBinaryExp.Operators.DivideCeil,
                        
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
                    SolveTypeLazy(new UnsolvedTypeReference(tcast.TargetType), ctx, null));
            }
            
            case AccessNode @identc: return SolveReferenceChain(identc, ctx, null);
            case IdentifierNode @ident: return SolveReferenceChain(ident, ctx, null);

            case IntegerLiteralNode @intlit:
                return new IRIntegerLiteral(intlit, intlit.Value, new ComptimeIntegerTypeReference());
            case StringLiteralNode @strlit:
            {
                if (strlit.IsSimple) return new IRStringLiteral(strlit, strlit.RawContent);
                throw new NotImplementedException();
            }
            case BooleanLiteralNode @boollit:
                return new IRIntegerLiteral(boollit, boollit.Value ? 1 : 0, new RuntimeIntegerTypeReference(false, 1));
            case NullLiteralNode @nulllit: 
                return new IRNullLiteral(nulllit);
            
            case NewObjectNode @newobj:
            {
                List<IRAssign> asisgns = [];

                var typer = SolveTypeLazy(new UnsolvedTypeReference(newobj.Type), ctx, null);

                if (newobj.Inlined != null)
                {
                    foreach (var i in newobj.Inlined.Content)
                    {
                        if (i is not AssignmentExpressionNode @ass) throw new UnreachableException();
                        asisgns.Add(new IRAssign(ass,
                            new IRUnknownReference(ass.Left),
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


    private IRExpression SolveReferenceChain(ExpressionNode node, ExecutionContextData? ctx, LangObject? obj)
    {
        return node switch
        {
            AccessNode @access => new IRAccess(node,
                SolveReferenceChain(access.Left, ctx, obj), SolveReferenceChain(access.Right, ctx, obj)),
            
            IdentifierNode @ident => ((Func<IRExpression>)(() =>
            {
                var a = SolveShallowType(ident);
                return a is UnsolvedTypeReference
                    ? new IRUnknownReference(ident)
                    : new IRSolvedReference(ident, a);
            })).Invoke(),
            _ => UnwrapExecutionContext_Expression(node, ctx),
        };
    }
    
    private TypeReference SolveTypeLazy(TypeReference typeref, ExecutionContextData? ctx, LangObject? obj)
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

                    return new UnsolvedTypeReference(node);
                }
                case SliceTypeReference @slice: slice.InternalType = SolveTypeLazy(@slice.InternalType, ctx, obj); break;
                case ReferenceTypeReference @refer: refer.InternalType = SolveTypeLazy(@refer.InternalType, ctx, obj); break;
            }

            return typeref;
        }
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
