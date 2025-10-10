using System.Diagnostics;
using System.Numerics;
using Abstract.CodeProcess.Core.Language.EvaluationData;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Macros;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Statements;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FunctionReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.Integer;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess;

/*
 * Stage Four:
 *  Semantic analysis, solving automatic type inference, type conversion,
 *  operator overloading, function overloading, etc.
 */

public partial class Analyzer
{
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
            if (fun.Body != null) BlockSemaAnal(fun.Body, fun);
    } 
    
    private void FunctionSemaAnal(FunctionObject function)
    {
        foreach (var i in function.Parameters)
        {
            if (IsSolved(i.Type)) continue;
            i.Type = SolveTypeLazy(i.Type, function);
        }
    }
    private void FieldSemaAnal(FieldObject field)
    {
        if (IsSolved(field.Type)) return;
        field.Type = SolveTypeLazy(field.Type, field);
    }

    
    private void BlockSemaAnal(IRBlock block, FunctionObject function)
    {
        var ctx = new IrBlockExecutionContextData(function);
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
        switch (node.Target)
        {

            case IRSolvedReference @solvedref:
            {

                for (var i = 0; i < node.Arguments.Length; i++)
                    node.Arguments[i] = (IRExpression)NodeSemaAnal(node.Arguments[i], ctx);

                if (solvedref.Reference is SolvedFunctionGroupReference @fgroupref)
                    node.Target = NodeSemaAnal_Invoke_GetFunctionOverload(fgroupref.FunctionGroup, node.Arguments, node.Origin);

                // I SUPPOSE it is impossible that the analizer stages before here
                // are able to assign anything other than a FunctionGroup ref...
                // i may be wrong...
                else throw new NotImplementedException();
            } break;

            case IRReferenceAccess @refacc:
            {
                // instance access detected, instance type will be handled as one
                // of the arguments

                var instance = refacc.A;
                var call = (IRSolvedReference)refacc.B; // FIXME conversion not reliable
                
                for (var i = 0; i < node.Arguments.Length; i++)
                    node.Arguments[i] = (IRExpression)NodeSemaAnal(node.Arguments[i], ctx);

                var nargs = new IRExpression[node.Arguments.Length + 1];
                nargs[0] = new IRSolvedReference(refacc.Origin, instance);
                node.Arguments.CopyTo(nargs.AsSpan(1));
                
                if (call.Reference is SolvedFunctionGroupReference @fgroupref)
                    node.Target = NodeSemaAnal_Invoke_GetFunctionOverload(fgroupref.FunctionGroup, nargs, node.Origin);
                else throw new NotImplementedException();

                node.Arguments = nargs;
            } break;
            
            default: throw new NotImplementedException();
        }

        return node;
    }

    private IRReference NodeSemaAnal_Invoke_GetFunctionOverload(
        FunctionGroupObject group, IRExpression[] arguments, SyntaxNode origin)
    {
        // Node is a function group and must be analysed to point
        // to the correct or most optimal overload

        var overloads = group.Overloads;
        FunctionObject? betterFound = null;
        var betterFoundInstance = false;
        var betterFoundSum = 0;

        var instanceableStruct = group.Parent is StructObject { Static: false };

        foreach (var ov in overloads)
        {
            var instanceFunc = instanceableStruct && !ov.Static;
            
            if (ov.Parameters.Length + (instanceFunc ? 1 : 0) != arguments.Length) continue;
            if (ov.Parameters.Length == 0)
            {
                betterFound = ov;
                betterFoundSum = 0;
                continue;
            }
            
            var parameters = ov.Parameters;
            var suitability = new int[parameters.Length];

            for (int i = 0; i < parameters.Length; i++)
            {
                var argt = GetEffectiveTypeReference(arguments[instanceFunc ? i + 1 : i]);
                var s = (int)CalculateTypeSuitability(parameters[i].Type, argt, true);
                if (s == 0) goto NoSuitability;
                suitability[i] = s;
            }

            var sum = (suitability.Sum() * 100) / parameters.Length;
            if (sum <= betterFoundSum) continue;
            betterFound = ov;
            betterFoundSum = sum;
            betterFoundInstance = instanceFunc;

            NoSuitability: ;
        }

        if (betterFound == null) throw new Exception($"CompError: No overload that matches function call {origin}");


        for (var i = betterFoundInstance ? 1 : 0; i < arguments.Length; i++)
            arguments[i] = SolveTypeCast(betterFound.Parameters[betterFoundInstance ? i-1 : i].Type, arguments[i]);
        
        return new IRSolvedReference(origin, new SolvedFunctionReference(betterFound));
    }
    private IRNode NodeSemaAnal_Assign(IRAssign node, IrBlockExecutionContextData ctx)
    {
        var a = node;
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
        if (node.Value == null) return node;
        
        node.Value = (IRExpression)NodeSemaAnal(node.Value, ctx);
        node.Value = SolveTypeCast(ctx.Function.ReturnType!, node.Value, false);
        return node;
    }
    
}