using System.Diagnostics;
using System.Numerics;
using Abstract.CodeProcess.Core;
using Abstract.CodeProcess.Core.Language.EvaluationData;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Macros;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Statements;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.CodeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FieldReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FunctionReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.Integer;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

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
        List<FunctionObject> funclist = [];
        List<FieldObject> fldlist = [];

        foreach (var (_, i) in _globalReferenceTable)
        {
            switch (i)
            {
                case NamespaceObject nmsp: NamespaceSenaAnal(nmsp); break;
                case FunctionGroupObject group: funclist.AddRange(group.Overloads); break;
                case FunctionObject f: funclist.Add(f); break;
                case FieldObject f: fldlist.Add(f); break;
            }
        }
        
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
                
                case StructObject @struc:
                    StructureSemaAnal(struc);
                    break;
                
                case FieldObject @field:
                    FieldSemaAnal(field);
                    break;
            }
            
        }
        
        // Excution analysis
        foreach (var fld in fldlist)
        {
            if (fld.Value == null) continue;
            var ctx = new IrBlockExecutionContextData(fld);
            fld.Value = (IRExpression)NodeSemaAnal(fld.Value, ctx);
        }
        
        foreach (var fun in funclist)
        {
            var ctx = new IrBlockExecutionContextData(fun);
            if (fun.Body != null) BlockSemaAnal(fun.Body, ctx);
        }
    }

    private void NamespaceSenaAnal(NamespaceObject nmsp)
    {
        foreach (var a in nmsp.Imports)
        {
            foreach (var raw in a.Raw)
            {
                if (raw[^1] == "*")
                {
                    var refe = _globalReferenceTable[raw[..^1]];
                    if (refe is not NamespaceObject @namespaceObject) throw new Exception("Not a namespace");
                    
                    foreach (var i in namespaceObject.Children) a.References.Add(i);
                }
                else
                {
                    var refe = _globalReferenceTable[raw[..^1]];
                    a.References.Add(refe);
                }
            }
        }
    }
    private void FunctionSemaAnal(FunctionObject function)
    {

        if (!IsSolved(function.ReturnType))
            function.ReturnType = SolveTypeLazy2(function.ReturnType, null, function);
        
        foreach (var i in function.Parameters)
        {
            if (IsSolved(i.Type)) continue;
            i.Type = SolveTypeLazy2(i.Type, null, function);
        }
    }

    private void StructureSemaAnal(StructObject structure)
    {
        foreach (var i in structure.Children.OfType<FieldObject>())
            FieldSemaAnal(i);
    }
    private void FieldSemaAnal(FieldObject field)
    {
        if (IsSolved(field.Type)) return;
        field.Type = SolveTypeLazy2(field.Type, null, field);
    }

    
    private void BlockSemaAnal(IRBlock block, IrBlockExecutionContextData ctx, bool newFrame = true)
    {
        if (newFrame) ctx.PushFrame();
        for (var i = 0; i < block.Content.Count; i++) block.Content[i] = NodeSemaAnal(block.Content[i], ctx);
        if (newFrame) ctx.PopFrame();
    }

    private IRNode NodeSemaAnal(IRNode node, IrBlockExecutionContextData ctx)
    {
        return node switch
        {
            IRDefLocal @dl => NodeSemaAnal_Macro_DefLocal(dl, ctx),
            
            IRInvoke @iv => NodeSemaAnal_Invoke(iv, ctx),
            IRAssign @ass => NodeSemaAnal_Assign(ass, ctx),
            IRCompareExp @ce => NodeSemaAnal_CmpExp(ce, ctx),
            IRBinaryExp @be => NodeSemaAnal_BinExp(be, ctx),
            IRUnaryExp @ue => NodeSemaAnal_UnExp(ue, ctx),
            IrConv @tc =>NodeSemaAnal_Conv(tc, ctx),
            IRNewObject @no => NodeSemaAnal_NewObj(no, ctx),
            IRReturn @re => NodeSemaAnal_Return(re, ctx),
            IRIf @iff => NodeSemaAnal_If(iff, ctx),
            IRWhile @iwhile => NodeSemaAnal_While(iwhile, ctx),
            
            IRSolvedReference
                or IRStringLiteral
                or IRIntegerLiteral 
                or IRNullLiteral => node,
            
            IRAccess @s => NodeSemaAnal_Access(s, ctx),
            IRUnknownReference @u => SolveReferenceLazy(u, ctx, null),
            
            _ => throw new NotImplementedException(),
        };
    }

    private IRNode NodeSemaAnal_Macro_DefLocal(IRDefLocal deflocal, IrBlockExecutionContextData ctx)
    {
        if (!IsSolved(deflocal.LocalVariable.Type))
            deflocal.LocalVariable.Type = SolveTypeLazy2(deflocal.LocalVariable.Type, ctx, null);
        
        ctx.RegisterLocalVariable(deflocal.LocalVariable);
        return deflocal;
    }
    
    private IRNode NodeSemaAnal_Invoke(IRInvoke node, IrBlockExecutionContextData ctx)
    {
        node.Target = (IRExpression)NodeSemaAnal(node.Target, ctx);
        var fng = ((SolvedFunctionGroupReference)ReferenceOf(node.Target)).FunctionGroup;

        IRExpression? instance = null;
        if (node.Target is IRAccess @irAccess)
        {
            instance = irAccess.A;
            node.Target = irAccess.B;
        }
        
        for (var i = 0; i < node.Arguments.Length; i++)
            node.Arguments[i] = (IRExpression)NodeSemaAnal(node.Arguments[i], ctx);

        
        node.Target = NodeSemaAnal_Invoke_GetFunctionOverload(fng, node.Arguments, node.Origin);
        node.Type = ((FunctionTypeReference)node.Target.Type!).Returns;
        //else throw new NotImplementedException();
        
        if (instance != null) node.Arguments = [(IRExpression)NodeSemaAnal(instance, ctx), ..node.Arguments];
        
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
            
            if (arguments.Length != ov.Parameters.Length) continue;
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
                var argt = GetEffectiveTypeReference(arguments[i]);
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

        if (betterFound == null)
            throw new Exception($"CompError: No overload that matches function call {origin}");
        

        for (var i = 0; i < arguments.Length; i++)
            arguments[i] = SolveTypeCast(betterFound.Parameters[i].Type, arguments[i]);
        
        var newref = new IRSolvedReference(origin, new SolvedFunctionReference(betterFound));
        newref.Type = new FunctionTypeReference(betterFound.ReturnType, betterFound.Parameters.Select(e => e.Type).ToArray());
        return newref;
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

    private IRNode NodeSemaAnal_CmpExp(IRCompareExp node, IrBlockExecutionContextData ctx)
    {
        node.Left = (IRExpression)NodeSemaAnal(node.Left, ctx);
        var leftTypeRef = GetEffectiveTypeReference(node.Left);
        node.Right = SolveTypeCast(leftTypeRef, (IRExpression)NodeSemaAnal(node.Right, ctx));

        var ltype = GetEffectiveTypeReference(node.Left);
        var rtype = GetEffectiveTypeReference(node.Right);
        TypeReference ftype = ltype;
        
        if (ltype is RuntimeIntegerTypeReference @left &&
            rtype is RuntimeIntegerTypeReference @right)
        {
            if (left.BitSize >= right.BitSize) ftype = left;
            else if (left.BitSize < right.BitSize) ftype = right;
        }
        
        else if (ltype is RuntimeIntegerTypeReference @left2 &&
                 rtype is ComptimeIntegerTypeReference)
        {
            ftype = left2;
            node.Right = new IRIntegerLiteral(node.Right.Origin, ((IRIntegerLiteral)node.Right).Value, left2);
        }
        
        else if (ltype is ComptimeIntegerTypeReference @left3 &&
                 rtype is RuntimeIntegerTypeReference @right3)
        {
            node.Left = new IRIntegerLiteral(node.Left.Origin, ((IRIntegerLiteral)node.Left).Value, right3);
            ftype = right3;
        }
        
        else throw new NotImplementedException();
        
        switch (node)
        {
            case { Left: IRIntegerLiteral @leftInt, Right: IRIntegerLiteral @rightInt }:
                return new IRIntegerLiteral(node.Origin, node.Operator switch
                {
                    IRCompareExp.Operators.GreaterThan => leftInt.Value > rightInt.Value,
                    IRCompareExp.Operators.LessThan => leftInt.Value < rightInt.Value,
                    IRCompareExp.Operators.LessThanOrEqual => leftInt.Value <= rightInt.Value,
                    IRCompareExp.Operators.GreaterThanOrEqual => leftInt.Value >= rightInt.Value,
                    _ => throw new UnreachableException()
                } ? 1 : 0, (IntegerTypeReference)ftype);
        }
        
        return node;
    }
    private IRNode NodeSemaAnal_BinExp(IRBinaryExp node, IrBlockExecutionContextData ctx)
    {
        node.Left = (IRExpression)NodeSemaAnal(node.Left, ctx);
        var leftTypeRef = GetEffectiveTypeReference(node.Left);
        node.Right = SolveTypeCast(leftTypeRef, (IRExpression)NodeSemaAnal(node.Right, ctx));

        // TODO solve operator overloading

        var ltype = GetEffectiveTypeReference(node.Left);
        var rtype = GetEffectiveTypeReference(node.Right);
        TypeReference ftype = ltype;
        
        switch (ltype)
        {
            case RuntimeIntegerTypeReference @left when
                rtype is RuntimeIntegerTypeReference @right:
            {
                if (left.BitSize >= right.BitSize) ftype = left;
                else if (left.BitSize < right.BitSize) ftype = right;
            } break;
            
            case RuntimeIntegerTypeReference @left2 when
                rtype is ComptimeIntegerTypeReference:
            {
                ftype = left2;
                node.Right = new IRIntegerLiteral(node.Right.Origin, ((IRIntegerLiteral)node.Right).Value, left2);
            } break;
            
            case ComptimeIntegerTypeReference @left3 when
                rtype is RuntimeIntegerTypeReference @right3:
            {
                node.Left = new IRIntegerLiteral(node.Left.Origin, ((IRIntegerLiteral)node.Left).Value, right3);
                ftype = right3;
            } break;

            case ComptimeIntegerTypeReference when
                rtype is ComptimeIntegerTypeReference:
            {
                ftype = new ComptimeIntegerTypeReference();
            } break;

            case StringTypeReference @sl when
                rtype is StringTypeReference @sr:
            {
                if (sl.Encoding == sr.Encoding) ftype = new StringTypeReference(sl.Encoding);
                else throw new Exception("Cannot concatenate strings with different encoding");
            } break;

            default: throw new NotImplementedException();
        }

        node.Type = ftype;
        
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
                
                    _ => throw new NotImplementedException(),
                }, (IntegerTypeReference)ftype);
            
            case { Left: IRStringLiteral @leftStr, Right: IRStringLiteral @rightStr }:
                return new IRStringLiteral(node.Origin, node.Operator switch
                    {
                        IRBinaryExp.Operators.Add => leftStr.Data + rightStr.Data,
                        _ => throw new UnreachableException()
                    }
                );
        }
        
        return node;
    }
    private IRNode NodeSemaAnal_UnExp(IRUnaryExp node, IrBlockExecutionContextData ctx)
    {
        node.Value = (IRExpression)NodeSemaAnal(node.Value, ctx);

        if (node is { Value: IRIntegerLiteral @valInt })
        {
            return new IRIntegerLiteral(node.Origin, node.Operation switch
            {
                IRUnaryExp.UnaryOperation.Plus => valInt.Value,
                IRUnaryExp.UnaryOperation.Minus => BigInteger.Negate(valInt.Value),
                IRUnaryExp.UnaryOperation.Not => ~valInt.Value,
                
                IRUnaryExp.UnaryOperation.PreIncrement => valInt.Value + BigInteger.One,
                IRUnaryExp.UnaryOperation.PreDecrement => valInt.Value - BigInteger.One,
                IRUnaryExp.UnaryOperation.PostIncrement or
                IRUnaryExp.UnaryOperation.PostDecrement => valInt.Value,
                
                _ => throw new UnreachableException(),
            }, (IntegerTypeReference)GetEffectiveTypeReference(node.Value));
        }

        node.Type = node.Value.Type;
        return node;
    }

    private IRNode NodeSemaAnal_Conv(IrConv node, IrBlockExecutionContextData ctx)
    {
        node.Expression = (IRExpression)NodeSemaAnal(node.Expression, ctx);
        return SolveTypeCast(node.Type, node.Expression, true);
    }
    private IRNode NodeSemaAnal_NewObj(IRNewObject node, IrBlockExecutionContextData ctx)
    {
        // TODO i prefer handle constructor overloading when
        //  we have actual constructable structures working

        node.Type = SolveTypeLazy2(node.Type, ctx, null);

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
        if (ctx.Parent is not FunctionObject function) throw new InvalidCastException();
        
        node.Value = (IRExpression)NodeSemaAnal(node.Value, ctx);
        node.Value = SolveTypeCast(function.ReturnType!, node.Value, false);
        return node;
    }
    private IRNode NodeSemaAnal_If(IRIf node, IrBlockExecutionContextData ctx)
    {
        node.Condition = (IRExpression)NodeSemaAnal(node.Condition, ctx);
        if (node.Else != null)
            node.Else = (IIfElse)(node.Else is IRIf @if
                ? NodeSemaAnal_If(@if, ctx) : NodeSemaAnal_Else((IRElse)node.Else, ctx));
        BlockSemaAnal(node.Then, ctx);
        return node;
    }
    private IRNode NodeSemaAnal_Else(IRElse node, IrBlockExecutionContextData ctx)
    {
        BlockSemaAnal(node.Then, ctx);
        return node;
    }

    public IRNode NodeSemaAnal_While(IRWhile node, IrBlockExecutionContextData ctx)
    {
        if (node.Define != null) BlockSemaAnal(node.Define, ctx, false);
        node.Condition = (IRExpression)NodeSemaAnal(node.Condition, ctx);
        if (node.Step != null) BlockSemaAnal(node.Step, ctx);
        BlockSemaAnal(node.Process, ctx);
        
        return node;
    }

    public IRNode NodeSemaAnal_Access(IRAccess node, IrBlockExecutionContextData ctx)
    {
        node.A = (IRExpression)NodeSemaAnal(node.A, ctx);
        LanguageReference baseRef = ReferenceOf(node.A);

        switch (baseRef)
        {
            case NamespaceReference @nmsp:
                return SolveReferenceLazy((IRUnknownReference)node.B, null, nmsp.NamespaceObject);

            case TypeTypeReference @ohboy:
            {
                var origin = node.B.Origin;
                var key = ((IdentifierNode)origin).Value;

                node.B = new IRSolvedReference(origin, new MetaReference(key switch
                {
                    "name" => MetaReference.MetaReferenceType.Name,
                    "size" => MetaReference.MetaReferenceType.ByteSize,
                    "bitSize" => MetaReference.MetaReferenceType.BitSize,
                    "alignment" => MetaReference.MetaReferenceType.Alignment,
                    _ => throw new Exception("ur dumb af get gud")
                }));
            } break;

            case LocalReference @local:
            {
                node.B = local.Local.Type switch
                {
                    SolvedStructTypeReference @s => SolveReferenceLazy((IRUnknownReference)node.B, null, s.Struct),
                    _ => throw new UnreachableException()
                };
            } break;
            case ParameterReference @arg:
            {
                node.B = arg.Parameter.Type switch
                {
                    SolvedStructTypeReference @s => SolveReferenceLazy((IRUnknownReference)node.B, null, s.Struct),
                    
                    ReferenceTypeReference @r => SolveReferenceLazy((IRUnknownReference)node.B, null, r.InternalType switch
                    {
                        SolvedStructTypeReference s => s.Struct,
                        _ => throw new UnreachableException()
                    }),
                    
                    BooleanTypeReference => new IRSolvedReference(null!, arg.Parameter.Type),
                    
                    _ => throw new UnreachableException()
                };
            } break;
            case SolvedFieldReference @field:
            {
                node.B = field.Field.Type switch
                {
                    SolvedStructTypeReference @s => SolveReferenceLazy((IRUnknownReference)node.B, null, s.Struct),
                    _ => throw new UnreachableException()
                };
            } break;

            default: throw new UnreachableException();
        }

        return node;
    }
    
    public IRSolvedReference SolveReferenceLazy(IRUnknownReference node, IrBlockExecutionContextData? ctx, LangObject? parent)
    {
        var syntaxNode = node.Origin;
        if (ctx == null && parent == null) throw new UnreachableException();
        if (parent == null && ctx != null) parent = ctx.Parent;

        switch (syntaxNode)
        {
            case IdentifierNode @idnode:
            {
                // Search in local variables
                var r = ctx?.LocalVariables.FirstOrDefault(e => e.Name == idnode.Value);
                if (r != null) return new IRSolvedReference(syntaxNode, new LocalReference(r)) { Type = r.Type };

                // Search in parameters
                var r2 = (parent as FunctionObject)?.Parameters.FirstOrDefault(e => e.Name == idnode.Value);
                if (r2 != null) return new IRSolvedReference(syntaxNode, new ParameterReference(r2)) { Type = r2.Type };
                
                // Search in parent tree
                var curr = parent;
                while (curr != null && curr is not NamespaceObject)
                {
                    var r3 = curr.Children.FirstOrDefault(e => e.Name == idnode.Value);
                    if (r3 != null) return new IRSolvedReference(syntaxNode, GetObjectReference(r3)) { Type = TypeRefOf(r3) };
                    curr = curr.Parent;
                }
                
                // Search inside namespace
                var r4 = parent.Namespace.Children.FirstOrDefault(e => e.Name == idnode.Value);
                if (r4 != null) return new IRSolvedReference(syntaxNode, GetObjectReference(r4)) { Type = TypeRefOf(r4) };

                // Search inside imports
                var r5 = parent.Imports?.References.FirstOrDefault(e => e.Name == idnode.Value);
                if (r5 != null) return new IRSolvedReference(syntaxNode, GetObjectReference(r5)) { Type = TypeRefOf(r5) };
                
                // Search global references
                var r6 = _globalReferenceTable.FirstOrDefault(e => e.Key.Length == 1 && e.Key[0] == idnode.Value);
                if (r6.Key != null) return new IRSolvedReference(syntaxNode, GetObjectReference(r6.Value)) { Type = TypeRefOf(r6.Value) };

                if (parent is NamespaceObject @nmsp)
                {
                    string[] name = [.. nmsp.Global, idnode.Value];
                    var r7 = _globalReferenceTable.FirstOrDefault(e => IdentifierComparer.IsEquals(e.Key, name));
                    if (r7.Key != null) return new IRSolvedReference(syntaxNode, GetObjectReference(r7.Value)) { Type = TypeRefOf(r7.Value) };
                }
                
                throw new Exception($"Cannot find reference to {idnode:pos}");
            }
            
            default: throw new UnreachableException(); break;
        }
    }

    private LanguageReference ReferenceOf(IRNode node) => node switch
        {
            IRAccess @acc => ReferenceOf(acc.B),
            IRSolvedReference @sr => sr.Reference,
            IRInvoke @iv => iv.Type!,
            _ => throw new UnreachableException(),
        };
    private TypeReference? TypeRefOf(LangObject obj) => obj switch
        {
            StructObject @st => new SolvedStructTypeReference(st),
            TypedefObject @td => new SolvedTypedefTypeReference(td),
            FieldObject @fi => fi.Type,
            
            NamespaceObject or
            FunctionGroupObject => null,
            
            _ => throw new UnreachableException()
        };
    
    private TypeReference SolveTypeLazy2(TypeReference typeref, IrBlockExecutionContextData? ctx, LangObject? obj)
    {
        switch (typeref)
        {
            case UnsolvedTypeReference @unsolved: typeref = (TypeReference)SolveReferenceLazy(new IRUnknownReference(unsolved.syntaxNode), ctx, obj).Reference; break;
            case SliceTypeReference @slice: slice.InternalType = SolveTypeLazy2(@slice.InternalType, ctx, obj); break;
            case ReferenceTypeReference @refer: refer.InternalType = SolveTypeLazy2(@refer.InternalType, ctx, obj); break;
            case NullableTypeReference @nullable: nullable.InternalType = SolveTypeLazy2(@nullable.InternalType, ctx, obj); break;
        }
        
        return typeref;
    }
}
