using System.Diagnostics;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.CodeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FieldReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FunctionReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.Integer;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression.TypeModifiers;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess;

public partial class Analyzer
{

    private static TypeReference GetEffectiveTypeReference(IRExpression expr)
    {
        return expr switch
        {
            IRIntegerLiteral => new ComptimeIntegerTypeReference(),
            IRStringLiteral => new StringTypeReference(StringEncoding.Undefined),
            IRSolvedReference @solvedFuck => solvedFuck.Reference switch
            {
                IntegerTypeReference @intt => intt,
                SolvedStructTypeReference @structt => structt,
                SolvedFieldReference field => field.Field.Type,
                SolvedFunctionReference @func => new FunctionTypeReference(
                    func.Function.ReturnType, func.Function.Parameters.Select(e => e.Type).ToArray()),

                LocalReference @local => local.Local.Type,
                ParameterReference @param => param.Parameter.Type,

                MetaReference @meta => meta.Type switch
                {
                    MetaReference.MetaReferenceType.Name => new StringTypeReference(StringEncoding.Ascii),
                    MetaReference.MetaReferenceType.ByteSize or
                    MetaReference.MetaReferenceType.BitSize or
                    MetaReference.MetaReferenceType.Alignment => new RuntimeIntegerTypeReference(false),
                    _ => throw new ArgumentOutOfRangeException()
                },
                
                _ => throw new NotImplementedException()
            },
            IRAccess @access => GetEffectiveTypeReference(access.B),
            IRInvoke @invoke => invoke.Type,
            
            IRBinaryExp @exp => exp.Type,
            
            IRUnaryExp @unexp => unexp.Operation != IRUnaryExp.UnaryOperation.Reference
                ? GetEffectiveTypeReference(unexp.Value)
                : new ReferenceTypeReference(GetEffectiveTypeReference(@unexp.Value)),
            
            IrConv @conv => conv.Type,
            IRIntCast @tcast => tcast.Type,
            IRIntExtend @icast => icast.Type,
            IRIntTrunc @itrunc => itrunc.Type,
            
            _ => throw new NotImplementedException()
            
        } ?? throw new UnreachableException("This function should not be called when this value is null");
    }
    
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
                    if (texp.Children.Length > 1)
                        throw new UnreachableException("Wtf i didn't even knew this was possible");
                    node = texp.Children[0];
                    continue;

                case AccessNode @idc:
                    if (idc.Children.Length != 1) return new UnsolvedTypeReference(idc);
                    node = idc.Children[0];
                    continue;

                case IdentifierNode @id:
                    var value = id.Value;
                    switch (value)
                    {
                        case "iptr": return new RuntimeIntegerTypeReference(true);
                        case "uptr": return new RuntimeIntegerTypeReference(false);
                        case "bool": return new BooleanTypeReference();
                        case "void": return new VoidTypeReference();
                        case "type": return new TypeTypeReference();
                        case "string": return new StringTypeReference(StringEncoding.Undefined);
                        case "anytype": return new AnytypeTypeReference();
                        case "noreturn": return new NoReturnTypeReference();
                    }

                    if (value.Length > 1 && value[0] is 'i' or 'u' && value[1..].All(char.IsNumber))
                        return new RuntimeIntegerTypeReference(value[0] == 'i', byte.Parse(value[1..]));


                    return new UnsolvedTypeReference(id);

                case ArrayTypeModifierNode @ar:
                    return new SliceTypeReference(SolveShallowType(ar.Type));

                case ReferenceTypeModifierNode @rf:
                    return new ReferenceTypeReference(SolveShallowType(rf.Type));

                case NullableTypeModifierNode @nullable:
                    return new NullableTypeReference(SolveShallowType(nullable.Type));
                
                case BinaryExpressionNode @b: return new UnsolvedTypeReference(b);

                default: throw new NotImplementedException();
            }
        }
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
        var a = typeTo;
        var b = value;
        
        switch (typeTo)
        {
            case RuntimeIntegerTypeReference typetoRi when value is IRIntegerLiteral @lit:
                return new IRIntegerLiteral(lit.Origin, lit.Value, typetoRi);
            
            case RuntimeIntegerTypeReference typetoRi:
            {
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

                break;
            }

            case SolvedTypedefTypeReference typedef:
            {
              // TODO idk handle it somehow  
            } break;
        }
        
        return value;
    }
    
    private Suitability CalculateTypeSuitability(TypeReference typeTo, TypeReference typeFrom, bool allowImplicit)
    {
        switch (typeTo)
        { 
            case AnytypeTypeReference: return Suitability.NeedsSoftCast;
            
            case RuntimeIntegerTypeReference intParam:
                switch (typeFrom)
                {
                    case ComptimeIntegerTypeReference: return Suitability.Perfect;
                    case RuntimeIntegerTypeReference intArg:
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
                    default: return Suitability.None;
                }

            case StringTypeReference stringParam:
                if (typeFrom is StringTypeReference @strArg
                    && (strArg.Encoding == StringEncoding.Undefined
                        || strArg.Encoding == stringParam.Encoding)) return Suitability.Perfect;
                return Suitability.None;

            case ReferenceTypeReference @refe:
                return typeFrom is ReferenceTypeReference @refArg 
                       && CalculateTypeSuitability(refe.InternalType, refArg.InternalType, false) == Suitability.Perfect
                    ? Suitability.Perfect
                    : Suitability.None;
            
            case SolvedStructTypeReference @solvedstruct:
                if (typeFrom is SolvedStructTypeReference @solvedstructarg)
                    return (Suitability)solvedstruct.CalculateSuitability(solvedstructarg);
                return Suitability.None;
            
            case SolvedTypedefTypeReference @solvedTypedef:
                if (typeFrom is ComptimeIntegerTypeReference) return Suitability.Perfect;
                if (typeFrom is not SolvedTypedefTypeReference @solvedTypedefarg) return Suitability.None;
                return solvedTypedef.Typedef == solvedTypedefarg.Typedef ? Suitability.Perfect : Suitability.None;
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

}