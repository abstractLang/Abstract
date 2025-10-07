using System.Diagnostics;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
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
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression.TypeModifiers;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess;

public partial class Analyzer
{

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
                    }

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
            TypedefObject @t => new SolvedTypedefTypeReference(t),
            
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
                    // FIXME it should in reality return a FunctionTypeReference,
                    // not the function's return type
                    SolvedFunctionReference @func => func.Function.ReturnType,

                    LocalReference @local => local.Local.Type,
                    ParameterReference @param => param.Parameter.Type,

                    _ => throw new NotImplementedException()
                };
            case IRReferenceAccess @access: return GetEffectiveTypeReference(access.B);


            case IRInvoke @invoke:
                // FIXME it should in reality return a FunctionTypeReference,
                // not the function's return type
                return GetEffectiveTypeReference(invoke.Target);

            case IRBinaryExp @exp:
                return exp.ResultType ?? throw new UnreachableException(
                    "This function should not be called when this value is null");
            case IRUnaryExp @unexp:
                return unexp.Prefix != IRUnaryExp.UnaryPrefix.Reference
                    ? GetEffectiveTypeReference(unexp.Value)
                    : new ReferenceTypeReference(GetEffectiveTypeReference(@unexp.Value));
                    

            case IrConv @conv: return conv.TargetType;
            case IRIntCast @tcast: return tcast.TargetType;
            case IRIntExtend @icast: return icast.toType;
            case IRIntTrunc @itrunc: return itrunc.toType;

            default: throw new NotImplementedException();
        }
    }

    private static bool IsSolved(TypeReference typeref) =>
        typeref switch
        {
            UnsolvedTypeReference @unsolv => false,
            SliceTypeReference @slice => IsSolved(slice.InternalType),
            ReferenceTypeReference @refe => IsSolved(refe.InternalType),
            _ => true
        };
}