using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FieldReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FunctionReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;

namespace Abstract.CodeProcess;

public partial class Analyzer
{
    
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

    private static bool IsSolved(TypeReference typeref) =>
        typeref switch
        {
            UnsolvedTypeReference @unsolv => false,
            SliceTypeReference @slice => IsSolved(slice.InternalType),
            ReferenceTypeReference @refe => IsSolved(refe.InternalType),
            _ => true
        };
    
    private IEnumerable<FunctionObject> EnumerateFunctions(IEnumerable<LangObject> list)
    {
        foreach (var i in list)
        {
            switch (i)
            {
                case FunctionObject f: yield return f; break;
                case FunctionGroupObject g: foreach (var inner in g.Overloads) yield return inner; break;
            }
        }
    }

}