using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FieldReferences;

public class SolvedFieldReference(FieldObject field) : FieldReference
{
    public readonly FieldObject Field = field;

    public override string ToString() => $"Field<{string.Join('.', Field.Global)}>";
}
