using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.ProgData.DataReference;

public class FieldDataRef(Field field) : DataRef
{

    public readonly Field field = field;

    public override TypeReference refferToType => field.dataType;

    public override string ToString() => $"s({field})";
}
