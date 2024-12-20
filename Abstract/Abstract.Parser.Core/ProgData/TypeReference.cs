namespace Abstract.Parser.Core.ProgData;

public abstract class TypeReference : IGenericExpression
{
    public virtual string ToString(string format) => ToString()!;
}