using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.ProgData.FunctionExecution;

public class AbstractCallable(Function target) : ICallable
{
    public readonly Function target = target;

    public void Call(params object?[] args)
    {
        throw new NotImplementedException();
    }


    public static implicit operator AbstractCallable(Function target) => new(target);
}
