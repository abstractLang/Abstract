namespace Abstract.Parser.Core.ProgData.FunctionExecution;

public class CSharpCallable(Delegate target) : ICallable
{
    public readonly Delegate target = target;

    public void Call(params object?[] args)
    {
        throw new NotImplementedException();
    }
}
