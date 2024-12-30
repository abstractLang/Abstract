namespace Abstract.Parser.Core.ProgData.FunctionExecution;

public interface ICallable
{
    public void Call(params object?[] args);
}
