using Abstract.Parser.Core.Language.AbstractSyntaxTree;

namespace Abstract.Parser.Core.Exeptions.Evaluation;

public class NoFuncOverloadException (string name, int expected, int received, SyntaxNode node) : EvaluationException(node)
{

    private readonly SyntaxNode _node = node;
    private readonly string _name = name;
    private readonly int _expected = expected;
    private readonly int _received = received;

    public override string Message => $"Function {_name} expects {_expected} arguments. Found {_received}";

}
