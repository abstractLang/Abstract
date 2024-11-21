namespace Abstract.Parser.Core.Language.AbstractSyntaxTree.CodeInstructions;

// General statements
public class WhileStatementNode : StatementNode
{
    public ExpressionNode condition = null!;
    public SyntaxNode statement = null!;

    public override string ToString() => $"while {condition} => {statement}";
}

public class ForStatementNode : StatementNode
{
    public ValueExpressionNode Symbol { get; set; } = null!;
    public RangeLiteralValueNode ConditionRange { get; set; } = null!;
    public SyntaxNode statement = null!;

    public override string ToString() => $"for {Symbol} in {ConditionRange} => {statement}";
}

public class IfStatementNode : InstructionalNode
{
    public ExpressionNode condition = null!;
    public SyntaxNode statement = null!;

    public override string ToString() => $"if {condition} => {statement}";
}
public class ElifStatementNode : InstructionalNode
{
    public ExpressionNode condition = null!;
    public SyntaxNode statement = null!;

    public override string ToString() => $"elif {condition} => {statement}";
}
public class ElseStatementNode : InstructionalNode
{
    public SyntaxNode statement = null!;

    public override string ToString() => $"else => {statement}";
}

public class ReturnInstructionNode : InstructionalNode
{
    public ExpressionNode? expression = null;

    public override string ToString() => "return" + (expression != null ? $" {expression}" : "");
}

// Operation expressions
public class AssignmentExpressionNode(ExpressionNode l, string op, ExpressionNode r) : InstructionalNode
{
    public ExpressionNode Left { get; private set; } = l;
    public ExpressionNode Right { get; private set; } = r;
    public string Operator { get; private set; } = op;

    public override string ToString() => $"{Left} {Operator} {Right}";
}
public class BinaryOperationExpressionNode(ExpressionNode l, string op, ExpressionNode r) : ExpressionNode
{
    public ExpressionNode Left { get; private set; } = l;
    public ExpressionNode Right { get; private set; } = r;
    public string Op { get; private set; } = op;

    public override string ToString() => $"({Left} {Op} {Right})";
}
public class UnaryOperationExpressionNode(string op, ExpressionNode ex) : ExpressionNode
{
    public ExpressionNode Expression { get; private set; } = ex;
    public string Op { get; private set; } = op;

    public override string ToString() => $"{Op}{Expression}";
}

