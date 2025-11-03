using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

public class AccessNode : ExpressionNode
{
    public ExpressionNode Left => (ExpressionNode)_children[0];
    public ExpressionNode Right => (ExpressionNode)_children[2];


    public bool Incomplete => Left is ImplicitAccessNode;
    public string[] StringValues
    {
        get
        {
            List<string> values = [];
            
            switch (Left)
            {
                case ImplicitAccessNode: values.Add(null!); break;
                case IdentifierNode @i: values.Add(i.Value); break;
                case AccessNode @a: values.AddRange(a.StringValues); break;
                default: values.Add(null!); break;
            }
            switch (Right)
            {
                case IdentifierNode @i: values.Add(i.Value); break;
                default: values.Add(null!); break;
            }
            
            return [..values];
        }
    }
    public ExpressionNode[] Values
    {
        get
        {
            List<ExpressionNode> values = [];
            
            switch (Left)
            {
                case AccessNode @a: values.AddRange(a.Values); break;
                default: values.Add(Left); break;
            }
            values.Add(Right);
            
            return [..values];
        }
    }

    public override string ToString() => $"{Left}.{Right}";
}