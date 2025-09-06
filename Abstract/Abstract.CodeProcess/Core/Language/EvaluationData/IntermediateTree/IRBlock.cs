using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Statements;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree;

public class IRBlock(SyntaxNode origin): IRNode(origin)
{
    public readonly List<LocalVariableObject> Locals = [];
    public readonly List<IRNode> Content = [];

    public override string ToString()
    {
        var sb = new StringBuilder();

        foreach (var l in Locals)
            sb.AppendLine($"{l}");
        if (Locals.Count > 0) sb.AppendLine();
        
        foreach (var i in Content)
            sb.AppendLine(i.ToString());
        
        return sb.ToString();
    }
}
