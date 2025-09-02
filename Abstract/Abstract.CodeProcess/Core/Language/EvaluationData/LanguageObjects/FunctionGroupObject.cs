using System.Text;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class FunctionGroupObject(string[] g): LangObject(g)
{
    private List<FunctionObject> _overloads = [];
    public FunctionObject[] Overloads => [.. _overloads];
    
    public void AddOverload(FunctionObject overload) => _overloads.Add(overload);
    
    public override string ToString()
    {
        var sb = new StringBuilder();
        
        sb.AppendLine($"FunctionGroup '{string.Join('.', Global)}' ");

        foreach (var i in Overloads)
        {
            var lines = i.ToString().Split(Environment.NewLine);
            foreach (var l in lines) sb.AppendLine($"\t{l}");
        }
        
        return sb.ToString();
    }
}
