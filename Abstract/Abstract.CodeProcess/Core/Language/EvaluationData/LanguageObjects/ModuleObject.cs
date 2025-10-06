using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class ModuleObject(string n) : LangObject([n], n), IStaticModifier {
    
    public bool Static { get => true; set {} }

    public override string ToString()
    {
        var sb = new StringBuilder();
        sb.AppendLine($"Module '{Name}':");
        
        foreach (var c in Children)
        {
            var lines = c.ToString()!.Split(Environment.NewLine);
            foreach (var l in lines) sb.AppendLine($"\t{l}");
        }
        
        return sb.ToString();
    }
}