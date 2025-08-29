namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class FunctionGroupObject(string[] g): LangObject(g)
{
    private List<FunctionObject> _overloads = [];
    public FunctionObject[] Overloads => [.. _overloads];
    
    public void AddOverload(FunctionObject overload) => _overloads.Add(overload);
}
