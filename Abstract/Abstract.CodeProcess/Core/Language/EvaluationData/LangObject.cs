using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData;

public abstract class LangObject(string[] global)
{
    public readonly string[] Global = global;
    private readonly List<AttributeNode> _attributes = [];

    public void AppendAttributes(params AttributeNode[] attrs) => _attributes.AddRange(attrs);
}
