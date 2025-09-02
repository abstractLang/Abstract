using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class StructObject(string[] g, StructureDeclarationNode synnode)
    : LangObject(g),
        IPublicModifier,
        IStaticModifier,
        IInternalModifier,
        IAbstractModifier
{
    bool IPublicModifier.Public { get; set; } = false;
    bool IStaticModifier.Static { get; set; } = false;
    bool IInternalModifier.Internal { get; set; } = false;
    bool IAbstractModifier.Abstract { get; set; } = false;
    
    public bool Interface { get; set; } =  false;
    public bool Final { get; set; } =  false;
    
    public readonly StructureDeclarationNode syntaxNode = synnode;
}
