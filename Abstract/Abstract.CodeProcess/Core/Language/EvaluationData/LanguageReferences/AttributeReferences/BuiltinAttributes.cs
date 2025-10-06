namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.AttributeReferences;

public enum BuiltinAttributes
{
    _undefined = 0,
        
    // General
    Static,
    DefineGlobal,
    Align,
    ConstExp,
        
    // Protection and inheritance
    Public,
    Private,
    Internal,
    Final,
    Abstract,
    Interface,
    Virtual,
    Override,
    AllowAccessTo,
    DenyAccessTo,
    
    // Low level and linkage
    Extern,
    Export,
        
    // Optimization
    Inline,
    Noinline,
    Comptime,
    Runtime,
    CallConv,
        
    // Properties
    Getter,
    Setter,
        
    // Conversions and operators
    ExplicitConvert,
    ImplicitConvert,
    OverrideOperator,
    IndexerGetter,
    IndexerSetter,
}