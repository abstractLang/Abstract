using System.ComponentModel.DataAnnotations;
using Abstract.Realizer.Builder;
using Abstract.Realizer.Core.Configuration.LangOutput;

namespace Abstract.Module.Core.Configuration;

public struct ModuleConfiguration
{
    public string Name;
    public string Description;
    public string Author;
    public string Version;

    public ModuleLanguageTargetConfiguration[] Targets;
}


public struct ModuleLanguageTargetConfiguration
{
    public delegate void CompilerDelegate(ProgramBuilder program);
    
    public string TargetName;
    public string TargetDescription;
    public string TargetIdentifier;
    
    public ILanguageOutputConfiguration LanguageOutput;
    public CompilerDelegate CompilerInvoke;
}
