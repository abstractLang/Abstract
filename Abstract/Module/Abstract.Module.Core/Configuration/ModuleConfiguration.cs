using System.ComponentModel.DataAnnotations;
using Abstract.Realizer.Core.Configuration.LangOutput;

namespace Abstract.Module.Core.Configuration;

public struct ModuleConfiguration
{
    [Required] public string Name;
    [Required] public string Description;
    [Required] public string Author;
    [Required] public string Version;

    [Required] public ModuleLanguageTargetConfiguration[] Targets;
}


public struct ModuleLanguageTargetConfiguration
{
    [Required] public string TargetName;
    [Required] public string TargetDescription;
    [Required] public string TargetIdentifier;
    [Required] public ILanguageOutputConfiguration LanguageOutput;
}
