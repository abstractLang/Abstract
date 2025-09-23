using Abstract.Module.Core.Configuration;

namespace Abstract.Module.Core;

public interface IModule
{

    public ModuleConfiguration Config { get; }
    
}
