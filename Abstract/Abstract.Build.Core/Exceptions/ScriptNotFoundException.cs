namespace Abstract.Build.Core.Exceptions;

public class ScriptNotFoundException(string path) : BuildException ($"File in path \"{path}\" does not exist!")
{
}
