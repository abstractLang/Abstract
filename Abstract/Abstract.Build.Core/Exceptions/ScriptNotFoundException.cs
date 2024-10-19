namespace Abstract.Build.Core.Exceptions;

public class ScriptNotFoundException(string path) : BuildException ($"File on path \"{path}\" do not exist!")
{
}
