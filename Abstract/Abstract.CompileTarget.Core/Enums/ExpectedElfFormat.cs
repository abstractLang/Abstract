namespace Abstract.CompileTarget.Core.Enums;

public enum ExpectedELFFormat {

    /// <summary>
    /// One ELF object per source file
    /// </summary>
    //onePerFile,
    /// <summary>
    /// One ELF object per project
    /// </summary>
    onePerProject,
    /// <summary>
    /// One ELF object for the entire program
    /// </summary>
    OnePerProgram

}
