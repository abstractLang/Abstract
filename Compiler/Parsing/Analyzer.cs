using Abstract.Core.Language;

namespace Abstract.Parsing;

public static class Analyzer
{

    /// <summary>
    /// 
    /// Shallow analyze do a quick analysis of the tree.
    /// It should be used to check for basic syntax errors
    /// and to reduce the code into a simpler and linkable
    /// form. The result should be cached into disk for
    /// be used in next builds, if the source code is still
    /// valid for it.
    /// 
    /// Each tree sent to the shallow analyzer should include
    /// only one script per tree, allowing the result to be
    /// modular and easy to manipulate using incremental
    /// compilation.
    /// 
    /// </summary>
    /// <param name="tree"> The tree of the source file </param>
    public static void ShallowAnalyze(SyntaxTree tree)
    {



    }

}
