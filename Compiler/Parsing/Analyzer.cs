using System.Text;
using Abstract.Binutils.ELF;
using Abstract.Binutils.ELF.ProgramNodes;
using Abstract.Core.Language;
using Directory = Abstract.Binutils.ELF.ProgramNodes.Directory;

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

        var programBlock = new ElfProgram();

        ShallowAnalyzeRoot(tree.root, programBlock.Root);

    }

    public static void ShallowAnalyzeRoot(SyntaxNode parentNode, Directory parent)
    {
        foreach (var node in parentNode.Children)
        {
            if (node.Kind == NodeKind.FromImport)
            {
                var importNode = (Directory)parent.Branch("IMPORT", NodeTypes.Directory);
                var importFrom = (Content)importNode.Branch("FROM", NodeTypes.Content);
                importFrom.Stream.Write(Encoding.ASCII.GetBytes(((SyntaxNode)node).Children[2].ToString()));
            }
            else if (node.Kind == NodeKind.FunctionDeclaration)
            {
                var funcNode = (Directory)parent.Branch("FUNC", NodeTypes.Directory);
            }
            else if (node.Kind == NodeKind.StructureDeclaration)
            {
                var structNode = (Directory)parent.Branch("STRUCT", NodeTypes.Directory);
            }
            else if (node.Kind == NodeKind.EnumDeclaration)
            {

            }

            //else throw new Exception();
        }
    }
}
