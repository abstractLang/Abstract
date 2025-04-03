using System.Text;
using Abstract.Binutils.ELF;
using Abstract.Binutils.ELF.ElfBuilder;
using Abstract.Binutils.ELF.ElfBuilder.ProgramNodes;
using Abstract.Build;
using Abstract.Core.Language;
using Directory = Abstract.Binutils.ELF.ElfBuilder.ProgramNodes.Directory;

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
    public static ELFProgram ShallowAnalyze(BuildContext ctx, ulong hash, SyntaxTree tree)
    {
        var programBlock = new ElfProgramBuilder();
        var data = new ModuleData(programBlock);

        ShallowAnalyzeRoot(data, tree.root, programBlock.Module);

        var bakedProgram = programBlock.Bake();

        File.WriteAllText(Path.Combine(ctx.cacheDir, "debug", $"{hash:X16}-elf.txt"), bakedProgram.ToString());

        var sb = new StringBuilder();
        foreach (var item in data.referenceTable) sb.AppendLine($"{item.Key.PadRight(15)} {item.Value.kind}");
        File.WriteAllText(Path.Combine(ctx.cacheDir, "debug", $"{hash:X16}-reftable.txt"), sb.ToString());

        return bakedProgram;
    }

    private static void ShallowAnalyzeRoot(ModuleData d, SyntaxNode parentNode, Directory parent)
    {
        List<SyntaxNode> attributes = [];

        foreach (var node in parentNode.Children)
        {
            if (node.Kind == NodeKind.FromImport)
            {
                var importNode = (Directory)parent.Branch("IMPORT", NodeTypes.Directory);
                var importFrom = (Content)importNode.Branch("FROM", NodeTypes.Content);
                importFrom.Stream.WriteString_ASCII(GetIdentifier( (SyntaxNode)(((SyntaxNode)node).Children[1]) ));
            }
            else if (node.Kind == NodeKind.Attribute)
            {
                attributes.Add((SyntaxNode)node);
            }


            else if (node.Kind == NodeKind.FunctionDeclaration)
            {
                var funcNode = (Directory)parent.Branch("FUNC", NodeTypes.Directory);
                var identifier = GetIdentifier((SyntaxNode)(((SyntaxNode)node).Children[1]));

                var name = (Content)funcNode.Branch("NAME", NodeTypes.Content);
                name.Stream.WriteString_ASCII(identifier);

                AppendAttributes(d, funcNode, attributes);
                RegisterReference(d, identifier, funcNode, ReferenceKind.function);
            }

            else if (node.Kind == NodeKind.StructureDeclaration)
            {
                var structNode = (Directory)parent.Branch("STRUCT", NodeTypes.Directory);
                var identifier = GetIdentifier((SyntaxNode)(((SyntaxNode)node).Children[1]));

                var name = (Content)structNode.Branch("NAME", NodeTypes.Content);
                name.Stream.WriteString_ASCII(identifier);

                AppendAttributes(d, structNode, attributes);
                RegisterReference(d, identifier, structNode, ReferenceKind.structure);
            }

            else if (node.Kind == NodeKind.EnumDeclaration)
            {
                //AppendAttributes(d, structNode, attributes);
            }

            //else throw new Exception();
        }

        // TODO check here if there's any orphan attribute
    }



    /// <summary>
    /// Handles the process of serializing the attribute into the ELF structure
    /// Clears the list after use it
    /// </summary>
    /// <param name="member"> The targeted program member directory </param>
    /// <param name="attributesList"> The list of attributes </param>
    private static void AppendAttributes(ModuleData d, Directory member, List<SyntaxNode> attributesList)
    {
        foreach (var i in attributesList)
        {
            // If the attribute doesn't has arguments, generate only ATTRREF pointer
            // if it has, generate the entire ATTRB directory

            if (i.Children.Length == 2)
            {
                var identifier = GetIdentifier((SyntaxNode)i.Children[1]);

                Node reference;
                if (TryGetReference(d, identifier, out var o)) reference = o.node;
                else reference = RegisterExternalReference(d, identifier, ReferenceKind.external);

                var attributePtr = (Pointer)member.Branch("ATTRREF", NodeTypes.Pointer);
                attributePtr.PointsTo = reference;
            }
            else
            {
                // TODO
                var attributeNode = (Directory)member.Branch("ATTRB", NodeTypes.Directory);
                var attributePtr = (Pointer)attributeNode.Branch("ATTRREF", NodeTypes.Pointer);
            }
        }

        attributesList.Clear();
    }
    private static string GetIdentifier(SyntaxNode identifierNode)
    {
        if (identifierNode.Kind != NodeKind.Identifier) throw new Exception();

        var sb = new StringBuilder();
        foreach (var node in identifierNode.Children)
        {
            var tkn = (TokenNode)node;
            sb.Append(tkn.Value);
        }
        return sb.ToString();
    }


    private static void RegisterReference(ModuleData d, string identifier, Node node, ReferenceKind refkind)
    {
        d.referenceTable.Add(identifier, new() {
            identifier = identifier,
            node = node,
            kind = refkind,
            dir = (Directory)node
        });
    }
    private static Node RegisterExternalReference(ModuleData d, string identifier, ReferenceKind refkind)
    {
        var newExRef = (Directory)d.dependences.Branch("MEMBER", NodeTypes.Directory);
        var nameLump = (Content)newExRef.Branch("SYMBOL", NodeTypes.Content);

        nameLump.Stream.WriteString_ASCII(identifier);

        d.referenceTable.Add(identifier, new()
        {
            identifier = identifier,
            node = newExRef,
            kind = refkind,
        });
        return newExRef;
    }
    
    private static bool TryGetReference(ModuleData d, string identifier, out ProgramMemberReference o)
    {
        if (d.referenceTable.TryGetValue(identifier, out var v))
        {
            o = v;
            return true;
            
        }

        o = null!;
        return false;
    }


    private class ModuleData(ElfProgramBuilder program)
    {
        public ElfProgramBuilder program = program;
        public Directory module => program.Module;
        public Directory dependences => program.Dependences;


        public Dictionary<string, ProgramMemberReference> referenceTable = [];
    }
    private class ProgramMemberReference
    {
        public string identifier;
        public Node node;
        public ReferenceKind kind;
        public Directory dir;
    }
    public enum ReferenceKind
    {
        unknown = 0,

        function,
        structure,
        enumerator,

        external,
    }
}
