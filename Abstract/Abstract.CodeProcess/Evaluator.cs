using System.Diagnostics.CodeAnalysis;
using Abstract.Build;
using Abstract.Parser.Core.Language.SyntaxNodes.Base;
using Abstract.Parser.Core.Language.SyntaxNodes.Control;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;
using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser;

public class Evaluator(ErrorHandler errHandler)
{

    private readonly ErrorHandler _errHandler = errHandler;

    public void EvaluateProgram(ProgramNode programRoot)
    {
        Console.WriteLine("Starting evaluation process...");

        program = new(new("Std"));
        SearchMembersRecursive(program, [.. programRoot.Children]);

        Console.WriteLine("Dumping reference tables:\n");
        Console.WriteLine("# Namespaces:");
        Console.WriteLine(string.Join(";\n", namespaces.Keys));
        Console.WriteLine("\n# Functions:");
        Console.WriteLine(string.Join(";\n", functions.Keys));
        Console.WriteLine("\n# Types:");
        Console.WriteLine(string.Join(";\n", types.Keys));
    }

    #region Reference tables
    private ProgramRoot program = null!;
    private Dictionary<MemberIdentifier, Namespace> namespaces = [];
    private Dictionary<MemberIdentifier, List<Function>> functions = [];
    private Dictionary<MemberIdentifier, Structure> types = [];
    #endregion

    private void SearchMembersRecursive(ProgramMember parentMember, SyntaxNode[] children)
    {
        foreach (var child in children)
        {
            if (child is ScriptReferenceNode @script)
            {
                SearchMembersRecursive(parentMember, [.. script.Children]);
            }

            else if (child is NamespaceNode @nmespace)
            {
                var registred = RegisterNamespace(nmespace, parentMember);
                SearchMembersRecursive(registred, [.. nmespace.Body.Content]);
            }
            else if (child is StructureDeclarationNode @structure)
            {
                var registred = RegisterStructure(structure, parentMember);
                SearchMembersRecursive(registred, [.. structure.Body.Content]);
            }
            else if (child is FunctionDeclarationNode @func)
            {
                RegisterFunction(func, parentMember);
            }
        }
    }

    #region Register methods
    /*
    *   These methods should evaluate members declaration and
    *   save it reference in the reference tables
    */
    private ProgramMember RegisterNamespace(NamespaceNode namespaceNode, ProgramMember? parent)
    {
        var identifierNode = namespaceNode.Identifier;
        string[] identifierTokens = [.. identifierNode.Children.Select(e => ((IdentifierNode)e).Value)];

        var namespaceReference = new Namespace(parent, new(identifierTokens));

        namespaces.Add(namespaceReference.GlobalReference, namespaceReference);

        return namespaceReference;
    }
    private ProgramMember RegisterFunction(FunctionDeclarationNode functionNode, ProgramMember? parent)
    {
        var identifier = functionNode.Identifier.Value;

        var functionReference = new Function(parent, new(identifier));

        if (functions.TryGetValue(functionReference.GlobalReference, out var funcList))
            funcList.Add(functionReference);
        else functions.Add(functionReference.GlobalReference, [functionReference]);

        return functionReference;
    }
    private ProgramMember RegisterStructure(StructureDeclarationNode structureNode, ProgramMember? parent)
    {
        var identifier = structureNode.Identifier.Value;

        var structReference = new Structure(parent, new(identifier));

        types.Add(structReference.GlobalReference, structReference);

        return structReference;
    }
    #endregion

}
 