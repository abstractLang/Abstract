using System.Text;
using Abstract.Build;
using Abstract.Parser.Core.Language.SyntaxNodes.Base;
using Abstract.Parser.Core.Language.SyntaxNodes.Control;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;
using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser;

public partial class Evaluator(ErrorHandler errHandler)
{

    private readonly ErrorHandler _errHandler = errHandler;

    // temporary debug shit
    private StringBuilder typeMatchingLog = new("## Type Matching operations log: #\n");

    public void EvaluateProgram(ProgramNode programRoot)
    {
        Console.WriteLine("Starting evaluation process...");


        program = new(new("Std"));
        SearchMembersRecursive(program, [.. programRoot.Children]);

        // Evaluation process starts here

        RegisterExtraReferences();
        ControlLevelMatchTypeReference();

        // Evaluation process ends here


        // Debuggin shits here

        var refTableStr = new StringBuilder();
        refTableStr.AppendLine("## Reference Tables ##");
        refTableStr.AppendLine("# Namespaces:");
        foreach (var i in namespaces.Keys) refTableStr.AppendLine($"\t- {i}");
        refTableStr.AppendLine("\n# Functions:");
        foreach (var i in functions.Keys) refTableStr.AppendLine($"\t- {i}");
        refTableStr.AppendLine("\n# Types:");
        foreach (var i in types.Keys) refTableStr.AppendLine($"\t- {i}");
        refTableStr.AppendLine("\n# Fields:");
        foreach (var i in fields.Keys) refTableStr.AppendLine($"\t- {i}");
        refTableStr.AppendLine("\n# Enums:");
        foreach (var i in enums.Keys) refTableStr.AppendLine($"\t- {i}");
        refTableStr.AppendLine("\n# Attributes:");
        //foreach (var i in []) refTableStr.AppendLine($"\t- {i}");

        var progStructStr = new StringBuilder();
        progStructStr.AppendLine("## Program Structure ##");
        progStructStr.Append(BuildProgramStructureTree(program));

        File.WriteAllText($"{programRoot.outDirectory}/reftable.txt", refTableStr.ToString());
        File.WriteAllText($"{programRoot.outDirectory}/prgstruc.txt", progStructStr.ToString());
        File.WriteAllText($"{programRoot.outDirectory}/tymatlog.txt", typeMatchingLog.ToString());
    }

    private void SearchMembersRecursive(ProgramMember parentMember, SyntaxNode[] children)
    {
        List<MemberAttribute> attributes = [];

        foreach (var child in children)
        {
            if (child is ScriptReferenceNode @script)
            {
                SearchMembersRecursive(parentMember, [.. script.Children]);
            }

            else if (child is AttributeNode @attribute)
            {
                attributes.Add(new(attribute));
            }

            else if (child is NamespaceNode @nmespace)
            {
                var registred = RegisterNamespace(nmespace, parentMember);
                registred.AppendAttribute([.. attributes]);
                attributes.Clear();
                SearchMembersRecursive(registred, [.. nmespace.Body.Content]);
            }
            else if (child is StructureDeclarationNode @structure)
            {
                var registred = RegisterStructure(structure, parentMember);
                registred.AppendAttribute([.. attributes]);
                attributes.Clear();
                SearchMembersRecursive(registred, [.. structure.Body.Content]);
            }
            else if (child is FunctionDeclarationNode @func)
            {
                var registred = RegisterFunction(func, parentMember);
                registred.AppendAttribute([.. attributes]);
                attributes.Clear();
            }
            else if (child is TopLevelVariableNode @field)
            {
                var registred = RegisterField(field, parentMember);
                registred.AppendAttribute([.. attributes]);
                attributes.Clear();
            }
            else if (child is EnumDeclarationNode @enumer)
            {
                var registred = RegisterEnum(enumer, parentMember);
                registred.AppendAttribute([.. attributes]);
                attributes.Clear();
            }
        }

        if (attributes.Count > 0)
            throw new Exception("TODO");
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

        parent?.AppendChild(namespaceReference);
        return namespaceReference;
    }
    private ProgramMember RegisterFunction(FunctionDeclarationNode functionNode, ProgramMember? parent)
    {
        var identifier = functionNode.Identifier.Value;

        var functionReference = new Function(parent, new(identifier), functionNode);

        if (functions.TryGetValue(functionReference.GlobalReference, out var funcList))
            funcList.Add(functionReference);
        else functions.Add(functionReference.GlobalReference, [functionReference]);

        parent?.AppendChild(functionReference);
        return functionReference;
    }
    private ProgramMember RegisterStructure(StructureDeclarationNode structureNode, ProgramMember? parent)
    {
        var identifier = structureNode.Identifier.Value;

        var structReference = new Structure(parent, new(identifier));

        types.Add(structReference.GlobalReference, structReference);

        parent?.AppendChild(structReference);
        return structReference;
    }
    private ProgramMember RegisterField(TopLevelVariableNode fieldNode, ProgramMember? parent)
    {
        var identifier = fieldNode.Identifier.Value;

        var fieldReference = new Field(parent, new(identifier));

        fields.Add(fieldReference.GlobalReference, fieldReference);

        parent?.AppendChild(fieldReference);
        return fieldReference;
    }
    private ProgramMember RegisterEnum(EnumDeclarationNode enumNode, ProgramMember? parent)
    {
        var identifier = enumNode.Identifier.Value;

        var enumReference = new Enumerator(parent, new(identifier));

        enums.Add(enumReference.GlobalReference, enumReference);

        parent?.AppendChild(enumReference);
        return enumReference;
    }
    #endregion


    private string BuildProgramStructureTree(ProgramMember member)
    {
        var str = new StringBuilder();

        str.AppendLine(member.ToString());
        if ((member is not Function) && (member is not Field) && (member is not Enumerator))
        {
            str.AppendLine("{");

            foreach (var i in member.ChildrenNamespaces)
            {
                var lines = BuildProgramStructureTree(i).Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
                str.AppendLine($"\t#NMSP {lines[0]}");
                foreach (var line in lines[1..]) str.AppendLine($"\t{line}");
            }

            foreach (var i in member.ChildrenTypes)
            {
                var lines = BuildProgramStructureTree(i).Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
                str.AppendLine($"\t#TYPE {lines[0]}");
                foreach (var line in lines[1..]) str.AppendLine($"\t{line}");
            }

            foreach (var i in member.ChildrenFields)
                str.AppendLine($"\t#FILD {BuildProgramStructureTree(i)}");

            foreach (var i in member.ChildrenFunctions)
                str.AppendLine($"\t#FUNC {BuildProgramStructureTree(i)}");

            foreach (var i in member.ChildrenEnums)
                str.AppendLine($"\t#ENUM {BuildProgramStructureTree(i)}");

            str.AppendLine("}");
        }

        return str.ToString();
    }
}
