using System.Text;
using Abstract.Build;
using Abstract.Parser.Core.Exceptions.Evaluation;
using Abstract.Parser.Core.Language.SyntaxNodes.Base;
using Abstract.Parser.Core.Language.SyntaxNodes.Control;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;
using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser;

public partial class Evaluator(ErrorHandler errHandler)
{

    private readonly ErrorHandler _errHandler = errHandler;

    private ProgramRoot program = null!;

    // temporary debug shit
    private StringBuilder typeMatchingLog = new("## Type Matching operations log #\n\n");

    public ProgramRoot EvaluateProgram(ProgramNode programNode)
    {
        program = new(new("Program"));


        foreach (var i in programNode.Children)
        {
            if (i is ProjectNode @project)
            {
                var proj = new Project(program, project.projectName);
                SearchMembersRecursive(proj, [.. project.Children]);
                program.AppendProject(proj);
            }
        }

        // Evaluation process starts here

        // Registring references and root dependences
        //RegisterAttributes();
        RegisterLevel_1_ExtraReferences();
        ControlLevelMatchTypeReference();
        RegisterLevel_2_ExtraReferences();

        // Evaluating code execution
        ScanCodeBlocks();

        // Evaluation process ends here

        // Debuggin shits here

        var refTableStr = new StringBuilder();
        refTableStr.AppendLine("## Reference Tables ##\n");

        refTableStr.AppendLine($"# Namespaces ({program.namespaces.Count}):");
        foreach (var i in program.namespaces.Keys) refTableStr.AppendLine($"\t- {i}");
        refTableStr.AppendLine($"\n# Functions ({program.functions.Count}):");
        foreach (var i in program.functions)
        {
            var concreteCount = 0;
            var genericCount = 0;

            foreach (var j in i.Value.Overloads)
                _ = j.IsGeneric ? genericCount++ : concreteCount++;

            refTableStr.AppendLine($"\t- {i.Key} ({concreteCount};{genericCount})");
        }
        refTableStr.AppendLine($"\n# Types ({program.types.Count}):");
        foreach (var i in program.types.Keys) refTableStr.AppendLine($"\t- {i}");
        refTableStr.AppendLine($"\n# Fields ({program.fields.Count}):");
        foreach (var i in program.fields.Keys) refTableStr.AppendLine($"\t- {i}");
        refTableStr.AppendLine($"\n# Enums ({program.enums.Count}):");
        foreach (var i in program.enums.Keys) refTableStr.AppendLine($"\t- {i}");
        refTableStr.AppendLine($"\n# Attributes ({program.attributes.Count}):");
        foreach (var i in program.attributes.Keys) refTableStr.AppendLine($"\t- {i}");

        var progStructStr = new StringBuilder();
        progStructStr.AppendLine("## Program Structure ##\n");
        progStructStr.Append(BuildProgramStructureTree(program));

        if (programNode.debugOutDirectory != null) File.WriteAllText($"{programNode.debugOutDirectory}/reftable.txt", refTableStr.ToString());
        if (programNode.debugOutDirectory != null) File.WriteAllText($"{programNode.debugOutDirectory}/prgstruc.txt", progStructStr.ToString());
        if (programNode.debugOutDirectory != null) File.WriteAllText($"{programNode.debugOutDirectory}/tymatlog.txt", typeMatchingLog.ToString());

        var proot = program;
        program = null!;
        return proot;
    }

    private void SearchMembersRecursive(ProgramMember parentMember, SyntaxNode[] children)
    {
        List<MemberAttribute> attributes = [];

        foreach (var child in children)
        {
            if (child is ProjectNode @project)
            {
                SearchMembersRecursive(parentMember, [.. project.Children]);
            }
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
        
            else if (child is ImportFromNode @import)
            {
                // TODO handle import
            }

            else throw new Exception();
        }

        while (attributes.Count > 0)
        {
            try
            {
                var attrib = attributes[0].node;
                attributes.RemoveAt(0);
                throw new AttributeWithNoMemberException(attrib);
            }
            catch (AttributeWithNoMemberException e) { _errHandler.RegisterError(e); }

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

        program.namespaces.Add(namespaceReference.GlobalReference, namespaceReference);

        parent?.AppendChild(namespaceReference);
        return namespaceReference;
    }
    private ProgramMember RegisterFunction(FunctionDeclarationNode functionNode, ProgramMember? parent)
    {
        var identifier = functionNode.Identifier.Value;

        var functionReference = new Function(parent, new(identifier), functionNode);
        var r = functionReference.GlobalReference;
        FunctionGroup group;

        if (!program.functions.TryGetValue(r, out group!))
        {
            group = new(parent, identifier);
            program.functions.Add(r, group);
            parent?.AppendChild(group);
        }
        group.AddOverload(functionReference);

        return functionReference;
    }
    private ProgramMember RegisterStructure(StructureDeclarationNode structureNode, ProgramMember? parent)
    {
        var identifier = structureNode.Identifier.Value;

        var structReference = new Structure(structureNode, parent, new(identifier));

        program.types.Add(structReference.GlobalReference, structReference);

        parent?.AppendChild(structReference);
        return structReference;
    }
    private ProgramMember RegisterField(TopLevelVariableNode fieldNode, ProgramMember? parent)
    {
        var identifier = fieldNode.Identifier.Value;

        var fieldReference = new Field(parent, new(identifier));

        program.fields.Add(fieldReference.GlobalReference, fieldReference);

        parent?.AppendChild(fieldReference);
        return fieldReference;
    }
    private ProgramMember RegisterEnum(EnumDeclarationNode enumNode, ProgramMember? parent)
    {
        var identifier = enumNode.Identifier.Value;

        var enumReference = new Enumerator(parent, new(identifier));

        program.enums.Add(enumReference.GlobalReference, enumReference);

        parent?.AppendChild(enumReference);
        return enumReference;
    }
    #endregion

    private string BuildProgramStructureTree(ProgramRoot root)
    {
        var str = new StringBuilder();

        str.AppendLine(root.ToString());
        str.AppendLine("{");

        foreach (var i in root.projects.Values)
        {
            var lines = BuildProgramStructureTree(i)
            .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
            str.AppendLine($"\t{lines[0]}");
            foreach (var line in lines[1..]) str.AppendLine($"\t{line}");
        }

        str.AppendLine("}");

        return str.ToString();
    }
    private string BuildProgramStructureTree(ProgramMember member)
    {
        var str = new StringBuilder();

        str.AppendLine(member.ToString());
        if ((member is not FunctionGroup)
        && (member is not Field)
        && (member is not Enumerator))
        {
            str.AppendLine("{");

            if (member is Structure @struc)
            {
                if (struc.AvaliableOperators.Length > 0)
                    str.AppendLine($"\t#OPRS {string.Join(" ", struc.AvaliableOperators)}");
            }

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
