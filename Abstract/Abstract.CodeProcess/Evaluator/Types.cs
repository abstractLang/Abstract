using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression.TypeModifiers;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;
using Abstract.Parser.Core.ProgData;
using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser;

public partial class Evaluator
{
    /*
    *   Type matching in abstract code is really complex! :p
    *   I will try to document it somehow and link the docs here
    *   Questions can be made directly to me anyway
    *   - lumi
    */

    public void ControlLevelMatchTypeReference()
    {
        foreach (var i in functions.Values)
        {
            foreach (var function in i)
            {
                var funcNode = function.functionNode;

                /*
                * Function's type references:
                * - Return type
                * - Parameter types
                *
                * Function parameters should be parsed first
                * as they can countain generic type that can be
                * used as the return type.
                */

                List<TypeReference> paramTypes = [];
                for (var j = 1; j < funcNode.ParameterCollection.Children.Length - 1; j++)
                {
                    var parameter = (TypedIdentifierNode)funcNode.ParameterCollection.Children[j];

                    var reference = GetTypeFromTypeExpressionNode(parameter.Type, function);
                    paramTypes.Add(reference);

                    if (reference is SolvedTypeReference @solved
                    && solved.structure.GlobalReference == "Std.Types.Type")
                    {
                        // is of type "type" (bruh)
                        function.AppendGeneric(parameter.Identifier.ToString(), j);
                    }
                }
                function.parameterTypes = [.. paramTypes];

                function.returnType = GetTypeFromTypeExpressionNode(funcNode.ReturnType, function);

            }
        }
    }

    // Utils
    public TypeReference GetTypeFromTypeExpressionNode(TypeExpressionNode type, ProgramMember? parent)
    {
        var a = TryGetTypeFromTypeExpressionNode(type, parent);
        typeMatchingLog.AppendLine($"{type} -> {a?.ToString("") ?? null}");
        return a!;
    }
    public TypeReference TryGetTypeFromTypeExpressionNode(TypeExpressionNode type, ProgramMember? parent)
    {
        // type keyword
        if (type.Children[0] is IdentifierNode @keyword)
        {
            var struc = SearchStructure(keyword.ToString(), parent);
            return new SolvedTypeReference(struc);
        }

        // identifier
        if (type.Children[0] is IdentifierCollectionNode @identifier)
        {
            var struc = SearchStructure(identifier.ToString(), parent);
            if (struc != null) return new SolvedTypeReference(struc);
            
            if (parent is Function @func)
            {
                var gen = func.SearchForGeneric(identifier.ToString());
                if (gen != null) return gen;
            }
        }
        
        // array modifier
        if (type.Children[0] is ArrayTypeModifierNode @array)
        {
            var child = GetTypeFromTypeExpressionNode(array.Type, parent);
            var arrayStruct = SearchStructure("Std.Types.Collections.Array");

            return new SolvedTypeReference(arrayStruct, child);
        }

        Console.WriteLine(type.Children[0].GetType().Name);
        return null!;
    }
    public Structure SearchStructure(MemberIdentifier name, ProgramMember? parent = null)
    {
        ProgramMember currNode;
        MemberIdentifier step;
        MemberIdentifier? next;
        
        // Search scope
        if (parent != null)
        {
            currNode = parent;
            (step, next) = name.Step;

            do {
                currNode = currNode.SearchForChild(step)!;

                if (next.HasValue) (step, next) = next.Value.Step;
                else break;
            }
            while (next.HasValue && currNode != null);
            
            if (currNode is Structure @found) return found;
        }

        // Search imports

        // search in libs
        else if (name.Step.step == program.identifier && name.Step.next.HasValue)
        {
            currNode = program;
            (step, next) = name.Step.next.Value.Step;

            do {
                currNode = currNode.SearchForChild(step)!;

                if (next.HasValue) (step, next) = next.Value.Step;
                else break;
            }
            while (currNode != null);
            
            if (currNode is Structure @found) return found;
        }

        // Search global
        var member = program.TryGetGlobal(name);
        return (member as Structure)!;
    }
}
