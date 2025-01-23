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
        // Structures scan
        foreach (var i in program.types.Values)
        {
            /*
            * Function's type references:
            * - Generic parametes
            */

            var structureNode = i.structureNode;
            if (!structureNode.HasGenericArguments) continue;

            List<TypeReference> paramTypes = [];
            for (var j = 0; j < structureNode.ParameterCollection!.Items.Length; j++)
            {
                var parameter = structureNode.ParameterCollection!.Items[j];

                var reference = GetTypeFromTypeExpressionNode(parameter.Type, i);
                paramTypes.Add(reference);

                if (reference is SolvedTypeReference @solved
                && solved.structure.GlobalReference == "Std.Types.Type")
                {
                    // is of type "type" (bruh)
                    i.AppendGeneric(parameter.Identifier.ToString(), j);
                }
            }
        }

        // Functions scan
        foreach (var i in program.functions.Values)
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

                List<(TypeReference, MemberIdentifier)> parameters = [];
                for (var j = 0; j < funcNode.ParameterCollection.Items.Length; j++)
                {
                    var parameter = funcNode.ParameterCollection.Items[j];

                    var reference = GetTypeFromTypeExpressionNode(parameter.Type, function);
                    parameters.Add((reference, parameter.Identifier.ToString()));

                    if (reference is SolvedTypeReference @solved
                    && solved.structure.GlobalReference == "Std.Types.Type")
                    {
                        // is of type "type" (bruh)
                        function.AppendGeneric(parameter.Identifier.ToString(), j);
                    }
                }
                
                function.parameters = [.. parameters];
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
            if (parent is Structure @structure)
            {
                var gen = structure.SearchForGeneric(identifier.ToString());
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

        var diagnostics = (new System.Diagnostics.StackFrame(0, true));
        Console.WriteLine($"(...{diagnostics.GetFileName()?[^15..]}:{diagnostics.GetFileLineNumber()}) "
            + $"{type.Children[0].GetType().Name} in {parent?.GetType().Name}");
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

        // search in projects
        if (program.SearchForChild(name) is Structure r)
            return r;

        // Search global
        var member = program.TryGetGlobal(name) as Structure;
        return member!;
    }

    public (Function? f, Function?[] c) TryGetOveloadIndirect(FunctionGroup funcGroup, TypeReference[] types)
    {
        Function? func = null!;
        Function?[] tconvert = null!;
        
        foreach (var f in funcGroup)
        {
            var parameters = f.parameters;

            if (types.Length != parameters.Length) continue;

            List<Function?> toConvert = [];
            for (var i = 0; i < parameters.Length; i++)
            {
                if (!CanBeAssignedTo(types[i], parameters[i].type, out var cf)) goto Break;
                toConvert.Add(cf);
            }

            func = f;
            tconvert = [.. toConvert];
            break;

            Break: continue;
        }

        return (func, tconvert);
    }
    public Function? TryGetOveloadDirect(FunctionGroup funcGroup, TypeReference[] types)
    {
        Function? func = null!;
        
        foreach (var f in funcGroup)
        {
            var parameters = f.parameters;

            if (types.Length != parameters.Length) continue;
            for (var i = 0; i < parameters.Length; i++)
            {
                if (!CanBeDirectlyAssignedTo(types[i], parameters[i].type)) goto Break;
            }

            func = f;
            break;

            Break: continue;
        }

        return func;
    }

    public bool CanBeDirectlyAssignedTo(TypeReference b, TypeReference to)
        => CanBeAssignedTo(b, to, out var a) && a == null;
    public bool CanBeAssignedTo(TypeReference b, TypeReference to, out Function? how)
    {
        bool result = false;
        how = null;

        if (b == null) goto Return;
        
        var bSolved = b as SolvedTypeReference;
        var toSolved = to as SolvedTypeReference;

        // both are solved references
        if (bSolved != null && toSolved != null)
        {
            // true if both are the same type
            if (bSolved.structure == toSolved.structure)
            {
                result = true;
                goto Return;
            }

            // true if b extends to
            var curr = bSolved.structure;
            while (curr != null)
            {
                curr = bSolved.structure.extends;
                if (toSolved == bSolved)
                {
                    result = true;
                    goto Return;
                }
            }

            // true if b is implicitly castable to
            how = bSolved.structure.TryGetImplicitConvertTo(toSolved.structure);
            if (how != null) {
                result = true;
                goto Return;
            }
        }

        // if b is anytype
        if (toSolved != null && toSolved.structure.GlobalReference == "Std.Types.AnyType")
        {
            result = true;
            goto Return;
        }

        Return:
            typeMatchingLog.AppendLine($"testing {b} -> {to}: {result}" + (how != null ? " (needs cast)" : ""));
            return result;
    }
}
