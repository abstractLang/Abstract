using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression.TypeModifiers;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;
using Abstract.Parser.Core.ProgData;
using Abstract.Parser.Core.ProgData.DataReference;
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

    /*
    *   Yah type matching is so complex that i fucked around with
    *   everything around here, good luck to understand anything
    *   because i can't too
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
            if (struc != null)
            {
                var isAnytype = struc.GlobalReference == "Std.Types.AnyType";
                return new SolvedTypeReference(struc, [], isAnytype);
            }
            
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
            var childGeneric = (child as SolvedTypeReference)?.isGeneric ?? false;

            return new SolvedTypeReference(arrayStruct, [child], childGeneric);
        }

        // failable modifier
        else if (type.Children[0] is FailableTypeModifierNode @failable)
        {
            var child = GetTypeFromTypeExpressionNode(failable.Type, parent);
            var failableStruct = SearchStructure("Std.Types.Failable");
            var childGeneric = (child as SolvedTypeReference)?.isGeneric ?? false;

            return new SolvedTypeReference(failableStruct, [child], childGeneric);
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

    // FIXME these 3 next needs a fucking hard rework

    /// <summary>
    /// Returns a valid function overload for the designed argument
    /// types
    /// TODO describle here the diference between direct and indirect
    /// </summary>
    /// <param name="funcGroup">The function group</param>
    /// <param name="types">The argument types</param>
    /// <returns></returns>
    public (Function? f, Function?[] c) TryGetOveloadIndirect(FunctionGroup funcGroup, DataRef[] args)
    {
        Function? func = null!;
        Function?[] tconvert = null!;
        
        foreach (var f in funcGroup)
        {
            var parameters = f.parameters;

            Dictionary<int, TypeReference> _generics = [];

            if (args.Length != parameters.Length) continue;

            List<Function?> toConvert = [];
            for (var i = 0; i < parameters.Length; i++)
            {
                if (!CanBeAssignedTo(args[i].refferToType, parameters[i].type, out var cf))
                    goto Break;

                if (parameters[i].type is GenericTypeReference @generic)
                {
                    if (generic.from == f)
                    {
                        if (!CanBeDirectlyAssignedTo(args[i].refferToType, 
                            _generics[generic.parameterIndex]))
                            goto Break;
                    }
                    else throw new NotImplementedException();
                }
                
                if (args[i] is TypeDataRef @typeDataRef)
                    _generics.Add(i, typeDataRef.type);
                
                toConvert.Add(cf);
            }

            func = f;
            tconvert = [.. toConvert];
            break;

            Break: continue;
        }

        return (func, tconvert);
    }
    /// <summary>
    /// Returns a valid function overload for the designed argument
    /// types
    /// TODO describle here the diference between direct and indirect
    /// </summary>
    /// <param name="funcGroup">The function group</param>
    /// <param name="types">The argument types</param>
    /// <returns></returns>
    public Function? TryGetOveloadDirect(FunctionGroup funcGroup, DataRef[] args)
    {
        Function? func = null!;
        
        foreach (var f in funcGroup)
        {
            var parameters = f.parameters;

            Dictionary<int, TypeReference> _generics = [];

            if (args.Length != parameters.Length) continue;
            for (var i = 0; i < parameters.Length; i++)
            {
                if (parameters[i].type is SolvedTypeReference &&
                    !CanBeDirectlyAssignedTo(args[i].refferToType, parameters[i].type))
                    goto Break;
                
                if (parameters[i].type is GenericTypeReference @generic)
                {
                    if (generic.from == f)
                    {
                        if (!CanBeDirectlyAssignedTo(args[i].refferToType, 
                            _generics[generic.parameterIndex]))
                            goto Break;
                    }
                    else throw new NotImplementedException();
                }
                
                if (args[i] is TypeDataRef @typeDataRef)
                    _generics.Add(i, typeDataRef.type);
            }

            func = f;
            break;

            Break: continue;
        }

        return func;
    }
    public TypeReference GetFunctionReturnType(Function func, DataRef[] args)
    {
        var rtype = func.returnType;
        if (rtype is SolvedTypeReference) return rtype;
        else if (rtype is GenericTypeReference @g)
        {
            if (g.from == func) return ((TypeDataRef)args[g.parameterIndex]).type;
            else throw new NotImplementedException();
        }
        else throw new NotImplementedException("I don't even remember if this is "
        + "fucking possible");
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
