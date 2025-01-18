using Abstract.Parser.Core.Language.SyntaxNodes.Expression.TypeModifiers;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;
using Abstract.Parser.Core.ProgData;
using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser;

public partial class Evaluator
{
    public void RegisterLevel_1_ExtraReferences()
    {
        /* This runs before any type be acessible.
        *  Here can only contains the process that does not
        *  depends of types or parameters
        */

        foreach (var _type in program.types.Values.ToArray())
        {
            if (_type.TryGetAttribute("defineGlobal", out var attrib))
                ExecuteAttrtribute(_type, attrib);
        }
        
        // registring uptr and sptr FIXME
        RegisterGlobal(SearchStructure("Std.Types.UnsignedInteger64"), "uptr");
        RegisterGlobal(SearchStructure("Std.Types.SignedInteger64"), "sptr");

    }

    public void RegisterLevel_2_ExtraReferences()
    {
        /* This runs after the main level type references be solved
        *  Here must be processed things related to types like parameters
        *  and type-dependand attributes
        */

        foreach (var funcOverloads in program.functions.Values)
        {
            foreach (var func in funcOverloads)
            {
                MemberAttribute attrib;
                if (func.TryGetAttribute("getter", out attrib)) ExecuteAttrtribute(func, attrib);
                if (func.TryGetAttribute("setter", out attrib)) ExecuteAttrtribute(func, attrib);
                if (func.TryGetAttribute("indexerGetter", out attrib)) ExecuteAttrtribute(func, attrib);
                if (func.TryGetAttribute("indexerSetter", out attrib)) ExecuteAttrtribute(func, attrib);
                if (func.TryGetAttribute("overrideOperator", out attrib)) ExecuteAttrtribute(func, attrib);
                if (func.TryGetAttribute("implicitConvert", out attrib)) ExecuteAttrtribute(func, attrib);
            }
        }
    }

    private void ExecuteAttrtribute(ProgramMember member, MemberAttribute attrib)
    {
        if (attrib.Name == "getter")
        {
            if (attrib.Arguments.Length == 1)
            {
                if (attrib.Arguments[0] is StringLiteralNode @str)
                {
                    var parent = member.parent!;
                    var identifier = new MemberIdentifier(str.RawContent);
                    var search = parent.SearchForChild(identifier);

                    if (search == null)
                    {
                        // Create new
                        var nf = new Property(parent, identifier);
                        parent.AppendChild(nf);
                        program.fields.Add(nf.GlobalReference, nf);
                        nf.getter = (Function)member!;
                    }
                    else if (search is Property @prop)
                    {
                        // Append to existing one
                        if (prop.getter != null) throw new Exception("TODO this property already have a getter function");
                        prop.getter = (Function)member!;
                    }
                    else throw new Exception("TODO this node already have a member with that name");

                }
                else throw new Exception("TODO no function overload matching types");
            }
            else throw new Exception("TODO no function overload with X arguments");
        }
        else if (attrib.Name == "setter")
        {
            if (attrib.Arguments.Length == 1)
            {
                if (attrib.Arguments[0] is StringLiteralNode @str)
                {
                    var parent = member.parent!;
                    var identifier = new MemberIdentifier(str.RawContent);
                    var search = parent.SearchForChild(identifier);

                    if (search == null)
                    {
                        // Create new
                        var nf = new Property(parent, identifier);
                        parent.AppendChild(nf);
                        program.fields.Add(nf.GlobalReference, nf);
                        nf.setter = (Function)member!;
                    }
                    else if (search is Property @prop)
                    {
                        // Append to existing one
                        if (prop.setter != null) throw new Exception("TODO this property already have a setter function");
                        prop.setter = (Function)member!;
                    }
                    else throw new Exception("TODO this node already have a member with that name");

                }
                else throw new Exception("TODO no function overload matching types");
            }
            else throw new Exception("TODO no function overload with X arguments");
        }

        else if (attrib.Name == "indexerGetter")
        {
            if (attrib.Arguments.Length == 0)
            {
                var parent = member.parent!;
                var identifier = new MemberIdentifier("!indexer");
                var search = parent.SearchForChild(identifier);

                if (search == null)
                {
                    // Create new
                    var nf = new Property(parent, identifier);
                    parent.AppendChild(nf);
                    program.fields.Add(nf.GlobalReference, nf);
                    nf.getter = (Function)member!;
                }
                else if (search is Property @prop)
                {
                    // Append to existing one
                    if (prop.getter != null) throw new Exception("TODO the indexer already have a getter function");
                    prop.getter = (Function)member!;
                }
            }
            else throw new Exception("TODO no function overload with X arguments");
        }
        else if (attrib.Name == "indexerSetter")
        {
            if (attrib.Arguments.Length == 0)
            {
                var parent = member.parent!;
                var identifier = new MemberIdentifier("!indexer");
                var search = parent.SearchForChild(identifier);

                if (search == null)
                {
                    // Create new
                    var nf = new Property(parent, identifier);
                    parent.AppendChild(nf);
                    program.fields.Add(nf.GlobalReference, nf);
                    nf.setter = (Function)member!;
                }
                else if (search is Property @prop)
                {
                    // Append to existing one
                    if (prop.setter != null) throw new Exception("TODO the indexer already have a setter function");
                    prop.setter = (Function)member!;
                }
            }
            else throw new Exception("TODO no function overload with X arguments");
        }

        else if (attrib.Name == "defineGlobal")
        {
            if (attrib.Arguments.Length == 1)
            {
                if (attrib.Arguments[0] is StringLiteralNode @alias)
                {
                    RegisterGlobal(member, alias.RawContent);
                }
                else if (attrib.Arguments[0] is GenericCollectionExpressionNode @aliases)
                {
                    foreach (var i in aliases.Items)
                    {
                        if (i is not StringLiteralNode @asString)
                            throw new Exception("TODO invalid type X in collection of type string");
                        RegisterGlobal(member, asString.RawContent);
                    }
                }
                
                else throw new Exception("TODO no function overload matching types");
            }
            else throw new Exception("TODO no function overload with X arguments");
        }
    
        else if (attrib.Name == "overrideOperator")
        {
            if (attrib.Arguments.Length == 1)
            {
                if (member is not Function @func)
                    throw new Exception("TODO attribute can only be implemented on functions");
                if (attrib.Arguments[0] is not StringLiteralNode @str)
                    throw new Exception("TODO no function overload matching types");
                
                AppendOperatorOverload(func, str.RawContent);
            }
            else throw new Exception("TODO no function overload with X arguments");
        }
    
        else if (attrib.Name == "implicitConvert")
        {
            if (attrib.Arguments.Length == 0) AppendImplicitConversion((Function)member);
            else throw new Exception("TODO no function overload with X arguments");
        }
    }

    private static readonly string[] allowedOperatorOverloading= [
        "==", "!=", ">", "<", ">=", "<=", "+", "-", "*", "/", "%", "**"];
    private void AppendOperatorOverload(Function func, string op)
    {
        if (!allowedOperatorOverloading.Contains(op))
            throw new Exception($"TODO operator {op} not allowd for overload!");
        if (func.parent is not Structure @parent)
            throw new Exception($"TODO function parent must be a structure!");

        parent.AppendOperator(op, func);
    }
    private void AppendImplicitConversion(Function func)
    {
        if (func.parent is not Structure @parent)
            throw new Exception($"TODO function parent must be a structure!");
        if (func.parameters[0].type is not SolvedTypeReference @solvedP1)
            throw new Exception($"TODO unable to solve parameter 1 type!");
        if (func.returnType is not SolvedTypeReference @solvedrt)
            throw new Exception($"TODO unable to solve returning type!");

        var p1 = solvedP1.structure;
        var rt = solvedrt.structure;

        if (p1 == parent)
            parent.AppendImplicitConvert(rt, func);
        
        else if (rt == parent)
            rt.AppendImplicitConvert(parent, func);
        
        else throw new Exception($"TODO the implicit convert function"
        + "parameter or return type must be the parent structure of the function!");
    }
    private void RegisterGlobal(ProgramMember member, string alias)
    {
        if (member is Namespace @nmsp) program.namespaces.Add(alias, nmsp);
        else if (member is Structure @struc) program.types.Add(alias, struc);
        else if (member is Field @field) program.fields.Add(alias, field);
        else if (member is Function @func)
        {
            FunctionGroup group;
            if (!program.functions.TryGetValue(alias, out group!))
            {
                group = new FunctionGroup(null, alias);
                program.AppendGlobals(alias, group);
            }
            group.AddOverload(func);

            return;
        }

        program.AppendGlobals(alias, member);
    }
}
