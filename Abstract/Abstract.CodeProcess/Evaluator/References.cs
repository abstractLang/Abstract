using Abstract.Parser.Core.Language.SyntaxNodes.Expression.TypeModifiers;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;
using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser;

public partial class Evaluator
{
    public void RegisterExtraReferences()
    {

        foreach (var _type in types.Values.ToArray())
        {
            if (_type.TryGetAttribute("defineGlobal", out var attrib))
                ExecuteAttrtribute(_type, attrib);
        }
        
        // registring uptr and sptr FIXME
        RegisterGlobal(SearchStructure("Std.Types.UnsignedInteger64"), "uptr");
        RegisterGlobal(SearchStructure("Std.Types.SignedInteger64"), "sptr");

        foreach (var funcOverloads in functions.Values)
        {
            foreach (var func in funcOverloads)
            {
                MemberAttribute attrib;
                if (func.TryGetAttribute("getter", out attrib)) ExecuteAttrtribute(func, attrib);
                if (func.TryGetAttribute("setter", out attrib)) ExecuteAttrtribute(func, attrib);
                if (func.TryGetAttribute("indexerGetter", out attrib)) ExecuteAttrtribute(func, attrib);
                if (func.TryGetAttribute("indexerSetter", out attrib)) ExecuteAttrtribute(func, attrib);
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
                    var identifier = new MemberIdentifier(str.Value);
                    var search = parent.SearchForChild(identifier);

                    if (search == null)
                    {
                        // Create new
                        var nf = new Property(parent, identifier);
                        parent.AppendChild(nf);
                        fields.Add(nf.GlobalReference, nf);
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
                    var identifier = new MemberIdentifier(str.Value);
                    var search = parent.SearchForChild(identifier);

                    if (search == null)
                    {
                        // Create new
                        var nf = new Property(parent, identifier);
                        parent.AppendChild(nf);
                        fields.Add(nf.GlobalReference, nf);
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
                    fields.Add(nf.GlobalReference, nf);
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
                    fields.Add(nf.GlobalReference, nf);
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
                    RegisterGlobal(member, alias.Value);
                }
                else if (attrib.Arguments[0] is GenericCollectionExpressionNode @aliases)
                {
                    foreach (var i in aliases.Items)
                    {
                        if (i is not StringLiteralNode @asString)
                            throw new Exception("TODO invalid type X in collection of type string");
                        RegisterGlobal(member, asString.Value);
                    }
                }
                
                else throw new Exception("TODO no function overload matching types");
            }
            else throw new Exception("TODO no function overload with X arguments");
        }
    }

    private void RegisterGlobal(ProgramMember member, string alias)
    {
        if (member is Namespace @nmsp) namespaces.Add(alias, nmsp);
        else if (member is Structure @struc) types.Add(alias, struc);
        else if (member is Field @field) fields.Add(alias, field);

        else if (member is Function @func)
        {
            if (functions.ContainsKey(alias)) functions.Add(alias, []);
            functions[alias].Add(func);
        }
        program.AppendGlobals(alias, member);
    }
}
