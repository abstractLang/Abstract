using Abstract.Parser.Core.Language.SyntaxNodes.Control;
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
                ExecuteGlobalAttrtribute(_type, attrib);
        }
        
        // rigistring uptr and sptr FIXME
        RegisterGlobal(SearchStructure("Std.Types.UnsignedInteger64"), "uptr");
        RegisterGlobal(SearchStructure("Std.Types.SignedInteger64"), "sptr");
    }

    private void ExecuteGlobalAttrtribute(ProgramMember member, MemberAttribute attrib)
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
            else
                throw new Exception("TODO no function overload matching types");
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
