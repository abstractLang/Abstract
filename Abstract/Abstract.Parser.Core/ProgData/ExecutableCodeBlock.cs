using Abstract.Parser.Core.ProgData.DataReference;
using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.ProgData;

public class ExecutableCodeBlock
{
    public ProgramMember? ProgramMemberParent { get; set; }
    public ExecutableCodeBlock? BlockParent { get; set; }

    private Dictionary<MemberIdentifier, (TypeReference type, bool constant)> _localVariables = [];
    private List<dynamic> _localConstants = [];

    public void AppendLocalReference(MemberIdentifier name, TypeReference type, bool isConstant)
    {
        if (_localVariables.ContainsKey(name))
            throw new Exception("TODO this shit is already registred bruh");

        _localVariables.Add(name, (type, isConstant));
    }
    public (TypeReference type, bool constant) GetLocalReference(MemberIdentifier name)
    {
        return _localVariables[name];
    }

    public int AppendConstantReference(dynamic value)
    {
        _localConstants.Add(value);
        return _localConstants.Count - 1;
    }
    public dynamic GetConstantReference(int index) => _localConstants[index];

    public DataRef TryGetReference(MemberIdentifier name)
    {
        // Search local
        if (_localVariables.ContainsKey(name))
            return new LocalDataRef(this, name);
        
        // Search parent
        else if (ProgramMemberParent != null)
        {

            var tr = ProgramMemberParent.SearchForGeneric(name);
            if (tr != null) throw new NotImplementedException();

            var res = ProgramMemberParent.SearchForChild(name);
            if (res == null)
            {
                var program = ProgramMemberParent.ParentProgram!;
                res = program.SearchForChild(name);
            }
            
            if (res != null)
            {
                if (res is FunctionGroup fgroup)
                    return new FunctionGroupRef(fgroup);

                else
                {
                    Console.WriteLine($"{res}");
                    return null!;
                }
            }
        }
        return null!;
    }
    public ProgramMember? TryGetMember(MemberIdentifier name)
    {
        if (ProgramMemberParent != null)
        {

            var res = ProgramMemberParent.SearchForChild(name);
            if (res == null)
            {
                var program = ProgramMemberParent.ParentProgram!;
                res = program.SearchForChild(name);
            }
            
            return res;
        }
        return null!;
    }
}
