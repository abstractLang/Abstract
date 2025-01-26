using Abstract.Build;
using Abstract.Parser.Core.Exceptions.Evaluation;
using Abstract.Parser.Core.Language.SyntaxNodes.Statement;
using Abstract.Parser.Core.ProgData.DataReference;
using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.ProgData;

public class ExecutableContext
{
    public ProgramMember? ProgramMemberParent { get; set; }
    public Project? ParentProject => ProgramMemberParent?.ParentProject;
    public ExecutableContext? ParentContext { get; set; }

    private Dictionary<MemberIdentifier, (int idx, TypeReference type)> _scopeParameters = [];
    private Dictionary<MemberIdentifier, (int idx, TypeReference type, bool constant)> _localVariables = [];
    private List<dynamic> _localConstants = [];

    public Dictionary<MemberIdentifier, (int idx, TypeReference type, bool constant)> LocalVariables => _localVariables;


    public void AppendLocalParameter(MemberIdentifier name, TypeReference type)
    {
        if (_scopeParameters.ContainsKey(name) || _localVariables.ContainsKey(name))
            throw new Exception("TODO this shit is already registred bruh");

        _scopeParameters.Add(name, ((-_scopeParameters.Count)-1, type));
    }
    public void AppendLocalVariable(MemberIdentifier name, TypeReference type, bool isConstant, LocalVariableNode node)
    {
        if (_scopeParameters.ContainsKey(name) || _localVariables.ContainsKey(name))
            throw new LocalAlreadyDeclaratedException(node, name.ToString());

        _localVariables.Add(name, (_localVariables.Count, type, isConstant));
    }
    
    public (int idx, TypeReference type, bool constant) GetLocalReference(MemberIdentifier name)
    {
        if (_scopeParameters.TryGetValue(name, out var v)) return (v.idx, v.type, true);
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
        // Search local and parameters
        if (_localVariables.ContainsKey(name))
            return new LocalDataRef(this, name);
        else if (_scopeParameters.ContainsKey(name))
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

                else if (res is Field @field)
                    return new FieldDataRef(field);

                else
                {
                    var diagnostics = (new System.Diagnostics.StackFrame(0, true));
                    Console.WriteLine($"(...{diagnostics.GetFileName()?[^15..]}:{diagnostics.GetFileLineNumber()}) "
                    + $"not implemented return for {res}");
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
