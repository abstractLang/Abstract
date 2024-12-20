using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser;

public partial class Evaluator
{

    #region Reference tables
    private ProgramRoot program = null!;
    private Dictionary<MemberIdentifier, Namespace> namespaces = [];
    private Dictionary<MemberIdentifier, List<Function>> functions = [];
    private Dictionary<MemberIdentifier, Structure> types = [];
    private Dictionary<MemberIdentifier, Field> fields = [];
    private Dictionary<MemberIdentifier, Enumerator> enums = [];
    #endregion

}
