using System.Text;

namespace Abstract.Binutils.Abs.Bytecode;

internal static class InstructionsManager
{
    public static InstructionInfo[] _opCodesList = new InstructionInfo[256];


    static InstructionsManager()
    {
        BakeOpCodes(GenOpCodes());

        var writer = new StringBuilder();
        for (int i = 0; i < 256; i++)
            writer.AppendLine($"{i:X2}  {_opCodesList[i].ToFancyString()}");
        File.WriteAllText("./instructionset.txt", writer.ToString());
    }

    private static List<InstructionBuilder> GenOpCodes()
    {
        List<InstructionBuilder> opCodes = [];

        opCodes.Add(InstructionBuilder.Create(Base.Nop));
        opCodes.Add(InstructionBuilder.Create(Base.Invalid));

        opCodes.Add(InstructionBuilder.CreateIllegal()); // ldConst void
        opCodes.Add(InstructionBuilder.Create(Base.LdConst).WithTypes(Types.Null));
        opCodes.Add(InstructionBuilder.Create(Base.LdConst)
            .WithTypes(
                Types.i8, Types.i16, Types.i32, Types.i64, Types.i128,
                Types.u8, Types.u16, Types.u32, Types.u64, Types.u128,
                Types.f32, Types.f64,
                Types.Char
            ).WithArgs("imm{t}"));
        opCodes.Add(InstructionBuilder.Create(Base.LdConst).WithTypes(Types.Str)
            .WithArgs("ref{t}"));
        opCodes.Add(InstructionBuilder.Create(Base.LdConst)
            .WithTypes(Types.Bool).WithArgs("imm{t}"));
        opCodes.Add(InstructionBuilder.Create(Base.LdConst).WithTypes(
                Types.Struct,
                Types.Arr
            )
            .WithArgs("ref{t}"));

        opCodes.Add(InstructionBuilder.Create(Base.LdStatic).WithArgs("refstatic"));
        opCodes.Add(InstructionBuilder.Create(Base.LdField).WithArgs("reffield"));
        opCodes.Add(InstructionBuilder.Create(Base.LdLocal).WithArgs("immu16"));
        opCodes.Add(InstructionBuilder.Create(Base.LdIndex));
        opCodes.Add(InstructionBuilder.Create(Base.LdType).WithArgs("reftype"));
        opCodes.Add(InstructionBuilder.Create(Base.LdPType).WithArgs("immtype"));

        opCodes.Add(InstructionBuilder.Create(Base.SetStatic).WithArgs("refstatic"));
        opCodes.Add(InstructionBuilder.Create(Base.SetField).WithArgs("reffield"));
        opCodes.Add(InstructionBuilder.Create(Base.SetLocal).WithArgs("immu16"));
        opCodes.Add(InstructionBuilder.Create(Base.SetIndex));

        opCodes.Add(InstructionBuilder.Create(Base.GetType));

        opCodes.Add(InstructionBuilder.Create(Base.New));
        opCodes.Add(InstructionBuilder.Create(Base.Pop));
        opCodes.Add(InstructionBuilder.Create(Base.Dup));
        opCodes.Add(InstructionBuilder.Create(Base.Destroy));

        opCodes.Add(InstructionBuilder.Create(Base.Call).WithTypes(Types.Void)
        .WithArgs("reffunc"));
        opCodes.Add(InstructionBuilder.CreateIllegal()); // call null
        opCodes.Add(InstructionBuilder.Create(Base.Call).WithAllTypesMinus(Types.Null, Types.Void)
        .WithArgs("reffunc"));
        opCodes.Add(InstructionBuilder.Create(Base.Conv)
        .WithAllTypesMinus(Types.Void, Types.Null).WithArgs("immtype"));

        opCodes.Add(InstructionBuilder.Create(Base.Add));
        opCodes.Add(InstructionBuilder.Create(Base.Sub));
        opCodes.Add(InstructionBuilder.Create(Base.Mul).WithSigness());
        opCodes.Add(InstructionBuilder.Create(Base.Div).WithSigness());
        opCodes.Add(InstructionBuilder.Create(Base.Rem).WithSigness());
        opCodes.Add(InstructionBuilder.Create(Base.Mod));

        opCodes.Add(InstructionBuilder.Create(Base.Or));
        opCodes.Add(InstructionBuilder.Create(Base.And));
        opCodes.Add(InstructionBuilder.Create(Base.Not));
        opCodes.Add(InstructionBuilder.Create(Base.Xor));
        opCodes.Add(InstructionBuilder.Create(Base.Nor));

        opCodes.Add(InstructionBuilder.Create(Base.ShiftL).WithSigness());
        opCodes.Add(InstructionBuilder.Create(Base.ShiftR).WithSigness());

        opCodes.Add(InstructionBuilder.Create(Base.Cmp).WithAllConditionsMinus(Condition.Forced,
            Condition.GreatThan, Condition.LessThan, Condition.GreatEqualThan, Condition.LessEqualThan));
        opCodes.Add(InstructionBuilder.Create(Base.Cmp).WithConditions(
            Condition.GreatThan, Condition.LessThan, Condition.GreatEqualThan, Condition.LessEqualThan)
            .WithSigness());

        opCodes.Add(InstructionBuilder.Create(Base.Jmp).WithAllConditionsMinus(
            Condition.GreatThan, Condition.LessThan, Condition.GreatEqualThan, Condition.LessEqualThan));
        opCodes.Add(InstructionBuilder.Create(Base.Jmp).WithConditions(
            Condition.GreatThan, Condition.LessThan, Condition.GreatEqualThan, Condition.LessEqualThan)
            .WithSigness());

        opCodes.Add(InstructionBuilder.Create(Base.If).WithAllConditionsMinus(
            Condition.GreatThan, Condition.LessThan, Condition.GreatEqualThan, Condition.LessEqualThan));
        opCodes.Add(InstructionBuilder.Create(Base.If).WithConditions(
            Condition.GreatThan, Condition.LessThan, Condition.GreatEqualThan, Condition.LessEqualThan)
            .WithSigness());

        opCodes.Add(InstructionBuilder.Create(Base.Else));
        opCodes.Add(InstructionBuilder.Create(Base.EndIf));

        opCodes.Add(InstructionBuilder.Create(Base.Ret));

        opCodes.Add(InstructionBuilder.Create(Base.EnterFrame).WithArgs("immi16", "immi16"));
        opCodes.Add(InstructionBuilder.Create(Base.LeaveFrame));

        opCodes.Add(InstructionBuilder.Create(Base.Throw));

        return opCodes;
    }

    private static void BakeOpCodes(List<InstructionBuilder> builders)
    {
        List<InstructionInfo> opCodes = [];

        foreach (var i in builders)
        {
            List<InstructionInfo> _base = [];

            _base.Add(new(0, i.Base, Types.Unspecified, Condition.Forced, false, DataSize.Unespecified, []));
            if (i.Base == Base.Illegal) goto SkipProcess;

            if (i.Types != null)
            {
                InstructionInfo[] _ob = [.. _base];
                _base.Clear();

                foreach (var j in _ob)
                {
                    foreach (var t in i.Types)
                    {
                        _base.Add(new(0, j.b, t, j.c, j.u, j.s, []));
                    }
                }
            }
            
            if (i.Conditions != null)
            {
                InstructionInfo[] _ob = [.. _base];
                _base.Clear();

                foreach (var j in _ob)
                {
                    foreach (var k in i.Conditions)
                    {
                        _base.Add(new(0, j.b, j.t, k, j.u, j.s, []));
                    }
                }
            }
            
            if (i.signess)
            {
                InstructionInfo[] _ob = [.. _base];
                _base.Clear();

                foreach (var j in _ob)
                {
                    _base.Add(new(0, j.b, j.t, j.c, false, j.s, []));
                    _base.Add(new(0, j.b, j.t, j.c, true, j.s, []));
                }
            }

            if (i.sizes != null)
            {
                InstructionInfo[] _ob = [.. _base];
                _base.Clear();

                foreach (var j in _ob)
                {
                    foreach (var k in i.sizes)
                    {
                        _base.Add(new(0, j.b, j.t, j.c, j.u, k, []));
                    }
                }
            }

            if (i.args != null)
            {
                InstructionInfo[] _ob = [.. _base];
                _base.Clear();

                foreach (var j in _ob)
                {
                    string[] args = [.. i.args.Select(e => e.Replace("{t}", j.t.ToString().ToLower()))];
                    _base.Add(new(0, j.b, j.t, j.c, j.u, j.s, args));
                }
            }

            SkipProcess:
            foreach (var j in _base)
            {
                if (j.b != Base.Illegal && opCodes.Contains(j))
                    Console.WriteLine($"Repeated instruction {j} detected!");
                opCodes.Add(j);
            }
        }
    
        for (byte i = 0; i < opCodes.Count; i++)
        {
            var opc = opCodes[i];
            _opCodesList[i] = new(i, opc.b, opc.t, opc.c, opc.u, opc.s, opc.args);
        }
    }
}

internal struct InstructionBuilder()
{
    private static readonly Types[] allTypes = Enum.GetValues(typeof(Types)).Cast<Types>().Except([(Types)0]).ToArray();
    private static readonly Condition[] allConditions = Enum.GetValues(typeof(Condition)).Cast<Condition>().ToArray();
    private static readonly DataSize[] allSizes = Enum.GetValues(typeof(DataSize)).Cast<DataSize>().ToArray();

    public Base Base;
    public Types[]? Types = null;
    public Condition[]? Conditions = null;
    public bool signess = false;
    public DataSize[]? sizes = null;

    public string[]? args = null;

    public static InstructionBuilder Create(Base b) => new() { Base = b };
    public static InstructionBuilder CreateIllegal() => new() { Base = Base.Illegal };

    public InstructionBuilder WithAllTypes()
    {
        Types = allTypes;
        return this;
    }
    public InstructionBuilder WithAllConditions()
    {
        Conditions = allConditions;
        return this;
    }
    public InstructionBuilder WithSigness()
    {
        signess = true;
        return this;
    }
    public InstructionBuilder WithAllSizes()
    {
        sizes = allSizes;
        return this;
    }

    public InstructionBuilder WithAllTypesMinus(params Types[] t)
    {
        Types = [.. allTypes.Except(t)];
        return this;
    }
    public InstructionBuilder WithAllConditionsMinus(params Condition[] c)
    {
        Conditions = [.. allConditions.Except(c)];
        return this;
    }
    public InstructionBuilder WithAllSizesMinus(params DataSize[] s)
    {
        sizes = [.. allSizes.Except(s)];
        return this;
    }

    public InstructionBuilder WithTypes(params Types[] t)
    {
        Types = t;
        return this;
    }
    public InstructionBuilder WithConditions(params Condition[] c)
    {
        Conditions = c;
        return this;
    }
    public InstructionBuilder WithSizes(params DataSize[] s)
    {
        sizes = s;
        return this;
    }

    public InstructionBuilder WithArgs(params string[] a)
    {
        args = a;
        return this;
    }
}
