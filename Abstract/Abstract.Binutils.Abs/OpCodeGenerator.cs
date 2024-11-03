using System.Text;

namespace Abstract.Binutils.Abs;

internal static class OpCodeManager
{
    public static OpCodeInfo[] _opCodesList = new OpCodeInfo[256];

    static OpCodeManager()
    {
        BakeOpCodes(GenOpCodes());

        for (int i = 0; i < 256; i++)
        {
            Console.WriteLine($"{i:X2}  {_opCodesList[i]}");
        }
    }

    private static List<OpCodeBuilder> GenOpCodes()
    {
        List<OpCodeBuilder> opCodes = [];

        opCodes.Add(OpCodeBuilder.Create(Base.Nop));
        opCodes.Add(OpCodeBuilder.Create(Base.Invalid));

        opCodes.Add(OpCodeBuilder.Create(Base.LdConst).WithAllTypesMinus(Types.Void));
        opCodes.Add(OpCodeBuilder.Create(Base.LdStatic));
        opCodes.Add(OpCodeBuilder.Create(Base.LdLocal));
        opCodes.Add(OpCodeBuilder.Create(Base.LdIndex));
        opCodes.Add(OpCodeBuilder.Create(Base.LdType));
        opCodes.Add(OpCodeBuilder.Create(Base.LdType));
        opCodes.Add(OpCodeBuilder.Create(Base.LdIndex));

        opCodes.Add(OpCodeBuilder.Create(Base.SetStat));
        opCodes.Add(OpCodeBuilder.Create(Base.SetField));
        opCodes.Add(OpCodeBuilder.Create(Base.SetLocal));
        opCodes.Add(OpCodeBuilder.Create(Base.SetIndex));

        opCodes.Add(OpCodeBuilder.Create(Base.New));
        opCodes.Add(OpCodeBuilder.Create(Base.Pop));
        opCodes.Add(OpCodeBuilder.Create(Base.Dup));
        opCodes.Add(OpCodeBuilder.Create(Base.Destroy));

        opCodes.Add(OpCodeBuilder.Create(Base.Call).WithAllTypesMinus(Types.Null));
        opCodes.Add(OpCodeBuilder.Create(Base.Conv).WithAllTypesMinus(Types.Void, Types.Null));

        opCodes.Add(OpCodeBuilder.Create(Base.Add));
        opCodes.Add(OpCodeBuilder.Create(Base.Sub));
        opCodes.Add(OpCodeBuilder.Create(Base.Mul).WithSigness());
        opCodes.Add(OpCodeBuilder.Create(Base.Div).WithSigness());
        opCodes.Add(OpCodeBuilder.Create(Base.Rem).WithSigness());
        opCodes.Add(OpCodeBuilder.Create(Base.Mod));

        opCodes.Add(OpCodeBuilder.Create(Base.Or));
        opCodes.Add(OpCodeBuilder.Create(Base.And));
        opCodes.Add(OpCodeBuilder.Create(Base.Not));
        opCodes.Add(OpCodeBuilder.Create(Base.Xor));
        opCodes.Add(OpCodeBuilder.Create(Base.Nor));

        opCodes.Add(OpCodeBuilder.Create(Base.ShiftL).WithSigness());
        opCodes.Add(OpCodeBuilder.Create(Base.ShiftR).WithSigness());

        opCodes.Add(OpCodeBuilder.Create(Base.Cmp).WithAllConditionsMinus(Condition.Forced,
            Condition.GreatThan, Condition.LessThan, Condition.GreatEqualThan, Condition.LessEqualThan));
        opCodes.Add(OpCodeBuilder.Create(Base.Cmp).WithConditions(
            Condition.GreatThan, Condition.LessThan, Condition.GreatEqualThan, Condition.LessEqualThan)
            .WithSigness());

        opCodes.Add(OpCodeBuilder.Create(Base.Jmp).WithAllConditionsMinus(
            Condition.GreatThan, Condition.LessThan, Condition.GreatEqualThan, Condition.LessEqualThan));
        opCodes.Add(OpCodeBuilder.Create(Base.Jmp).WithConditions(
            Condition.GreatThan, Condition.LessThan, Condition.GreatEqualThan, Condition.LessEqualThan)
            .WithSigness());

        opCodes.Add(OpCodeBuilder.Create(Base.If).WithAllConditionsMinus(
            Condition.GreatThan, Condition.LessThan, Condition.GreatEqualThan, Condition.LessEqualThan));
        opCodes.Add(OpCodeBuilder.Create(Base.If).WithConditions(
            Condition.GreatThan, Condition.LessThan, Condition.GreatEqualThan, Condition.LessEqualThan)
            .WithSigness());

        opCodes.Add(OpCodeBuilder.Create(Base.Else));
        opCodes.Add(OpCodeBuilder.Create(Base.EndIf));

        opCodes.Add(OpCodeBuilder.Create(Base.Ret));

        opCodes.Add(OpCodeBuilder.Create(Base.EnterFrame));
        opCodes.Add(OpCodeBuilder.Create(Base.LeaveFrame));

        opCodes.Add(OpCodeBuilder.Create(Base.Throw));

        return opCodes;
    }

    private static void BakeOpCodes(List<OpCodeBuilder> builders)
    {
        List<OpCodeInfo> opCodes = [];

        foreach (var i in builders)
        {
            List<OpCodeInfo> _base = [];

            _base.Add(new(i.Base, Types.Void, Condition.Forced, false, DataSize.Unespecified));

            if (i.Types != null)
            {
                OpCodeInfo[] _ob = [.. _base];
                _base.Clear();

                foreach (var j in _ob)
                {
                    foreach (var k in i.Types)
                    {
                        _base.Add(new(j.b, k, j.c, j.u, j.s));
                    }
                }
            }
            
            if (i.Conditions != null)
            {
                OpCodeInfo[] _ob = [.. _base];
                _base.Clear();

                foreach (var j in _ob)
                {
                    foreach (var k in i.Conditions)
                    {
                        _base.Add(new(j.b, j.t, k, j.u, j.s));
                    }
                }
            }
            
            if (i.signess)
            {
                OpCodeInfo[] _ob = [.. _base];
                _base.Clear();

                foreach (var j in _ob)
                {
                    _base.Add(new(j.b, j.t, j.c, false, j.s));
                    _base.Add(new(j.b, j.t, j.c, true, j.s));
                }
            }

            if (i.sizes != null)
            {
                OpCodeInfo[] _ob = [.. _base];
                _base.Clear();

                foreach (var j in _ob)
                {
                    foreach (var k in i.sizes)
                    {
                        _base.Add(new(j.b, j.t, j.c, j.u, k));
                    }
                }
            }

            opCodes.AddRange(_base);
        }
    
        for (int i = 0; i < opCodes.Count; i++)
        {
            _opCodesList[i] = opCodes[i];
        }
    }
}

public readonly struct OpCodeInfo (Base b, Types t, Condition c, bool u, DataSize s)
{
    public readonly Base b = b;
    public readonly Types t = t;
    public readonly Condition c = c;
    public readonly bool u = u;
    public readonly DataSize s = s;

    public override string ToString()
    {
        StringBuilder sb = new();

        sb.Append(b.ToString());
        if (t != 0) sb.Append($".{t.ToString().ToLower()}");
        if (c != 0) sb.Append($".{c}");
        if (u) sb.Append($".u");

        if (s != DataSize.Unespecified) sb.Append($".{s switch {
            DataSize.Byte => 'b',
            DataSize.Short => 's',
            DataSize.Int => 'i',
            DataSize.Long => 'l',
            _ => 'b'
        }}");

        return sb.ToString();
    }
}

internal struct OpCodeBuilder()
{
    private static readonly Types[] allTypes = Enum.GetValues(typeof(Types)).Cast<Types>().ToArray();
    private static readonly Condition[] allConditions = Enum.GetValues(typeof(Condition)).Cast<Condition>().ToArray();
    private static readonly DataSize[] allSizes = Enum.GetValues(typeof(DataSize)).Cast<DataSize>().ToArray();

    public Base Base;
    public Types[]? Types = null;
    public Condition[]? Conditions = null;
    public bool signess = false;
    public DataSize[]? sizes = null;

    public static OpCodeBuilder Create(Base b) => new OpCodeBuilder() { Base = b };

    public OpCodeBuilder WithAllTypes()
    {
        Types = allTypes;
        return this;
    }
    public OpCodeBuilder WithAllConditions()
    {
        Conditions = allConditions;
        return this;
    }
    public OpCodeBuilder WithSigness()
    {
        signess = true;
        return this;
    }
    public OpCodeBuilder WithAllSizes()
    {
        sizes = allSizes;
        return this;
    }

    public OpCodeBuilder WithAllTypesMinus(params Types[] t)
    {
        Types = [.. allTypes.Except(t)];
        return this;
    }
    public OpCodeBuilder WithAllConditionsMinus(params Condition[] c)
    {
        Conditions = [.. allConditions.Except(c)];
        return this;
    }
    public OpCodeBuilder WithAllSizesMinus(params DataSize[] s)
    {
        sizes = [.. allSizes.Except(s)];
        return this;
    }

    public OpCodeBuilder WithTypes(params Types[] t)
    {
        Types = t;
        return this;
    }
    public OpCodeBuilder WithConditions(params Condition[] c)
    {
        Conditions = c;
        return this;
    }
    public OpCodeBuilder WithSizes(params DataSize[] s)
    {
        sizes = s;
        return this;
    }
}

public enum Base : byte
{
    Invalid,
    Nop,

    LdConst,
    LdStatic,
    LdLocal,
    LdIndex,
    LdType,
    LdSelf,

    SetStat,
    SetField,
    SetLocal,
    SetIndex,

    New,
    Pop,
    Dup,
    Destroy,

    Call,

    Conv,

    Add, Sub, Mul, Div, Rem, Mod,
    Or, And, Not, Xor, Nor,

    ShiftL, ShiftR,

    Cmp,
    Jmp,
    If,
    Else,
    EndIf,

    Ret,

    EnterFrame,
    LeaveFrame,

    Throw
}

public enum Types : byte
{
    Void = 0,
    Null,

    i8, i16, i32, i64, i128,
    u8, u16, u32, u64, u128,

    f32,f64,
    Char,
    Str,
    Bool,

    Obj,
    Arr
}

public enum Condition : byte
{
    Forced = 0,
    True, False,
    Zero, NotZero,
    Positive, Negative,
    Equal, Unequal,
    LessThan, GreatThan,
    LessEqualThan, GreatEqualThan,

}

public enum Signess : byte
{
    Signed = 0,
    Unsigned
}

public enum DataSize : byte
{
    Unespecified,
    Byte,
    Short,
    Int,
    Long
}
