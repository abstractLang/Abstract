namespace Abstract.Binutils.Abs.Bytecode;

public enum Base : byte
{
    Invalid,
    Illegal,
    Nop,

    LdConst,
    LdStatic,
    LdField,
    LdLocal,
    LdIndex,
    LdType,
    LdSelf,

    SetStatic,
    SetField,
    SetLocal,
    SetIndex,

    GetType,

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
    Unspecified = 0,
    Void,
    Null,

    i8, i16, i32, i64, i128,
    u8, u16, u32, u64, u128,

    f32,f64,
    Char,
    Str,
    Bool,

    Struct,
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
