namespace Abstract.Binutils.Abs.Bytecode;

public static class Instructions
{
    // Get a OpCode
    public static InstructionInfo Get(string mnemonic) => InstructionsManager._opCodesList
        .FirstOrDefault(e => e.ToString().ToLower().StartsWith(mnemonic.ToLower()));
    public static InstructionInfo Get(byte index) => InstructionsManager._opCodesList[index];
    
    public static InstructionInfo Get(Base b, Types t = Types.Unspecified) => InstructionsManager._opCodesList
    .FirstOrDefault(e => e.b == b && e.t == t);
    public static InstructionInfo Get(Base b, bool unsigned) => InstructionsManager._opCodesList
    .FirstOrDefault(e => e.b == b && e.u == unsigned);


    // Get the binary value of a OpCode
    public static byte GetBin(string mnemonic) => GetBin(Get(mnemonic));
    public static byte GetBin(InstructionInfo opCode) => (byte)Array.IndexOf(InstructionsManager._opCodesList, opCode);


    public static byte GetBin(Base b, Types t = Types.Void)
        => (byte)Array.FindIndex(InstructionsManager._opCodesList, e => e.b == b && e.t == t);
    public static byte GetBin(Base b, bool unsigned)
        => (byte)Array.FindIndex(InstructionsManager._opCodesList, e => e.b == b && e.u == unsigned);
}
