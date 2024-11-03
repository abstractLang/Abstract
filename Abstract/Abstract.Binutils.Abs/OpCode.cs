using System.Reflection.Emit;

namespace Abstract.Binutils.Abs;

public static class OpCode
{
    // Get a OpCode
    public static OpCodeInfo Get(string mnemonic) => OpCodeManager._opCodesList
        .FirstOrDefault(e => e.ToString().ToLower().StartsWith(mnemonic.ToLower()));
    public static OpCodeInfo Get(byte index) => OpCodeManager._opCodesList[index];

    // Get the binary value of a OpCode
    public static byte GetBin(string mnemonic) => GetBin(Get(mnemonic));
    public static byte GetBin(OpCodeInfo opCode) => (byte)Array.IndexOf(OpCodeManager._opCodesList, opCode);

}
