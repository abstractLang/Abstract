using System.Buffers.Binary;
using System.Numerics;
using Abstract.Binutils.Abs.Elf;
using Lump = Abstract.Binutils.Abs.Elf.ElfBuilder.LumpBuilder;

namespace Abstract.Binutils.Abs.Bytecode;

public static class Instructions
{
    // general utils
    public static void DirectoryReference(this Lump l, ElfBuilder.DirBuilder immref) {
        immref._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
    }

    // Instructions
    public static void Nop(this Lump l) => l.Content.WriteByte(0x00);
    public static void Illegal(this Lump l) => l.Content.WriteByte(0x01);

    public static void LdConst_nullptr(this Lump l) => l.Content.Write([0x02]);

    public static void LdConst_i1(this Lump l, bool immi8) => l.Content.Write([0x03, (byte)(immi8 ? 0 : 1)]);
    public static void LdConst_i8(this Lump l, byte immi8) => l.Content.Write([0x04, immi8]);
    public static void LdConst_i8(this Lump l, sbyte immi8) => l.Content.Write([0x04, (byte)immi8]);
    
    public static void LdConst_i16(this Lump l, short immi16) {
        l.Content.WriteByte(0x05);
        l.Content.WriteI16(immi16);
    }
    public static void LdConst_i16(this Lump l, ushort immi16) {
        l.Content.WriteByte(0x05);
        l.Content.WriteU16(immi16);
    }

    public static void LdConst_i32(this Lump l, int immi32) {
        l.Content.WriteByte(0x06);
        l.Content.WriteI32(immi32);
    }
    public static void LdConst_i32(this Lump l, uint immi32) {
        l.Content.WriteByte(0x06);
        l.Content.WriteU32(immi32);
    }

    public static void LdConst_i64(this Lump l, long immi64) {
        l.Content.WriteByte(0x07);
        l.Content.WriteI64(immi64);
    }
    public static void LdConst_i64(this Lump l, ulong immi64) {
        l.Content.WriteByte(0x08);
        l.Content.WriteU64(immi64);
    }

    public static void LdConst_i128(this Lump l, Int128 immi128) {
        l.Content.WriteByte(0x08);
        l.Content.WriteI128(immi128);
    }
    public static void LdConst_i128(this Lump l, UInt128 immi128) {
        l.Content.WriteByte(0x08);
        l.Content.WriteU128(immi128);
    }

    public static void LdConst_iptr(this Lump l, long immptr) {
        l.Content.WriteByte(0x09);
        l.Content.WriteI64(immptr);
    }
    public static void LdConst_iptr(this Lump l, ulong immptr) {
        l.Content.WriteByte(0x09);
        l.Content.WriteU64(immptr);
    }

    public static void LdConst_i_immu8(this Lump l, byte i_immu8, BigInteger immi_arg0) {
        var byteCount = (int)Math.Ceiling(i_immu8 / 8.0);
        var buf = immi_arg0.ToByteArray()
        [0 .. Math.Min(immi_arg0.GetByteCount(), byteCount)]
        // .ToByteArray() returns little endian
        // so it needs to be reversed after cutted
        .Reverse().ToArray();
        l.Content.Write([0x0A, ..buf]);
    }

    public static void LdConst_struct(this Lump l, ElfBuilder.DirBuilder immref, byte[] immbuf) {
        l.Content.WriteByte(0x0B);
        immref._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
        l.Content.Write(immbuf);
    }

    public static void LdField(this Lump l, ElfBuilder.DirBuilder immref) {
        l.Content.WriteByte(0x0C);
        immref._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
    }
    public static void LdLocal(this Lump l, byte immu8) {
        l.Content.Write([0x0D, immu8]);
    }
    public static void LdLocal(this Lump l, ushort immu16) {
        var buf = new byte[2];
        BinaryPrimitives.WriteUInt16BigEndian(buf, immu16);
        l.Content.Write([0x0E, ..buf]);
    }

    public static void LdType(this Lump l, ElfBuilder.DirBuilder immref) {
        l.Content.WriteByte(0x0F);
        immref._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
    }

    public static void SetField(this Lump l, ElfBuilder.DirBuilder immref) {
        l.Content.WriteByte(0x010);
        immref._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
    }
    public static void SetLocal(this Lump l, byte immu8) {
        l.Content.Write([0x11, immu8]);
    }
    public static void SetLocal(this Lump l, ushort immu16) {
        var buf = new byte[2];
        BinaryPrimitives.WriteUInt16BigEndian(buf, immu16);
        l.Content.Write([0x12, ..buf]);
    }

    public static void GetType(this Lump l) => l.Content.WriteByte(0x13);
    public static void New(this Lump l) => l.Content.WriteByte(0x14);
    public static void Destroy(this Lump l) => l.Content.WriteByte(0x15);
    public static void Pop(this Lump l) => l.Content.WriteByte(0x16);
    public static void Dup(this Lump l) => l.Content.WriteByte(0x17);

    public static void Call_void(this Lump l, ElfBuilder.DirBuilder immref) {
        l.Content.WriteByte(0x18);
        immref._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
    }
    public static void Call_i1(this Lump l, ElfBuilder.DirBuilder immref)
    {
        l.Content.WriteByte(0x19);
        immref._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
    }
    public static void Call_i8(this Lump l, ElfBuilder.DirBuilder immref) {
        l.Content.WriteByte(0x1A);
        immref._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
    }
    public static void Call_i16(this Lump l, ElfBuilder.DirBuilder immref) {
        l.Content.WriteByte(0x1B);
        immref._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
    }
    public static void Call_i32(this Lump l, ElfBuilder.DirBuilder immref) {
        l.Content.WriteByte(0x1C);
        immref._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
    }
    public static void Call_i64(this Lump l, ElfBuilder.DirBuilder immref) {
        l.Content.WriteByte(0x1D);
        immref._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
    }
    public static void Call_i128(this Lump l, ElfBuilder.DirBuilder immref) {
        l.Content.WriteByte(0x1E);
        immref._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
    }
    public static void Call_iptr(this Lump l, ElfBuilder.DirBuilder immref) {
        l.Content.WriteByte(0x1F);
        immref._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
    }
    public static void Call_i_immu8(this Lump l, byte i_immu8, ElfBuilder.DirBuilder immref) {
        l.Content.Write([0x20, i_immu8]);
        immref._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
    }
    public static void Call_struct(this Lump l, ElfBuilder.DirBuilder immref_0, ElfBuilder.DirBuilder immref_1) {
        l.Content.WriteByte(0x21);
        immref_0._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
        immref_1._externReferences.Add((l, l.Content.Position));
        l.Content.Write([0, 0, 0, 0]); // ptr offset
    }

    public static void Conv(this Lump l, byte i_immu8_0, byte i_immu8_1) {
        l.Content.Write([0x22, i_immu8_0, i_immu8_0]);
    }

    public static void Add(this Lump l) => l.Content.WriteByte(0x23);
    public static void Sub(this Lump l) => l.Content.WriteByte(0x24);
    public static void Mul(this Lump l) => l.Content.WriteByte(0x25);
    public static void Mul_U(this Lump l) => l.Content.WriteByte(0x26);
    public static void Div(this Lump l) => l.Content.WriteByte(0x27);
    public static void Div_U(this Lump l) => l.Content.WriteByte(0x28);
    public static void Rem(this Lump l) => l.Content.WriteByte(0x29);
    public static void Rem_U(this Lump l) => l.Content.WriteByte(0x2A);
    public static void Abs(this Lump l) => l.Content.WriteByte(0x2B);
    public static void Or(this Lump l) => l.Content.WriteByte(0x2C);
    public static void And(this Lump l) => l.Content.WriteByte(0x2D);
    public static void Not(this Lump l) => l.Content.WriteByte(0x2E);
    public static void Xor(this Lump l) => l.Content.WriteByte(0x2F);
    public static void Nand(this Lump l) => l.Content.WriteByte(0x30);
    public static void ShiftL(this Lump l) => l.Content.WriteByte(0x31);
    public static void ShiftL_U(this Lump l) => l.Content.WriteByte(0x32);
    public static void ShiftR(this Lump l) => l.Content.WriteByte(0x33);
    public static void ShiftR_U(this Lump l) => l.Content.WriteByte(0x34);

    //public static void Cmp(this Lump l) => l.Content.WriteByte(0x35);
    //public static void Jmp(this Lump l, uint immu32) {
    //    l.Content.WriteByte(0x36);
    //    l.Content.WriteU32(immu32);
    //}
    //public static void If(this Lump l) => l.Content.WriteByte(0x37);
    //public static void Else(this Lump l) => l.Content.WriteByte(0x38);
    //public static void EndIf(this Lump l) => l.Content.WriteByte(0x39);
    

    public static void EnterFrame(this Lump l, ushort immu16) {
        l.Content.WriteByte(0x3A);
        l.Content.WriteU32(immu16);
    }
    public static void LeaveFrame(this Lump l) => l.Content.WriteByte(0x3B);
    
    public static void Throw(this Lump l) => l.Content.WriteByte(0x3C);
}
