using System.Buffers.Binary;
using System.Text;

namespace Abstract.Binutils.Abs.Elf;

public static class ExStream {

    public static byte[] ReadArray(this Stream stream, long len)
    {
        var buf = new byte[len];
        stream.ReadExactly(buf);
        return buf;
    }
    public static byte[] LookArray(this Stream stream, long len)
    {
        var pos = stream.Position;
        var buf = new byte[len];
        stream.ReadExactly(buf);
        stream.Position = pos;
        return buf;
    }


    public static sbyte ReadI8(this Stream stream) => (sbyte)stream.ReadByte();
    public static short ReadI16(this Stream stream) => BinaryPrimitives.ReadInt16BigEndian(stream.ReadArray(2));
    public static int ReadI32(this Stream stream) => BinaryPrimitives.ReadInt32BigEndian(stream.ReadArray(4));
    public static long ReadI64(this Stream stream) => BinaryPrimitives.ReadInt64BigEndian(stream.ReadArray(8));
    public static Int128 ReadI128(this Stream stream) => BinaryPrimitives.ReadInt128BigEndian(stream.ReadArray(8));

    public static byte ReadU8(this Stream stream) => (byte)stream.ReadByte();
    public static ushort ReadU16(this Stream stream) => BinaryPrimitives.ReadUInt16BigEndian(stream.ReadArray(2));
    public static uint ReadU32(this Stream stream) => BinaryPrimitives.ReadUInt32BigEndian(stream.ReadArray(4));
    public static ulong ReadU64(this Stream stream) => BinaryPrimitives.ReadUInt64BigEndian(stream.ReadArray(8));
    public static UInt128 ReadU128(this Stream stream) => BinaryPrimitives.ReadUInt128BigEndian(stream.ReadArray(8));

    public static string ReadStringUTF8(this Stream stream) {
        var length = stream.ReadU32();
        return Encoding.UTF8.GetString(stream.ReadArray(length-1));
    }


    public static uint WriteI8(this Stream stream, sbyte value) { 
        stream.WriteByte((byte)value);
        return (uint)(stream.Position - 1);
    }
    public static uint WriteI16(this Stream stream, short value) {
        Span<byte> buf = new(new byte[2]);
        BinaryPrimitives.WriteInt16BigEndian(buf, value);
        stream.Write(buf);
        return (uint)(stream.Position - 2);
    }
    public static uint WriteI32(this Stream stream, int value) {
        Span<byte> buf = new(new byte[4]);
        BinaryPrimitives.WriteInt32BigEndian(buf, value);
        stream.Write(buf);
        return (uint)(stream.Position - 4);
    }
    public static uint WriteI64(this Stream stream, long value) {
        Span<byte> buf = new(new byte[8]);
        BinaryPrimitives.WriteInt64BigEndian(buf, value);
        stream.Write(buf);
        return (uint)(stream.Position - 8);
    }
    public static uint WriteI128(this Stream stream, Int128 value) {
        Span<byte> buf = new(new byte[16]);
        BinaryPrimitives.WriteInt128BigEndian(buf, value);
        stream.Write(buf);
        return (uint)(stream.Position - 16);
    }

    public static uint WriteU8(this Stream stream, byte value) {
        stream.WriteByte((byte)value);
        return (uint)(stream.Position - 1);
    }
    public static uint WriteU16(this Stream stream, ushort value) {
        Span<byte> buf = new(new byte[2]);
        BinaryPrimitives.WriteUInt16BigEndian(buf, value);
        stream.Write(buf);
        return (uint)(stream.Position - 2);
    }
    public static uint WriteU32(this Stream stream, uint value) {
        Span<byte> buf = new(new byte[4]);
        BinaryPrimitives.WriteUInt32BigEndian(buf, value);
        stream.Write(buf);
        return (uint)(stream.Position - 4);
    }
    public static uint WriteU64(this Stream stream, ulong value) {
        Span<byte> buf = new(new byte[8]);
        BinaryPrimitives.WriteUInt64BigEndian(buf, value);
        stream.Write(buf);
        return (uint)(stream.Position - 8);
    }
    public static uint WriteU128(this Stream stream, UInt128 value) {
        Span<byte> buf = new(new byte[16]);
        BinaryPrimitives.WriteUInt128BigEndian(buf, value);
        stream.Write(buf);
        return (uint)(stream.Position - 16);
    }

    public static uint WriteF32(this Stream stream, float value) {
        Span<byte> buf = new(new byte[16]);
        BinaryPrimitives.WriteSingleBigEndian(buf, value);
        stream.Write(buf);
        return (uint)(stream.Position - 4);
    }
    public static uint WriteF64(this Stream stream, double value) {
        Span<byte> buf = new(new byte[16]);
        BinaryPrimitives.WriteDoubleBigEndian(buf, value);
        stream.Write(buf);
        return (uint)(stream.Position - 8);
    }

    public static uint WriteBool(this Stream stream, bool value) { 
        stream.WriteByte((byte)(value ? 1 : 0));
        return (uint)(stream.Position - 1);
    }

    public static uint WriteStringUTF8(this Stream stream, string value) {
        byte[] buf = [.. Encoding.UTF8.GetBytes(value), 0];
        stream.WriteU32((uint)buf.Length);
        stream.Write(buf);
        return (uint)(stream.Position - buf.Length - 4);
    }
    public static uint WriteStringASCII(this Stream stream, string value) {
        byte[] buf = [.. Encoding.ASCII.GetBytes(value), 0];
        stream.WriteU32((uint)buf.Length);
        stream.Write(buf);
        return (uint)(stream.Position - buf.Length - 4);
    }

}
