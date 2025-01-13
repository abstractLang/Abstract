using Abstract.Binutils.Abs.Elf;
using Abstract.CompileTarget.Core;

namespace Abstract.CompileTarget.Wasm.Wasm;

public class WasmCompiler : CompilerBase
{
    public override void Compile(
        (string dir, string name) outPath,
        ElfProgram[] source)
    {
        
        foreach (var i in source)
        {
            var module = Compiler.GenerateWasmFileFromElf(i);
            var fstream = File.OpenWrite($"{outPath.dir}/{i.Name}.wasm");

            // reset file
            fstream.SetLength(0);
            fstream.Flush();
            fstream.Position = 0;

            module.WriteToBinary(fstream);
            fstream.Close();
        }

    }
}
