using System.Reflection;
using Abstract.Binutils.Abs.Elf;
using Abstract.CompileTarget.Core;
using Abstract.CompileTarget.Core.Enums;

namespace Abstract.CompileTarget.Wasm.Wasm;

public class WasmCompiler : CompilerBase
{
    public override ExpectedELFFormat ExpectedELFFormat => ExpectedELFFormat.single;

    public override void Compile(string outPath, ElfProgram[] source)
    {
        
        foreach (var i in source)
        {
            var module = Compiler.GenerateWasmFileFromElf(i);
            var fstream = File.OpenWrite($"{outPath}/{i.Name}.wasm");

            // reset file
            fstream.SetLength(0);
            fstream.Flush();
            fstream.Position = 0;

            // write .wasm
            module.WriteToBinary(fstream);
            fstream.Close();
        }

        // copy web dependences
        var assembly = Assembly.GetExecutingAssembly();
        var asmname = assembly.GetName().Name;

        (string from, string to)[] toCopyResources = [
            ($"{asmname}.webProgram.index.html", "index.html"),
            ($"{asmname}.webProgram.script.js",  "script.js"),
            ($"{asmname}.webProgram.std.js",     "std.js"),
        ];

        foreach (var (from, to) in toCopyResources)
        {
            using Stream stream = assembly.GetManifestResourceStream(from)!;
            using StreamReader reader = new(stream);

            var content = reader.ReadToEnd();
            content = content.Replace("cs.replace.webassembly.path", $"./{source[0].Name}.wasm");

            File.WriteAllText($"{outPath}/{to}", content);
        }

    }
}
