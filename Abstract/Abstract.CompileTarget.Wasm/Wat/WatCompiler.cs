using System.Diagnostics;
using System.Reflection;
using Abstract.Binutils.Abs.Elf;
using Abstract.CompileTarget.Core;

namespace Abstract.CompileTarget.Wasm.Wat;

public class WatCompiler : CompilerBase
{
    public override void Compile(string outPath, ElfProgram[] source)
    {
        
        foreach (var i in source)
        {
            var module = Compiler.GenerateWasmFileFromElf(i);
            using var bytes = new MemoryStream();
            module.WriteToBinary(bytes);
            
            var fstream = File.OpenWrite($"{outPath}/{i.Name}.wasm");

            // reset file
            fstream.SetLength(0);
            fstream.Flush();
            fstream.Position = 0;

            module.WriteToBinary(fstream);
            fstream.Close();

            try {

                var process = new Process
                {
                    StartInfo = new ProcessStartInfo
                    {
                        FileName = "wasm2wat",
                        Arguments = $"\"{outPath}/{i.Name}.wasm\" -o \"{outPath}/{i.Name}.wat\"",
                        RedirectStandardOutput = true,
                        RedirectStandardError = true,
                        UseShellExecute = false,
                        CreateNoWindow = true,
                    }
                };

                process.Start();
                process.WaitForExit();

                string errorOutput = process.StandardError.ReadToEnd();

                if (process.ExitCode != 0)
                    throw new Exception("Error during translation!\n" + errorOutput);

            }
            catch (Exception ex)
            {
                Console.ForegroundColor = ConsoleColor.DarkRed;
                Console.WriteLine(ex);
                Console.ResetColor();
            }
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
