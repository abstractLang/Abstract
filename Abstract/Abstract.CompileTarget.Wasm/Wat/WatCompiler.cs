using System.Diagnostics;
using Abstract.Binutils.Abs.Elf;
using Abstract.CompileTarget.Core;

namespace Abstract.CompileTarget.Wasm.Wasm;

public class WatCompiler : CompilerBase
{
    public override void Compile(
        (string dir, string name) outPath,
        ElfProgram[] source)
    {
        
        foreach (var i in source)
        {
            var module = Compiler.GenerateWasmFileFromElf(i);
            using var bytes = new MemoryStream();
            module.WriteToBinary(bytes);
            
            var fstream = File.OpenWrite($"{outPath.dir}/{i.Name}.wasm");

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
                        Arguments = $"\"{outPath.dir}/{i.Name}.wasm\" -o \"{outPath.dir}/{i.Name}.wat\"",
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

    }
}
