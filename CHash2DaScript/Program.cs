using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;

using CHash2Das;
using System.IO;
using System.Collections.Immutable;
using static System.Net.Mime.MediaTypeNames;

namespace Main
{
    class Program
    {
        const string OUTPUT_PATH = "../../../Test";
        static void Main(string[] args)
        {
            string path = Path.Combine(OUTPUT_PATH, "test.cs");
            string programText = System.IO.File.ReadAllText(path);

            // creating syntax tree
            SyntaxTree tree = CSharpSyntaxTree.ParseText(programText);
            CompilationUnitSyntax root = tree.GetCompilationUnitRoot();

            // collect all dlls inside dllsPath
            var dlls = new List<string>();
            dlls.AddRange(Directory.GetFiles(Path.GetDirectoryName(typeof(string).Assembly.Location), "*.dll"));

            // creating compilation and semantic model
            var compilation = CSharpCompilation.Create("HelloWorld")
                .AddReferences(dlls.Select(dll => MetadataReference.CreateFromFile(dll)))
                .AddSyntaxTrees(tree);
            SemanticModel model = compilation.GetSemanticModel(tree);

            string result = "";

            var diagnostics = compilation.GetDiagnostics();
            var errors = diagnostics.Where(diag => diag.Severity == DiagnosticSeverity.Error);
            if (errors.Any())
            {
                var errorsValue = "";
                foreach (var error in errors)
                {
                    if (error.Id == "CS5001") continue; // entry point not found
                    var error_text = $"{error.Id}: {error.GetMessage()} at {error.Location}";
                    errorsValue += $"//{error_text}\n";
                }
                if (errorsValue.Length > 0)
                {
                    result += "// Compilation errors:\n";
                    result += errorsValue;
                    result += "\n";
                }
            }

            result += $"module {Path.GetFileNameWithoutExtension(path)}\n\n";

            CHashConverter hasher = new CHashConverter();
            CHashDefaults.registerInvocations(hasher);

            result += hasher.convert(compilation, model, root);
            Console.WriteLine(result);
            System.IO.File.WriteAllText(Path.Combine(OUTPUT_PATH, "test.das"), result);
        }
    }
}
