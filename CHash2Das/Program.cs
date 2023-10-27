using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;

using CHash2Das;
using System.IO;

namespace Main
{
    class Program
    {
        const string OUTPUT_PATH = "../../../Test";
        static void Main(string[] args)
        {
            string programText = System.IO.File.ReadAllText(Path.Combine(OUTPUT_PATH, "test.cs"));

            // creating syntax tree
            SyntaxTree tree = CSharpSyntaxTree.ParseText(programText);
            CompilationUnitSyntax root = tree.GetCompilationUnitRoot();

            // creating compilation and semantic model
            var compilation = CSharpCompilation.Create("HelloWorld")
                .AddReferences(MetadataReference.CreateFromFile(
                    typeof(string).Assembly.Location))
                .AddSyntaxTrees(tree);
            SemanticModel model = compilation.GetSemanticModel(tree);

            CHashConverter hasher = new CHashConverter();
            CHashDefaults.registerInvocations(hasher);

            var result = hasher.convert(compilation, model, root);
            Console.WriteLine(result);
            System.IO.File.WriteAllText(Path.Combine(OUTPUT_PATH, "test.das"), result);
        }
    }
}
