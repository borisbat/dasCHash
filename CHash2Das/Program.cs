using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Linq.Expressions;
using System;

using CHash2Das;

namespace Main
{
    class Program
    {
        static void Main(string[] args)
        {
            string programText = System.IO.File.ReadAllText("../../../Test/test.cs");

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
            CHashDefaults.registerInvocations( hasher );

            Console.WriteLine(hasher.convert(compilation,model,root));
        }
    }
}
