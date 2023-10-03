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

            SyntaxTree tree = CSharpSyntaxTree.ParseText(programText);
            CompilationUnitSyntax root = tree.GetCompilationUnitRoot();

            CHashConverter hasher = new CHashConverter();
            CHashDefaults.registerInvocations( hasher );

            Console.WriteLine(hasher.convert(root));
        }
    }
}
