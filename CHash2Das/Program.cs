using static System.Console;
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

        // <Snippet1>
        const string programText =
@"using System;
using System.Collections;
using System.Linq;
using System.Text;

namespace HelloWorld
{
    class Program
    {
        static void Main(string[] args)
        {
            WriteLine(""Hello, World!"");
            Console.WriteLine(""Hello, World!"");
            System.Console.WriteLine(""Hello, World!"");

            Write(""Hello, World!\\n"");
            Console.Write(""Hello, World!\\n"");
            System.Console.Write(""Hello, World!\\n"");

        }
        static double add(double a, double b)
        {
            return a + b;
        }
    }
}";
        static string das_WriteLine ( CHashConverter converter, InvocationExpressionSyntax invocationExpression )
        {
            var args = converter.onArgumentListSyntax(invocationExpression.ArgumentList);
            return $"print(({args})+\"\\n\")";
        }

        static string das_Write(CHashConverter converter, InvocationExpressionSyntax invocationExpression)
        {
            var args = converter.onArgumentListSyntax(invocationExpression.ArgumentList);
            return $"print({args})";
        }

        static void registerInvocations ( CHashConverter converter )
        {
            converter.addInvocation("System.Console.WriteLine", das_WriteLine);
            converter.addInvocation("Console.WriteLine", das_WriteLine);
            converter.addInvocation("WriteLine", das_WriteLine);

            converter.addInvocation("System.Console.Write", das_Write);
            converter.addInvocation("Console.Write", das_Write);
            converter.addInvocation("Write", das_Write);
        }

        static void Main(string[] args)
        {
            SyntaxTree tree = CSharpSyntaxTree.ParseText(programText);
            CompilationUnitSyntax root = tree.GetCompilationUnitRoot();

            CHashConverter hasher = new CHashConverter();
            registerInvocations( hasher );

            WriteLine(hasher.convert(root));

            Console.Write("blah");
        }
    }
}
