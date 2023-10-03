using static System.Console;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Linq.Expressions;
using System;


namespace CHash2Das
{
    public class CHashDefaults
    {
        static bool isSingleString(ArgumentListSyntax list)
        {
            if(list.Arguments.Count != 1)
                return false;
            var kind = (list.Arguments[0] as ArgumentSyntax).Expression.Kind();
            return kind == SyntaxKind.StringLiteralExpression;
        }

        static string das_WriteLine(CHashConverter converter, InvocationExpressionSyntax invocationExpression)
        {
            var args = converter.onArgumentListSyntax(invocationExpression.ArgumentList);
            return $"print(StringBuilder({args},\"\\n\"))";
        }

        static string das_Write(CHashConverter converter, InvocationExpressionSyntax invocationExpression)
        {
            var args = converter.onArgumentListSyntax(invocationExpression.ArgumentList);
            if ( !isSingleString(invocationExpression.ArgumentList) )
                return $"print(StringBuilder({args}))";
            else
                return $"print({args})";
        }

        public static void registerInvocations(CHashConverter converter)
        {
            converter.addInvocation("System.Console.WriteLine", das_WriteLine);
            converter.addInvocation("Console.WriteLine", das_WriteLine);
            converter.addInvocation("WriteLine", das_WriteLine);

            converter.addInvocation("System.Console.Write", das_Write);
            converter.addInvocation("Console.Write", das_Write);
            converter.addInvocation("Write", das_Write);
        }
    }
}
