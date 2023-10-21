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
    public struct INamedTypeSymbolField
    {
        public string MetadataName;
        public string ContainingNamespace;
        public string FieldName;
    }

    public class CHashDefaults
    {
        static bool isSingleString(ArgumentListSyntax list)
        {
            if (list.Arguments.Count != 1)
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
            if (!isSingleString(invocationExpression.ArgumentList))
                return $"print(StringBuilder({args}))";
            else
                return $"print({args})";
        }
        static string das_Add(CHashConverter converter, InvocationExpressionSyntax inv)
        {
            var ma = inv.Expression as MemberAccessExpressionSyntax;
            return $"*{converter.onExpressionSyntax(ma.Expression)} |> push";
        }
        static string das_Clear(CHashConverter converter, InvocationExpressionSyntax inv)
        {
            var ma = inv.Expression as MemberAccessExpressionSyntax;
            return $"*{converter.onExpressionSyntax(ma.Expression)} |> clear";
        }
        static string das_Count(CHashConverter converter, MemberAccessExpressionSyntax acc)
        {
            return $"*{converter.onExpressionSyntax(acc.Expression)} |> length()";
        }

        public static void registerInvocations(CHashConverter converter)
        {
            const string CollectionGeneric = "System.Collections.Generic";
            converter.addInvocation("System.Console.WriteLine", das_WriteLine);
            converter.addInvocation("Console.WriteLine", das_WriteLine);
            converter.addInvocation("WriteLine", das_WriteLine);

            converter.addInvocation("System.Console.Write", das_Write);
            converter.addInvocation("Console.Write", das_Write);
            converter.addInvocation("Write", das_Write);
            converter.addMethod(new INamedTypeSymbolField() { MetadataName = "List`1", ContainingNamespace = CollectionGeneric, FieldName = "Add" }, das_Add);
            converter.addMethod(new INamedTypeSymbolField() { MetadataName = "List`1", ContainingNamespace = CollectionGeneric, FieldName = "Clear" }, das_Clear);
            converter.addMemberAccess(new INamedTypeSymbolField() { MetadataName = "List`1", ContainingNamespace = CollectionGeneric, FieldName = "Count" }, das_Count);
        }
    }
}
