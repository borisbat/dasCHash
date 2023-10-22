using static System.Console;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp.Symbols;
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
            var contTypeInfo = converter.semanticModel.GetTypeInfo(ma.Expression);
            var args = converter.onArgumentListSyntaxCast(inv.ArgumentList, (contTypeInfo.Type as INamedTypeSymbol).TypeArguments[0], new bool[] { true });
            var artType = converter.semanticModel.GetTypeInfo(ma.Expression);
            var deref = converter.isPointerType(artType.Type) ? "*" : "";
            return $"{deref}{converter.onExpressionSyntax(ma.Expression)} |> push({args})";
        }
        static string das_Insert(CHashConverter converter, InvocationExpressionSyntax inv)
        {
            var ma = inv.Expression as MemberAccessExpressionSyntax;
            var contTypeInfo = converter.semanticModel.GetTypeInfo(ma.Expression);
            var args = converter.onArgumentReverseListSyntaxCast(inv.ArgumentList, (contTypeInfo.Type as INamedTypeSymbol).TypeArguments[0], new bool[] { false, true });
            var artType = converter.semanticModel.GetTypeInfo(ma.Expression);
            var deref = converter.isPointerType(artType.Type) ? "*" : "";
            return $"{deref}{converter.onExpressionSyntax(ma.Expression)} |> push({args})";
        }
        static string das_Contains(CHashConverter converter, InvocationExpressionSyntax inv)
        {
            var ma = inv.Expression as MemberAccessExpressionSyntax;
            var contTypeInfo = converter.semanticModel.GetTypeInfo(ma.Expression);
            var args = converter.onArgumentListSyntaxCast(inv.ArgumentList, (contTypeInfo.Type as INamedTypeSymbol).TypeArguments[0], new bool[] { true });
            var artType = converter.semanticModel.GetTypeInfo(ma.Expression);
            var deref = converter.isPointerType(artType.Type) ? "*" : "";
            return $"{deref}{converter.onExpressionSyntax(ma.Expression)} |> has_value({args})";
        }
        static string das_IndexOf(CHashConverter converter, InvocationExpressionSyntax inv)
        {
            var ma = inv.Expression as MemberAccessExpressionSyntax;
            var contTypeInfo = converter.semanticModel.GetTypeInfo(ma.Expression);
            var args = converter.onArgumentListSyntaxCast(inv.ArgumentList, (contTypeInfo.Type as INamedTypeSymbol).TypeArguments[0], new bool[] { true });
            var artType = converter.semanticModel.GetTypeInfo(ma.Expression);
            var deref = converter.isPointerType(artType.Type) ? "*" : "";
            return $"{deref}{converter.onExpressionSyntax(ma.Expression)} |> find_index({args})";
        }
        static string das_Sort(CHashConverter converter, InvocationExpressionSyntax inv)
        {
            var ma = inv.Expression as MemberAccessExpressionSyntax;
            var contTypeInfo = converter.semanticModel.GetTypeInfo(ma.Expression);
            if (inv.ArgumentList.Arguments.Count != 0)
                converter.Fail("Sort with comparer not supported");
            var artType = converter.semanticModel.GetTypeInfo(ma.Expression);
            var deref = converter.isPointerType(artType.Type) ? "*" : "";
            return $"{deref}{converter.onExpressionSyntax(ma.Expression)} |> sort()";
        }
        static string das_Clear(CHashConverter converter, InvocationExpressionSyntax inv)
        {
            var ma = inv.Expression as MemberAccessExpressionSyntax;
            var artType = converter.semanticModel.GetTypeInfo(ma.Expression);
            var deref = converter.isPointerType(artType.Type) ? "*" : "";
            return $"{deref}{converter.onExpressionSyntax(ma.Expression)} |> clear()";
        }
        static string das_RemoveAt(CHashConverter converter, InvocationExpressionSyntax inv)
        {
            var ma = inv.Expression as MemberAccessExpressionSyntax;
            var artType = converter.semanticModel.GetTypeInfo(ma.Expression);
            var deref = converter.isPointerType(artType.Type) ? "*" : "";
            return $"{deref}{converter.onExpressionSyntax(ma.Expression)} |> erase({converter.onArgumentListSyntax(inv.ArgumentList)})";
        }

        static string das_Remove(CHashConverter converter, InvocationExpressionSyntax inv)
        {
            var ma = inv.Expression as MemberAccessExpressionSyntax;
            var contTypeInfo = converter.semanticModel.GetTypeInfo(ma.Expression);
            var args = converter.onArgumentListSyntaxCast(inv.ArgumentList, (contTypeInfo.Type as INamedTypeSymbol).TypeArguments[0], new bool[] { true });
            var artType = converter.semanticModel.GetTypeInfo(ma.Expression);
            var deref = converter.isPointerType(artType.Type) ? "*" : "";
            return $"{deref}{converter.onExpressionSyntax(ma.Expression)} |> remove_value({args})";
        }

        static string das_ToString(CHashConverter converter, InvocationExpressionSyntax inv)
        {
            var ma = inv.Expression as MemberAccessExpressionSyntax;
            var artType = converter.semanticModel.GetTypeInfo(ma.Expression);
            var deref = converter.isPointerType(artType.Type) ? "*" : "";
            return $"({deref}{converter.onExpressionSyntax(ma.Expression)})"; // maybe string(*<arg>)
        }
        static string das_Count(CHashConverter converter, MemberAccessExpressionSyntax acc)
        {
            var artType = converter.semanticModel.GetTypeInfo(acc.Expression);
            var deref = converter.isPointerType(artType.Type) ? "*" : "";
            return $"{deref}{converter.onExpressionSyntax(acc.Expression)} |> length()";
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
            converter.addMethod(new INamedTypeSymbolField() { MetadataName = "List`1", ContainingNamespace = CollectionGeneric, FieldName = "RemoveAt" }, das_RemoveAt);
            converter.addMethod(new INamedTypeSymbolField() { MetadataName = "List`1", ContainingNamespace = CollectionGeneric, FieldName = "RemoveRange" }, das_RemoveAt);
            converter.addMethod(new INamedTypeSymbolField() { MetadataName = "List`1", ContainingNamespace = CollectionGeneric, FieldName = "Remove" }, das_Remove);
            converter.addMethod(new INamedTypeSymbolField() { MetadataName = "List`1", ContainingNamespace = CollectionGeneric, FieldName = "Insert" }, das_Insert);
            converter.addMethod(new INamedTypeSymbolField() { MetadataName = "List`1", ContainingNamespace = CollectionGeneric, FieldName = "Contains" }, das_Contains);
            converter.addMethod(new INamedTypeSymbolField() { MetadataName = "List`1", ContainingNamespace = CollectionGeneric, FieldName = "IndexOf" }, das_IndexOf);
            converter.addMethod(new INamedTypeSymbolField() { MetadataName = "List`1", ContainingNamespace = CollectionGeneric, FieldName = "Sort" }, das_Sort);
            converter.addMethod(new INamedTypeSymbolField() { MetadataName = "List`1", ContainingNamespace = CollectionGeneric, FieldName = "ToString" }, das_ToString);
            converter.addMemberAccess(new INamedTypeSymbolField() { MetadataName = "List`1", ContainingNamespace = CollectionGeneric, FieldName = "Count" }, das_Count);
        }
    }
}
