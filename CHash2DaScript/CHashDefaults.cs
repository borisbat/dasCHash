using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace CHash2Das
{
    public struct INamedTypeSymbolField
    {
        public string TypeName;
        public string Namespace;
        public string FieldName;
    }

    public class CHashDefaults
    {
        static string das_WriteLine(CHashConverter converter, InvocationExpressionSyntax invocationExpression)
        {
            return das_Write(converter, invocationExpression, true);
        }

        static string das_Write(CHashConverter converter, InvocationExpressionSyntax invocationExpression)
        {
            return das_Write(converter, invocationExpression, false);
        }

        static string das_Write(CHashConverter converter, InvocationExpressionSyntax invocationExpression, bool newLine)
        {
            var res = "print(\"";
            var num = invocationExpression.ArgumentList.Arguments.Count;
            var i = 0;
            foreach (var arg in invocationExpression.ArgumentList.Arguments)
            {
                var argStr = converter.onExpressionSyntax((arg as ArgumentSyntax).Expression);
                if (argStr.EndsWith("\""))
                {
                    var sub = argStr.Substring(1, argStr.Length - 2);
                    res += sub;
                }
                else
                    res += $"{{{argStr}}}";
                if (i < num - 1)
                    res += " ";
                i++;
            }
            if (newLine)
                res += "\\n";
            res += "\")";
            return res;
        }

        static CHashConverter.InvocationDelegate das_fn(string fnName)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                var ma = inv.Expression as MemberAccessExpressionSyntax;
                var args = converter.onArgumentListSyntax(inv.ArgumentList);
                return $"{fnName}({args})";
            };
            return res;
        }

        static CHashConverter.InvocationDelegate das_method(string fnName)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                var ma = inv.Expression as MemberAccessExpressionSyntax;
                var args = converter.onArgumentListSyntax(inv.ArgumentList);
                return $"{converter.derefExpr(ma.Expression)} |> {fnName}({args})";
            };
            return res;
        }

        static CHashConverter.InvocationDelegate das_method_reverse_args(string fnName)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                var ma = inv.Expression as MemberAccessExpressionSyntax;
                var args = converter.onArgumentReverseListSyntax(inv.ArgumentList);
                return $"{converter.derefExpr(ma.Expression)} |> {fnName}({args})";
            };
            return res;
        }

        static CHashConverter.InvocationDelegate das_method_noargs(string fnName)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                var ma = inv.Expression as MemberAccessExpressionSyntax;
                if (inv.ArgumentList.Arguments.Count != 0)
                    converter.Fail("Sort with comparer not supported");
                return $"{converter.derefExpr(ma.Expression)} |> {fnName}()";
            };
            return res;
        }

        static string das_ToString(CHashConverter converter, InvocationExpressionSyntax inv)
        {
            var ma = inv.Expression as MemberAccessExpressionSyntax;
            if (inv.Parent.IsKind(SyntaxKind.Interpolation))
                return $"{converter.derefExpr(ma.Expression)}";
            else
                return $"\"{{{converter.derefExpr(ma.Expression)}}}\"";
        }
        static string das_Count(CHashConverter converter, MemberAccessExpressionSyntax acc)
        {
            return $"{converter.derefExpr(acc.Expression)} |> length()";
        }

        // Console.CapsLock
        static string das_CapsLock(CHashConverter converter, MemberAccessExpressionSyntax acc)
        {
            return $"false // {converter.derefExpr(acc.Expression)} |> get_caps_lock()";
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

            // TODO: add `require math`
            converter.addInvocation("System.Math.Sqrt", das_fn("math::sqrt"));
            converter.addInvocation("Math.Sqrt", das_fn("math::sqrt"));
            // static member access
            converter.addMemberAccess(new INamedTypeSymbolField() { TypeName = "Console", Namespace = "<global namespace>", FieldName = "CapsLock" }, das_CapsLock);

            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "Add" }, das_method("push"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "Clear" }, das_method("clear"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "RemoveAt" }, das_method("erase"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "RemoveRange" }, das_method("erase"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "Remove" }, das_method("remove_value"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "Insert" }, das_method_reverse_args("push"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "Contains" }, das_method("has_value"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "IndexOf" }, das_method("find_index"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "Sort" }, das_method_noargs("sort"));
            converter.addMemberAccess(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "Count" }, das_Count);
            converter.addObjectMethod("ToString", das_ToString);
        }
    }
}
