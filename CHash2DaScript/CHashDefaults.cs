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
        static string das_WriteLineError(CHashConverter converter, InvocationExpressionSyntax invocationExpression)
        {
            return das_Write(converter, invocationExpression, "error", true);
        }

        static string das_WriteError(CHashConverter converter, InvocationExpressionSyntax invocationExpression)
        {
            return das_Write(converter, invocationExpression, "error", false);
        }

        static string das_WriteLine(CHashConverter converter, InvocationExpressionSyntax invocationExpression)
        {
            return das_Write(converter, invocationExpression, "print", true);
        }

        static string das_Write(CHashConverter converter, InvocationExpressionSyntax invocationExpression)
        {
            return das_Write(converter, invocationExpression, "print", false);
        }

        static string das_Write(CHashConverter converter, InvocationExpressionSyntax invocationExpression, string function_name, bool newLine)
        {
            var res = $"{function_name}(\"";
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

        static string das_NameOf(CHashConverter converter, InvocationExpressionSyntax invocationExpression)
        {
            return $"\"{invocationExpression.ArgumentList.Arguments[0].Expression}\"";
        }

        static CHashConverter.InvocationDelegate req(CHashConverter.InvocationDelegate sub, string module_name)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                converter.addRequirement(module_name);
                return sub(converter, inv);
            };
            return res;
        }

        static CHashConverter.MemberAccessDelegate req(CHashConverter.MemberAccessDelegate sub, string module_name)
        {
            CHashConverter.MemberAccessDelegate res = delegate (CHashConverter converter, MemberAccessExpressionSyntax inv)
            {
                converter.addRequirement(module_name);
                return sub(converter, inv);
            };
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

        static CHashConverter.InvocationDelegate das_fn_noargs(string fnName)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                if (inv.ArgumentList.Arguments.Count != 0)
                    converter.Fail($"{fnName} with arguments not supported");
                var ma = inv.Expression as MemberAccessExpressionSyntax;
                return $"{fnName}()";
            };
            return res;
        }

        static CHashConverter.InvocationDelegate das_method(string fnName, bool doDeref = true)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                var ma = inv.Expression as MemberAccessExpressionSyntax;
                var args = converter.onArgumentListSyntax(inv.ArgumentList);
                return $"{converter.derefExpr(ma.Expression, doDeref)} |> {fnName}({args})";
            };
            return res;
        }

        static CHashConverter.InvocationDelegate das_method_reverse_args(string fnName, bool doDeref = true)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                var ma = inv.Expression as MemberAccessExpressionSyntax;
                var args = converter.onArgumentReverseListSyntax(inv.ArgumentList);
                return $"{converter.derefExpr(ma.Expression, doDeref)} |> {fnName}({args})";
            };
            return res;
        }

        static CHashConverter.InvocationDelegate das_method_noargs(string fnName, bool doDeref = true)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                var ma = inv.Expression as MemberAccessExpressionSyntax;
                if (inv.ArgumentList.Arguments.Count != 0)
                    converter.Fail($"{fnName} with comparer not supported");
                return $"{converter.derefExpr(ma.Expression, doDeref)} |> {fnName}()";
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

        static CHashConverter.MemberAccessDelegate das_member(string value, bool doDeref = true)
        {
            CHashConverter.MemberAccessDelegate res = delegate (CHashConverter converter, MemberAccessExpressionSyntax acc)
            {
                return $"{converter.derefExpr(acc.Expression, doDeref)}{value}";
            };
            return res;
        }

        static CHashConverter.MemberAccessDelegate das_static(string value)
        {
            CHashConverter.MemberAccessDelegate res = delegate (CHashConverter converter, MemberAccessExpressionSyntax acc)
            {
                return value;
            };
            return res;
        }

        public static void registerInvocations(CHashConverter converter)
        {
            const string CollectionGeneric = "System.Collections.Generic";
            const string GlobalNamespace = "System";
            converter.addInvocation("System.Console.WriteLine", das_WriteLine);
            converter.addInvocation("Console.WriteLine", das_WriteLine);
            converter.addInvocation("WriteLine", das_WriteLine);

            converter.addInvocation("System.Console.Write", das_Write);
            converter.addInvocation("Console.Write", das_Write);
            converter.addInvocation("Write", das_Write);

            converter.addInvocation("nameof", das_NameOf);

            converter.addInvocation("Debug.Fail", das_WriteError);

            var mathSqrt = req(das_fn("math::sqrt"), "math");
            converter.addInvocation("System.Math.Sqrt", mathSqrt);
            converter.addInvocation("Math.Sqrt", mathSqrt);
            // static member access
            converter.addMemberAccess(new INamedTypeSymbolField() { TypeName = "Console", Namespace = GlobalNamespace, FieldName = "CapsLock" }, das_member(" |> get_caps_lock()", false));

            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "Add" }, das_method("push"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "Clear" }, das_method("clear"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "RemoveAt" }, das_method("erase"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "RemoveRange" }, das_method("erase"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "Remove" }, das_method("remove_value"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "Insert" }, das_method_reverse_args("push"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "Contains" }, das_method("has_value"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "IndexOf" }, das_method("find_index"));
            converter.addMethod(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "Sort" }, das_method_noargs("sort"));
            converter.addMemberAccess(new INamedTypeSymbolField() { TypeName = "List`1", Namespace = CollectionGeneric, FieldName = "Count" }, das_member(" |> length()"));
            converter.addObjectMethod("ToString", das_ToString);
            converter.addObjectMethod("Invoke", das_method("invoke"));
        }
    }
}