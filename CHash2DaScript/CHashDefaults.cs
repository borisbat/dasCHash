using System;
using System.Linq;
using System.Diagnostics;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace CHash2Das
{
    public struct TypeData
    {
        public string type;
        public string ns;
    }

    public struct TypeField
    {
        public string type;
        public string ns;
        public string field;

        public TypeField(TypeData ti, string field)
        {
            type = ti.type;
            ns = ti.ns;
            this.field = field;
        }
    }

    public struct OperatorOverload
    {
        public string type;
        public string ns;
        public SyntaxKind kind;

        public OperatorOverload(TypeData ti, SyntaxKind kind)
        {
            type = ti.type;
            ns = ti.ns;
            this.kind = kind;
        }
    }

    public partial class CHashDefaults
    {
        const string GlobalNS = "<global namespace>";
        const string SystemNS = "System";
        const string CollectionNS = "System.Collections.Generic";

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

        static CHashConverter.TypeRenameDelegate req(CHashConverter.TypeRenameDelegate sub, string module_name)
        {
            CHashConverter.TypeRenameDelegate res = delegate (CHashConverter converter, TypeData td)
            {
                converter.addRequirement(module_name);
                return sub(converter, td);
            };
            return res;
        }

        static CHashConverter.InvocationDelegate das_fn(string fnName, bool genericTypes = true)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                var args = converter.onArgumentListSyntax(inv, genericTypes: genericTypes);
                return $"{fnName}{args}";
            };
            return res;
        }

        static CHashConverter.InvocationDelegate das_member(string member, bool genericTypes = true, bool addBrackets = false)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                var args = converter.onArgumentListSyntax(inv, addBrackets: addBrackets, genericTypes: genericTypes);
                return $"{args}{member}";
            };
            return res;
        }

        static CHashConverter.InvocationDelegate das_fn_noargs(string fnName)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                if (inv.ArgumentList.Arguments.Count != 0)
                    converter.Fail($"{fnName} with arguments not supported");
                return $"{fnName}()";
            };
            return res;
        }

        static string expressionName(CHashConverter converter, InvocationExpressionSyntax inv, bool doDeref)
        {
            if (inv.Expression is MemberAccessExpressionSyntax mae)
                return converter.derefExpr(mae.Expression, doDeref);
            if (inv.Expression is GenericNameSyntax)
                return "";
            return converter.derefExpr(inv.Expression, doDeref);
        }

        static CHashConverter.InvocationDelegate das_method(string fnName, bool doDeref = true, bool genericTypes = true)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                var args = converter.onArgumentListSyntax(inv, genericTypes: genericTypes);
                var self = expressionName(converter, inv, doDeref);
                var call = $"{fnName}{args}";
                return self == "" ? call : $"{self}->{call}";
            };
            return res;
        }

        static CHashConverter.InvocationDelegate das_method_fn(string fnName, bool doDeref = true, bool genericTypes = true)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                var args = converter.onArgumentListSyntax(inv, genericTypes: genericTypes);
                var self = expressionName(converter, inv, doDeref);
                var call = $"{fnName}{args}";
                return self == "" ? call : $"{self} |> {call}";
            };
            return res;
        }

        static CHashConverter.InvocationDelegate das_method_fn_reverse_args(string fnName, bool doDeref = true, bool genericTypes = true)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                var args = converter.onArgumentListSyntax(inv, genericTypes, reverseArgs: true);
                var self = expressionName(converter, inv, doDeref);
                var call = $"{fnName}{args}";
                return self == "" ? call : $"{self} |> {call}";
            };
            return res;
        }

        static CHashConverter.InvocationDelegate das_method_noargs(string fnName, bool doDeref = true)
        {
            CHashConverter.InvocationDelegate res = delegate (CHashConverter converter, InvocationExpressionSyntax inv)
            {
                if (inv.ArgumentList.Arguments.Count != 0)
                    converter.Fail($"{fnName} with comparer not supported");
                var self = expressionName(converter, inv, doDeref);
                var call = $"{fnName}()";
                return self == "" ? call : $"{self} |> {call}";
            };
            return res;
        }

        struct LeftRightExpr
        {
            public ExpressionSyntax Left;
            public ExpressionSyntax Right;
        }

        static LeftRightExpr get_left_right(ExpressionSyntax expr)
        {
            if (expr is BinaryExpressionSyntax binop)
                return new LeftRightExpr() { Left = binop.Left, Right = binop.Right };
            if (expr is AssignmentExpressionSyntax assign)
                return new LeftRightExpr() { Left = assign.Left, Right = assign.Right };
            return new LeftRightExpr() { Left = default, Right = default };
        }

        static CHashConverter.OperatorOverloadDelegate das_bin_operator_raw(string fnName, bool doDeref = true, bool genericTypes = true)
        {
            CHashConverter.OperatorOverloadDelegate res = delegate (CHashConverter converter, ExpressionSyntax expr)
            {
                var binop = get_left_right(expr);
                var left = converter.onExpressionSyntax(binop.Left);
                var right = converter.onExpressionArgumentSyntax(binop.Right);
                return $"{left}{fnName}{right}";
            };
            return res;
        }

        static string das_ToString(CHashConverter converter, InvocationExpressionSyntax inv)
        {
            if (inv.Parent.IsKind(SyntaxKind.Interpolation))
                return $"{expressionName(converter, inv, true)}";
            else
                return $"\"{{{expressionName(converter, inv, true)}}}\"";
        }

        /// <summary>
        /// Returns a member access delegate that returns a raw member access string,
        /// useful for static member access. Be careful with this one, pass actual member name with the dot.
        /// </summary>
        static CHashConverter.MemberAccessDelegate das_raw_member(string value, bool doDeref = true)
        {
            CHashConverter.MemberAccessDelegate res = delegate (CHashConverter converter, MemberAccessExpressionSyntax acc)
            {
                if (acc == null)
                    return $"self{value}";
                return $"{converter.derefExpr(acc.Expression, doDeref)}{value}";
            };
            return res;
        }

        static CHashConverter.SetMemberAccessDelegate das_raw_set_member(string value, bool doDeref = true)
        {
            CHashConverter.SetMemberAccessDelegate res = delegate (CHashConverter converter, AssignmentExpressionSyntax ae)
            {
                return $"{converter.derefExpr(ae.Left, doDeref)}{value}{converter.derefExpr(ae.Right)}";
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
        static CHashConverter.CtorDelegate das_raw_ctor(string value)
        {
            CHashConverter.CtorDelegate res = delegate (CHashConverter converter, ObjectCreationExpressionSyntax oce)
            {
                if (oce.ArgumentList == null)
                    return value;
                var arguments = oce.ArgumentList.Arguments.Select(arg => converter.onExpressionSyntax(arg.Expression)).Aggregate((current, next) => $"{current}, {next}");
                return $"{value}({arguments})";
            };
            return res;
        }

        static CHashConverter.CtorDelegate das_ctor_args(string value)
        {
            CHashConverter.CtorDelegate res = delegate (CHashConverter converter, ObjectCreationExpressionSyntax oce)
            {
                if (oce.ArgumentList == null)
                    return value;
                var result = value;
                var arguments = oce.ArgumentList.Arguments.Select(arg => converter.onExpressionSyntax(arg.Expression));
                var idx = 0;
                var unknownArgs = false;
                foreach (var arg in arguments)
                {
                    var argName = $"%{idx}%";
                    if (result.Contains(argName))
                    {
                        result = result.Replace(argName, arg);
                    }
                    else
                    {
                        if (unknownArgs)
                            result += ", ";
                        else
                            result += "(";
                        unknownArgs = true;
                        result += arg;
                    }
                    idx++;
                }
                if (unknownArgs)
                    result += ")";
                return result;
            };
            return res;
        }

        static CHashConverter.TypeRenameDelegate das_type_name(string value)
        {
            CHashConverter.TypeRenameDelegate res = delegate (CHashConverter converter, TypeData type)
            {
                return value;
            };
            return res;
        }

        static CHashConverter.UsingRenameDelegate das_using(string value)
        {
            CHashConverter.UsingRenameDelegate res = delegate (CHashConverter converter, string usingName)
            {
                return $"{value}";
            };
            return res;
        }

        public static void registerInvocations(CHashConverter converter)
        {
            var consoleTd = new TypeData() { type = nameof(Console), ns = SystemNS };
            converter.addMethod(consoleTd, nameof(Console.Write), das_Write);
            converter.addMethod(consoleTd, nameof(Console.WriteLine), das_WriteLine);
            // static member access
            converter.addField(consoleTd, "CapsLock", das_raw_member(" |> get_caps_lock()", false));

            var debugTd = new TypeData() { type = nameof(Debug), ns = SystemNS };
            converter.addMethod(debugTd, nameof(Debug.Fail), das_WriteError);

            converter.addInvocation("nameof", das_NameOf);

            var mathTd = new TypeData() { type = nameof(Math), ns = SystemNS };
            converter.addField(mathTd, nameof(Math.PI), req(das_static("PI"), "math"));
            converter.addField(mathTd, nameof(Math.Tau), req(das_static("2. * PI"), "math"));
            converter.addMethod(mathTd, nameof(Math.Sqrt), req(das_fn("sqrt"), "math"));
            converter.addMethod(mathTd, nameof(Math.Sin), req(das_fn("sin"), "math"));
            converter.addMethod(mathTd, nameof(Math.Cos), req(das_fn("cos"), "math"));
            converter.addMethod(mathTd, nameof(Math.Tan), req(das_fn("tan"), "math"));
            converter.addMethod(mathTd, nameof(Math.Asin), req(das_fn("asin"), "math"));
            converter.addMethod(mathTd, nameof(Math.Acos), req(das_fn("acos"), "math"));
            converter.addMethod(mathTd, nameof(Math.Atan), req(das_fn("atan"), "math"));
            converter.addMethod(mathTd, nameof(Math.Atan2), req(das_fn("atan2"), "math"));
            converter.addMethod(mathTd, nameof(Math.Pow), req(das_fn("pow"), "math"));
            converter.addMethod(mathTd, nameof(Math.Abs), req(das_fn("abs"), "math"));
            converter.addMethod(mathTd, nameof(Math.Round), req(das_fn("round"), "math"));
            converter.addMethod(mathTd, nameof(Math.Max), req(das_fn("max"), "math"));
            converter.addMethod(mathTd, nameof(Math.Min), req(das_fn("min"), "math"));
            converter.addMethod(mathTd, nameof(Math.Log), req(das_fn("log"), "math"));
            converter.addMethod(mathTd, nameof(Math.Log2), req(das_fn("log2"), "math"));
            converter.addMethod(mathTd, nameof(Math.Ceiling), req(das_fn("ceil"), "math"));
            converter.addMethod(mathTd, nameof(Math.Floor), req(das_fn("floor"), "math"));
            converter.addMethod(mathTd, nameof(Math.Exp), req(das_fn("exp"), "math"));
            converter.addMethod(mathTd, nameof(Math.Clamp), req(das_fn("clamp"), "math"));

            var mathfTd = new TypeData() { type = nameof(MathF), ns = SystemNS };
            converter.addField(mathfTd, nameof(MathF.PI), req(das_static("PI"), "math"));
            converter.addField(mathfTd, nameof(MathF.Tau), req(das_static("2. * PI"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Sqrt), req(das_fn("sqrt"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Sin), req(das_fn("sin"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Cos), req(das_fn("cos"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Tan), req(das_fn("tan"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Asin), req(das_fn("asin"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Acos), req(das_fn("acos"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Atan), req(das_fn("atan"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Pow), req(das_fn("pow"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Round), req(das_fn("round"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Max), req(das_fn("max"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Min), req(das_fn("min"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Log), req(das_fn("log"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Log2), req(das_fn("log2"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Ceiling), req(das_fn("ceil"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Floor), req(das_fn("floor"), "math"));
            converter.addMethod(mathfTd, nameof(MathF.Exp), req(das_fn("exp"), "math"));

            var listTd = new TypeData() { type = "List`1", ns = CollectionNS };
            converter.addMethod(listTd, "Add", das_method_fn("push"));
            converter.addMethod(listTd, "Clear", das_method_fn("clear"));
            converter.addMethod(listTd, "RemoveAt", das_method_fn("erase"));
            converter.addMethod(listTd, "RemoveRange", das_method_fn("erase"));
            converter.addMethod(listTd, "Remove", das_method_fn("remove_value"));
            converter.addMethod(listTd, "Insert", das_method_fn_reverse_args("push"));
            converter.addMethod(listTd, "Contains", das_method_fn("has_value"));
            converter.addMethod(listTd, "IndexOf", das_method_fn("find_index"));
            converter.addMethod(listTd, "Sort", das_method_noargs("sort"));
            converter.addField(listTd, "Count", das_raw_member(" |> length()"));

            var arrayTd = new TypeData() { type = "Array", ns = SystemNS };
            converter.addField(arrayTd, nameof(Array.Length), das_raw_member(" |> length()"));

            var stringTd = new TypeData() { type = nameof(String), ns = SystemNS };
            converter.addCtor(stringTd, das_raw_ctor(""));
            converter.addField(stringTd, nameof(String.Empty), das_static("\"\""));
            converter.addMethod(stringTd, nameof(String.IsNullOrEmpty), das_fn("empty"));

            converter.addObjectMethod("ToString", das_ToString);

            var delegateTd = new TypeData() { type = nameof(Delegate), ns = SystemNS };
            converter.addMethod(delegateTd, "Invoke", das_method_fn("invoke"));

            var actionTd = new TypeData() { type = nameof(Action), ns = SystemNS };
            converter.renameType(actionTd, das_type_name("lambda<void>"));
            converter.addOperatorOverload(actionTd, SyntaxKind.AddAssignmentExpression, das_bin_operator_raw(" |> AddListener() <| "));
            // converter.instantiateTemplate(new TypeData() { type = "Vec", ns = "HelloWorld" }, new string[] { "int" });

            // converter.renameUsing("System.Collections.Generic", das_using("require daslib/array_boost"));

            // converter.addMethod(new TypeField() { type = "ContTest", ns = "HelloWorld", field = "StaticGetEmptyCont" }, das_method("__StaticGetEmptyCont"));
            // converter.addMethod(new TypeField() { type = "ContTest", ns = "HelloWorld", field = "StaticGetCont" }, das_method("__StaticGetEmptyCont"));
            // converter.addMethod(new TypeField() { type = "ContTest", ns = "HelloWorld", field = "GetEmptyCont" }, das_method("__GetEmptyCont"));
            // converter.addMethod(new TypeField() { type = "ContTest", ns = "HelloWorld", field = "GetCont" }, das_method("__GetCont"));
        }
    }
}