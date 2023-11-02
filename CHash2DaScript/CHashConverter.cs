using static System.Console;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Diagnostics;
using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.Text;
using System.Text.RegularExpressions;

namespace CHash2Das
{
    public class CHashConverter
    {
        bool failToDebug = true;
        int tabs = 0;
        int tempVars = 0;
        public SemanticModel semanticModel;
        CSharpCompilation compilation;
        Dictionary<int, SyntaxTrivia> allComments = new Dictionary<int, SyntaxTrivia>();

        List<string> requirements = new List<string>();

        public void addRequirement(string module_name)
        {
            if (!requirements.Contains(module_name))
                requirements.Add(module_name);
        }

        public void Fail(string message)
        {
            if (failToDebug)
                Debug.Fail(message);
            else
                Console.WriteLine(message);
        }

        public void Log(string message)
        {
            Console.WriteLine(message);
        }

        string onVarTypeSyntax(TypeSyntax ts)
        {
            var tt = semanticModel.GetTypeInfo(ts);
            var txt = onTypeSyntax(ts);
            if (isPointerType(tt.Type)) txt += "?";
            return txt;
        }

        string onTypeSyntax(TypeSyntax type)
        {
            if (type == null)
            {
                return "void";
            }

            switch (type.Kind())
            {
                case SyntaxKind.ArrayType:
                    {
                        var atype = type as ArrayTypeSyntax;
                        var ranks = "";
                        var tname = "";
                        int count = 0;
                        foreach (ArrayRankSpecifierSyntax rank in atype.RankSpecifiers)
                        {
                            ranks += "[";
                            var first = true;
                            foreach (ExpressionSyntax size in rank.Sizes)
                            {
                                if (first) first = false;
                                else ranks += ",";
                                ranks += onExpressionSyntax(size);
                                tname += "array<";
                                count++;
                            }
                            ranks += "]";
                        }
                        var tail = new string('>', count);
                        return $"{tname}{onVarTypeSyntax(atype.ElementType)}{tail} /*{atype.ElementType}{ranks}*/";
                    }
                case SyntaxKind.PredefinedType:
                    {
                        var ptype = type as PredefinedTypeSyntax;
                        switch (ptype.Keyword.Text)
                        {
                            case "void":
                            case "string":
                            case "int":
                            case "float":
                            case "double":
                            case "bool":
                                return ptype.Keyword.Text;
                            case "sbyte": return "int8";
                            case "byte": return "uint8";
                            case "uint": return "uint";
                            case "short": return "int16";
                            case "ushort": return "uint16";
                            case "long": return "int64";
                            case "ulong": return "uint64";
                            default:
                                Fail($"unknown PredefinedType keyword {ptype.Keyword}");
                                return $"{ptype.Keyword.Text}";
                        }
                    }
                case SyntaxKind.IdentifierName:
                    {
                        var itype = type as IdentifierNameSyntax;
                        switch (itype.Identifier.Text)
                        {
                            case "Int16": return "int16";
                            case "UInt16": return "uint16";
                            case "Int32": return "int";
                            case "UInt32": return "uint";
                            case "Int64": return "int64";
                            case "UInt64": return "uint64";
                            case "var": return "var";       // huh?

                            default:
                                // Fail($"unknown identifier type {itype.Identifier.Text}");
                                return $"{itype.Identifier.Text}";
                        }
                    }
                case SyntaxKind.GenericName:
                    {
                        var genn = type as GenericNameSyntax;
                        switch (genn.Identifier.Text)
                        {
                            case "Dictionary":
                                {
                                    if (genn.TypeArgumentList.Arguments.Count == 2)
                                    {
                                        return $"table<{onTypeSyntax(genn.TypeArgumentList.Arguments[0])}; {onTypeSyntax(genn.TypeArgumentList.Arguments[1])}>";
                                    }
                                    break;
                                }
                            case "List":
                                if (genn.TypeArgumentList.Arguments.Count == 1)
                                {
                                    return $"array<{onTypeSyntax(genn.TypeArgumentList.Arguments[0])}>";
                                }
                                break;
                        }
                        Fail($"unsupported TypeSyntax {genn}");
                        return $"{type}";
                    }
                case SyntaxKind.QualifiedName:
                    {
                        var cname = type as QualifiedNameSyntax;
                        return $"{onTypeSyntax(cname.Left)}::{onTypeSyntax(cname.Right)}";
                    }
                default:
                    Fail($"unsupported TypeSyntax {type.Kind()}");
                    return $"{type}";
            }
        }

        public delegate string InvocationDelegate(CHashConverter converter, InvocationExpressionSyntax inv);
        public delegate string MemberAccessDelegate(CHashConverter converter, MemberAccessExpressionSyntax inv);

        Dictionary<string, InvocationDelegate> onInvExpr = new Dictionary<string, InvocationDelegate>();
        Dictionary<string, InvocationDelegate> objectInvExpr = new Dictionary<string, InvocationDelegate>();
        Dictionary<INamedTypeSymbolField, InvocationDelegate> methodInvExpr = new Dictionary<INamedTypeSymbolField, InvocationDelegate>();
        Dictionary<INamedTypeSymbolField, MemberAccessDelegate> memberAccessExpr = new Dictionary<INamedTypeSymbolField, MemberAccessDelegate>();

        public void addInvocation(string key, InvocationDelegate inv)
        {
            if (onInvExpr.ContainsKey(key))
            {
                Debug.Fail("invocation expression {key} is already declared");
                return;
            }
            onInvExpr[key] = inv;
        }

        public void addMethod(INamedTypeSymbolField typeWithMethod, InvocationDelegate inv)
        {
            if (methodInvExpr.ContainsKey(typeWithMethod))
            {
                Debug.Fail($"method {typeWithMethod.TypeName}.{typeWithMethod.FieldName} is already declared");
                return;
            }
            methodInvExpr[typeWithMethod] = inv;
        }
        public void addObjectMethod(string member, InvocationDelegate inv)
        {
            if (objectInvExpr.ContainsKey(member))
            {
                Debug.Fail($"method Object.{member} is already declared");
                return;
            }
            objectInvExpr[member] = inv;
        }

        public void addMemberAccess(INamedTypeSymbolField typeWithMethod, MemberAccessDelegate acc)
        {
            if (memberAccessExpr.ContainsKey(typeWithMethod))
            {
                Debug.Fail($"member access {typeWithMethod.TypeName}.{typeWithMethod.FieldName} is already declared");
                return;
            }
            memberAccessExpr[typeWithMethod] = acc;
        }

        public string onArgumentListSyntax(ArgumentListSyntax argumentList)
        {
            return string.Join(", ", argumentList.Arguments.Select(arg => onExpressionSyntax(arg.Expression)));
        }

        public string onArgumentReverseListSyntax(ArgumentListSyntax argumentList)
        {
            return string.Join(", ", argumentList.Arguments.Reverse().Select(arg => onExpressionSyntax(arg.Expression)));
        }

        bool IsCallingClassMethod(InvocationExpressionSyntax invocation)
        {
            var symbolInfo = semanticModel.GetSymbolInfo(invocation);
            if (symbolInfo.Symbol is IMethodSymbol methodSymbol)
            {
                return methodSymbol.ContainingType.TypeKind == TypeKind.Class;
            }
            return false;
        }

        bool IsCallingStaticMethod(InvocationExpressionSyntax invocation)
        {
            var symbolInfo = semanticModel.GetSymbolInfo(invocation);
            if (symbolInfo.Symbol is IMethodSymbol methodSymbol)
            {
                return methodSymbol.IsStatic;
            }
            return false;
        }

        string onInvocationExpression(InvocationExpressionSyntax inv)
        {
            string key = inv.Expression.ToString();
            var callText = "";
            if (IsCallingClassMethod(inv))
            {
                if (IsCallingStaticMethod(inv))
                {
                    // Log($"class static method {key}");
                    // static methods
                    if (onInvExpr.TryGetValue(key, out InvocationDelegate invExpr))
                        return invExpr(this, inv);
                    SymbolInfo symbolInfo = semanticModel.GetSymbolInfo(inv);
                    IMethodSymbol methodSymbol = symbolInfo.Symbol as IMethodSymbol;
                    if (methodSymbol != null)
                    {
                        string methodName = methodSymbol.Name; // Name of the method
                        string className = methodSymbol.ContainingType.Name; // Name of the class containing the method
                        callText = $"{className}`{methodName}({onArgumentListSyntax(inv.ArgumentList)})";
                    }
                }
                else
                {
                    if (inv.Expression.Kind() == SyntaxKind.SimpleMemberAccessExpression)
                    {
                        var ma = inv.Expression as MemberAccessExpressionSyntax;
                        var exprTypeInfo = semanticModel.GetTypeInfo(ma.Expression);
                        // Log($"type name : {exprTypeInfo.Type.MetadataName}, namespace : {exprTypeInfo.Type.ContainingNamespace?.ToDisplayString()} field : {ma.Name.Identifier.Text}");
                        methodInvExpr.TryGetValue(new INamedTypeSymbolField()
                        {
                            TypeName = exprTypeInfo.Type.MetadataName,
                            Namespace = exprTypeInfo.Type.ContainingNamespace?.ToDisplayString(),
                            FieldName = ma.Name.Identifier.Text
                        }, out var invExpr);
                        if (invExpr == null)
                        {
                            objectInvExpr.TryGetValue(ma.Name.Identifier.Text, out invExpr);
                        }
                        if (invExpr != null)
                            callText = invExpr(this, inv);
                        else
                        {
                            SymbolInfo symbolInfo = semanticModel.GetSymbolInfo(inv);
                            IMethodSymbol methodSymbol = symbolInfo.Symbol as IMethodSymbol;
                            var methodName = uniqueMethodName(methodSymbol);
                            callText = $"{onExpressionSyntax(ma.Expression)}->{methodName}({onArgumentListSyntax(inv.ArgumentList)})";
                        }
                    }
                }
            }
            else
            {
                // in case of compilation error
                // Log($"static method {key}");
                if (onInvExpr.TryGetValue(key, out InvocationDelegate invExpr))
                    return invExpr(this, inv);
            }
            if (callText == "")
            {
                callText = $"{onExpressionSyntax(inv.Expression)}({onArgumentListSyntax(inv.ArgumentList)})";
            }
            return callText;
        }

        bool isString(ITypeSymbol ts)
        {
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_String));
        }

        bool isBool(ITypeSymbol ts)
        {
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_Boolean));
        }

        bool isDouble(ITypeSymbol ts)
        {
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_Double));
        }

        bool isFloat(ITypeSymbol ts)
        {
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_Single));
        }

        bool isInt32(ITypeSymbol ts)
        {
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_Int32));
        }

        bool isUInt32(ITypeSymbol ts)
        {
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_UInt32));
        }

        bool isInt64(ITypeSymbol ts)
        {
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_Int64));
        }

        bool isUInt64(ITypeSymbol ts)
        {
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_UInt64));
        }

        bool isInt16(ITypeSymbol ts)
        {
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_Int16));
        }

        bool isUInt16(ITypeSymbol ts)
        {
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_UInt16));
        }

        bool isInt8(ITypeSymbol ts)
        {
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_SByte));
        }

        bool isUInt8(ITypeSymbol ts)
        {
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_Byte));
        }

        bool isLowBitsInteger(ITypeSymbol ts)
        {
            return isInt16(ts) || isUInt16(ts) || isInt8(ts) || isUInt8(ts);
        }

        bool IsNullableValueType(ITypeSymbol typeSymbol)
        {
            return (typeSymbol is INamedTypeSymbol namedType && namedType.IsGenericType && namedType.OriginalDefinition.SpecialType == SpecialType.System_Nullable_T);
        }

        bool isNullable(ITypeSymbol ts)
        {
            return ts.IsReferenceType || IsNullableValueType(ts);
        }

        bool isTable(ITypeSymbol ts)
        {
            // Check if the type is a generic type
            if (ts is INamedTypeSymbol namedType && namedType.IsGenericType)
            {
                // Check if the type is a Dictionary by comparing the metadata name and containing namespace
                return namedType.MetadataName == "Dictionary`2" &&
                       namedType.ContainingNamespace?.ToDisplayString() == "System.Collections.Generic";
            }

            return false;
        }

        bool isArray(ITypeSymbol ts)
        {
            // Check if the type is a generic type
            if (ts is INamedTypeSymbol namedType && namedType.IsGenericType)
            {
                // Check if the type is a List by comparing the metadata name and containing namespace
                return namedType.MetadataName == "List`1" &&
                       namedType.ContainingNamespace?.ToDisplayString() == "System.Collections.Generic";
            }

            return false;
        }

        bool isClassOrStruct(ITypeSymbol ts)
        {
            return ts.TypeKind == TypeKind.Class || ts.TypeKind == TypeKind.Struct;
        }

        public bool isPointerType(ITypeSymbol typeSymbol)
        {
            if (typeSymbol == null)
                return false;

            if (isString(typeSymbol))
                return false;

            // Check if it's a built-in type (like int, double, etc.)
            if (typeSymbol.IsValueType && typeSymbol.SpecialType != SpecialType.None)
                return false;

            switch (typeSymbol.TypeKind)
            {
                case TypeKind.Class:
                    return true;
                default:
                    return false;
            }
        }

        public bool isStructType(ITypeSymbol typeSymbol)
        {
            if (typeSymbol == null)
                return false;

            // Check if it's a built-in type (like int, double, etc.)
            if (typeSymbol.IsValueType && typeSymbol.SpecialType != SpecialType.None)
                return false;

            switch (typeSymbol.TypeKind)
            {
                case TypeKind.Struct:
                    return true;
                default:
                    return false;
            }
        }

        public bool isMoveType(ITypeSymbol typeSymbol)
        {
            if (typeSymbol == null)
                return false;
            if (isString(typeSymbol))
                return false;
            if (isPointerType(typeSymbol))
                return true;
            switch (typeSymbol.TypeKind)
            {
                case TypeKind.Array:
                case TypeKind.Class:
                    return true;
                default:
                    return false;
            }
        }

        public bool isCloneType(ITypeSymbol typeSymbol)
        {
            return isStructType(typeSymbol);
        }

        string onBinaryExpressionSyntax(BinaryExpressionSyntax binop)
        {
            var result = "(";
            result += onExpressionSyntax(binop.Left);
            var token = binop.OperatorToken.ToString();
            if (token == "^" && isBool(semanticModel.GetTypeInfo(binop.Left).Type))
                token = "^^";
            result += $" {token} ";
            result += onExpressionSyntax(binop.Right);
            return $"{result})";
        }

        string onArrayCreationExpressionSyntax(ArrayCreationExpressionSyntax ac)
        {
            var elemType = onTypeSyntax(ac.Type.ElementType);
            if (ac.Initializer != null)
                return onArrayInitializerExpressionSyntax(ac.Initializer, elemType);

            var dim = 0;
            foreach (ArrayRankSpecifierSyntax rank in ac.Type.RankSpecifiers)
                dim += rank.Sizes.Count;
            if (dim == 1)
            {
                var iter = makeTempVar("iter");
                return $"[{{ for {iter} in range({onExpressionSyntax(ac.Type.RankSpecifiers[0].Sizes[0])}); {iter} }}]";
            }

            var result = "newArray(";
            var first = true;
            foreach (ArrayRankSpecifierSyntax rank in ac.Type.RankSpecifiers)
            {
                foreach (ExpressionSyntax size in rank.Sizes)
                {
                    if (first) first = false;
                    else result += ", ";
                    var es = onExpressionSyntax(size);
                    if (es.Length == 0) result += "-1";
                    else result += es;
                }
            }
            result += ")";
            return result;
        }

        string onArrayInitializerExpressionSyntax(InitializerExpressionSyntax ass, string elemType)
        {
            return $"[{{{elemType} " + string.Join("; ", ass.Expressions.Select(exp => onExpressionSyntax(exp))) + "}]";
        }

        string onObjectCreationExpression(ObjectCreationExpressionSyntax oce)
        {
            var restype = semanticModel.GetTypeInfo(oce);
            if (isTable(restype.Type) && oce.Initializer != null)
            {
                return onObjectCreationExpression_Table(oce);
            }
            else if (isArray(restype.Type) && oce.Initializer != null)
            {
                return onObjectCreationExpression_Array(oce);
            }
            else if (isClassOrStruct(restype.Type))
            {
                return onObjectCreationExpression_ClassOrStruct(oce, restype);
            }
            Fail($"unsupported object creation {oce}");
            return $"{oce}";
        }

        private string onObjectCreationExpression_ClassOrStruct(ObjectCreationExpressionSyntax oce, TypeInfo resType)
        {
            var init = "";
            if (oce.Initializer != null)
            {
                foreach (var initExpr in oce.Initializer.Expressions)
                {
                    init += $" {onExpressionSyntax(initExpr)},";
                }
            }
            if (init.Length > 0)
                init += " ";
            var arguments = oce.ArgumentList.Arguments
                .Select(arg => onExpressionSyntax(arg.Expression));
            var newCall = isPointerType(resType.Type) ? "new " : "";
            if (arguments.Count() == 0)
                return $"{newCall}[[{onTypeSyntax(oce.Type)}(){init}]]";
            var arguments2 = arguments.Aggregate((current, next) => $"{current}, {next}");
            return $"{newCall}[[{onTypeSyntax(oce.Type)}({arguments2}){init}]]";
        }

        private string onObjectCreationExpression_Table(ObjectCreationExpressionSyntax oce)
        {
            var result = "new {{";
            if (oce.Initializer.Kind() == SyntaxKind.CollectionInitializerExpression)
            {
                foreach (ExpressionSyntax element in oce.Initializer.Expressions)
                {
                    if (element.Kind() == SyntaxKind.ComplexElementInitializerExpression)
                    {
                        var kv = element as InitializerExpressionSyntax;
                        if (kv.Expressions.Count == 2)
                        {
                            var key = kv.Expressions[0];
                            var value = kv.Expressions[1];
                            result += $"{onExpressionSyntax(key)} => {onExpressionSyntax(value)}; ";
                        }
                        else
                        {
                            Fail($"expecting key => value in {kv}");
                            result += $"{kv}; ";
                        }
                    }
                    else
                    {
                        Fail($"expecting complex initialization in {element.Kind()}");
                        result += $"{element}; ";
                    }
                }
            }
            else
            {
                Fail("unsupported table initialization {oce.Initializer}");
                result += $"{oce.Initializer}";
            }
            result += "}}";
            return result;
        }

        private string onObjectCreationExpression_Array(ObjectCreationExpressionSyntax oce)
        {
            var result = $"new [{{{onVarTypeSyntax((oce.Type as GenericNameSyntax).TypeArgumentList.Arguments[0])} ";
            if (oce.Initializer.Kind() == SyntaxKind.CollectionInitializerExpression)
            {
                foreach (ExpressionSyntax element in oce.Initializer.Expressions)
                {
                    // if (element.Kind() == SyntaxKind.ObjectInitializerExpression)
                    // {
                    //     var kv = element as InitializerExpressionSyntax;
                    //     if (kv.Expressions.Count == 1)
                    //     {
                    //         var value = kv.Expressions[0];
                    //         result += $"{onExpressionSyntax(value)}; ";
                    //     }
                    //     else
                    //     {
                    //         Fail($"expecting key => value in {kv}");
                    //         result += $"{kv}; ";
                    //     }
                    // }
                    if (element.Kind() == SyntaxKind.NumericLiteralExpression)
                    {
                        var value = element as LiteralExpressionSyntax;
                        result += $"{onExpressionSyntax(value)}; ";
                    }
                    else if (element.Kind() == SyntaxKind.ObjectCreationExpression)
                    {
                        var value = element as ObjectCreationExpressionSyntax;
                        result += $"{onObjectCreationExpression(value)}; ";
                    }
                    else if (element.Kind() == SyntaxKind.ArrayCreationExpression)
                    {
                        var value = element as ArrayCreationExpressionSyntax;
                        result += $"{onArrayCreationExpressionSyntax(value)}; ";
                    }
                    else if (element.Kind() == SyntaxKind.ComplexElementInitializerExpression)
                    {
                        var kv = element as InitializerExpressionSyntax;
                        if (kv.Expressions.Count == 1)
                        {
                            var value = kv.Expressions[0];
                            result += $"{onExpressionSyntax(value)}; ";
                        }
                        else
                        {
                            Fail($"expecting key => value in {kv}");
                            result += $"{kv}; ";
                        }
                    }
                    else
                    {
                        Fail($"expecting complex initialization in {element.Kind()}");
                        result += $"{element}; ";
                    }
                }
            }
            else
            {
                Fail("unsupported table initialization {oce.Initializer}");
                result += $"{oce.Initializer}";
            }
            result += "}]";
            return result;
        }

        public string dasTypeName(ITypeSymbol typeInfo)
        {
            // TODO: support all sorts of types (arrays, pointers, etc)
            if (isInt8(typeInfo)) return "int8";
            else if (isInt16(typeInfo)) return "int16";
            else if (isInt32(typeInfo)) return "int";
            else if (isInt64(typeInfo)) return "int64";
            else if (isUInt8(typeInfo)) return "uint8";
            else if (isUInt16(typeInfo)) return "uint16";
            else if (isUInt32(typeInfo)) return "uint";
            else if (isUInt64(typeInfo)) return "uint64";
            else if (isFloat(typeInfo)) return "float";
            else if (isDouble(typeInfo)) return "double";
            return "";
        }
        public string dasTypePostfix(ITypeSymbol typeInfo)
        {
            // if (isInt8(typeInfo)) return "";
            // else if (isInt16(typeInfo)) return "";
            // else if (isInt32(typeInfo)) return "";
            if (isInt64(typeInfo)) return "l";
            else if (isUInt8(typeInfo)) return "u8";
            // else if (isUInt16(typeInfo)) return "u16";
            else if (isUInt32(typeInfo)) return "u";
            else if (isUInt64(typeInfo)) return "ul";
            else if (isFloat(typeInfo)) return "f";
            else if (isDouble(typeInfo)) return "d";
            return "";
        }

        public string castExpr(ExpressionSyntax expr, ITypeSymbol typeInfo)
        {
            var res = onExpressionSyntax_(expr);
            if (expr.IsKind(SyntaxKind.NumericLiteralExpression) && !res.EndsWith("f"))
            {
                var postfix = dasTypePostfix(typeInfo);
                if (postfix != "")
                    return $"{res}{postfix}";
            }
            var cast = dasTypeName(typeInfo);
            return cast != "" ? $"{cast}({res})" : res;
        }

        public string derefExpr(ExpressionSyntax expr)
        {
            var typeInfo = semanticModel.GetTypeInfo(expr);
            return derefExpr(onExpressionSyntax(expr), typeInfo, false);
        }

        public string safeDerefExpr(ExpressionSyntax expr)
        {
            var typeInfo = semanticModel.GetTypeInfo(expr);
            return derefExpr(onExpressionSyntax(expr), typeInfo, true);
        }

        public string derefExpr(string res, TypeInfo typeInfo, bool safe)
        {
            if (isPointerType(typeInfo.Type))
                return safe ? $"(*{res})" : $"*{res}";
            return res;
        }

        public string onExpressionSyntax(ExpressionSyntax expression)
        {
            var itemType = semanticModel.GetTypeInfo(expression);
            if (itemType.Type != null && !itemType.Type.Equals(itemType.ConvertedType))
            {
                return $"{castExpr(expression, itemType.ConvertedType)}";
            }
            return onExpressionSyntax_(expression);
        }

        bool isProperty(ExpressionSyntax expression)
        {
            if (expression.Kind() != SyntaxKind.SimpleMemberAccessExpression) return false;
            var memberAccess = expression as MemberAccessExpressionSyntax;
            ISymbol accessedSymbol = semanticModel.GetSymbolInfo(memberAccess).Symbol;
            return (accessedSymbol is IPropertySymbol);
        }

        bool isStaticProperty(ExpressionSyntax expression, string prefix, out string callPrefix)
        {
            callPrefix = "";
            if (expression.Kind() != SyntaxKind.SimpleMemberAccessExpression) return false;
            var memberAccess = expression as MemberAccessExpressionSyntax;
            ISymbol accessedSymbol = semanticModel.GetSymbolInfo(memberAccess).Symbol;
            if (accessedSymbol is IPropertySymbol)
            {
                var smm = (IPropertySymbol)accessedSymbol;
                if (smm.IsStatic)
                {
                    callPrefix = $"{smm.ContainingSymbol.Name}`{prefix}{smm.Name}";
                    return true;
                }
            }
            return false;
        }

        bool isNullExpression(ExpressionSyntax expr)
        {
            return expr.Kind() == SyntaxKind.NullLiteralExpression;
        }

        public string onExpressionSyntax_(ExpressionSyntax expression)
        {
            if (expression == null)
                return "";
            switch (expression.Kind())
            {
                case SyntaxKind.ParenthesizedExpression:
                    return $"({onExpressionSyntax((expression as ParenthesizedExpressionSyntax).Expression)})";
                case SyntaxKind.InvocationExpression:
                    return onInvocationExpression(expression as InvocationExpressionSyntax);
                case SyntaxKind.UnaryPlusExpression:
                case SyntaxKind.UnaryMinusExpression:
                case SyntaxKind.BitwiseNotExpression:
                case SyntaxKind.LogicalNotExpression:
                    {
                        var unop = expression as PrefixUnaryExpressionSyntax;
                        return $"({unop.OperatorToken}{onExpressionSyntax(unop.Operand)})";
                    }
                case SyntaxKind.AddExpression:
                case SyntaxKind.SubtractExpression:
                case SyntaxKind.MultiplyExpression:
                case SyntaxKind.DivideExpression:
                case SyntaxKind.ModuloExpression:
                case SyntaxKind.EqualsExpression:
                case SyntaxKind.NotEqualsExpression:
                case SyntaxKind.LessThanExpression:
                case SyntaxKind.LessThanOrEqualExpression:
                case SyntaxKind.GreaterThanExpression:
                case SyntaxKind.GreaterThanOrEqualExpression:
                case SyntaxKind.LeftShiftExpression:
                case SyntaxKind.RightShiftExpression:
                case SyntaxKind.ExclusiveOrExpression:
                case SyntaxKind.LogicalAndExpression:
                case SyntaxKind.LogicalOrExpression:
                case SyntaxKind.BitwiseAndExpression:
                case SyntaxKind.BitwiseOrExpression:
                    return onBinaryExpressionSyntax(expression as BinaryExpressionSyntax);
                case SyntaxKind.SimpleAssignmentExpression:
                    {
                        var binop = expression as AssignmentExpressionSyntax;
                        var typeInfo = semanticModel.GetTypeInfo(binop.Left);
                        var assign = isMoveType(typeInfo.Type) ? "<-" : isCloneType(typeInfo.Type) ? ":=" : "=";
                        if (isNullExpression(binop.Right)) assign = "=";
                        if (isProperty(binop.Left))
                        {
                            if (isStaticProperty(binop.Left, "set__", out string propName))
                            {
                                return $"{propName}({onExpressionSyntax(binop.Right)})";
                            }
                            else
                                assign = ":=";
                        }
                        return $"{onExpressionSyntax(binop.Left)} {assign} {onExpressionSyntax(binop.Right)}";
                    }
                case SyntaxKind.LeftShiftAssignmentExpression:
                case SyntaxKind.RightShiftAssignmentExpression:
                case SyntaxKind.ExclusiveOrAssignmentExpression:
                case SyntaxKind.OrAssignmentExpression:
                case SyntaxKind.AndAssignmentExpression:
                case SyntaxKind.AddAssignmentExpression:
                case SyntaxKind.SubtractAssignmentExpression:
                case SyntaxKind.MultiplyAssignmentExpression:
                case SyntaxKind.DivideAssignmentExpression:
                    {
                        var binop = expression as AssignmentExpressionSyntax;
                        if (isProperty(binop.Left))
                        {
                            var op = binop.OperatorToken.Text;
                            var cutop = op.Substring(0, op.Length - 1);
                            if (isStaticProperty(binop.Left, "set__", out string propName))
                            {
                                isStaticProperty(binop.Left, "get__", out string getPropName);
                                return $"{propName}({getPropName}() {cutop} {onExpressionSyntax(binop.Right)})";
                            }
                            else
                                return $"{onExpressionSyntax(binop.Left)} := {onExpressionSyntax(binop.Left)} {cutop} {onExpressionSyntax(binop.Right)}";
                        }
                        // TODO: convert operator token properly
                        // WriteLine($"OP2 {binop.OperatorToken} // {expression.Kind()}\n");
                        return $"{onExpressionSyntax(binop.Left)} {binop.OperatorToken} {onExpressionSyntax(binop.Right)}";
                    }
                case SyntaxKind.OmittedArraySizeExpression:
                    return "";  // in int[], this is the portion between the brackets
                case SyntaxKind.NumericLiteralExpression:
                    return $"{expression}";
                case SyntaxKind.StringLiteralExpression:
                    return onSyntaxToken((expression as LiteralExpressionSyntax).Token);
                case SyntaxKind.SimpleMemberAccessExpression:
                    {
                        var smm = expression as MemberAccessExpressionSyntax;
                        TypeInfo typeInfo = semanticModel.GetTypeInfo(smm.Expression);
                        // Log($"type name : {typeInfo.Type.MetadataName}, namespace : {typeInfo.Type.ContainingNamespace?.ToDisplayString()} field : {smm.Name.Identifier.Text}");
                        if (typeInfo.Type != null && memberAccessExpr.TryGetValue(new INamedTypeSymbolField()
                        {
                            TypeName = typeInfo.Type.MetadataName,
                            Namespace = typeInfo.Type.ContainingNamespace?.ToDisplayString(),
                            FieldName = smm.Name.Identifier.Text
                        }, out MemberAccessDelegate acc))
                        {
                            return acc(this, smm);
                        }
                        if (isStaticProperty(smm, "get__", out string staticPropName))
                        {
                            return $"{staticPropName}()";
                        }
                        return $"{onExpressionSyntax(smm.Expression)}.{smm.Name.Identifier.Text}";
                    }
                case SyntaxKind.IdentifierName:
                    return $"{(expression as IdentifierNameSyntax).Identifier.Text}";
                case SyntaxKind.PreIncrementExpression:
                case SyntaxKind.PreDecrementExpression:
                    {
                        var preop = expression as PrefixUnaryExpressionSyntax;
                        return $"{preop.OperatorToken.Value}{onExpressionSyntax(preop.Operand)}";
                    }
                case SyntaxKind.PostIncrementExpression:
                case SyntaxKind.PostDecrementExpression:
                    {
                        var postop = expression as PostfixUnaryExpressionSyntax;
                        return $"{onExpressionSyntax(postop.Operand)}{postop.OperatorToken.Value}";
                    }
                case SyntaxKind.InterpolatedStringExpression:
                    return onInterpolatedStringExpressionSyntax(expression as InterpolatedStringExpressionSyntax);
                case SyntaxKind.ArrayCreationExpression:
                    return onArrayCreationExpressionSyntax(expression as ArrayCreationExpressionSyntax);
                case SyntaxKind.ArrayInitializerExpression:
                    return onArrayInitializerExpressionSyntax(expression as InitializerExpressionSyntax, "auto");
                case SyntaxKind.ObjectCreationExpression:
                    return onObjectCreationExpression(expression as ObjectCreationExpressionSyntax);
                case SyntaxKind.ElementAccessExpression:
                    {
                        var eae = expression as ElementAccessExpressionSyntax;
                        var result = $"{safeDerefExpr(eae.Expression)}[";
                        var first = true;
                        foreach (var arg in eae.ArgumentList.Arguments)
                        {
                            if (first) first = false;
                            else result += ", ";
                            result += onExpressionSyntax(arg.Expression);
                        }
                        result += "]";
                        return result;
                    }
                case SyntaxKind.DefaultExpression:
                    return $"[[{onVarTypeSyntax((expression as DefaultExpressionSyntax).Type)}]]";
                case SyntaxKind.ConditionalExpression:
                    {
                        var ce = expression as ConditionalExpressionSyntax;
                        return $"{onExpressionSyntax(ce.Condition)} ? {onExpressionSyntax(ce.WhenTrue)} : {onExpressionSyntax(ce.WhenFalse)}";
                    }
                case SyntaxKind.CastExpression:
                    {
                        var ce = expression as CastExpressionSyntax;
                        return $"{onTypeSyntax(ce.Type)}({onExpressionSyntax(ce.Expression)})";
                    }
                case SyntaxKind.TrueLiteralExpression:
                    return "true";
                case SyntaxKind.FalseLiteralExpression:
                    return "false";
                case SyntaxKind.NullLiteralExpression:
                    return "null";
                case SyntaxKind.ThisExpression:
                    return "self";
                default:
                    Fail($"unsupported ExpressionSyntax {expression.Kind()}");
                    return $"{expression.ToString()}";
            }
        }

        string onInterpolatedStringExpressionSyntax(InterpolatedStringExpressionSyntax iss)
        {
            var result = "\"";
            foreach (var content in iss.Contents)
            {
                switch (content)
                {
                    case InterpolatedStringTextSyntax textSyntax:
                        result += $"{content}"; // TODO: print string better
                        break;
                    case InterpolationSyntax interpolationSyntax:
                        result += $"{{{onExpressionSyntax(interpolationSyntax.Expression)}}}";
                        break;
                }
            }
            return result + "\"";
        }

        string onSyntaxToken(SyntaxToken token)
        {
            switch (token.Kind())
            {
                case SyntaxKind.StringLiteralToken:
                    return $"\"{token.ValueText}\"";
                case SyntaxKind.NumericLiteralToken:
                    return token.ValueText;
                default:
                    Fail($"unsupported SyntaxToken {token.Kind()}");
                    return $"{token}";
            }
        }

        string onUsing(UsingDirectiveSyntax u)
        {
            return $"// using {u.Name}\n";
        }

        string onNamespaceDeclaration(NamespaceDeclarationSyntax namespaceDeclaration)
        {
            var result = $"// namespaced {namespaceDeclaration.Name}\n\n";
            TextSpan prevSpan = new TextSpan(namespaceDeclaration.Span.Start, 1);
            foreach (MemberDeclarationSyntax memberDeclaration in namespaceDeclaration.Members)
            {
                InsertComments(ref result, prevSpan, memberDeclaration.Span, memberDeclaration.SyntaxTree);
                result += onMemberDeclaration(memberDeclaration) + "\n";
                prevSpan = memberDeclaration.Span;
            }
            return result;
        }

        string onClassDeclaration(ClassDeclarationSyntax classDeclaration)
        {
            var result = $"class {classDeclaration.Identifier}\n";
            tabs++;
            TextSpan prevSpan = new TextSpan(classDeclaration.Span.Start, 1);
            foreach (MemberDeclarationSyntax membersDeclaration in classDeclaration.Members)
            {
                InsertComments(ref result, prevSpan, membersDeclaration.Span, membersDeclaration.SyntaxTree);
                result += onMemberDeclaration(membersDeclaration) + "\n";
                prevSpan = membersDeclaration.Span;
            }
            tabs--;
            return result;
        }

        string onStructDeclaration(StructDeclarationSyntax classDeclaration)
        {
            var result = $"struct {classDeclaration.Identifier}\n";
            tabs++;
            TextSpan prevSpan = new TextSpan(classDeclaration.Span.Start, 1);
            foreach (MemberDeclarationSyntax membersDeclaration in classDeclaration.Members)
            {
                InsertComments(ref result, prevSpan, membersDeclaration.Span, membersDeclaration.SyntaxTree);
                result += onMemberDeclaration(membersDeclaration) + "\n";
                prevSpan = membersDeclaration.Span;
            }
            tabs--;
            return result;
        }

        bool IsConstantVariable(ISymbol symbol)
        {
            if (symbol is ILocalSymbol localSymbol)
            {
                return localSymbol.IsConst;
            }
            else if (symbol is IFieldSymbol fieldSymbol)
            {
                return fieldSymbol.IsConst;
            }
            return false;
        }

        string varPrefix(ParameterSyntax param)
        {
            if (param.Modifiers.Any(modifier => modifier.Kind() == SyntaxKind.InKeyword))
                return "";
            var paramType = semanticModel.GetTypeInfo(param);
            return IsConstantVariable(paramType.Type) ? "" : "var ";
        }

        string varSuffix(ParameterSyntax param)
        {
            return (param.Modifiers.Any(modifier => modifier.Kind() == SyntaxKind.OutKeyword)) ? "&" : "";
        }

        string onConstructorDeclaration(ConstructorDeclarationSyntax methodDeclaration)
        {
            var tabstr = new string('\t', tabs);
            var result = $"{tabstr}def {methodDeclaration.Identifier}";
            if (methodDeclaration.ParameterList.Parameters.Count != 0)
            {
                var parameters = methodDeclaration.ParameterList.Parameters
                    .Select(param => $"{varPrefix(param)}{param.Identifier} : {onVarTypeSyntax(param.Type)}{varSuffix(param)}");
                result += $" ({string.Join("; ", parameters)})";
            }
            if (methodDeclaration.Body != null)
                result += $"\n{onBlockSyntax(methodDeclaration.Body)}";
            else if (methodDeclaration.ExpressionBody != null)
                result += $"\n{tabstr}\t{onExpressionSyntax(methodDeclaration.ExpressionBody.Expression)}\n";
            else
                result += $"\n{tabstr}\tpass\n";
            return result;
        }

        bool classHasMethodWithTheSameName(IMethodSymbol methodSymbol)
        {
            if (methodSymbol == null)
                return false;
            INamedTypeSymbol containingType = methodSymbol.ContainingType;
            string methodName = methodSymbol.Name;
            return containingType.GetMembers(methodName).OfType<IMethodSymbol>().Count() > 1;
        }

        string uniqueMethodName(IMethodSymbol methodSymbol)
        {
            if (methodSymbol.IsStatic)
                return methodSymbol.Name;
            if (!classHasMethodWithTheSameName(methodSymbol))
                return methodSymbol.Name;
            var uniqueName = string.Join("`", methodSymbol.Parameters.Select(p => p.Type.ToString()));
            uniqueName = Regex.Replace(uniqueName, "[<>,\\[\\]]", "_");
            if (uniqueName.Length > 0)
                uniqueName = $"`{uniqueName}";
            return $"{methodSymbol.Name}{uniqueName}";
        }

        string onMethodDeclaration(MethodDeclarationSyntax methodDeclaration)
        {
            var tabstr = new string('\t', tabs);
            var prefix = "";
            if (methodDeclaration.Modifiers.Any(mod => mod.Kind() == SyntaxKind.PrivateKeyword))
                prefix += "private ";
            if (methodDeclaration.Modifiers.Any(mod => mod.Kind() == SyntaxKind.StaticKeyword))
                prefix += "static ";
            IMethodSymbol methodSymbol = semanticModel.GetDeclaredSymbol(methodDeclaration);
            var result = $"{tabstr}def {prefix}{uniqueMethodName(methodSymbol)}";
            if (methodDeclaration.ParameterList.Parameters.Count != 0)
            {
                var parameters = methodDeclaration.ParameterList.Parameters
                    .Select(param => $"{varPrefix(param)}{param.Identifier} : {onVarTypeSyntax(param.Type)}{varSuffix(param)}");
                result += $" ({string.Join("; ", parameters)})";
            }
            result += $" : {onVarTypeSyntax(methodDeclaration.ReturnType)}";
            if (methodDeclaration.Body != null)
                result += $"\n{onBlockSyntax(methodDeclaration.Body)}";
            else if (methodDeclaration.ExpressionBody != null)
                result += $"\n{tabstr}\t{onExpressionSyntax(methodDeclaration.ExpressionBody.Expression)}\n";
            else
                result += $"\n{tabstr}\tpass\n";
            return result;
        }

        List<string> onVariableDeclarationSyntax(VariableDeclarationSyntax vardecl, bool needVar = true)
        {
            var values = new List<string>();
            var tname = onVarTypeSyntax(vardecl.Type);
            var typeInfo = semanticModel.GetTypeInfo(vardecl.Type);
            foreach (VariableDeclaratorSyntax declarator in vardecl.Variables)
            {
                var result = needVar ? "var " : "";
                result += $"{declarator.Identifier.Text}";
                if (tname != "var" && tname != "var?")
                    result += $" : {tname}";
                if (declarator.Initializer != null)
                {
                    var itemTypeInfo = semanticModel.GetTypeInfo(declarator.Initializer.Value);
                    var assign = "=";
                    if (isMoveType(typeInfo.Type))
                        assign = "<-";
                    else if (declarator.Initializer.Value.IsKind(SyntaxKind.IdentifierName) && isCloneType(typeInfo.Type))
                        assign = ":=";
                    if (!typeInfo.Type.Equals(itemTypeInfo.ConvertedType))
                        result += $" {assign} {castExpr(declarator.Initializer.Value, typeInfo.Type)}";
                    else
                        result += $" {assign} {onExpressionSyntax(declarator.Initializer.Value)}";
                }
                values.Add(result);
            }
            return values;
        }

        bool hasBreakOrContinue(StatementSyntax statementSyntax)
        {
            return statementSyntax.DescendantNodes()
                 .Any(node => node is BreakStatementSyntax || node is ContinueStatementSyntax);
        }

        SyntaxNode getEnclosingStatement(BreakStatementSyntax breakStatement)
        {
            var enclosingStatement = breakStatement.Ancestors()
                                          .FirstOrDefault(a => a is ForStatementSyntax ||
                                                               a is WhileStatementSyntax ||
                                                               a is DoStatementSyntax ||
                                                               a is SwitchStatementSyntax);
            return enclosingStatement;
        }

        bool hasSpecificBreak(StatementSyntax statementSyntax, SyntaxNode enclosure)
        {
            return statementSyntax.DescendantNodes()
                 .Any(
                node => (node is BreakStatementSyntax) && (getEnclosingStatement(node as BreakStatementSyntax) == enclosure)
                );
        }

        bool isIdentifier(ExpressionSyntax expression, string id)
        {
            if (!(expression is IdentifierNameSyntax))
                return false;
            if ((expression as IdentifierNameSyntax).Identifier.Text != id)
                return false;
            return true;
        }

        bool isOne(ExpressionSyntax expression)
        {
            if (!(expression is LiteralExpressionSyntax)) return false;
            if ((expression as LiteralExpressionSyntax).Token.ValueText != "1") return false;
            return true;
        }

        bool isRangeFor(ForStatementSyntax fstmt, out string rangeExpr)
        {
            rangeExpr = null;
            if (fstmt.Declaration.Variables.Count != 1) return false;           // only 1 variable
            var vdecl = fstmt.Declaration.Variables[0];
            var vname = vdecl.Identifier.Text;
            ITypeSymbol vtype;
            ISymbol symbol = semanticModel.GetDeclaredSymbol(vdecl);
            if (symbol is ILocalSymbol localVariableSymbol)
                vtype = localVariableSymbol.Type;
            else
                return false;
            var range = "";
            if (isInt32(vtype)) range = "range";                                // int, uint, int64, or uint64
            else if (isUInt32(vtype)) range = "urange";
            else if (isInt64(vtype)) range = "range64";
            else if (isUInt64(vtype)) range = "urange64";
            else return false;
            if (fstmt.Incrementors.Count != 1) return false;                    // only 1 incrementor
            var inc = fstmt.Incrementors[0];
            switch (inc.Kind())
            {
                case SyntaxKind.PreIncrementExpression:                         // it's ++var
                    if (!isIdentifier((inc as PrefixUnaryExpressionSyntax).Operand, vname))
                        return false;
                    break;
                case SyntaxKind.PostIncrementExpression:                        // it's var++
                    if (!isIdentifier((inc as PostfixUnaryExpressionSyntax).Operand, vname))
                        return false;
                    break;
                case SyntaxKind.AddAssignmentExpression:                        // it's var += 1
                    {
                        var aa = inc as AssignmentExpressionSyntax;
                        if (!isIdentifier(aa.Left, vname)) return false;
                        if (!isOne(aa.Right)) return false;
                        break;
                    }
                case SyntaxKind.SimpleAssignmentExpression:                     // its var = var + 1 or var = 1 + var
                    {
                        var aa = inc as AssignmentExpressionSyntax;
                        if (!isIdentifier(aa.Left, vname)) return false;
                        if (!(aa.Right is BinaryExpressionSyntax)) return false;
                        var bb = aa.Right as BinaryExpressionSyntax;
                        if (isIdentifier(bb.Left, vname) && isOne(bb.Right)) { }            // var = var + 1
                        else if (isIdentifier(bb.Right, vname) && isOne(bb.Left)) { }       // var = 1 + var
                        else return false;
                        break;
                    }
                default:
                    return false;
            }
            // condition is var < value or var != value or value > var or value != var
            var torange = "";
            switch (fstmt.Condition.Kind())
            {
                case SyntaxKind.NotEqualsExpression:
                    {
                        var binop = fstmt.Condition as BinaryExpressionSyntax;
                        if (isIdentifier(binop.Left, vname))
                            torange = onExpressionSyntax(binop.Right);
                        else if (isIdentifier(binop.Right, vname))
                            torange = onExpressionSyntax(binop.Left);
                        else
                            return false;
                        break;
                    }
                case SyntaxKind.LessThanExpression:
                    {
                        var binop = fstmt.Condition as BinaryExpressionSyntax;
                        if (isIdentifier(binop.Left, vname))
                            torange += onExpressionSyntax(binop.Right);
                        else
                            return false;
                        break;
                    }
                case SyntaxKind.GreaterThanExpression:
                    {
                        var binop = fstmt.Condition as BinaryExpressionSyntax;
                        if (isIdentifier(binop.Right, vname))
                            torange += onExpressionSyntax(binop.Left);
                        else
                            return false;
                        break;
                    }

            }
            rangeExpr = $"for {vname} in {range}({onExpressionSyntax(vdecl.Initializer.Value)},{torange})";
            return true;
        }

        string loopBlock(StatementSyntax expr)
        {
            if (expr is BlockSyntax)
                return onStatementSyntax(expr);
            tabs++;
            string result = onStatementSyntax(expr);
            tabs--;
            return result;
        }

        string onForStatement(ForStatementSyntax fstmt)
        {
            if (isRangeFor(fstmt, out string rangeExpr))
                return $"{rangeExpr}\n{loopBlock(fstmt.Statement)}";
            var result = $"if true // for loop\n";
            tabs++;
            var tabstr = new string('\t', tabs);
            var values = onVariableDeclarationSyntax(fstmt.Declaration);
            foreach (string val in values)
                result += $"{tabstr}{val}\n";
            result += $"{tabstr}while {onExpressionSyntax(fstmt.Condition)}\n";
            bool hasBorC = hasBreakOrContinue(fstmt.Statement);
            if (hasBorC)
            {
                result += $"{tabstr}\tif true // to create block for finally section\n";
                tabs++;
            }
            result += loopBlock(fstmt.Statement);
            if (hasBorC)
            {
                tabs--;
                result += $"{tabstr}\tfinally\n";
            }
            tabs++;
            tabstr = new string('\t', hasBorC ? (tabs + 1) : tabs);
            foreach (ExpressionSyntax i in fstmt.Incrementors)
            {
                result += $"{tabstr}{onExpressionSyntax(i)}\n";
            }
            tabs--;
            tabs--;
            return result;
        }

        string onWhileStatement(WhileStatementSyntax wstmt)
        {
            return $"while {onExpressionSyntax(wstmt.Condition)}\n{loopBlock(wstmt.Statement)}";
        }

        string makeTempVar(string suffix)
        {
            return $"_temp_{++tempVars}_{suffix}_";
        }

        string onDoStatement(DoStatementSyntax wstmt)
        {
            var tv = makeTempVar("doWhileCond");
            var tabstr = new string('\t', tabs);
            bool hasBorC = hasBreakOrContinue(wstmt.Statement);
            var result = $"var {tv} = true\n{tabstr}while {tv}\n";
            if (hasBorC)
            {
                tabstr += '\t';
                result += $"{tabstr}if true\n";
                tabs++;
            }
            result += loopBlock(wstmt.Statement);
            if (hasBorC)
            {
                tabs--;
                result += $"{tabstr}finally\n";
            }
            result += $"{tabstr}\t{tv} = {onExpressionSyntax(wstmt.Condition)}\n";
            return result;
        }

        string onIfStatement(IfStatementSyntax ifstmt, bool isElif = false)
        {
            var tabstr = new string('\t', tabs);
            var result = isElif ? $"elif " : $"if ";
            result += $"{onExpressionSyntax(ifstmt.Condition)}\n{loopBlock(ifstmt.Statement)}";
            if (ifstmt.Else != null)
            {
                if (ifstmt.Else.Statement is IfStatementSyntax)
                    result += $"{tabstr}{onIfStatement(ifstmt.Else.Statement as IfStatementSyntax, true)}";
                else
                    result += $"{tabstr}else\n{loopBlock(ifstmt.Else.Statement)}";
            }
            return result;
        }

        string onForeachStatement(ForEachStatementSyntax fs)
        {
            return $"for {fs.Identifier.Text} in {derefExpr(fs.Expression)}\n{loopBlock(fs.Statement)}";
        }

        string onSwitchStatement(SwitchStatementSyntax fs)
        {
            // optimize simple cases, where while loop is not necessary
            // i.e. every section ends with break, and there are no extra breaks
            // TODO: make this ruleset more inclusive
            var simpleSwitchCase = fs.Sections.All(section =>
                section.Statements.LastOrDefault() is BreakStatementSyntax &&
                !section.Statements.Any(stmt => hasSpecificBreak(stmt, fs)));

            var tempval = makeTempVar("switchcase");
            var tabstr = new string('\t', tabs);
            var result = "";
            result += $"let {tempval} = {onExpressionSyntax(fs.Expression)}\n";
            var firstIf = true;
            if (!simpleSwitchCase)
            {
                result += $"{tabstr}while true\n";
                tabs++;
                tabstr += '\t';
            }
            var hasDefault = false;

            // lets build sorted sections list. we are making sure default is last
            int sectionCount = fs.Sections.Count;
            SwitchSectionSyntax[] sortedSections = fs.Sections.ToArray();
            int defaultIndex = Array.FindIndex(sortedSections, section =>
                section.Labels.Any(label => label is DefaultSwitchLabelSyntax));
            if (defaultIndex != -1)
            {
                sortedSections[defaultIndex] = sortedSections[sectionCount - 1];
                sortedSections[sectionCount - 1] = fs.Sections[defaultIndex];
            }

            for (int si = 0; si < sectionCount; ++si)
            {
                SwitchSectionSyntax section = sortedSections[si];
                var isElseTrue = (si == sectionCount - 1) && (section.Labels.Count == 1) && (section.Labels[0] is DefaultSwitchLabelSyntax);
                if (firstIf)
                {
                    result += $"{tabstr}if ";
                    firstIf = false;
                    isElseTrue = false;
                }
                else if (isElseTrue)
                    result += $"{tabstr}else\n";
                else
                    result += $"{tabstr}elif ";
                if (!isElseTrue)
                {
                    var first = true;
                    foreach (SwitchLabelSyntax st in section.Labels)
                    {
                        if (first) first = false;
                        else result += " || ";
                        if (st is CaseSwitchLabelSyntax)
                            result += $"{tempval}=={(st as CaseSwitchLabelSyntax).Value}";
                        else if (st is DefaultSwitchLabelSyntax)
                        {
                            hasDefault = true;
                            result += "true /*default*/";
                        }
                    }
                    result += "\n";
                }
                tabs++;
                StatementSyntax prevExpr = null;
                foreach (var ex in section.Statements)
                {
                    if (!simpleSwitchCase || !(ex is BreakStatementSyntax))
                    {
                        if (prevExpr != null)
                            InsertSpacesAndComments(ref result, prevExpr.Span, ex.Span, ex.SyntaxTree);
                        result += $"{onStatementSyntax(ex)}";
                    }
                    prevExpr = ex;
                }
                if (simpleSwitchCase && section.Statements.Count == 1)
                    result += $"{tabstr}\tpass\n";
                tabs--;
            }
            if (!simpleSwitchCase)
            {
                tabs--;
                if (!hasDefault)
                    result += $"{tabstr}break\n";
            }
            return result;
        }

        string onStatementSyntax(StatementSyntax statement)
        {
            var tabstr = new string('\t', tabs);
            switch (statement.Kind())
            {
                case SyntaxKind.ExpressionStatement:
                    return $"{tabstr}{onExpressionSyntax((statement as ExpressionStatementSyntax).Expression)}\n";
                case SyntaxKind.ReturnStatement:
                    var expr = (statement as ReturnStatementSyntax).Expression;
                    if (expr == null)
                        return $"{tabstr}return\n";
                    else
                        return $"{tabstr}return {onExpressionSyntax(expr)}\n";
                case SyntaxKind.LocalDeclarationStatement:
                    {
                        var values = onVariableDeclarationSyntax((statement as LocalDeclarationStatementSyntax).Declaration);
                        string result = "";
                        foreach (string val in values)
                            result += $"{tabstr}{val}\n";
                        return result;
                    }
                case SyntaxKind.IfStatement:
                    return $"{tabstr}{onIfStatement(statement as IfStatementSyntax)}";
                case SyntaxKind.WhileStatement:
                    return $"{tabstr}{onWhileStatement(statement as WhileStatementSyntax)}";
                case SyntaxKind.DoStatement:
                    return $"{tabstr}{onDoStatement(statement as DoStatementSyntax)}";
                case SyntaxKind.ForStatement:
                    return $"{tabstr}{onForStatement(statement as ForStatementSyntax)}";
                case SyntaxKind.Block:
                    return onBlockSyntax(statement as BlockSyntax);
                case SyntaxKind.BreakStatement:
                    return $"{tabstr}break\n";
                case SyntaxKind.ContinueStatement:
                    return $"{tabstr}continue\n";
                case SyntaxKind.ForEachStatement:
                    return $"{tabstr}{onForeachStatement(statement as ForEachStatementSyntax)}";
                case SyntaxKind.SwitchStatement:
                    return $"{tabstr}{onSwitchStatement(statement as SwitchStatementSyntax)}";
                default:
                    Fail($"unsupported StatementSyntax {statement.Kind()}");
                    return $"{statement};";
            }
        }

        void InsertComments(ref string result, TextSpan prev, TextSpan current, SyntaxTree tree)
        {
            InsertSpacesAndComments(ref result, prev, current, tree, false);
        }
        void InsertSpacesAndComments(ref string result, TextSpan prev, TextSpan current, SyntaxTree tree, bool insertNewLine = true)
        {
            if (result.EndsWith("\n"))
                result = result.Substring(0, result.Length - 1);

            var sourceText = tree.GetText();
            string tabstr = new string('\t', tabs);
            var newLines = 0;
            for (var i = prev.End; i < current.Start; ++i)
            {
                allComments.TryGetValue(i, out var token);
                if (token.IsKind(SyntaxKind.SingleLineCommentTrivia))
                {
                    result += $"{tabstr}{token}\n";
                    newLines++;
                    i = token.Span.End + 2;
                }
                else if (token.IsKind(SyntaxKind.MultiLineCommentTrivia))
                {
                    result += $"{tabstr}// {token}\n";
                    newLines++;
                    i = token.Span.End + 2;
                }

                if (insertNewLine && sourceText[i] == '\n')
                {
                    result += "\n";
                    newLines++;
                }
            }
            if (newLines == 0) // restore new line if it was removed
                result += "\n";
        }

        string onBlockSyntax(BlockSyntax block)
        {
            var result = "";
            tabs++;
            if (block.Statements.Count == 0)
            {
                var tabstr = new string('\t', tabs);
                result = $"{tabstr}pass\n";
            }
            else
            {
                StatementSyntax prevExpr = null;
                foreach (StatementSyntax expr in block.Statements)
                {
                    if (prevExpr != null)
                        InsertSpacesAndComments(ref result, prevExpr.Span, expr.Span, expr.SyntaxTree);
                    result += onStatementSyntax(expr);
                    prevExpr = expr;
                }
            }
            tabs--;
            return result;
        }

        string onFieldDeclaration(FieldDeclarationSyntax field)
        {
            var tabstr = new string('\t', tabs);
            var values = onVariableDeclarationSyntax(field.Declaration, false);
            string result = "";
            var prefix = "";
            if (field.Modifiers.Any(mod => mod.Kind() == SyntaxKind.StaticKeyword))
                prefix += "static ";
            if (field.Modifiers.Any(mod => mod.Kind() == SyntaxKind.PrivateKeyword))
                prefix += "private ";
            foreach (string val in values)
                result += $"{tabstr}{prefix}{val}\n";
            return result;
        }

        string onAccessorDeclarationSyntrax(AccessorDeclarationSyntax accessor)
        {
            return "";
        }

        string onPropertyDeclaration(PropertyDeclarationSyntax propertySyntax)
        {
            var tabstr = new string('\t', tabs);
            string ptype = onTypeSyntax(propertySyntax.Type);
            bool isOverride = propertySyntax.Modifiers.Any(mod => mod.Kind() == SyntaxKind.OverrideKeyword);
            bool isAbstract = propertySyntax.Modifiers.Any(mod => mod.Kind() == SyntaxKind.AbstractKeyword);
            bool isStatic = propertySyntax.Modifiers.Any(mod => mod.Kind() == SyntaxKind.StaticKeyword);
            string abstractMod = isAbstract ? "abstract " : "";
            string staticMod = isStatic ? "static " : "";
            string result = $"{tabstr}// {staticMod}property {propertySyntax.Identifier.Text} : {ptype}\n";
            if (propertySyntax.AccessorList != null)
            {
                bool needStorage = false;
                foreach (var accessor in propertySyntax.AccessorList.Accessors)
                {
                    if (accessor.Kind() == SyntaxKind.GetAccessorDeclaration)
                    {
                        if (!isOverride && !isStatic)
                        {
                            result += $"{tabstr}def operator . {propertySyntax.Identifier.Text} : {ptype}\n";
                            result += $"{tabstr}\treturn get__{propertySyntax.Identifier.Text}()\n";
                        }
                        result += $"{tabstr}def {abstractMod}{staticMod}get__{propertySyntax.Identifier.Text} : {ptype}\n";
                        if (accessor.Body != null)
                            result += onBlockSyntax(accessor.Body);
                        else if (accessor.ExpressionBody != null)
                            result += $"{tabstr}\treturn {onExpressionSyntax(accessor.ExpressionBody.Expression)}\n";
                        else if (!isAbstract)
                        {
                            needStorage = true;
                            result += $"{tabstr}\treturn {propertySyntax.Identifier.Text}`Storage\n";
                        }
                    }
                    else if (accessor.Kind() == SyntaxKind.SetAccessorDeclaration)
                    {
                        if (!isOverride && !isStatic)
                        {
                            result += $"{tabstr}def operator . {propertySyntax.Identifier.Text} := ( value:{ptype} )\n";
                            result += $"{tabstr}\tset__{propertySyntax.Identifier.Text}(value)\n";
                        }
                        result += $"{tabstr}def {abstractMod}{staticMod}set__{propertySyntax.Identifier.Text} ( value:{ptype} ) : void\n";
                        if (accessor.Body != null)
                            result += onBlockSyntax(accessor.Body);
                        else if (accessor.ExpressionBody != null)
                            result += $"{tabstr}\t{onExpressionSyntax(accessor.ExpressionBody.Expression)}\n";
                        else if (!isAbstract)
                        {
                            needStorage = true;
                            result += $"{tabstr}\t{propertySyntax.Identifier.Text}`Storage = value\n";
                        }
                    }
                }
                if (needStorage)
                {
                    result += $"{tabstr}private {propertySyntax.Identifier.Text}`Storage : {ptype}";
                    if (propertySyntax.Initializer != null)
                        // TODO: use correct assignment operator
                        result += $" = {onExpressionSyntax(propertySyntax.Initializer.Value)}";
                }
            }

            return result;
        }

        string onEnumDeclaration(EnumDeclarationSyntax enu)
        {
            var tabstr = new string('\t', tabs + 1);
            var result = $"enum ";
            if (enu.Modifiers.Any(SyntaxKind.PublicKeyword))
                result += "public ";
            else if (enu.Modifiers.Any(SyntaxKind.PrivateKeyword))
                result += "private ";
            result += enu.Identifier.Text;
            if (enu.BaseList != null && enu.BaseList.Types.Count > 0)
                result += $" : {onTypeSyntax(enu.BaseList.Types[0].Type)}";
            result += "\n";
            foreach (var member in enu.Members)
            {
                var identifierName = member.Identifier.Text;
                if (member.EqualsValue != null)
                    result += $"{tabstr}{identifierName} = {onExpressionSyntax(member.EqualsValue.Value)}\n";
                else
                    result += $"{tabstr}{identifierName}\n";
            }
            return result;
        }

        string onMemberDeclaration(MemberDeclarationSyntax member)
        {
            switch (member.Kind())
            {
                case SyntaxKind.EnumDeclaration:
                    return onEnumDeclaration(member as EnumDeclarationSyntax);
                case SyntaxKind.NamespaceDeclaration:
                    return onNamespaceDeclaration(member as NamespaceDeclarationSyntax);
                case SyntaxKind.ClassDeclaration:
                    return onClassDeclaration(member as ClassDeclarationSyntax);
                case SyntaxKind.MethodDeclaration:
                    return onMethodDeclaration(member as MethodDeclarationSyntax);
                case SyntaxKind.FieldDeclaration:
                    return onFieldDeclaration(member as FieldDeclarationSyntax);
                case SyntaxKind.ConstructorDeclaration:
                    return onConstructorDeclaration(member as ConstructorDeclarationSyntax);
                case SyntaxKind.StructDeclaration:
                    return onStructDeclaration(member as StructDeclarationSyntax);
                case SyntaxKind.PropertyDeclaration:
                    return onPropertyDeclaration(member as PropertyDeclarationSyntax);
                default:
                    Fail($"Unsupported member {member.Kind()}");
                    return $"{member}";
            }
        }

        public string convert(CSharpCompilation comp, SemanticModel model, CompilationUnitSyntax root)
        {
            compilation = comp;
            semanticModel = model;
            foreach (var token in root.DescendantTrivia())
            {
                if (token.IsKind(SyntaxKind.SingleLineCommentTrivia) || token.IsKind(SyntaxKind.MultiLineCommentTrivia))
                    allComments.Add(token.Span.Start, token);
            }

            var result = "";
            UsingDirectiveSyntax prevDirective = null;
            foreach (UsingDirectiveSyntax u in root.Usings)
            {
                if (prevDirective != null)
                    InsertSpacesAndComments(ref result, prevDirective.Span, u.Span, u.SyntaxTree);
                result += onUsing(u);
                prevDirective = u;
            }
            result += "\n";
            MemberDeclarationSyntax prevMember = null;
            foreach (MemberDeclarationSyntax mem in root.Members)
            {
                if (prevMember != null)
                    InsertSpacesAndComments(ref result, prevMember.Span, mem.Span, mem.SyntaxTree);
                result += onMemberDeclaration(mem);
                prevMember = mem;
            }
            if (requirements.Count > 0)
            {
                var requirementsStr = string.Join("\nrequire ", requirements);
                result = $"require {requirementsStr}\n\n{result}";
            }
            return result;
        }
    }

}
