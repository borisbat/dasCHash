using static System.Console;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Linq.Expressions;
using System;
using System.Collections.Generic;
using System.Xml.Linq;

namespace CHash2Das
{
    public class CHashConverter
    {
        bool failToDebug = true;
        int tabs = 0;
        int tempVars = 0;
        SemanticModel semanticModel;
        CSharpCompilation compilation;

        void Fail(string message)
        {
            if (failToDebug)
                Debug.Fail(message);
            else
                Console.WriteLine(message);
        }

        string onVarTypeSyntax(TypeSyntax ts)
        {
            var tt = semanticModel.GetTypeInfo(ts);
            var txt = onTypeSyntax(ts);
            if (isClassOrStructRef(tt.Type)) txt += "?";
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
                Debug.Fail($"method {typeWithMethod.MetadataName}.{typeWithMethod.FieldName} is already declared");
                return;
            }
            methodInvExpr[typeWithMethod] = inv;
        }

        public void addMemberAccess(INamedTypeSymbolField typeWithMethod, MemberAccessDelegate acc)
        {
            if (memberAccessExpr.ContainsKey(typeWithMethod))
            {
                Debug.Fail($"member access {typeWithMethod.MetadataName}.{typeWithMethod.FieldName} is already declared");
                return;
            }
            memberAccessExpr[typeWithMethod] = acc;
        }

        public string onArgumentListSyntax(ArgumentListSyntax argumentList)
        {
            return string.Join(", ", argumentList.Arguments.Select(arg => onExpressionSyntax(arg.Expression)));
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
            onInvExpr.TryGetValue(key, out InvocationDelegate invExpr);
            if (invExpr != null)
                return invExpr(this, inv);
            var callText = "";
            if (IsCallingClassMethod(inv) && !IsCallingStaticMethod(inv))
            {
                if (inv.Expression.Kind() == SyntaxKind.SimpleMemberAccessExpression)
                {
                    var ma = inv.Expression as MemberAccessExpressionSyntax;
                    var exprTypeInfo = semanticModel.GetTypeInfo(ma.Expression);
                    methodInvExpr.TryGetValue(new INamedTypeSymbolField()
                    {
                        MetadataName = exprTypeInfo.Type.MetadataName,
                        ContainingNamespace = exprTypeInfo.Type.ContainingNamespace?.ToDisplayString(),
                        FieldName = ma.Name.Identifier.Text
                    }, out invExpr);
                    if (invExpr != null)
                        callText = invExpr(this, inv);
                    else
                        callText = $"{onExpressionSyntax(ma.Expression)}->{ma.Name.Identifier.Text}";
                }
            }
            if (callText == "") callText = onExpressionSyntax(inv.Expression);
            return $"{callText}({onArgumentListSyntax(inv.ArgumentList)})";
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

        bool isClassOrStructRef(ITypeSymbol typeSymbol)
        {
            if (typeSymbol == null)
                return false;

            // Check if it's a built-in type (like int, double, etc.)
            if (typeSymbol.IsValueType && typeSymbol.SpecialType != SpecialType.None)
                return false;

            switch (typeSymbol.TypeKind)
            {
                case TypeKind.Class:
                    return true;
                case TypeKind.Struct:
                    // Exclude pointers and other non-standard structs
                    if (typeSymbol is INamedTypeSymbol namedType && namedType.IsTupleType)
                        return false;
                    return true;
                default:
                    return false;
            }
        }


        int getTypeRank(ITypeSymbol ts)
        {
            if (ts == null) return -1;
            // double > float > uint64 > int64 > uint32 > int32
            // any int bellow is going to int32
            if (isDouble(ts)) return 7;
            if (isFloat(ts)) return 6;
            if (isUInt64(ts)) return 5;
            if (isInt64(ts)) return 4;
            if (isUInt32(ts)) return 3;
            if (isInt32(ts)) return 2;
            // lowest rank
            if (isLowBitsInteger(ts)) return 0;
            return -1;
        }

        string getDasCastName(int rank)
        {
            switch (rank)
            {
                case 7: return "double";
                case 6: return "float";
                case 5: return "uint64";
                case 4: return "int64";
                case 3: return "uint";
                case 2: return "int";
                case 0: return "int";
            }
            Fail("we should not be here. why cast?");
            return "";
        }

        int getBinaryExpressionCastType(string op, int leftRank, int rightRank)
        {
            // TODO: make operator dependent type promotion
            //  for example i32 + u32 is LONG
            var maxRank = Math.Max(leftRank, rightRank);
            if (maxRank == -1) return -1;
            else if (maxRank == 0) return 2;
            else return maxRank;
        }

        string onBinaryExpressionSyntax(BinaryExpressionSyntax binop)
        {
            var leftType = semanticModel.GetTypeInfo(binop.Left);
            var rightType = semanticModel.GetTypeInfo(binop.Right);
            var leftRank = getTypeRank(leftType.Type);
            var rightRank = getTypeRank(rightType.Type);
            var castRank = getBinaryExpressionCastType(binop.OperatorToken.Text, leftRank, rightRank);
            var result = "(";
            if (leftRank != castRank && castRank != -1)
                result += $"{getDasCastName(castRank)}(";
            result += onExpressionSyntax(binop.Left);
            if (leftRank != castRank && castRank != -1)
                result += ")";
            result += $" {binop.OperatorToken} ";
            if (rightRank != castRank && castRank != -1)
                result += $"{getDasCastName(castRank)}(";
            result += onExpressionSyntax(binop.Right);
            if (rightRank != castRank && castRank != -1)
                result += ")";
            return $"{result})";
        }

        string onArrayCreationExpressionSyntax(ArrayCreationExpressionSyntax ac)
        {
            var result = ac.Initializer != null ? "newInitArray(" : "newArray(";
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
            if (ac.Initializer != null)
                result += $", {onArrayInitializerExpressionSyntax(ac.Initializer)}";
            result += ")";
            return result;
        }

        string onArrayInitializerExpressionSyntax(InitializerExpressionSyntax ass)
        {
            return "[{auto " + string.Join("; ", ass.Expressions.Select(exp => onExpressionSyntax(exp))) + "}]";
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
                return onObjectCreationExpression_ClassOrStruct(oce);
            }
            Fail($"unsupported object creation {oce}");
            return $"{oce}";
        }

        private string onObjectCreationExpression_ClassOrStruct(ObjectCreationExpressionSyntax oce)
        {
            var arguments = oce.ArgumentList.Arguments
                .Select(arg => onExpressionSyntax(arg.Expression));
            if (arguments.Count() == 0)
                return $"new [[{onTypeSyntax(oce.Type)}()]]";
            var arguments2 = arguments.Aggregate((current, next) => $"{current}, {next}");
            return $"new [[{onTypeSyntax(oce.Type)}({arguments2})]]";
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
            var result = "new [{auto "; // TODO: actual type
            if (oce.Initializer.Kind() == SyntaxKind.CollectionInitializerExpression)
            {
                foreach (ExpressionSyntax element in oce.Initializer.Expressions)
                {
                    if (element.Kind() == SyntaxKind.ObjectInitializerExpression)
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

        bool reqMove(ExpressionSyntax expression)
        {
            if (expression == null)
                return false;

            switch (expression.Kind())
            {
                case SyntaxKind.ParenthesizedExpression:
                    return reqMove((expression as ParenthesizedExpressionSyntax).Expression);
                case SyntaxKind.NumericLiteralExpression:
                case SyntaxKind.NullLiteralExpression:
                case SyntaxKind.TrueLiteralExpression:
                case SyntaxKind.FalseLiteralExpression:
                case SyntaxKind.CharacterLiteralExpression:
                    return false;
                case SyntaxKind.ArrayInitializerExpression:
                case SyntaxKind.ObjectInitializerExpression:
                case SyntaxKind.CollectionInitializerExpression:
                case SyntaxKind.ComplexElementInitializerExpression:
                    return true;
                default:
                    var type = semanticModel.GetTypeInfo(expression);
                    if (type.Type == null)
                    {
                        Fail($"unsupported ExpressionSyntax {expression.Kind()}");
                    }
                    if (type.Type == null || type.Type.IsValueType)
                    {
                        return false;
                    }
                    return true;
            }
        }

        public string onExpressionSyntax(ExpressionSyntax expression)
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
                        // TODO: convert operator token properly
                        // WriteLine($"OP2 {binop.OperatorToken} // {expression.Kind()}\n");
                        return $"{onExpressionSyntax(binop.Left)} {binop.OperatorToken} {onExpressionSyntax(binop.Right)}";
                    }
                case SyntaxKind.OmittedArraySizeExpression:
                    return "";  // in int[], this is the portion between the brackets
                case SyntaxKind.NumericLiteralExpression:
                    var leftType = semanticModel.GetTypeInfo(expression);
                    var typeRank = getTypeRank(leftType.Type);
                    if (typeRank == -1)
                        return $"{expression}";
                    else
                        return $"{getDasCastName(typeRank)}({expression})";
                case SyntaxKind.StringLiteralExpression:
                    return onSyntaxToken((expression as LiteralExpressionSyntax).Token);
                case SyntaxKind.SimpleMemberAccessExpression:
                    {
                        var smm = expression as MemberAccessExpressionSyntax;
                        TypeInfo typeInfo = semanticModel.GetTypeInfo(smm.Expression);
                        if (typeInfo.Type != null && memberAccessExpr.TryGetValue(new INamedTypeSymbolField()
                        {
                            MetadataName = typeInfo.Type.MetadataName,
                            ContainingNamespace = typeInfo.Type.ContainingNamespace?.ToDisplayString(),
                            FieldName = smm.Name.Identifier.Text
                        }, out MemberAccessDelegate acc))
                        {
                            return acc(this, smm);
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
                    return onArrayInitializerExpressionSyntax(expression as InitializerExpressionSyntax);
                case SyntaxKind.ObjectCreationExpression:
                    return onObjectCreationExpression(expression as ObjectCreationExpressionSyntax);
                default:
                    Fail($"unsupported ExpressionSyntax {expression.Kind()}");
                    return $"{expression.ToString()}";
            }
        }

        string onInterpolatedStringExpressionSyntax(InterpolatedStringExpressionSyntax iss)
        {
            var result = "StringBuilder(";
            var first = true;
            foreach (var content in iss.Contents)
            {
                if (first)
                    first = false;
                else
                    result += ", ";
                switch (content)
                {
                    case InterpolatedStringTextSyntax textSyntax:
                        result += $"\"{content}\""; // TODO: print string better
                        break;
                    case InterpolationSyntax interpolationSyntax:
                        result += onExpressionSyntax(interpolationSyntax.Expression);
                        break;
                }
            }
            return $"{result})";
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
            var result = $"// namespaced {namespaceDeclaration.Name}\n";
            foreach (MemberDeclarationSyntax memberDeclaration in namespaceDeclaration.Members)
            {
                result += onMemberDeclaration(memberDeclaration) + "\n";
            }
            return result;
        }

        string onClassDeclaration(ClassDeclarationSyntax classDeclaration)
        {
            var result = $"class {classDeclaration.Identifier}\n";
            tabs++;
            foreach (MemberDeclarationSyntax membersDeclaration in classDeclaration.Members)
            {
                result += onMemberDeclaration(membersDeclaration) + "\n";
            }
            tabs--;
            return result;
        }

        string onConstructorDeclaration(ConstructorDeclarationSyntax methodDeclaration)
        {
            var tabstr = new string('\t', tabs);
            var result = $"{tabstr}def {methodDeclaration.Identifier}";
            if (methodDeclaration.ParameterList.Parameters.Count != 0)
            {
                var parameters = methodDeclaration.ParameterList.Parameters
                    .Select(param => $"{param.Identifier} : {onVarTypeSyntax(param.Type)}");
                result += $" ({string.Join("; ", parameters)})";
            }
            result += $"\n{onBlockSyntax(methodDeclaration.Body)}";
            return result;
        }

        string onMethodDeclaration(MethodDeclarationSyntax methodDeclaration)
        {
            var tabstr = new string('\t', tabs);
            var result = $"{tabstr}def {methodDeclaration.Identifier}";
            if (methodDeclaration.ParameterList.Parameters.Count != 0)
            {
                var parameters = methodDeclaration.ParameterList.Parameters
                    .Select(param => $"{param.Identifier} : {onVarTypeSyntax(param.Type)}");
                result += $" ({string.Join("; ", parameters)})";
            }
            result += $" : {onVarTypeSyntax(methodDeclaration.ReturnType)}\n";
            result += onBlockSyntax(methodDeclaration.Body);
            return result;
        }

        List<string> onVariableDeclarationSyntax(VariableDeclarationSyntax vardecl, bool needVar = true)
        {
            var values = new List<string>();
            var tname = onVarTypeSyntax(vardecl.Type);
            foreach (VariableDeclaratorSyntax declarator in vardecl.Variables)
            {
                var result = needVar ? "var " : "";
                result += $"{declarator.Identifier.Text}";
                if (tname != "var" && tname != "var?")
                    result += $" : {tname}";
                if (declarator.Initializer != null)
                {
                    var assign = reqMove(declarator.Initializer.Value) ? "<-" : "=";
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
            Console.WriteLine("here");
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
            var tabstr = new string('\t', tabs);
            if (isRangeFor(fstmt, out string rangeExpr))
                return $"{rangeExpr}\n{loopBlock(fstmt.Statement)}";
            var result = "// for\n";
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
            return $"for {fs.Identifier.Text} in {onExpressionSyntax(fs.Expression)}\n{loopBlock(fs.Statement)}";
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
                foreach (var ex in section.Statements)
                    if (!simpleSwitchCase || !(ex is BreakStatementSyntax))
                        result += $"{onStatementSyntax(ex)}";
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
                    return $"{tabstr}return {onExpressionSyntax((statement as ReturnStatementSyntax).Expression)}\n";
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
                foreach (StatementSyntax expr in block.Statements)
                {
                    result += onStatementSyntax(expr);
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
            var isPrivate = field.Modifiers.Any(mod => mod.Kind() == SyntaxKind.PrivateKeyword);
            var prefix = isPrivate ? "private " : "";
            foreach (string val in values)
                result += $"{tabstr}{prefix}{val}\n";
            return result;
        }

        string onMemberDeclaration(MemberDeclarationSyntax member)
        {
            switch (member.Kind())
            {
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
                default:
                    Fail($"Unsupported member {member.Kind()}");
                    return $"{member}";
            }
        }

        public string convert(CSharpCompilation comp, SemanticModel model, CompilationUnitSyntax root)
        {
            compilation = comp;
            semanticModel = model;

            var result = "";
            foreach (UsingDirectiveSyntax u in root.Usings)
            {
                result += onUsing(u);
            }
            result += "\n";
            foreach (MemberDeclarationSyntax mem in root.Members)
            {
                result += onMemberDeclaration(mem);
            }
            return result;
        }
    }

}
