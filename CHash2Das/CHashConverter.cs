﻿using static System.Console;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Linq.Expressions;
using System;
using System.Collections.Generic;

namespace CHash2Das
{
    public class CHashConverter
    {
        int tabs = 0;

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
                        foreach (ArrayRankSpecifierSyntax rank in atype.RankSpecifiers)
                        {
                            foreach (ExpressionSyntax size in rank.Sizes)
                            {
                                ranks += "[";
                                ranks += onExpressionSyntax(size);
                                ranks += "]";
                            }
                        }
                        return $"{atype.ElementType}{ranks}";
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
                            default:
                                Debug.Fail($"unsupported PredefinedType keyword {ptype.Keyword}");
                                return $"{ptype.Keyword.Text}";
                        }
                    }
                case SyntaxKind.IdentifierName:
                    {
                        var itype = type as IdentifierNameSyntax;
                        return $"{itype.Identifier.Text}";
                    }
                default:
                    Debug.Fail($"unsupported TypeSyntax {type.Kind()}");
                    break;
            }
            return $"{type.Kind()}";
        }

        public delegate string InvocationDelegate(CHashConverter converter, InvocationExpressionSyntax inv);

        Dictionary<string, InvocationDelegate> onInvExpr = new Dictionary<string, InvocationDelegate>();

        public void addInvocation(string key, InvocationDelegate inv)
        {
            if (onInvExpr.ContainsKey(key))
            {
                Debug.Fail("invocation expression {key} is already declared");
                return;
            }
            onInvExpr[key] = inv;
        }

        public string onArgumentListSyntax ( ArgumentListSyntax argumentList )
        {
            var arguments = "";
            var first = true;
            foreach (ArgumentSyntax arg in argumentList.Arguments)
            {
                if (first)
                    first = false;
                else
                    arguments += ", ";
                // TODO: handle named calls
                arguments += onExpressionSyntax(arg.Expression);
            }
            return arguments;
        }

        string onInvocationExpression( InvocationExpressionSyntax inv )
        {
            string key = inv.Expression.ToString();
            onInvExpr.TryGetValue(key, out InvocationDelegate invExpr);
            if (invExpr != null)
                return invExpr(this,inv);
            return $"{onExpressionSyntax(inv.Expression)}({onArgumentListSyntax(inv.ArgumentList)})";
        }

        string onExpressionSyntax(ExpressionSyntax expression)
        {
            if (expression == null)
                return "";
            switch (expression.Kind())
            {
                case SyntaxKind.ParenthesizedExpression:
                    return $"({onExpressionSyntax((expression as ParenthesizedExpressionSyntax).Expression)})";
                case SyntaxKind.InvocationExpression:
                    return onInvocationExpression(expression as  InvocationExpressionSyntax);
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
                    {
                        var binop = expression as BinaryExpressionSyntax;
                        // TODO: convert operator token properly
                        // WriteLine($"OP2 {binop.OperatorToken} // {expression.Kind()}\n");
                        return $"({onExpressionSyntax(binop.Left)} {binop.OperatorToken} {onExpressionSyntax(binop.Right)})";
                    }
                case SyntaxKind.SimpleAssignmentExpression:
                case SyntaxKind.LeftShiftAssignmentExpression:
                case SyntaxKind.RightShiftAssignmentExpression:
                case SyntaxKind.ExclusiveOrAssignmentExpression:
                case SyntaxKind.OrAssignmentExpression:
                case SyntaxKind.AndAssignmentExpression:
                    {
                        var binop = expression as AssignmentExpressionSyntax;
                        // TODO: convert operator token properly
                        // WriteLine($"OP2 {binop.OperatorToken} // {expression.Kind()}\n");
                        return $"({onExpressionSyntax(binop.Left)} {binop.OperatorToken} {onExpressionSyntax(binop.Right)})";
                    }
                case SyntaxKind.OmittedArraySizeExpression:
                    return "";  // in int[], this is the portion between the brackets
                case SyntaxKind.NumericLiteralExpression:
                case SyntaxKind.StringLiteralExpression:
                    return onSyntaxToken((expression as LiteralExpressionSyntax).Token);
                case SyntaxKind.SimpleMemberAccessExpression:
                    {
                        var smm = expression as MemberAccessExpressionSyntax;
                        return $"{onExpressionSyntax(smm.Expression)}.{smm.Name.Identifier.Text}";
                    }
                case SyntaxKind.IdentifierName:
                    return $"{(expression as IdentifierNameSyntax).Identifier.Text}";
                default:
                    Debug.Fail($"unsupported ExpressionSyntax {expression.Kind()}");
                    break;
            }
            return $"{expression.ToString()}";
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
                    Debug.Fail($"unsupported SyntaxToken {token.Kind()}");
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
                result += onMemberDeclaration(memberDeclaration);
            }
            return result;
        }

        string onClassDeclaration(ClassDeclarationSyntax classDeclaration)
        {
            var result = $"class {classDeclaration.Identifier}\n";
            tabs++;
            foreach (MemberDeclarationSyntax membersDeclaration in classDeclaration.Members)
            {
                result += onMemberDeclaration(membersDeclaration);
            }
            tabs--;
            return result;
        }

        string onMethodDeclaration(MethodDeclarationSyntax methodDeclaration)
        {
            var tabstr = new string('\t', tabs);
            var result = $"{tabstr}def {methodDeclaration.Identifier} ";
            if (methodDeclaration.ParameterList.Parameters.Count != 0)
            {
                result += "(";
                var first = true;
                foreach (ParameterSyntax param in methodDeclaration.ParameterList.Parameters)
                {
                    if (first)
                        first = false;
                    else
                        result += "; ";
                    result += $"{param.Identifier} : {onTypeSyntax(param.Type)}";
                }
                result += ")";
            }
            result += $" : {onTypeSyntax(methodDeclaration.ReturnType)}\n";
            result += onBlockSyntax(methodDeclaration.Body);
            return result;
        }

        List<string> onVariableDeclarationSyntax(VariableDeclarationSyntax vardecl)
        {
            var values = new List<string>();
            var tname = onTypeSyntax(vardecl.Type);
            foreach(SyntaxNode svar in vardecl.Variables)
            {
                var result = "var ";
                var declarator = svar as VariableDeclaratorSyntax;
                result += $"{declarator.Identifier.Text}";
                if (tname != "var")
                    result += $" : {tname}";
                if ( declarator.Initializer != null)
                    result += $" = {onExpressionSyntax(declarator.Initializer.Value)}";
                values.Add(result);
            }
            return values;
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
                        foreach(string val in values)
                            result += $"{tabstr}{val}\n";
                        return result;
                    }
                default:
                    Debug.Fail($"unsupported StatementSyntax {statement.Kind()}");
                    return $"{statement};";
            }
        }

        string onBlockSyntax(BlockSyntax block)
        {
            var result = "";
            tabs++;
            foreach (StatementSyntax expr in block.Statements)
            {
                result += onStatementSyntax(expr);
            }
            tabs--;
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
                default:
                    Debug.Fail($"Unsupported member {member.Kind()}");
                    return "";
            }
        }

        public string convert(CompilationUnitSyntax root)
        {
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