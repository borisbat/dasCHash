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

namespace CHash2Das
{
    public class CHashConverter
    {
        int tabs = 0;
        SemanticModel semanticModel;
        CSharpCompilation compilation;

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
                            case "sbyte":   return "int8";
                            case "byte":    return "uint8";
                            case "uint":    return "uint";
                            default:
                                Debug.Fail($"unknown PredefinedType keyword {ptype.Keyword}");
                                return $"{ptype.Keyword.Text}";
                        }
                    }
                case SyntaxKind.IdentifierName:
                    {
                        var itype = type as IdentifierNameSyntax;
                        switch (itype.Identifier.Text)
                        {
                            case "Int16":   return "int16";
                            case "UInt16":  return "uint16";
                            case "Int32":   return "int";
                            case "UInt32":  return "uint";
                            case "Int64":   return "int64";
                            case "UInt64":  return "uint64";
                            case "var":     return "var";       // huh?
                            default:
                                Debug.Fail($"unknown identifier type {itype.Identifier.Text}");
                                return $"{itype.Identifier.Text}";
                        }
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
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_UInt64));
        }

        bool isUInt64(ITypeSymbol ts)
        {
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_UInt64));
        }

        bool isLowBitsInteger(ITypeSymbol ts)
        {
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_Int16))
                || ts.Equals(compilation.GetSpecialType(SpecialType.System_UInt16))
                || ts.Equals(compilation.GetSpecialType(SpecialType.System_Byte))
                || ts.Equals(compilation.GetSpecialType(SpecialType.System_SByte));
        }

        int getTypeRank(ITypeSymbol ts)
        {
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

        string getDasCastName ( int rank )
        {
            switch ( rank)
            {
                case 7: return "double";
                case 6: return "float";
                case 5: return "uint64";
                case 4: return "int64";
                case 3: return "uint";
                case 2: return "int";
                case 0: return "int";
            }
            Debug.Fail("we should not be here. why cast?");
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
            var castRank = getBinaryExpressionCastType(binop.OperatorToken.Text,leftRank,rightRank);
            var result = "(";
            if (leftRank != castRank && castRank!=-1)
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
                    return onBinaryExpressionSyntax(expression as BinaryExpressionSyntax);
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
                        return $"{onExpressionSyntax(binop.Left)} {binop.OperatorToken} {onExpressionSyntax(binop.Right)}";
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

        string onWhileStatement(WhileStatementSyntax wstmt)
        {
            var tabstr = new string('\t', tabs);
            var result = $"while {wstmt.Condition}\n";
            if (wstmt.Statement is BlockSyntax)
                result += onStatementSyntax(wstmt.Statement);
            else
            {
                tabs++;
                result += onStatementSyntax(wstmt.Statement);
                tabs--;
            }
            return result;
        }
        string onIfStatement(IfStatementSyntax ifstmt, bool isElif = false)
        {
            var tabstr = new string('\t', tabs);
            var result = isElif ? $"elif " : $"if ";
            result += $"{onExpressionSyntax(ifstmt.Condition)}\n";
            if ( ifstmt.Statement is BlockSyntax )
                result += onStatementSyntax(ifstmt.Statement);
            else
            {
                tabs++;
                result += onStatementSyntax(ifstmt.Statement);
                tabs--;

            }
            if (ifstmt.Else != null)
            {
                if (ifstmt.Else.Statement is IfStatementSyntax)
                    result += $"{tabstr}{onIfStatement(ifstmt.Else.Statement as IfStatementSyntax,true)}";
                else
                {
                    result += $"{tabstr}else\n";
                    if (ifstmt.Else.Statement is BlockSyntax)
                        result += onStatementSyntax(ifstmt.Else.Statement);
                    else
                    {
                        tabs++;
                        result += onStatementSyntax(ifstmt.Else.Statement);
                        tabs--;
                    }
                }
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
                        foreach(string val in values)
                            result += $"{tabstr}{val}\n";
                        return result;
                    }
                case SyntaxKind.IfStatement:
                    return $"{tabstr}{onIfStatement(statement as IfStatementSyntax)}";
                case SyntaxKind.WhileStatement:
                    return $"{tabstr}{onWhileStatement(statement as WhileStatementSyntax)}";
                case SyntaxKind.Block:
                    return onBlockSyntax(statement as BlockSyntax);
                case SyntaxKind.BreakStatement:
                    return $"{tabstr}break\n";
                case SyntaxKind.ContinueStatement:
                    return $"{tabstr}continue\n";
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
