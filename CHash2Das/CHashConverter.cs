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

        void Fail ( string message )
        {
            if (failToDebug)
                Debug.Fail(message);
            else
                Console.WriteLine(message);
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
                        return $"{tname}{onTypeSyntax(atype.ElementType)}{tail} /*{atype.ElementType}{ranks}*/";
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
                                Fail($"unknown identifier type {itype.Identifier.Text}");
                                return $"{itype.Identifier.Text}";
                        }
                    }
                default:
                    Fail($"unsupported TypeSyntax {type.Kind()}");
                    return $"{type.Kind()}";
            }
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

        string onArrayCreationExpressionSyntax(ArrayCreationExpressionSyntax ac)
        {
            var result = ac.Initializer!=null ? "newInitArray(" : "newArray(";
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
            var result = "[{auto ";
            var first = true;
            foreach(ExpressionSyntax exp in ass.Expressions) 
            { 
                if (first) first = false;
                else result += "; ";
                result += onExpressionSyntax(exp);
            }
            result += "}]";
            return result;
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
                case SyntaxKind.InterpolatedStringExpression:
                    return onInterpolatedStringExpressionSyntax(expression as InterpolatedStringExpressionSyntax);
                case SyntaxKind.ArrayCreationExpression:
                    return onArrayCreationExpressionSyntax(expression as ArrayCreationExpressionSyntax);
                case SyntaxKind.ArrayInitializerExpression:
                    return onArrayInitializerExpressionSyntax(expression as InitializerExpressionSyntax);
                default:
                    Fail($"unsupported ExpressionSyntax {expression.Kind()}");
                    return $"{expression.ToString()}";
            }
        }

        string onInterpolatedStringExpressionSyntax( InterpolatedStringExpressionSyntax iss )
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

        List<string> onVariableDeclarationSyntax(VariableDeclarationSyntax vardecl, bool needVar = true)
        {
            var values = new List<string>();
            var tname = onTypeSyntax(vardecl.Type);
            foreach(SyntaxNode svar in vardecl.Variables)
            {
                var result = needVar ? "var " : "";
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

        bool hasBreakOrContinue(StatementSyntax statementSyntax)
        {
           return statementSyntax.DescendantNodes()
                .Any(node => node is BreakStatementSyntax || node is ContinueStatementSyntax);
        }

        bool isIdentifier ( ExpressionSyntax expression, string id )
        {
            if (!(expression is IdentifierNameSyntax))
                return false;
            if ((expression as IdentifierNameSyntax).Identifier.Text != id)
                return false;
            return true;
        }

        bool isOne ( ExpressionSyntax expression )
        {
            if (!(expression is LiteralExpressionSyntax)) return false;
            if ((expression as LiteralExpressionSyntax).Token.ValueText != "1") return false;
            return true;
        }

        bool isRangeFor(ForStatementSyntax fstmt, out string rangeExpr )
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
            switch ( inc.Kind() )
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
                        if ( !isIdentifier(aa.Left, vname)) return false;
                        if ( !isOne(aa.Right)) return false;
                        break;
                    }
                case SyntaxKind.SimpleAssignmentExpression:                     // its var = var + 1 or var = 1 + var
                    {
                        var aa = inc as AssignmentExpressionSyntax;
                        if ( !isIdentifier(aa.Left, vname)) return false;
                        if ( !(aa.Right is BinaryExpressionSyntax)) return false;
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
                        if(isIdentifier(binop.Left, vname))
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
            if (isRangeFor(fstmt,out string rangeExpr))
                return  $"{rangeExpr}\n{loopBlock(fstmt.Statement)}";
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
            tabstr = new string('\t', hasBorC ? (tabs+1) : tabs);
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

        string makeTempVar ( string suffix )
        {
            return $"__temp_{++tempVars}_{suffix}";
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
            if (!hasBorC)
                tabs--;
            if (hasBorC)
                result += $"{tabstr}finally\n";
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
                    result += $"{tabstr}{onIfStatement(ifstmt.Else.Statement as IfStatementSyntax,true)}";
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
            var tempval = makeTempVar("switchcase");
            var tabstr = new string('\t', tabs);
            var result = $"let {tempval} = {onExpressionSyntax(fs.Expression)}\n{tabstr}while true\n";
            var firstIf = true;
            tabs++; tabstr += '\t';
            var hasDefault = false;
            foreach (SwitchSectionSyntax section in fs.Sections)
            {
                if (firstIf)
                {
                    result += $"{tabstr}if ";
                    firstIf = false;
                } 
                else
                    result += $"{tabstr}elif ";
                var first = true;
                foreach(SwitchLabelSyntax st in section.Labels)
                {
                    if (first) first = false;
                    else result += " || ";
                    if (st is CaseSwitchLabelSyntax)
                        result += $"{tempval}=={(st as CaseSwitchLabelSyntax).Value}";
                    else if (st is DefaultSwitchLabelSyntax)
                    {
                        hasDefault = true;
                        result += "true";
                    }
                }
                result += "\n";
                tabs++;
                foreach (var ex in section.Statements)
                    result += $"{onStatementSyntax(ex)}";
                tabs--;
            }
            tabs--;
            if ( !hasDefault )
                result += $"{tabstr}break\n";
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
            var values = onVariableDeclarationSyntax(field.Declaration,false);
            string result = "";
            foreach (string val in values)
                result += $"{tabstr}{val}\n";
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
