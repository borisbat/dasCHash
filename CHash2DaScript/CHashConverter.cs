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
        List<string> topLevel = new List<string>();

        Dictionary<TypeData, ClassDeclarationSyntax> genericTypes = new Dictionary<TypeData, ClassDeclarationSyntax>();
        HashSet<TypeData> dummyTypes = new HashSet<TypeData>();

        HashSet<TemplateInstance> instantiateTemplates = new HashSet<TemplateInstance>();
        HashSet<string> instantiatedTemplates = new HashSet<string>();

        public void addRequirement(string module_name)
        {
            if (!requirements.Contains(module_name))
                requirements.Add(module_name);
        }

        public void addTopLevel(string code)
        {
            topLevel.Add(code);
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

        string onVarTypeSyntax(ITypeSymbol? tt)
        {
            if (tt == null)
                return "var";
            if (tt is ITypeParameterSymbol tps)
            {
                var td = new TypeData()
                {
                    type = tps.Name,
                    ns = tps.DeclaringType?.Name,
                };
                if (typesRename.TryGetValue(td, out var rename))
                {
                    return rename(this, td);
                }
            }
            else if (tt is INamedTypeSymbol nts)
            {
                var className = nts.Name;
                var td = new TypeData()
                {
                    type = className,
                    ns = nts.ContainingNamespace?.ToDisplayString()
                };
                if (typesRename.TryGetValue(td, out var rename))
                {
                    return rename(this, td);
                }
            }
            else
            {
                Fail($"unknown type {tt}");
            }
            var res = dasTypeName(tt);
            if (isString(tt))
                res = "string";
            if (isBool(tt))
                res = "bool";
            if (isVoid(tt))
                res = "void";
            return !string.IsNullOrEmpty(res) ? res : tt.Name;
        }
        string onVarTypeSyntax(TypeSyntax ts)
        {
            if (ts == null)
                return "var"; // unknown (auto) type
            var tt = semanticModel.GetTypeInfo(ts);
            if (tt.Type is ITypeParameterSymbol tps)
            {
                var td = new TypeData()
                {
                    type = tps.Name,
                    ns = tps.DeclaringType?.Name,
                };
                if (typesRename.TryGetValue(td, out var rename))
                {
                    return rename(this, td);
                }
                var contType = semanticModel.GetSymbolInfo(ts).Symbol?.ContainingType;
                if (contType != null)
                {
                    td = new TypeData()
                    {
                        type = tps.ToString(),
                        ns = contType.Name,
                    };
                    if (typesRename.TryGetValue(td, out rename))
                    {
                        return rename(this, td);
                    }
                }
            }
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
            {
                var td = new TypeData()
                {
                    type = type.ToString(),
                    ns = semanticModel.GetSymbolInfo(type).Symbol?.ContainingNamespace?.ToDisplayString(),
                };
                if (typesRename.TryGetValue(td, out var rename))
                {
                    return rename(this, td);
                }
            }
            var contType = semanticModel.GetSymbolInfo(type).Symbol?.ContainingType;
            if (contType != null)
            {
                var td = new TypeData()
                {
                    type = type.ToString(),
                    ns = contType.Name,
                };
                if (typesRename.TryGetValue(td, out var rename))
                {
                    return rename(this, td);
                }
            }
            var tt = semanticModel.GetTypeInfo(type);
            if (tt.Type is INamedTypeSymbol nts)
            {
                var className = nts.Name;
                var td = new TypeData()
                {
                    type = className,
                    ns = nts.ContainingNamespace?.ToDisplayString()
                };
                if (typesRename.TryGetValue(td, out var rename))
                {
                    return rename(this, td);
                }
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
                            case "object": return "any";
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
                            case "IEnumerator": return "iterator";

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
                            case "IEnumerator":
                                return $"iterator<{onTypeSyntax(genn.TypeArgumentList.Arguments[0])}>";
                        }
                        // Fail($"unsupported TypeSyntax {genn}");
                        var args = genn.TypeArgumentList.Arguments.Select(arg => onTypeSyntax(arg)).ToArray();
                        var argsVal = genn.TypeArgumentList.Arguments.Count > 0 ? $"_{string.Join("_", args)}" : "";
                        foreach (var arg in genn.TypeArgumentList.Arguments)
                        {
                            // register type instance only if type argument is a known type
                            if (semanticModel.GetTypeInfo(arg).Type is INamedTypeSymbol ntype)
                            {
                                if (!ntype.IsDefinition)
                                {
                                    instantiateTemplate(new TypeData()
                                    {
                                        type = genn.Identifier.Text,
                                        ns = semanticModel.GetSymbolInfo(genn).Symbol.ContainingNamespace?.ToDisplayString(),
                                    }, args);
                                    break;
                                }
                            }
                        }
                        return $"{genn.Identifier.Text}{argsVal}";
                    }
                case SyntaxKind.QualifiedName:
                    {
                        var cname = type as QualifiedNameSyntax;
                        return $"{onTypeSyntax(cname.Left)}::{onTypeSyntax(cname.Right)}";
                    }
                case SyntaxKind.NullableType:
                    {
                        var ntype = type as NullableTypeSyntax;
                        return $"{onTypeSyntax(ntype.ElementType)}?";
                    }
                default:
                    Fail($"unsupported TypeSyntax {type.Kind()}");
                    return $"{type}";
            }
        }

        public delegate string InvocationDelegate(CHashConverter converter, InvocationExpressionSyntax inv);
        public delegate string MemberAccessDelegate(CHashConverter converter, MemberAccessExpressionSyntax inv);
        public delegate string SetMemberAccessDelegate(CHashConverter converter, AssignmentExpressionSyntax inv);
        public delegate string OperatorOverloadDelegate(CHashConverter converter, ExpressionSyntax expr);
        public delegate string TypeRenameDelegate(CHashConverter converter, TypeData ts);
        public delegate string UsingRenameDelegate(CHashConverter converter, string usingName);
        public delegate string CtorDelegate(CHashConverter converter, ObjectCreationExpressionSyntax objCreation);

        Dictionary<string, InvocationDelegate> onInvExpr = new Dictionary<string, InvocationDelegate>();
        Dictionary<string, InvocationDelegate> objectInvExpr = new Dictionary<string, InvocationDelegate>();
        Dictionary<TypeField, InvocationDelegate> methodInvExpr = new Dictionary<TypeField, InvocationDelegate>();
        Dictionary<TypeField, MemberAccessDelegate> memberAccessExpr = new Dictionary<TypeField, MemberAccessDelegate>();
        Dictionary<TypeField, SetMemberAccessDelegate> setMemberAccessExpr = new Dictionary<TypeField, SetMemberAccessDelegate>();
        Dictionary<string, MemberAccessDelegate> objectMemberAccessExpr = new Dictionary<string, MemberAccessDelegate>();
        Dictionary<OperatorOverload, OperatorOverloadDelegate> operatorOverloads = new Dictionary<OperatorOverload, OperatorOverloadDelegate>();
        Dictionary<TypeData, TypeRenameDelegate> typesRename = new Dictionary<TypeData, TypeRenameDelegate>();
        HashSet<TypeData> dropPointersFlags = new HashSet<TypeData>();
        Dictionary<string, UsingRenameDelegate> usingRename = new Dictionary<string, UsingRenameDelegate>();
        Dictionary<TypeData, CtorDelegate> ctorExpr = new Dictionary<TypeData, CtorDelegate>();

        struct TemplateInstance
        {
            public TypeData typeData;
            public string[] paramNames;

            public override int GetHashCode()
            {
                var hash = typeData.GetHashCode();
                foreach (var param in paramNames)
                {
                    hash = hash * 23 + param.GetHashCode();
                }
                return hash;
            }
        }

        Stack<string[]> currentClassParams = new Stack<string[]>();

        public void addInvocation(string key, InvocationDelegate inv, bool override_ = false)
        {
            if (!override_ && onInvExpr.ContainsKey(key))
            {
                Fail("invocation expression {key} is already declared");
                return;
            }
            onInvExpr[key] = inv;
        }

        public void addMethod(TypeData typeInfo, string field, InvocationDelegate inv, bool override_ = false)
        {
            addMethod(new TypeField(typeInfo, field), inv, override_);
        }
        public void addMethod(TypeField typeWithMethod, InvocationDelegate inv, bool override_ = false)
        {
            if (!override_ && methodInvExpr.ContainsKey(typeWithMethod))
            {
                Fail($"method {typeWithMethod.type}.{typeWithMethod.field} is already declared");
                return;
            }
            methodInvExpr[typeWithMethod] = inv;
        }

        public bool getMethod(IMethodSymbol mti, out InvocationDelegate inv)
        {
            return getMethod(mti.ContainingType, mti.Name, out inv);
        }
        public bool getMethod(TypeInfo ti, string name, out InvocationDelegate inv)
        {
            return getMethod(ti.Type, name, out inv);
        }
        public bool getMethod(ITypeSymbol? ti, string name, out InvocationDelegate inv)
        {
            var curType = ti;
            while (curType != null)
            {
                if (methodInvExpr.TryGetValue(new TypeField()
                {
                    type = curType.MetadataName,
                    ns = curType.ContainingNamespace?.ToDisplayString(),
                    field = name
                }, out inv))
                {
                    return true;
                }
                curType = curType.BaseType;
            }
            inv = default;
            return false;
        }

        public void addObjectMethod(string member, InvocationDelegate inv, bool override_ = false)
        {
            if (!override_ && objectInvExpr.ContainsKey(member))
            {
                Fail($"method Object.{member} is already declared");
                return;
            }
            objectInvExpr[member] = inv;
        }

        public void addSetField(TypeData typeData, string field, SetMemberAccessDelegate acc, bool override_ = false)
        {
            addSetField(new TypeField(typeData, field), acc, override_);
        }
        public void addSetField(TypeField typeWithMethod, SetMemberAccessDelegate acc, bool override_ = false)
        {
            if (!override_ && setMemberAccessExpr.ContainsKey(typeWithMethod))
            {
                Fail($"set member access {typeWithMethod.type}.{typeWithMethod.field} is already declared");
                return;
            }
            setMemberAccessExpr[typeWithMethod] = acc;
        }

        public void addField(TypeData typeData, string field, MemberAccessDelegate acc, bool override_ = false)
        {
            addField(new TypeField(typeData, field), acc, override_);
        }
        public void addField(TypeField typeWithMethod, MemberAccessDelegate acc, bool override_ = false)
        {
            if (!override_ && memberAccessExpr.ContainsKey(typeWithMethod))
            {
                Fail($"member access {typeWithMethod.type}.{typeWithMethod.field} is already declared");
                return;
            }
            memberAccessExpr[typeWithMethod] = acc;
        }

        public bool getField(TypeInfo ti, string name, out MemberAccessDelegate acc)
        {
            return getField(ti.Type, name, out acc);
        }

        public bool getField(ITypeSymbol? ti, string name, out MemberAccessDelegate acc)
        {
            var curType = ti;
            while (curType != null)
            {
                if (memberAccessExpr.TryGetValue(new TypeField()
                {
                    type = curType.MetadataName,
                    ns = curType.ContainingNamespace?.ToDisplayString(),
                    field = name
                }, out acc))
                {
                    return true;
                }
                curType = curType.BaseType;
            }
            acc = default;
            return false;
        }

        public bool getSetField(TypeInfo ti, string name, out SetMemberAccessDelegate acc)
        {
            return getSetField(ti.Type, name, out acc);
        }

        public bool getSetField(ITypeSymbol? ti, string name, out SetMemberAccessDelegate acc)
        {
            var curType = ti;
            while (curType != null)
            {
                if (setMemberAccessExpr.TryGetValue(new TypeField()
                {
                    type = curType.MetadataName,
                    ns = curType.ContainingNamespace?.ToDisplayString(),
                    field = name
                }, out acc))
                {
                    return true;
                }
                curType = curType.BaseType;
            }
            acc = default;
            return false;
        }

        public void renameType(TypeData type, TypeRenameDelegate tr, bool override_ = false)
        {
            if (!override_ && typesRename.ContainsKey(type))
            {
                Fail($"type rename for {type.type} is already declared");
                return;
            }
            typesRename[type] = tr;
        }

        public void dropPointerFlag(TypeData type, bool override_ = false)
        {
            if (!override_ && dropPointersFlags.Contains(type))
            {
                Fail($"type {type.type} is already declared to drop pointer flag");
                return;
            }
            dropPointersFlags.Add(type);
        }

        public bool removeRenameType(TypeData type)
        {
            return typesRename.Remove(type);
        }

        /// <summary>
        /// Rename a using statement, pass "*" to rename all usings
        /// </summary>
        public void renameUsing(string usingName, UsingRenameDelegate ur, bool override_ = false)
        {
            if (!override_ && usingRename.ContainsKey(usingName))
            {
                Fail($"using {usingName} is already declared");
                return;
            }
            usingRename[usingName] = ur;
        }

        public void addObjectMemberAccess(string name, MemberAccessDelegate acc, bool override_ = false)
        {
            if (!override_ && objectMemberAccessExpr.ContainsKey(name))
            {
                Fail($"object member access {name} is already declared");
                return;
            }
            objectMemberAccessExpr[name] = acc;
        }

        public void instantiateTemplate(TypeData typeData, string[] paramNames)
        {
            instantiateTemplates.Add(new TemplateInstance()
            {
                typeData = typeData,
                paramNames = paramNames
            });
        }

        public void addOperatorOverload(TypeData td, SyntaxKind kind, OperatorOverloadDelegate ood, bool override_ = false)
        {
            addOperatorOverload(new OperatorOverload(td, kind), ood, override_);
        }
        public void addOperatorOverload(OperatorOverload operatorOverload, OperatorOverloadDelegate ood, bool override_ = false)
        {
            if (!override_ && operatorOverloads.ContainsKey(operatorOverload))
            {
                Fail($"operator overload {operatorOverload} is already declared");
                return;
            }
            operatorOverloads[operatorOverload] = ood;
        }

        public void addCtor(TypeData typeData, CtorDelegate ctor, bool override_ = false)
        {
            if (!override_ && ctorExpr.ContainsKey(typeData))
            {
                Fail($"ctor {typeData} is already declared");
                return;
            }
            ctorExpr[typeData] = ctor;
        }

        public string onExpressionArgumentSyntax(ExpressionSyntax expression)
        {
            var argSymbol = semanticModel.GetSymbolInfo(expression);
            if (expression.Kind() == SyntaxKind.IdentifierName && argSymbol.Symbol is IMethodSymbol methodSymbol)
            {
                // delegate which is not a lambda - convert to lambda
                var @params = "";
                var args = "";
                var first = true;
                foreach (var param in methodSymbol.Parameters)
                {
                    if (first) first = false;
                    else
                    {
                        args += ", ";
                        @params += "; ";
                    }
                    string v = onVarTypeSyntax(param.Type);
                    @params += $"var {param.Name} : {v}"; // all args are var
                    args += $"{param.Name}";
                }
                var returnPrefix = methodSymbol.ReturnsVoid ? "" : "return ";
                return $"@({@params}) {{ {returnPrefix}self->{onExpressionSyntax(expression)}({args}); }}";
            }
            return onExpressionSyntax(expression);
        }

        public string onArgumentListSyntax(InvocationExpressionSyntax inv, bool addBrackets = true, bool addSelf = false, bool genericTypes = false, bool pipeDelegate = true, bool reverseArgs = false)
        {
            var typeArgsVal = "";
            var argsVal = "";
            var invTypeInfo = semanticModel.GetSymbolInfo(inv.Expression);
            if (invTypeInfo.Symbol is IMethodSymbol methodSymbol)
            {
                if (genericTypes && methodSymbol.TypeArguments.Length > 0)
                {
                    var typeArgs = methodSymbol.TypeArguments.Select(x => $"type<{onVarTypeSyntax(x)}>");
                    typeArgsVal = string.Join(", ", typeArgs);
                }
                if (pipeDelegate && inv.ArgumentList.Arguments.Count > 0
                    && (reverseArgs && isDelegate(methodSymbol.Parameters.First().Type) || (!reverseArgs && isDelegate(methodSymbol.Parameters.Last().Type))))
                {
                    string args = "";
                    ArgumentSyntax lastArg;
                    if (reverseArgs)
                    {
                        args = string.Join(", ", inv.ArgumentList.Arguments.TakeLast(inv.ArgumentList.Arguments.Count - 1).Reverse().Select(arg => onExpressionArgumentSyntax(arg.Expression)));
                        lastArg = inv.ArgumentList.Arguments.First();
                    }
                    else
                    {
                        args = string.Join(", ", inv.ArgumentList.Arguments.Take(inv.ArgumentList.Arguments.Count - 1).Select(arg => onExpressionArgumentSyntax(arg.Expression)));
                        lastArg = inv.ArgumentList.Arguments.Last();
                    }
                    var isLastArgLambda = lastArg.Expression.Kind() == SyntaxKind.ParenthesizedLambdaExpression || lastArg.Expression.Kind() == SyntaxKind.SimpleLambdaExpression || lastArg.Expression.Kind() == SyntaxKind.AnonymousMethodExpression;
                    var lastArgValue = onExpressionArgumentSyntax(lastArg.Expression);
                    if (!isLastArgLambda)
                    {
                        var delimiter = args.Length > 0 ? ", " : "";
                        argsVal = $"{args}{delimiter}{lastArgValue})";
                    }
                    else
                    {
                        argsVal = $"{args}) <| {lastArgValue}";
                    }
                }
            }
            if (argsVal.Length == 0)
            {
                if (reverseArgs)
                    argsVal = string.Join(", ", inv.ArgumentList.Arguments.Reverse().Select(arg => onExpressionArgumentSyntax(arg.Expression)));
                else
                    argsVal = string.Join(", ", inv.ArgumentList.Arguments.Select(arg => onExpressionArgumentSyntax(arg.Expression)));
                if (addBrackets)
                    argsVal += ")";
            }
            if (typeArgsVal.Length > 0 && inv.ArgumentList.Arguments.Count > 0)
                return (addBrackets ? "(" : "") + (addSelf ? "self, " : "") + $"{typeArgsVal}, {argsVal}";
            if (typeArgsVal.Length > 0 || inv.ArgumentList.Arguments.Count > 0)
                return (addBrackets ? "(" : "") + (addSelf ? "self, " : "") + $"{typeArgsVal}{argsVal}";
            return (addBrackets ? "(" : "") + (addSelf ? "self" : "") + $"{typeArgsVal}{argsVal}";
        }

        ISymbol GetSymbolInfo(InvocationExpressionSyntax invocation)
        {
            var symbolInfo = semanticModel.GetSymbolInfo(invocation);
            if (symbolInfo.Symbol is IMethodSymbol)
                return symbolInfo.Symbol;
            foreach (var can in symbolInfo.CandidateSymbols)
            {
                if (can is IMethodSymbol methodSymbol && methodSymbol.IsExtensionMethod)
                    return can;
            }
            return symbolInfo.Symbol;
        }

        bool IsCallingClassMethod(ISymbol? symbol)
        {
            return symbol is IMethodSymbol;
        }

        bool IsCallingStaticMethod(ISymbol? symbol)
        {
            return symbol?.IsStatic ?? false;
        }

        string onInvocationExpression(InvocationExpressionSyntax inv)
        {
            var callText = "";
            var symbolInfo = GetSymbolInfo(inv);
            if (IsCallingClassMethod(symbolInfo))
            {
                if (IsCallingStaticMethod(symbolInfo))
                {
                    IMethodSymbol methodSymbol = symbolInfo as IMethodSymbol;

                    // Log($"class static method {key} {symbolInfo.Symbol}");
                    if (methodSymbol != null)
                    {
                        if (getMethod(methodSymbol, out var invExpr2))
                        {
                            callText = invExpr2(this, inv);
                        }
                        else
                        {
                            string methodName = methodSymbol.Name; // Name of the method
                            string className = onVarTypeSyntax(methodSymbol.ContainingType);
                            callText = $"{className}`{methodName}{onArgumentListSyntax(inv)}";
                        }
                    }
                }
                else
                {
                    if (inv.Expression.Kind() == SyntaxKind.SimpleMemberAccessExpression)
                    {
                        var ma = inv.Expression as MemberAccessExpressionSyntax;
                        var exprTypeInfo = semanticModel.GetTypeInfo(ma.Expression);
                        // Log($"type name : {exprTypeInfo.Type.MetadataName}, namespace : {exprTypeInfo.Type.ContainingNamespace?.ToDisplayString()} field : {ma.Name.Identifier.Text}");
                        getMethod(exprTypeInfo, ma.Name.Identifier.Text, out var invExpr);
                        if (invExpr == null)
                        {
                            objectInvExpr.TryGetValue(ma.Name.Identifier.Text, out invExpr);
                        }
                        if (invExpr != null)
                        {
                            callText = invExpr(this, inv);
                        }
                        else
                        {
                            IMethodSymbol methodSymbol = symbolInfo as IMethodSymbol;
                            var methodName = uniqueMethodName(methodSymbol);
                            var self = onExpressionSyntax(ma.Expression);
                            if (self != "")
                            {
                                if (ma.Expression.ToString() == "base")
                                {
                                    var selfType = semanticModel.GetTypeInfo(ma.Expression);
                                    var selfTypeName = onVarTypeSyntax(selfType.Type);
                                    callText = $"{methodName}{onArgumentListSyntax(inv, addSelf: true)}";
                                    callText = $"{selfTypeName}`{callText}";
                                }
                                else
                                {
                                    callText = $"{methodName}{onArgumentListSyntax(inv)}";
                                    callText = $"{self}->{callText}";
                                }
                            }
                            else
                            {
                                callText = $"{methodName}{onArgumentListSyntax(inv)}";
                            }
                        }
                    }
                    else if (inv.Expression.Kind() == SyntaxKind.GenericName)
                    {
                        var gns = inv.Expression as GenericNameSyntax;
                        var genericSymbol = semanticModel.GetSymbolInfo(gns);
                        InvocationDelegate invExpr = null;
                        if (genericSymbol.Symbol is IMethodSymbol methodSymbol)
                        {
                            getMethod(methodSymbol, out invExpr);
                        }
                        // Log($"type name : {exprTypeInfo.Type.MetadataName}, namespace : {exprTypeInfo.Type.ContainingNamespace?.ToDisplayString()} field : {gns.Identifier.Text}");
                        if (invExpr == null)
                        {
                            objectInvExpr.TryGetValue(gns.Identifier.Text, out invExpr);
                        }
                        if (invExpr != null)
                        {
                            callText = invExpr(this, inv);
                        }
                        else
                        {
                            IMethodSymbol methodSymbol2 = symbolInfo as IMethodSymbol;
                            var methodName = uniqueMethodName(methodSymbol2);
                            // callText = $"{methodName}{onArgumentListSyntax(inv, false, false, true)}";
                            var self = onExpressionSyntax(gns);
                            if (self != "")
                            {
                                if (gns.ToString() == "base")
                                {
                                    var selfType = semanticModel.GetTypeInfo(gns);
                                    var selfTypeName = onVarTypeSyntax(selfType.Type);
                                    callText = $"{methodName}{onArgumentListSyntax(inv, addSelf: true)}";
                                    callText = $"{selfTypeName}`{callText}";
                                }
                                else
                                {
                                    callText = $"{methodName}{onArgumentListSyntax(inv)}";
                                    callText = $"{self}->{callText}";
                                }
                            }
                            else
                            {
                                callText = $"{methodName}{onArgumentListSyntax(inv)}";
                            }
                        }
                    }
                    else if (inv.Expression.Kind() == SyntaxKind.IdentifierName)
                    {
                        var ins = inv.Expression as IdentifierNameSyntax;
                        var exprTypeInfo = semanticModel.GetTypeInfo(ins);
                        // Log($"type name : {exprTypeInfo.Type.MetadataName}, namespace : {exprTypeInfo.Type.ContainingNamespace?.ToDisplayString()} field : {ins.Identifier.Text}");
                        getMethod(exprTypeInfo, ins.Identifier.Text, out var invExpr);
                        if (invExpr == null)
                        {
                            objectInvExpr.TryGetValue(ins.Identifier.Text, out invExpr);
                        }
                        if (invExpr != null)
                        {
                            callText = invExpr(this, inv);
                        }
                        else
                        {
                            IMethodSymbol methodSymbol = symbolInfo as IMethodSymbol;
                            var methodName = uniqueMethodName(methodSymbol);
                            callText = $"{methodName}{onArgumentListSyntax(inv)}";
                        }
                    }
                }
            }
            else
            {
                // in case of compilation error or unknown function call (nameof for example)
                string key = inv.Expression.ToString();
                // Log($"unknown function call {key}");
                if (onInvExpr.TryGetValue(key, out InvocationDelegate invExpr))
                    return invExpr(this, inv);
            }
            if (callText == "")
            {
                callText = $"{onExpressionSyntax(inv.Expression)}{onArgumentListSyntax(inv)}";
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
        bool isVoid(ITypeSymbol ts)
        {
            return ts.Equals(compilation.GetSpecialType(SpecialType.System_Void));
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

            if (dropPointersFlags.Contains(new TypeData()
            {
                type = typeSymbol.Name,
                ns = typeSymbol.ContainingNamespace?.ToDisplayString(),
            }))
            {
                return false;
            }

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

        public bool isDelegate(ITypeSymbol? typeSymbol)
        {
            return typeSymbol != null && typeSymbol.TypeKind == TypeKind.Delegate;
        }

        public bool isMoveType(ITypeSymbol typeSymbol)
        {
            if (typeSymbol == null)
                return false;
            if (isString(typeSymbol))
                return false;
            switch (typeSymbol.TypeKind)
            {
                case TypeKind.Array:
                case TypeKind.Delegate:
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
            var leftType = semanticModel.GetTypeInfo(binop.Left).Type;
            if (leftType != null)
            {
                var td = new OperatorOverload()
                {
                    type = leftType.Name,
                    ns = leftType.ContainingNamespace?.ToDisplayString(),
                    kind = binop.Kind(),
                };
                if (operatorOverloads.TryGetValue(td, out var overload))
                {
                    return overload(this, binop);
                }
            }
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
            if (ac.Initializer != null)
            {
                var elemType = onTypeSyntax(ac.Type.ElementType);
                return onArrayInitializerExpressionSyntax(ac.Initializer, elemType);
            }

            var dim = 0;
            foreach (ArrayRankSpecifierSyntax rank in ac.Type.RankSpecifiers)
                dim += rank.Sizes.Count;
            if (dim == 1)
            {
                return $"[{{ for _ in range({onExpressionSyntax(ac.Type.RankSpecifiers[0].Sizes[0])}); [[{onVarTypeSyntax(ac.Type.ElementType)}]] }}]";
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
            // Fail($"unsupported object creation {oce}");
            return onObjectCreationExpression_ClassOrStruct(oce, restype);
        }

        private string onObjectCreationExpression_ClassOrStruct(ObjectCreationExpressionSyntax oce, TypeInfo resType)
        {
            if (ctorExpr.TryGetValue(new TypeData()
            {
                type = resType.Type.Name,
                ns = resType.Type.ContainingNamespace?.ToDisplayString(),
            }, out var ctor))
            {
                return ctor(this, oce);
            }
            var init = "";
            if (oce.Initializer != null)
            {
                foreach (var initExpr in oce.Initializer.Expressions)
                {
                    init += $" {onExpressionSyntax(initExpr)},";
                }
            }
            var newCall = isPointerType(resType.Type) ? "new " : "";
            var typeName = onTypeSyntax(oce.Type);
            var complexInit = init.Length > 0 || typeName.StartsWith("array<") || typeName.StartsWith("table<");
            if (oce.ArgumentList != null && oce.ArgumentList.Arguments.Count > 0)
            {
                var arguments = oce.ArgumentList.Arguments.Select(arg => onExpressionSyntax(arg.Expression)).Aggregate((current, next) => $"{current}, {next}");
                if (complexInit)
                    return $"{newCall}[[{typeName}({arguments}){init} ]]";
                else
                    return $"{newCall}{typeName}({arguments})";
            }
            if (complexInit)
                return $"{newCall}[[{typeName}(){init} ]]";
            else
                return $"{newCall}{typeName}()";
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
                    else if (element.Kind() == SyntaxKind.StringLiteralExpression
                        || element.Kind() == SyntaxKind.NumericLiteralExpression
                        || element.Kind() == SyntaxKind.NullLiteralExpression
                        || element.Kind() == SyntaxKind.Utf8StringLiteralExpression
                        || element.Kind() == SyntaxKind.TrueLiteralExpression
                        || element.Kind() == SyntaxKind.FalseLiteralExpression)
                    {
                        result += $"{onExpressionSyntax(element)}; ";
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

        public string derefExpr(ExpressionSyntax expr, bool doDeref = true)
        {
            var typeInfo = semanticModel.GetTypeInfo(expr);
            return derefExpr(onExpressionSyntax(expr), typeInfo, doDeref, false);
        }

        public string safeDerefExpr(ExpressionSyntax expr, bool doDeref = true)
        {
            var typeInfo = semanticModel.GetTypeInfo(expr);
            return derefExpr(onExpressionSyntax(expr), typeInfo, doDeref, true);
        }

        public string derefExpr(string res, TypeInfo typeInfo, bool doDeref, bool safe)
        {
            if (doDeref && isPointerType(typeInfo.Type))
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
            if (expression.Kind() == SyntaxKind.SimpleMemberAccessExpression)
            {
                var memberAccess = expression as MemberAccessExpressionSyntax;
                ISymbol propAccessedSymbol = semanticModel.GetSymbolInfo(memberAccess).Symbol;
                return propAccessedSymbol is IPropertySymbol;
            }
            ISymbol accessedSymbol = semanticModel.GetSymbolInfo(expression).Symbol;
            return accessedSymbol is IPropertySymbol;
        }

        bool isEnumerationConstant(ExpressionSyntax expression)
        {
            if (expression.Kind() != SyntaxKind.SimpleMemberAccessExpression) return false;
            var memberAccess = expression as MemberAccessExpressionSyntax;
            ISymbol accessedSymbol = semanticModel.GetSymbolInfo(memberAccess).Symbol;
            return (accessedSymbol != null && accessedSymbol.Kind == SymbolKind.Field
                && accessedSymbol.ContainingType is INamedTypeSymbol namedType
                && namedType.TypeKind == TypeKind.Enum);
        }

        bool isStaticProperty(ExpressionSyntax expression, string prefix, out string callPrefix)
        {
            callPrefix = "";
            if (expression.Kind() != SyntaxKind.SimpleMemberAccessExpression) return false;
            var memberAccess = expression as MemberAccessExpressionSyntax;
            ISymbol accessedSymbol = semanticModel.GetSymbolInfo(memberAccess).Symbol;

            if (accessedSymbol != null && accessedSymbol.IsStatic)
            {
                callPrefix = $"{onVarTypeSyntax(accessedSymbol.ContainingType)}`{prefix}{accessedSymbol.Name}";
                return true;
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
                        return $"{unop.OperatorToken}{onExpressionSyntax(unop.Operand)}";
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
                        var leftType = semanticModel.GetTypeInfo(binop.Left).Type;
                        if (leftType != null)
                        {
                            var td = new OperatorOverload()
                            {
                                type = leftType.Name,
                                ns = leftType.ContainingNamespace?.ToDisplayString(),
                                kind = binop.Kind(),
                            };
                            if (operatorOverloads.TryGetValue(td, out var overload))
                            {
                                return overload(this, binop);
                            }
                        }
                        var typeInfo = semanticModel.GetTypeInfo(binop.Left);
                        var assign = isMoveType(typeInfo.Type) ? "<-" : isCloneType(typeInfo.Type) ? ":=" : "=";
                        if (isNullExpression(binop.Right)) assign = "=";
                        if (isProperty(binop.Left))
                        {
                            if (binop.Left is MemberAccessExpressionSyntax leftAccess)
                            {
                                ISymbol accessedSymbol = semanticModel.GetSymbolInfo(leftAccess.Expression).Symbol;
                                if (accessedSymbol != null)
                                {
                                    var accessType = semanticModel.GetTypeInfo(leftAccess.Expression);
                                    if (getSetField(accessType, leftAccess.Name.ToString(), out var sf))
                                    {
                                        return sf(this, binop);
                                    }
                                }
                            }
                            if (isStaticProperty(binop.Left, "set__", out string propName))
                            {
                                return $"{propName}({onExpressionSyntax(binop.Right)})";
                            }
                            else
                                assign = ":=";
                        }
                        var rightValue = onExpressionSyntax(binop.Right);
                        if (rightValue == "self")
                            rightValue = "addr(self)";
                        return $"{onExpressionSyntax(binop.Left)} {assign} {rightValue}";
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
                        var leftType = semanticModel.GetTypeInfo(binop.Left).Type;
                        if (leftType != null)
                        {
                            var td = new OperatorOverload()
                            {
                                type = leftType.Name,
                                ns = leftType.ContainingNamespace?.ToDisplayString(),
                                kind = binop.Kind(),
                            };
                            if (operatorOverloads.TryGetValue(td, out var overload))
                            {
                                return overload(this, binop);
                            }
                        }
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
                        var typeInfo = semanticModel.GetTypeInfo(smm.Expression);
                        // if (typeInfo.Type != null)
                        //     Log($"type name : {typeInfo.Type.MetadataName}, namespace : {typeInfo.Type.ContainingNamespace?.ToDisplayString()} field : {smm.Name.Identifier.Text}");
                        if (getField(typeInfo, smm.Name.Identifier.Text, out MemberAccessDelegate acc))
                        {
                            return acc(this, smm);
                        }
                        if (objectMemberAccessExpr.TryGetValue(smm.Name.Identifier.Text, out acc))
                        {
                            return acc(this, smm);
                        }
                        if (isProperty(smm))
                        {
                            if (isStaticProperty(smm, "get__", out string staticPropName))
                            {
                                return $"{staticPropName}()";
                            }
                        }
                        if (isEnumerationConstant(smm))
                        {
                            return $"{onExpressionSyntax(smm.Expression)} {smm.Name.Identifier.Text}";
                        }
                        ISymbol accessedSymbol = semanticModel.GetSymbolInfo(smm).Symbol;
                        if (accessedSymbol?.IsStatic ?? false)
                            return $"{onExpressionSyntax(smm.Expression)}`{smm.Name.Identifier.Text}";
                        return $"{onExpressionSyntax(smm.Expression)}.{smm.Name.Identifier.Text}";
                    }
                case SyntaxKind.IdentifierName:
                    {
                        var baseSymbol = semanticModel.GetSymbolInfo(expression);
                        if (baseSymbol.Symbol is INamedTypeSymbol namedTypeSymbol)
                            return onVarTypeSyntax(namedTypeSymbol);

                        var typeInfo = parentClassOrStruct(expression);
                        var fieldName = (expression as IdentifierNameSyntax).Identifier.Text;
                        if (typeInfo != null && getField(typeInfo, fieldName, out MemberAccessDelegate acc))
                        {
                            MemberAccessExpressionSyntax syntax = default;
                            return acc(this, syntax);
                        }
                        if (objectMemberAccessExpr.TryGetValue(fieldName, out acc))
                        {
                            MemberAccessExpressionSyntax syntax = default;
                            return acc(this, syntax);
                        }
                        return $"{fieldName}";
                    }
                case SyntaxKind.PreIncrementExpression:
                case SyntaxKind.PreDecrementExpression:
                    {
                        var preop = expression as PrefixUnaryExpressionSyntax;
                        if (isProperty(preop.Operand))
                        {
                            var op = preop.OperatorToken.Text;
                            var cutop = op.Substring(0, op.Length - 1);
                            if (isStaticProperty(preop.Operand, "set__", out string propName))
                            {
                                isStaticProperty(preop.Operand, "get__", out string getPropName);
                                return $"{propName}({getPropName} {cutop} 1)";
                            }
                            else
                                return $"{onExpressionSyntax(preop.Operand)} := {onExpressionSyntax(preop.Operand)} {cutop} 1";
                        }
                        return $"{preop.OperatorToken.Value}{onExpressionSyntax(preop.Operand)}";
                    }
                case SyntaxKind.PostIncrementExpression:
                case SyntaxKind.PostDecrementExpression:
                    {
                        var postop = expression as PostfixUnaryExpressionSyntax;
                        if (isProperty(postop.Operand))
                        {
                            var op = postop.OperatorToken.Text;
                            var cutop = op.Substring(0, op.Length - 1);
                            if (isStaticProperty(postop.Operand, "set__", out string propName))
                            {
                                isStaticProperty(postop.Operand, "get__", out string getPropName);
                                return $"{propName}({getPropName} {cutop} 1)";
                            }
                            else
                                return $"{onExpressionSyntax(postop.Operand)} := {onExpressionSyntax(postop.Operand)} {cutop} 1";
                        }
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
                case SyntaxKind.AnonymousMethodExpression:
                    {
                        var ame = expression as AnonymousMethodExpressionSyntax;
                        var result = expression.Parent.IsKind(SyntaxKind.EqualsValueClause) ? "@ <| (" : "@(";
                        var first = true;
                        foreach (var param in ame.ParameterList.Parameters)
                        {
                            if (first) first = false;
                            else result += "; ";
                            string v = onVarTypeSyntax(param.Type);
                            if (v == "var" || v == "var?")
                                result += $"{varPrefix(param)}{param.Identifier}{varSuffix(param)}";
                            else
                                result += $"{varPrefix(param)}{param.Identifier} : {v}{varSuffix(param)}";
                        }
                        result += ")";
                        if (ame.Body is BlockSyntax bs)
                        {
                            result += $"\n{onBlockSyntax(bs)}";
                        }
                        else if (ame.Body is InvocationExpressionSyntax invEx)
                        {
                            var tabstr = new string('\t', tabs);
                            result += $"\n{tabstr}\t{onInvocationExpression(invEx)}";
                        }
                        else
                            result += $"\n\tpass\n";
                        return result;
                    }
                case SyntaxKind.ParenthesizedLambdaExpression:
                    {
                        var ame = expression as ParenthesizedLambdaExpressionSyntax;
                        var result = expression.Parent.IsKind(SyntaxKind.EqualsValueClause) ? "@ <| (" : "@(";
                        var first = true;
                        foreach (var param in ame.ParameterList.Parameters)
                        {
                            if (first) first = false;
                            else result += "; ";
                            string v = onVarTypeSyntax(param.Type);
                            if (v == "var" || v == "var?")
                                result += $"{varPrefix(param)}{param.Identifier}{varSuffix(param)}";
                            else
                                result += $"{varPrefix(param)}{param.Identifier} : {v}{varSuffix(param)}";
                        }
                        result += ")";
                        if (ame.Body is BlockSyntax bs)
                        {
                            result += $"\n{onBlockSyntax(bs)}";
                        }
                        else if (ame.Body is InvocationExpressionSyntax invEx)
                        {
                            var tabstr = new string('\t', tabs);
                            if ((semanticModel.GetSymbolInfo(invEx).Symbol as IMethodSymbol)?.ReturnsVoid ?? false)
                                result += $"\n{tabstr}\t{onInvocationExpression(invEx)}";
                            else
                                result += $"\n{tabstr}\treturn {onInvocationExpression(invEx)}";
                        }
                        else
                            result += $"\n\tpass\n";
                        return result;
                    }
                case SyntaxKind.SimpleLambdaExpression:
                    {
                        var ame = expression as SimpleLambdaExpressionSyntax;
                        var result = expression.Parent.IsKind(SyntaxKind.EqualsValueClause) ? "@ <| (" : "@(";
                        result += ")";
                        if (ame.Body is BlockSyntax bs)
                        {
                            result += $"\n{onBlockSyntax(bs)}";
                        }
                        else if (ame.Body is InvocationExpressionSyntax invEx)
                        {
                            var tabstr = new string('\t', tabs);
                            result += $"\n{tabstr}\t{onInvocationExpression(invEx)}";
                        }
                        else
                            result += $"\n\tpass\n";
                        return result;
                    }
                case SyntaxKind.IsExpression:
                case SyntaxKind.AsExpression:
                    {
                        var ise = expression as BinaryExpressionSyntax;
                        var isr = ise.Right as IdentifierNameSyntax;
                        var op = expression.Kind() == SyntaxKind.IsExpression ? "is" : "as";
                        return $"({onExpressionSyntax(ise.Left)} {op} {isr.Identifier.Text})";
                    }
                case SyntaxKind.DeclarationExpression:
                    {
                        var decs = expression as DeclarationExpressionSyntax;
                        var des = decs.Designation as SingleVariableDesignationSyntax;
                        var tabstr = new string('\t', tabs);
                        appendDecl($"{tabstr}var {des.Identifier.Text} : {onTypeSyntax(decs.Type)}\n");
                        return $"{des.Identifier.Text}";
                    }
                case SyntaxKind.CoalesceExpression:
                    {
                        var ce = expression as BinaryExpressionSyntax;
                        var l = onExpressionSyntax(ce.Left);
                        var r = onExpressionSyntax(ce.Right);
                        if (r == "") // fallback logic for code analysis without CoalesceAssignmentExpression support
                        {
                            var tabstr = new string('\t', tabs);
                            return $"if ({l} == null)\n{tabstr}\t{l}";
                        }
                        var nullableType = semanticModel.GetTypeInfo(ce.Left).Type?.MetadataName == "Nullable`1";
                        if (nullableType)
                        {
                            return $"({l} ?? {r})";
                        }
                        return $"({l} != null ? {l} : {r})";
                    }
                case SyntaxKind.CoalesceAssignmentExpression:
                    {
                        var cae = expression as AssignmentExpressionSyntax;
                        var l = onExpressionSyntax(cae.Left);
                        var r = onExpressionSyntax(cae.Right);
                        var nullableType = semanticModel.GetTypeInfo(cae.Left).Type?.MetadataName == "Nullable`1";
                        var tabstr = new string('\t', tabs);
                        if (nullableType)
                        {
                            var defType = semanticModel.GetTypeInfo(cae);
                            return $"if ({l} == null)\n{tabstr}\t{l} = new [[{onVarTypeSyntax(defType.Type)}]]\n{tabstr}\t*{l} = {r}\n";
                        }
                        return $"if ({l} == null)\n{tabstr}\t{l} = {r}\n";
                    }
                case SyntaxKind.GenericName:
                    {
                        return ""; // onArgumentListSyntax handle this
                    }
                case SyntaxKind.ConditionalAccessExpression:
                    {
                        var cae = expression as ConditionalAccessExpressionSyntax;
                        var l = onExpressionSyntax(cae.Expression);
                        var r = onExpressionSyntax(cae.WhenNotNull);
                        var tabstr = new string('\t', tabs);
                        return $"if {l} != null\n{tabstr}\t{l}.{r}";
                    }
                case SyntaxKind.MemberBindingExpression:
                    {
                        var mbe = expression as MemberBindingExpressionSyntax;
                        return $"{mbe.Name.Identifier.Text}";
                    }
                case SyntaxKind.BaseExpression:
                    {
                        var typeInfo = semanticModel.GetTypeInfo(expression);
                        return typeInfo.Type?.BaseType?.Name ?? "object";
                    }
                case SyntaxKind.PredefinedType:
                    {
                        var typeInfo = semanticModel.GetTypeInfo(expression);
                        return onVarTypeSyntax(typeInfo.Type);
                    }
                case SyntaxKind.QueryExpression:
                    {
                        // skip, just print as is
                        var tabstr = new string('\t', tabs);
                        return $"{tabstr}{expression.ToString()}";
                    }
                default:
                    {
                        Fail($"unsupported ExpressionSyntax {expression.Kind()}");
                        var tabstr = new string('\t', tabs);
                        return $"{tabstr}{expression.ToString()}";
                    }
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
            usingRename.TryGetValue(u.Name.ToString(), out UsingRenameDelegate rename);
            if (rename != null || usingRename.TryGetValue("*", out rename))
            {
                var usingValue = rename.Invoke(this, u.Name.ToString());
                return usingValue ?? "";
            }
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

        string parentNamespace(SyntaxNode? st)
        {
            var parentToken = st;
            while (parentToken != null)
            {
                if (parentToken is NamespaceDeclarationSyntax nsSyntax)
                    return nsSyntax.Name.ToString();
                parentToken = parentToken.Parent;
            }
            return "";
        }

        INamedTypeSymbol? parentClassOrStruct(SyntaxNode? st)
        {
            var parentToken = st;
            while (parentToken != null)
            {
                if (parentToken is ClassDeclarationSyntax classSyntax)
                    return semanticModel.GetDeclaredSymbol(classSyntax);
                if (parentToken is StructDeclarationSyntax structSyntax)
                    return semanticModel.GetDeclaredSymbol(structSyntax);
                parentToken = parentToken.Parent;
            }
            return default;
        }

        string onClassDeclaration(ClassDeclarationSyntax classDeclaration)
        {
            var result = "";
            var hasInstances = false;
            if (classDeclaration.TypeParameterList != null)
            {
                var ns = parentNamespace(classDeclaration);
                genericTypes[new TypeData { type = classDeclaration.Identifier.ToString(), ns = ns }] = classDeclaration;
                // stub declaration to register new type instances
                onClassDeclaration_(classDeclaration, new string[0], /*dry run*/ true);

                foreach (var inst in instantiateTemplates)
                {
                    if (inst.typeData.type == classDeclaration.Identifier.ToString() && inst.typeData.ns == ns)
                    {
                        hasInstances = true;
                        result = "\b"; // remove new line
                        break;
                    }
                }
                if (!hasInstances)
                {
                    dummyTypes.Add(new TypeData { type = classDeclaration.Identifier.ToString(), ns = ns });
                    // result += onClassDeclaration_(classDeclaration, new string[0]);
                }
            }
            else
            {
                // default instance
                result += onClassDeclaration_(classDeclaration, new string[0]);
            }
            return result;
        }

        string onClassDeclaration_(ClassDeclarationSyntax classDeclaration, string[] paramNames, bool dryRun = false)
        {
            BaseListSyntax? baseList = classDeclaration.BaseList;
            var paramsVal = paramNames.Length > 0 ? $"_{string.Join("_", paramNames)}" : classDeclaration.TypeParameterList != null ? "_" + classDeclaration.TypeParameterList.Parameters.Select(p => p.Identifier.Text).Aggregate((current, next) => $"{current}_{next}") : "";
            var parentTypeName = baseList != null ? baseList.Types[0].ToString() : "";
            var parentType = baseList != null ? semanticModel.GetTypeInfo(baseList.Types[0].Type) : default;
            if (parentType.Type != null)
            {
                var td = new TypeData()
                {
                    type = parentTypeName,
                    ns = parentType.Type.ContainingNamespace?.ToString() ?? ""
                };
                if (typesRename.TryGetValue(td, out var rename))
                {
                    parentTypeName = rename(this, td);
                }
            }

            var parent = baseList != null ? $" : {parentTypeName}" : "";
            var result = $"class {classDeclaration.Identifier}{paramsVal}{parent}\n";
            if (instantiatedTemplates.Contains(result))
                return "";

            if (!dryRun)
            {
                instantiatedTemplates.Add(result);
                dummyTypes.Remove(new TypeData { type = classDeclaration.Identifier.ToString(), ns = parentNamespace(classDeclaration) });
            }
            currentClassParams.Push(paramNames);
            var paramsData = new List<TypeData>();
            if (paramNames.Length > 0 && classDeclaration.TypeParameterList != null)
            {
                var idx = 0;
                foreach (TypeParameterSyntax param in classDeclaration.TypeParameterList.Parameters)
                {
                    var typeData = new TypeData { type = param.Identifier.Text, ns = classDeclaration.Identifier.ToString() };
                    paramsData.Add(typeData);
                    var newName = paramNames[idx++];
                    renameType(typeData, (CHashConverter converter, TypeData ts) =>
                    {
                        return newName;
                    }, true);
                }
            }
            tabs++;
            TextSpan prevSpan = new TextSpan(classDeclaration.Span.Start, 1);
            foreach (MemberDeclarationSyntax membersDeclaration in classDeclaration.Members)
            {
                InsertComments(ref result, prevSpan, membersDeclaration.Span, membersDeclaration.SyntaxTree);
                result += onMemberDeclaration(membersDeclaration) + "\n";
                prevSpan = membersDeclaration.Span;
            }
            tabs--;
            foreach (var param in paramsData)
            {
                removeRenameType(param);
            }
            currentClassParams.Pop();
            return result;
        }

        string onInterfaceDeclaration(InterfaceDeclarationSyntax classDeclaration)
        {
            BaseListSyntax? baseList = classDeclaration.BaseList;
            var paramNames = new string[0];
            var dryRun = false;
            var paramsVal = paramNames.Length > 0 ? $"_{string.Join("_", paramNames)}" : classDeclaration.TypeParameterList != null ? "_" + classDeclaration.TypeParameterList.Parameters.Select(p => p.Identifier.Text).Aggregate((current, next) => $"{current}_{next}") : "";
            var parentTypeName = baseList != null ? baseList.Types[0].ToString() : "";
            var parentType = baseList != null ? semanticModel.GetTypeInfo(baseList.Types[0].Type) : default;
            if (parentType.Type != null)
            {
                var td = new TypeData()
                {
                    type = parentTypeName,
                    ns = parentType.Type.ContainingNamespace?.ToString() ?? ""
                };
                if (typesRename.TryGetValue(td, out var rename))
                {
                    parentTypeName = rename(this, td);
                }
            }

            var parent = baseList != null ? $" : {parentTypeName}" : "";
            var result = $"class {classDeclaration.Identifier}{paramsVal}{parent}\n";
            if (instantiatedTemplates.Contains(result))
                return "";

            if (!dryRun)
            {
                instantiatedTemplates.Add(result);
                dummyTypes.Remove(new TypeData { type = classDeclaration.Identifier.ToString(), ns = parentNamespace(classDeclaration) });
            }
            currentClassParams.Push(paramNames);
            var paramsData = new List<TypeData>();
            if (paramNames.Length > 0 && classDeclaration.TypeParameterList != null)
            {
                var idx = 0;
                foreach (TypeParameterSyntax param in classDeclaration.TypeParameterList.Parameters)
                {
                    var typeData = new TypeData { type = param.Identifier.Text, ns = classDeclaration.Identifier.ToString() };
                    paramsData.Add(typeData);
                    var newName = paramNames[idx++];
                    renameType(typeData, (CHashConverter converter, TypeData ts) =>
                    {
                        return newName;
                    }, true);
                }
            }
            tabs++;
            TextSpan prevSpan = new TextSpan(classDeclaration.Span.Start, 1);
            foreach (MemberDeclarationSyntax membersDeclaration in classDeclaration.Members)
            {
                InsertComments(ref result, prevSpan, membersDeclaration.Span, membersDeclaration.SyntaxTree);
                result += onMemberDeclaration(membersDeclaration) + "\n";
                prevSpan = membersDeclaration.Span;
            }
            tabs--;
            foreach (var param in paramsData)
            {
                removeRenameType(param);
            }
            currentClassParams.Pop();
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
            var genericParams = "";
            if (methodDeclaration.Parent is ClassDeclarationSyntax)
            {
                if (currentClassParams.TryPeek(out string[] paramNames) && paramNames.Length > 0)
                {
                    genericParams = $"_{string.Join("_", paramNames)}";
                }
            }
            var result = $"{tabstr}def {methodDeclaration.Identifier}{genericParams}";
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
            var uniqueName = string.Join("_", methodSymbol.Parameters.Select(p => p.Type.ToString()));
            uniqueName = Regex.Replace(uniqueName, "[<>,\\[\\]]", "_");
            if (uniqueName.Length > 0)
                uniqueName = $"_{uniqueName}";
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
            if (methodDeclaration.Modifiers.Any(mod => mod.Kind() == SyntaxKind.OverrideKeyword))
                prefix += "override ";
            if (methodDeclaration.Body == null && methodDeclaration.ExpressionBody == null)
                prefix += "abstract ";
            IMethodSymbol methodSymbol = semanticModel.GetDeclaredSymbol(methodDeclaration);
            var resType = onVarTypeSyntax(methodDeclaration.ReturnType);
            var annotations = "";
            if (resType == "iterator" || resType.StartsWith("iterator<"))
            {
                annotations = "[coroutine] ";
                resType = resType == "iterator" ? "void" : resType.Substring(9, resType.Length - 10);
                addRequirement("daslib/coroutines");
            }
            var result = $"{tabstr}{annotations}def {prefix}{uniqueMethodName(methodSymbol)}";
            if (methodDeclaration.ParameterList.Parameters.Count != 0)
            {
                var parameters = methodDeclaration.ParameterList.Parameters
                    .Select(param => $"{varPrefix(param)}{param.Identifier} : {onVarTypeSyntax(param.Type)}{varSuffix(param)}");
                result += $" ({string.Join("; ", parameters)})";
            }
            result += $" : {resType}";
            if (methodDeclaration.Body != null)
                result += $"\n{onBlockSyntax(methodDeclaration.Body)}";
            else if (methodDeclaration.ExpressionBody != null)
                result += $"\n{tabstr}\t{onExpressionSyntax(methodDeclaration.ExpressionBody.Expression)}\n";
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
                case SyntaxKind.YieldBreakStatement:
                    return $"{tabstr}return false\n";
                case SyntaxKind.YieldReturnStatement:
                    return $"{tabstr}yield {onExpressionSyntax((statement as YieldStatementSyntax).Expression)}\n";
                case SyntaxKind.TryStatement:
                    {
                        var tryStatement = statement as TryStatementSyntax;
                        var result = $"{tabstr}try\n{onBlockSyntax(tryStatement.Block)}";
                        if (tryStatement.Catches.Count > 0)
                        {
                            result += $"{tabstr}recover\n";// {onCatchClause(catchClause)}";
                            foreach (var catchClause in tryStatement.Catches)
                            {
                                result += $"{tabstr}\t// {catchClause.Declaration}\n";
                                result += onBlockSyntax(catchClause.Block);
                            }
                        }
                        if (tryStatement.Finally != null)
                            result += $"{tabstr}finally\n{onBlockSyntax(tryStatement.Finally.Block)}";
                        return result;
                    }
                case SyntaxKind.ThrowStatement:
                    var expr2 = (statement as ThrowStatementSyntax).Expression;
                    if (expr2 == null)
                        return $"{tabstr}panic(\"error\")\n";

                    if (expr2.Kind() == SyntaxKind.ObjectCreationExpression)
                    {
                        var oce = expr2 as ObjectCreationExpressionSyntax;
                        var typeInfo = semanticModel.GetTypeInfo(oce);
                        if (typeInfo.ConvertedType != null && typeInfo.ConvertedType.Name == "Exception")
                        {
                            var result = $"{tabstr}panic({onExpressionSyntax(oce.ArgumentList.Arguments[0].Expression)})\n";
                            return result;
                        }
                    }
                    return $"{tabstr}panic(\"{onExpressionSyntax(expr2).Replace("\"", "\\\"")}\")\n";
                case SyntaxKind.EmptyStatement: // extra ;
                    return "";
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
                    result += $"\n{tabstr}{token}\n";
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

        List<string> declarationPrefix = new List<string>();

        void pushDecl()
        {
            declarationPrefix.Insert(0, "");
        }

        void appendDecl(string text)
        {
            declarationPrefix[0] += text;
        }

        string getDecl()
        {
            return declarationPrefix[0];
        }

        void popDecl()
        {
            declarationPrefix.RemoveAt(0);
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
                    pushDecl();
                    var text = onStatementSyntax(expr);
                    result += getDecl();
                    result += text;
                    popDecl();
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
            if (field.Modifiers.Any(mod => mod.Kind() == SyntaxKind.StaticKeyword || mod.Kind() == SyntaxKind.ConstKeyword))
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
            string overrideMod = isOverride ? "override " : "";
            string result = "";
            if (propertySyntax.AccessorList != null)
            {
                bool needStorage = false;
                foreach (var accessor in propertySyntax.AccessorList.Accessors)
                {
                    if (accessor.Kind() == SyntaxKind.GetAccessorDeclaration)
                    {
                        if (!isOverride && !isStatic)
                        {
                            result += $"{tabstr}def const operator . {propertySyntax.Identifier.Text} : {ptype}\n";
                            result += $"{tabstr}\treturn get__{propertySyntax.Identifier.Text}()\n\n";
                        }
                        result += $"{tabstr}def {abstractMod}{overrideMod}{staticMod}const get__{propertySyntax.Identifier.Text} : {ptype}\n";
                        if (accessor.Body != null)
                            result += onBlockSyntax(accessor.Body);
                        else if (accessor.ExpressionBody != null)
                            result += $"{tabstr}\treturn {onExpressionSyntax(accessor.ExpressionBody.Expression)}\n";
                        else if (!isAbstract)
                        {
                            needStorage = true;
                            result += $"{tabstr}\treturn {propertySyntax.Identifier.Text}__\n";
                        }
                        result += "\n";
                    }
                    else if (accessor.Kind() == SyntaxKind.SetAccessorDeclaration)
                    {
                        if (!isOverride && !isStatic)
                        {
                            result += $"{tabstr}def operator . {propertySyntax.Identifier.Text} := ( value:{ptype} )\n";
                            result += $"{tabstr}\tset__{propertySyntax.Identifier.Text}(value)\n\n";
                        }
                        result += $"{tabstr}def {abstractMod}{overrideMod}{staticMod}set__{propertySyntax.Identifier.Text} ( value:{ptype} ) : void\n";
                        if (accessor.Body != null)
                            result += onBlockSyntax(accessor.Body);
                        else if (accessor.ExpressionBody != null)
                            result += $"{tabstr}\t{onExpressionSyntax(accessor.ExpressionBody.Expression)}\n";
                        else if (!isAbstract)
                        {
                            needStorage = true;
                            result += $"{tabstr}\t{propertySyntax.Identifier.Text}__ = value\n";
                        }
                        result += "\n";
                    }
                }
                if (needStorage)
                {
                    result += $"{tabstr}private {propertySyntax.Identifier.Text}__ : {ptype}";
                    if (propertySyntax.Initializer != null)
                        // TODO: use correct assignment operator
                        result += $" = {onExpressionSyntax(propertySyntax.Initializer.Value)}";
                }
            }

            return result;
        }

        string onDelegateDeclaration(DelegateDeclarationSyntax member)
        {
            var result = $"typedef {member.Identifier.Text} = lambda<(";
            var first = true;
            foreach (var param in member.ParameterList.Parameters)
            {
                if (first) first = false;
                else result += "; ";
                result += $"{varPrefix(param)}{param.Identifier} : {onVarTypeSyntax(param.Type)}{varSuffix(param)}";
            }
            result += $") : {onVarTypeSyntax(member.ReturnType)}>";
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
                case SyntaxKind.InterfaceDeclaration:
                    return onInterfaceDeclaration(member as InterfaceDeclarationSyntax);
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
                case SyntaxKind.DelegateDeclaration:
                    addTopLevel(onDelegateDeclaration(member as DelegateDeclarationSyntax));
                    return "";
                default:
                    Fail($"Unsupported member {member.Kind()}");
                    return $"{member}";
            }
        }

        public static string ProcessBackspaces(string source)
        {
            char[] buffer = new char[source.Length];
            int idx = 0;

            foreach (char c in source)
            {
                if (c != '\b')
                {
                    buffer[idx] = c;
                    idx++;
                }
                else if (idx > 0)
                {
                    idx--;
                }
            }

            return new string(buffer, 0, idx);
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
                result = $"require {requirementsStr}\n{result}";
            }
            if (topLevel.Count > 0)
            {
                var topLevelStr = string.Join("\n", topLevel);
                result = $"{topLevelStr}\n\n{result}";
            }
            List<TemplateInstance>? tempList = instantiateTemplates.ToList();
            instantiateTemplates.Clear();
            foreach (var inst in tempList)
            {
                if (genericTypes.TryGetValue(inst.typeData, out var classDecl))
                {
                    result += onClassDeclaration_(classDecl, inst.paramNames);
                }
            }
            // dummy types, that were not instantiated but exist in code
            foreach (var td in dummyTypes)
            {
                if (genericTypes.TryGetValue(td, out var classDecl))
                {
                    result += onClassDeclaration_(classDecl, new string[0]);
                }
            }
            instantiatedTemplates.Clear();
            genericTypes.Clear();
            return ProcessBackspaces(result);
        }
    }

}