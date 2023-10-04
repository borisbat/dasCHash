# dasCHash
C# -> daScript converter

Main idea here is that the converter does all the tedious syntax conversion, and then code is copy-pasted on as-needed basis into the ported project.
It should mostly compile, with minor fixes required to address namespaces, using directives, as well as some other language specific and project specific constructs.

Go from C# code

```
using static System.Console;
using Microsoft.CodeAnalysis.VisualBasic.Syntax;
using System;
using System.Collections;
using System.Linq;
using System.Text;
using System.Net;

namespace HelloWorld
{
    class HelloProgram
    {
        static void _main(string[] args)
        {
            WriteLine("Hello, World!");
            Console.WriteLine("Hello, World!");
            System.Console.WriteLine("Hello, World!");

            Write("Hello, World!\\n");
            Console.Write("Hello, World!\\n");
            System.Console.Write("Hello, World!\\n");

            Write("1", "2", "3");
        }

        static void varDecl()
        {
            float t;
            int x = 1;
            int y = 2, z = 3;
        }

        static void allOperators ( int a, float b )
        {
            // assignment
            a = a;
            // op1
            int a1 = +a;
            int a2 = -a;
            int a3 = ~a;
            bool a4 = !(a==0);
            // op2
            float c = a + b;
            float d = a - b;
            float e = a * b;
            float f = a / b;
            float g = a % b;
            // op2 bool
            bool t1 = a == a;
            bool t2 = a != a;
            bool t3 = a <= a;
            bool t4 = a >= a;
            bool t5 = a > a;
            bool t6 = a < a;
            // more bool
            bool tt1 = t1 && t2;
            bool tt2 = t2 || t3;
            bool tt3 = t3 ^ t4;
            // binary
            int b1 = a & a;
            int b2 = a | a;
            int b3 = a ^ a;
            int b4 = a << 1;
            int b5 = a >> 1;
            // assignmetn
            a <<= 1;
            a >>= 1;
            a |= a;
            a &= a;
            a ^= a;
            Console.WriteLine(c);
        }

        static void ifThenElse(bool cond)
        {
            if (cond)
                Console.WriteLine("if no-block");
            if (cond)
            {
                Console.WriteLine("if block");
            }
            if (cond)
                Console.WriteLine("if no block");
            else
                Console.WriteLine("else no block");
            if (cond)
            {
                Console.WriteLine("if block");
            }
            else
                Console.WriteLine("else no block");
            if (cond)
                Console.WriteLine("if no block");
            else
            {
                Console.WriteLine("else block");
            }
            if (cond)
            {
                Console.WriteLine("if block");
            }
            else
            {
                Console.WriteLine("else block");
            }
            if (cond)
                if (cond)
                    Console.WriteLine("if cond if cond");
            if (cond)
                Console.WriteLine("cond1");
            else if (cond)
                Console.WriteLine("cond2");
            else
                Console.WriteLine("cond-else");
        }

    }
}
```

to daScript equivalent.

```
// using System.Console
// using Microsoft.CodeAnalysis.VisualBasic.Syntax
// using System
// using System.Collections
// using System.Linq
// using System.Text
// using System.Net

// namespaced HelloWorld
class HelloProgram
        def _main (args : string[]) : void
                print(StringBuilder("Hello, World!","\n"))
                print(StringBuilder("Hello, World!","\n"))
                print(StringBuilder("Hello, World!","\n"))
                print("Hello, World!\n")
                print("Hello, World!\n")
                print("Hello, World!\n")
                print(StringBuilder("1", "2", "3"))
        def varDecl  : void
                var t : float
                var x : int = 1
                var y : int = 2
                var z : int = 3
        def allOperators (a : int; b : float) : void
                a = a
                var a1 : int = (+a)
                var a2 : int = (-a)
                var a3 : int = (~a)
                var a4 : bool = (!((a == 0)))
                var c : float = (a + b)
                var d : float = (a - b)
                var e : float = (a * b)
                var f : float = (a / b)
                var g : float = (a % b)
                var t1 : bool = (a == a)
                var t2 : bool = (a != a)
                var t3 : bool = (a <= a)
                var t4 : bool = (a >= a)
                var t5 : bool = (a > a)
                var t6 : bool = (a < a)
                var tt1 : bool = (t1 && t2)
                var tt2 : bool = (t2 || t3)
                var tt3 : bool = (t3 ^ t4)
                var b1 : int = (a & a)
                var b2 : int = (a | a)
                var b3 : int = (a ^ a)
                var b4 : int = (a << 1)
                var b5 : int = (a >> 1)
                a <<= 1
                a >>= 1
                a |= a
                a &= a
                a ^= a
                print(StringBuilder(c,"\n"))
        def ifThenElse (cond : bool) : void
                if cond
                        print(StringBuilder("if no-block","\n"))
                if cond
                        print(StringBuilder("if block","\n"))
                if cond
                        print(StringBuilder("if no block","\n"))
                else
                        print(StringBuilder("else no block","\n"))
                if cond
                        print(StringBuilder("if block","\n"))
                else
                        print(StringBuilder("else no block","\n"))
                if cond
                        print(StringBuilder("if no block","\n"))
                else
                        print(StringBuilder("else block","\n"))
                if cond
                        print(StringBuilder("if block","\n"))
                else
                        print(StringBuilder("else block","\n"))
                if cond
                        if cond
                                print(StringBuilder("if cond if cond","\n"))
                if cond
                        print(StringBuilder("cond1","\n"))
                elif cond
                        print(StringBuilder("cond2","\n"))
                else
                        print(StringBuilder("cond-else","\n"))
```

```
