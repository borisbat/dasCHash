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
using System.Runtime.InteropServices;

namespace HelloWorld
{
    class Foo { 
        static void bar()
        {
        }
    }

    class HelloProgram
    {
        int count = 13;
        
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

        static void allCasts ( )
        {
            sbyte i8 = 0;
            byte u8 = 0;
            Int16 i16 = 0;
            UInt16 u16 = 0;
            Int32 i32 = 0;
            UInt32 u32 = 0;
            int i = 0;
            uint u = 0;
            Int64 i64 = 0;
            UInt64 u64 = 0;
            float f = 0;
            double d = 0;

            var i8i8 = i8 + i8;     // int
            var i8u8 = i8 + u8;     // int
            var i16i16 = i16 + i16; // int
            var i16u16 = i16 + u16; // int
            var ii = i + i;         // int
            var uu = u + u;         // uint
            var ui = i + u;         // long
            var i32i32 = i32 + i32; // int
            var i32u32 = i32 + u32; // long
            var i64i64 = i64 + i64; // long
            var u64u64 = u64 + u64; // ulong
            // var i64u64 = i64 + u64;  // ambiguity, does not compile
            var fd = f + d;   // goes to double
        }

        static void whileLoop()
        {
            var a = 1;
            while (a < 5)
                a++;
            while (a != 10)
            {
                ++a;
                --a;
                a--;
            }
            while ( a==a )
            {
                if (a != a)
                    break;
                if (a == a)
                    continue;
            }
        }

        static void forLoop()
        {
            for (var i = 0; i < 10; ++i)
                Console.WriteLine(i);
            for (int i = 0, j = 12; i + j < 100; i++, j--)
                Console.WriteLine($"i={i} j={j}");
            for (int i=0; i < 10; ++i)
            {
                if (i == 5)
                    break;
                Console.WriteLine(i);
            }

            for (var i = 1; i != 3; i += 2) // not it
                Console.WriteLine(i);
            for (var i = 1; i != 3; i = i + 2)    // not it
                Console.WriteLine(i);

            for (var i = 1; i != 3; ++i)    // for i in range(1,3)
                Console.WriteLine(i);
            for (var i = 1; 3 != i; ++i)    // for i in range(1,3)
                Console.WriteLine(i);
            for (var i = 1; i < 3; i++)    // for i in range(1,3)
                Console.WriteLine(i);
            for (var i = 1; 3 > i; i++)    // for i in range(1,3)
                Console.WriteLine(i);
            for (var i = 1; i != 3; i+=1)    // for i in range(1,3)
                Console.WriteLine(i);
            for (var i = 1; i != 3; i = i + 1)    // for i in range(1,3)
                Console.WriteLine(i);
            for (var i = 1; i != 3; i = 1 + i)    // for i in range(1,3)
                Console.WriteLine(i);
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
// using System.Runtime.InteropServices

// namespaced HelloWorld
class Foo
        def bar  : void
                pass
class HelloProgram
        count : int = 13
        def _main (args : string[]) : void
                print(StringBuilder("Hello, World!","\n"))
                print(StringBuilder("Hello, World!","\n"))
                print(StringBuilder("Hello, World!","\n"))
                print("Hello, World!\n")
                print("Hello, World!\n")
                print("Hello, World!\n")
                print(StringBuilder("1", "2", "3"))
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
                var c : float = (float(a) + b)
                var d : float = (float(a) - b)
                var e : float = (float(a) * b)
                var f : float = (float(a) / b)
                var g : float = (float(a) % b)
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
        def allCasts  : void
                var i8 : int8 = 0
                var u8 : uint8 = 0
                var i16 : int16 = 0
                var u16 : uint16 = 0
                var i32 : int = 0
                var u32 : uint = 0
                var i : int = 0
                var u : uint = 0
                var i64 : int64 = 0
                var u64 : uint64 = 0
                var f : float = 0
                var d : double = 0
                var i8i8 = (int(i8) + int(i8))
                var i8u8 = (int(i8) + int(u8))
                var i16i16 = (int(i16) + int(i16))
                var i16u16 = (int(i16) + int(u16))
                var ii = (i + i)
                var uu = (u + u)
                var ui = (uint(i) + u)
                var i32i32 = (i32 + i32)
                var i32u32 = (uint(i32) + u32)
                var i64i64 = (i64 + i64)
                var u64u64 = (u64 + u64)
                var fd = (double(f) + d)
        def whileLoop  : void
                var a = 1
                while (a < 5)
                        a++
                while (a != 10)
                        ++a
                        --a
                        a--
                while (a == a)
                        if (a != a)
                                break
                        if (a == a)
                                continue
        def forLoop  : void
                for i in range(0,10)
                        print(StringBuilder(i,"\n"))
                // for
                var i : int = 0
                var j : int = 12
                while ((i + j) < 100)
                        print(StringBuilder(StringBuilder("i=", i, " j=", j),"\n"))
                        i++
                        j--
                for i in range(0,10)
                        if (i == 5)
                                break
                        print(StringBuilder(i,"\n"))
                // for
                var i = 1
                while (i != 3)
                        print(StringBuilder(i,"\n"))
                        i += 2
                // for
                var i = 1
                while (i != 3)
                        print(StringBuilder(i,"\n"))
                        i = (i + 2)
                for i in range(1,3)
                        print(StringBuilder(i,"\n"))
                for i in range(1,3)
                        print(StringBuilder(i,"\n"))
                for i in range(1,3)
                        print(StringBuilder(i,"\n"))
                for i in range(1,3)
                        print(StringBuilder(i,"\n"))
                for i in range(1,3)
                        print(StringBuilder(i,"\n"))
                for i in range(1,3)
                        print(StringBuilder(i,"\n"))
                for i in range(1,3)
                        print(StringBuilder(i,"\n"))
```
