using static System.Console;
using Microsoft.CodeAnalysis.VisualBasic.Syntax;
using System;
using System.Collections;
using System.Linq;
using System.Text;
using System.Net;
using System.Runtime.InteropServices;
using System.Collections.Generic;

namespace HelloWorld
{
    class Foo {

        int a = 13;

        public Foo() { }
        public Foo(int A) { a = A; }

        public void bar()
        {
            a = 123;
        }

        public void bar(int t)
        {
            a = t;
        }

        static void farfar()
        {
            Console.WriteLine("static method");
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

        static void doWhileLoop()
        {
            var a = 1;
            do
                ++a;
            while (a != 10);
            do
            {
                --a;
                if (a == 5)
                    break;
            }
            while(a != 0);
        }

        static void arrayTypes()
        {
            int[] b = new int[10];
            int[][] bb = new int[20][];
            int[,] c = new int[10, 20];
            int[] d = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
            int[] dd = new int[] { 0, 1, 2, 4, 5 };
        }

        static void forEachLoop()
        {
            int[] b = { 1, 2, 3, 4 };
            foreach(var t in b)
                WriteLine(t);
        }

        static void switchCase()
        {
            var i = 13;
            switch ( i )
            {
                case 0:
                    break;
                case 1:
                case 2:
                    Console.WriteLine("12");
                    break;
                default:
                    Console.WriteLine("anything");
                    break;
            }
            Console.WriteLine("and thats that");
            switch ( i )
            {
                case 1: i++; break;
                case 2: i--; break;
            }
            // and the crazy case
            switch ( i )
            {
                case 1: if (i == 0) break; else { i++; break; }
                case 2: i--; break;
            }
            // default in the middle
            switch (i)
            {
                case 1: i++; break;
                default: break;
                case 2: i--; break;
            }
        }

        private static Dictionary<int, string> POTToText = new Dictionary<int, string>
        {
            {14, "16K"},            {15, "32K"},            {16, "64K"},            {17, "128K"},
            {18, "256K"},           {19, "512K"},           {20, "1M"},             {21, "2M"},
            {22, "4M"},             {23, "8M"},             {24, "16M"},            {25, "32M"},
            {26, "64M"},            {27, "128M"},           {28, "256M"},           {29, "512M"},
            {30, "1B"},             {31, "2B"},             {32, "4B"},             {33, "8B"},
            {34, "16B"},            {35, "32B"},            {36, "64B"},            {37, "128B"},
            {38, "256B"},           {39, "512B"},           {40, "1024B"},
        };

        void objectInit()
        {
            System.DateTime epochStart = new System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc);
            Foo t = new Foo();
            Foo tt = new Foo(1);
            t.bar();
            t.bar(1);
        }
    }
}
