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
using System.Collections.Generic;

namespace HelloWorld
{
    class Foo
    {
        static int count = 13;

        public int a = 13;
        private int b = 13;

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

        static public int next()
        {
            return count++;
        }
    }

    struct SFoo
    {
        public int a;
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

        static void allOperators(int a, float b)
        {
            // assignment
            a = a;
            // op1
            int a1 = +a;
            int a2 = -a;
            int a3 = ~a;
            bool a4 = !(a == 0);
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

        static void allCasts()
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
            while (a == a)
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
            for (int i = 0; i < 10; ++i)
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
            for (var i = 1; i != 3; i += 1)    // for i in range(1,3)
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
            while (a != 0);
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
            foreach (var t in b)
                WriteLine(t);
        }

        static void switchCase()
        {
            var i = 13;
            switch (i)
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
            switch (i)
            {
                case 1: i++; break;
                case 2: i--; break;
            }
            // and the crazy case
            switch (i)
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

        void structures()
        {
            var f1 = new SFoo();
            var f2 = new SFoo() { a = 1 };
            var f3 = f1; // clone!
            f1 = default(SFoo);
            f3 = f2;
            Console.WriteLine($"f1 = {f1} f2 = {f2.ToString()} f3 = {f3.a}");
        }

        void lists()
        {
            var a = new List<float>()
            {
                1, 2, 3, 4, 5, 6
            };
            a.Add(7);
            a.RemoveAt(0);
            a.Remove(2);
            a.Insert(0, 123);
            a.RemoveRange(0, 1);
            var b = a[0];
            Console.WriteLine($"a = {a.ToString()} size: {a.Count}");
            Console.WriteLine($"b = {b}");
            a.Clear();
            var sum = 0f;
            foreach (var i in a)
            {
                sum += i;

                Console.WriteLine($"i = {i}");
            }
            Console.WriteLine($"sum = {sum} has 6 {a.Contains(6)}, 7 index {a.IndexOf(7)}");
            a.Sort();

            var c = new List<Foo>() { new Foo() { a = 1 }, new Foo() { a = 2 } };
            c.Add(new Foo() { a = 3 });
            Console.WriteLine($"c = {c}");

            var d = new List<SFoo>() { new SFoo() { a = 1 }, new SFoo() { a = 2 } };
            d.Add(new SFoo() { a = 3 });
            Console.WriteLine($"d = {d.ToString()}");
        }

        void static_methods()
        {
            var i = Foo.next();
            Console.WriteLine($"i = {i}");
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
// using System.Collections.Generic

// namespaced HelloWorld
class Foo
        static count : int = int(13)

        a : int = int(13)

        private b : int = int(13)

        def Foo
                pass

        def Foo (A : int)
                a = A

        def bar : void
                a = int(123)

        def bar (t : int) : void
                a = t

        def static farfar : void
                print("static method\n")

        def static next : int
                return count++


struct SFoo
        a : int


class HelloProgram
        count : int = int(13)

        def static _main (args : array<string?> /*string[]*/) : void
                print("Hello, World!\n")
                print("Hello, World!\n")
                print("Hello, World!\n")

                print("Hello, World!\n")
                print("Hello, World!\n")
                print("Hello, World!\n")

                print("1 2 3")

        def static ifThenElse (cond : bool) : void
                if cond
                        print("if no-block\n")
                if cond
                        print("if block\n")
                if cond
                        print("if no block\n")
                else
                        print("else no block\n")
                if cond
                        print("if block\n")
                else
                        print("else no block\n")
                if cond
                        print("if no block\n")
                else
                        print("else block\n")
                if cond
                        print("if block\n")
                else
                        print("else block\n")
                if cond
                        if cond
                                print("if cond if cond\n")
                if cond
                        print("cond1\n")
                elif cond
                        print("cond2\n")
                else
                        print("cond-else\n")

        def static varDecl : void
                var t : float
                var x : int = int(1)
                var y : int = int(2)
                var z : int = int(3)

        def static allOperators (a : int; b : float) : void
                a = a

                var a1 : int = (+a)
                var a2 : int = (-a)
                var a3 : int = (~a)
                var a4 : bool = (!((a == int(0))))

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
                var b4 : int = (a << int(1))
                var b5 : int = (a >> int(1))

                a <<= int(1)
                a >>= int(1)
                a |= a
                a &= a
                a ^= a
                print("{c}\n")

        def static allCasts : void
                var i8 : int8 = int(0)
                var u8 : uint8 = int(0)
                var i16 : int16 = int(0)
                var u16 : uint16 = int(0)
                var i32 : int = int(0)
                var u32 : uint = int(0)
                var i : int = int(0)
                var u : uint = int(0)
                var i64 : int64 = int(0)
                var u64 : uint64 = int(0)
                var f : float = int(0)
                var d : double = int(0)

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

        def static whileLoop : void
                var a = int(1)
                while (a < int(5))
                        a++
                while (a != int(10))
                        ++a
                        --a
                        a--
                while (a == a)
                        if (a != a)
                                break
                        if (a == a)
                                continue

        def static forLoop : void
                for i in range(int(0),int(10))
                        print("{i}\n")
                // for
                var i : int = int(0)
                var j : int = int(12)
                while ((i + j) < int(100))
                        print("i={i} j={j}\n")
                        i++
                        j--
                for i in range(int(0),int(10))
                        if (i == int(5))
                                break
                        print("{i}\n")

                // for
                var i = int(1)
                while (i != int(3))
                        print("{i}\n")
                        i += int(2)
                // for
                var i = int(1)
                while (i != int(3))
                        print("{i}\n")
                        i = (i + int(2))

                for i in range(int(1),int(3))
                        print("{i}\n")
                for i in range(int(1),int(3))
                        print("{i}\n")
                for i in range(int(1),int(3))
                        print("{i}\n")
                for i in range(int(1),int(3))
                        print("{i}\n")
                for i in range(int(1),int(3))
                        print("{i}\n")
                for i in range(int(1),int(3))
                        print("{i}\n")
                for i in range(int(1),int(3))
                        print("{i}\n")

        def static doWhileLoop : void
                var a = int(1)
                var _temp_1_doWhileCond_ = true
                while _temp_1_doWhileCond_
                        ++a
                        _temp_1_doWhileCond_ = (a != int(10))
                var _temp_2_doWhileCond_ = true
                while _temp_2_doWhileCond_
                        if true
                                --a
                                if (a == int(5))
                                        break
                        finally
                                _temp_2_doWhileCond_ = (a != int(0))

        def static arrayTypes : void
                var b : array<int> /*int[]*/ <- newArray(int(10))
                var bb : array<array<int>> /*int[][]*/ <- newArray(int(20), -1)
                var c : array<array<int>> /*int[,]*/ <- newArray(int(10), int(20))
                var d : array<int> /*int[]*/ <- [{auto int(0); int(1); int(2); int(3); int(4); int(5); int(6); int(7); int(8); int(9); int(10)}]
                var dd : array<int> /*int[]*/ <- newInitArray(-1, [{auto int(0); int(1); int(2); int(4); int(5)}])

        def static forEachLoop : void
                var b : array<int> /*int[]*/ <- [{auto int(1); int(2); int(3); int(4)}]
                for t in b
                        print("{t}\n")

        def static switchCase : void
                var i = int(13)
                let _temp_3_switchcase_ = i
                if _temp_3_switchcase_==0
                        pass
                elif _temp_3_switchcase_==1 || _temp_3_switchcase_==2
                        print("12\n")
                else
                        print("anything\n")
                print("and thats that\n")
                let _temp_4_switchcase_ = i
                if _temp_4_switchcase_==1
                        i++
                elif _temp_4_switchcase_==2
                        i--

                let _temp_5_switchcase_ = i
                while true
                        if _temp_5_switchcase_==1
                                if (i == int(0))
                                        break
                                else
                                        i++
                                        break
                        elif _temp_5_switchcase_==2
                                i--
                                break
                        break

                let _temp_6_switchcase_ = i
                if _temp_6_switchcase_==1
                        i++
                elif _temp_6_switchcase_==2
                        i--
                else
                        pass

        static private POTToText : table<int; string>? <- new {{int(14) => "16K"; int(15) => "32K"; int(16) => "64K"; int(17) => "128K"; int(18) => "256K"; int(19) => "512K"; int(20) => "1M"; int(21) => "2M"; int(22) => "4M"; int(23) => "8M"; int(24) => "16M"; int(25) => "32M"; int(26) => "64M"; int(27) => "128M"; int(28) => "256M"; int(29) => "512M"; int(30) => "1B"; int(31) => "2B"; int(32) => "4B"; int(33) => "8B"; int(34) => "16B"; int(35) => "32B"; int(36) => "64B"; int(37) => "128B"; int(38) => "256B"; int(39) => "512B"; int(40) => "1024B"; }}

        def objectInit : void
                var epochStart : System::DateTime = [[System::DateTime(int(1970), int(1), int(1), int(0), int(0), int(0), System.DateTimeKind.Utc)]]
                var t : Foo? <- new [[Foo()]]
                var tt : Foo? <- new [[Foo(int(1))]]
                t->bar()
                t->bar(int(1))

        def structures : void
                var f1 = [[SFoo()]]
                var f2 = [[SFoo() a = int(1), ]]
                var f3 := f1
                f1 := [[SFoo]]
                f3 := f2
                print("f1 = {f1} f2 = {f2->ToString()} f3 = {f3.a}\n")

        def lists : void
                var a <- new [{float float(1); float(2); float(3); float(4); float(5); float(6); }]
                *a |> push(float(7))
                *a |> erase(int(0))
                *a |> remove_value(float(2))
                *a |> push(float(123), int(0))
                *a |> erase(int(0), int(1))
                var b = *a[int(0)]
                print("a = {(*a)} size: {*a |> length()}\n")
                print("b = {b}\n")
                *a |> clear()
                var sum = float(0f)
                for i in *a
                        sum += i

                        print("i = {i}\n")
                print("sum = {sum} has 6 {*a |> has_value(float(6))}, 7 index {*a |> find_index(float(7))}\n")
                *a |> sort()

                var c <- new [{Foo? new [[Foo() a = int(1), ]]; new [[Foo() a = int(2), ]]; }]
                *c |> push(new [[Foo() a = int(3), ]])
                print("c = {c}\n")

                var d <- new [{SFoo [[SFoo() a = int(1), ]]; [[SFoo() a = int(2), ]]; }]
                *d |> push([[SFoo() a = int(3), ]])
                print("d = {(*d)}\n")

        def static_methods : void
                var i = Foo`next()
                print("i = {i}\n")
```


