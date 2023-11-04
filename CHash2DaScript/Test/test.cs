using static System.Console;
using System;
using System.Collections.Generic;
using System.Collections;

namespace HelloWorld
{
    // enum )
    struct TestStruct
    {
        public int x;
    };

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

        public void bar(in int t)
        {
            a = t;
        }

        // static method
        static void farfar()
        {
            Console.WriteLine("static method");
        }

        static public int next()
        {
            return count++;
        }
    }

    // struct - value type
    struct SFoo
    {
        public int a;
    }

    class HelloProgram
    {
        /*
          multiline comment
        */
        int count = 13;

        static void _main(string[] args)
        {
            WriteLine("Hello, World!");
            Console.WriteLine("Hello, World!");
            System.Console.WriteLine("Hello, World!");

            Write("Hello, World!\\n");
            Console.Write("Hello, World!\\n");
            System.Console.Write("Hello, World!\\n");
            return;

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
            // true and false
            a4 = true;
            a4 = false;
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

            // casts
            double dd = 10.0d;
            float ff = 10.0f;
            double df = 10.0f;
            int ii8 = i8;
            int iu8 = u8;
            Int64 i64i = i;
            float fi = i;

            // long promotions
            long _score = 0;
            int levelTargetScore = 13;
            float progress = (float)_score / (float)levelTargetScore;
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

        /*
          multiline comment 2
        */
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
            var s = 10;
            int[] bs = new int[s];
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
            Console.WriteLine(f1.ToString());
            var f2Str = f2.ToString();
            Console.WriteLine(f2Str);
        }

        void lists()
        {
            var a = new List<float>()
            {
                1, 2, 3, 4, 5, 6
            };
            a.Add(7f);
            a.RemoveAt(0);
            a.Remove(2.1f);
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
            bool consoleCapsLock = Console.CapsLock;
        }

        void conditional_expression()
        {
            var a = 1;
            var b = (a > 0 ? 1 : 0) * (a < 0 ? 1 : 0);
        }

        void static_methods()
        {
            var i = Foo.next();
            Console.WriteLine($"{nameof(i)} = {i}");
        }

        static void m_in(in int a)
        {
            Console.WriteLine(a);
        }

        static void m_out(out int a)
        {
            a = 13;
        }

        static void testInOut()
        {
            var a = 0;
            m_out(out a);
            m_in(a);
        }
    }

    public class DefaultPerson
    {
        public string FirstName { get; set; }
    }

    public class DefaultPersonWithInit
    {
        public string FirstName { get; set; } = "blahblah";
    }

    public class Person
    {
        public string FirstName
        {
            get { return _firstName; }
            set { _firstName = value; }
        }
        private string _firstName;
    }

    public class PersonWithArrows
    {
        // new style operator
        public string FirstName
        {
            get => _firstName;
            set => _firstName = value;
        }
        private string _firstName;
    }

    class Goo
    {
        static void testProperty()
        {
            var p = new Person();
            // set first name
            p.FirstName = "Test";
            // concat
            p.FirstName += "Me";
            if (p.FirstName == "TestMe") { Console.WriteLine(p.FirstName); }
            // also null keyword
            p = null;
        }
    }

    abstract class Shape
    {
        public abstract double Area
        {
            get;
            set;
        }
    }

    class Square : Shape
    {
        public double side;

        //constructor
        public Square(double s) => side = s;

        public override double Area
        {
            get => side * side;
            set => side = System.Math.Sqrt(value);
        }
    }

    class Cube : Shape
    {
        public double side;

        //constructor
        public Cube(double s) => side = s;

        public override double Area
        {
            get => 6 * side * side;
            set => side = System.Math.Sqrt(value / 6);
        }
    }

    public class StaticPerson
    {
        public static string FirstName
        {
            get { return _firstName; }
            set { _firstName = value; }
        }
        private static string _firstName;

        static void foo()
        {
            StaticPerson.FirstName = "foo";
            StaticPerson.FirstName += "bar";
            Console.WriteLine(StaticPerson.FirstName);
        }
    }
    enum Season
    {
        Spring,
        Summer,
        Autumn,
        Winter
    }

    public enum ErrorCode : ushort
    {
        None = 0,
        Unknown = 1,
        ConnectionLost = 100,
        OutlierReading = 200
    }

    class EnumTester
    {
        static ErrorCode code = ErrorCode.Unknown;
    }

    public class Employee
    {
        public delegate string MyAction(int value);
        private string alias;
        private string name;

        void invokeMe(Action action)
        {
            action.Invoke();
        }

        string invokeMyAction(MyAction action)
        {
            return action.Invoke(10);
        }

        public Employee(string name_, string alias_)
        {
            // Use this to qualify the members of the class
            // instead of the constructor parameters.
            var a = delegate ()
            {
                this.name = name_;
                this.alias = alias_;
            };
            a.Invoke();

            invokeMe(delegate ()
            {
                this.name = name_;
                this.alias = alias_;
            });

            var str = invokeMyAction((val) => { return val.ToString(); });
            Console.WriteLine(str);
        }
    }

    class AsIsTester
    {
        static void testit()
        {
            var p = new Cube(13);
            if (p is Cube)
                Console.WriteLine(p as Cube);
        }
    }

    class TestDeclarationExpression
    {
        static int set13(out int t)
        {
            t = 13;
            return 13;
        }

        static void testit()
        {
            var z = set13(out int t) + set13(out int q);
            Console.WriteLine($"{t} - {q}");
        }

        IEnumerator Get()
        {
            yield break;
        }

        IEnumerator<string> GetNames()
        {
            yield return "Foo";
            yield return 1.ToString();
            yield return true.ToString();
        }
    }
}
