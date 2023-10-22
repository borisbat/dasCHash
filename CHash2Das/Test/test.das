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

	def farfar : void
		print(StringBuilder("static method","\n"))


class HelloProgram
	count : int = int(13)

	def _main (args : array<string?> /*string[]*/) : void
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

	def varDecl : void
		var t : float
		var x : int = int(1)
		var y : int = int(2)
		var z : int = int(3)

	def allOperators (a : int; b : float) : void
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
		print(StringBuilder(c,"\n"))

	def allCasts : void
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

	def whileLoop : void
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

	def forLoop : void
		for i in range(int(0),int(10))
			print(StringBuilder(i,"\n"))
		// for
		var i : int = int(0)
		var j : int = int(12)
		while ((i + j) < int(100))
			print(StringBuilder(StringBuilder("i=", i, " j=", j),"\n"))
			i++
			j--
		for i in range(int(0),int(10))
			if (i == int(5))
				break
			print(StringBuilder(i,"\n"))

		// for
		var i = int(1)
		while (i != int(3))
			print(StringBuilder(i,"\n"))
			i += int(2)
		// for
		var i = int(1)
		while (i != int(3))
			print(StringBuilder(i,"\n"))
			i = (i + int(2))

		for i in range(int(1),int(3))
			print(StringBuilder(i,"\n"))
		for i in range(int(1),int(3))
			print(StringBuilder(i,"\n"))
		for i in range(int(1),int(3))
			print(StringBuilder(i,"\n"))
		for i in range(int(1),int(3))
			print(StringBuilder(i,"\n"))
		for i in range(int(1),int(3))
			print(StringBuilder(i,"\n"))
		for i in range(int(1),int(3))
			print(StringBuilder(i,"\n"))
		for i in range(int(1),int(3))
			print(StringBuilder(i,"\n"))

	def doWhileLoop : void
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

	def arrayTypes : void
		var b : array<int> /*int[]*/ <- newArray(int(10))
		var bb : array<array<int>> /*int[][]*/ <- newArray(int(20), -1)
		var c : array<array<int>> /*int[,]*/ <- newArray(int(10), int(20))
		var d : array<int> /*int[]*/ <- [{auto int(0); int(1); int(2); int(3); int(4); int(5); int(6); int(7); int(8); int(9); int(10)}]
		var dd : array<int> /*int[]*/ <- newInitArray(-1, [{auto int(0); int(1); int(2); int(4); int(5)}])

	def forEachLoop : void
		var b : array<int> /*int[]*/ <- [{auto int(1); int(2); int(3); int(4)}]
		for t in b
			print(StringBuilder(t,"\n"))

	def switchCase : void
		var i = int(13)
		let _temp_3_switchcase_ = i
		if _temp_3_switchcase_==0
			pass
		elif _temp_3_switchcase_==1 || _temp_3_switchcase_==2
			print(StringBuilder("12","\n"))
		else
			print(StringBuilder("anything","\n"))
		print(StringBuilder("and thats that","\n"))
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

	private POTToText : table<int; string>? <- new {{int(14) => "16K"; int(15) => "32K"; int(16) => "64K"; int(17) => "128K"; int(18) => "256K"; int(19) => "512K"; int(20) => "1M"; int(21) => "2M"; int(22) => "4M"; int(23) => "8M"; int(24) => "16M"; int(25) => "32M"; int(26) => "64M"; int(27) => "128M"; int(28) => "256M"; int(29) => "512M"; int(30) => "1B"; int(31) => "2B"; int(32) => "4B"; int(33) => "8B"; int(34) => "16B"; int(35) => "32B"; int(36) => "64B"; int(37) => "128B"; int(38) => "256B"; int(39) => "512B"; int(40) => "1024B"; }}

	def objectInit : void
		var epochStart : System::DateTime = new [[System::DateTime(int(1970), int(1), int(1), int(0), int(0), int(0), System.DateTimeKind.Utc)]]
		var t : Foo? <- new [[Foo()]]
		var tt : Foo? <- new [[Foo(int(1))]]
		t->bar()
		t->bar(int(1))

	def lists : void
		var a <- new [{float float(1); float(2); float(3); float(4); float(5); float(6); }]
		*a |> push(float(7))
		*a |> erase(int(0))
		*a |> remove_value(float(2))
		*a |> push(float(123), int(0))
		*a |> erase(int(0), int(1))
		var b = a[int(0)]
		print(StringBuilder(StringBuilder("a = ", (*a), " size: ", *a |> length()),"\n"))
		print(StringBuilder(StringBuilder("b = ", b),"\n"))
		*a |> clear()
		var sum = float(0f)
		for i in a
			sum += i

			print(StringBuilder(StringBuilder("i = ", i),"\n"))
		print(StringBuilder(StringBuilder("sum = ", sum),"\n"))

		var c <- new [[array<int>()]]
		*c |> push(int(1))
		print(StringBuilder(StringBuilder("c = ", (*c)),"\n"))


