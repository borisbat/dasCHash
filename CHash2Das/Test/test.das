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
struct TestStruct
	x : int


class Foo
	static count : int = 13

	a : int = 13

	private b : int = 13

	def Foo
		pass

	def Foo (var A : int)
		a = A

	def bar : void
		a = 123

	def bar (t : int) : void
		a = t

	def static farfar : void
		print("static method\n")

	def static next : int
		return count++


struct SFoo
	a : int


class HelloProgram
	count : int = 13

	def static _main (var args : array<string?> /*string[]*/) : void
		print("Hello, World!\n")
		print("Hello, World!\n")
		print("Hello, World!\n")

		print("Hello, World!\n")
		print("Hello, World!\n")
		print("Hello, World!\n")

		print("1 2 3")

	def static ifThenElse (var cond : bool) : void
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
		var x : int = 1
		var y : int = 2
		var z : int = 3

	def static allOperators (var a : int; var b : float) : void
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
		var tt3 : bool = (t3 ^^ t4)

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
		print("{c}\n")

	def static allCasts : void
		var i8 : int8 = int8(0)
		var u8 : uint8 = 0u8
		var i16 : int16 = int16(0)
		var u16 : uint16 = uint16(0)
		var i32 : int = 0
		var u32 : uint = 0u
		var i : int = 0
		var u : uint = 0u
		var i64 : int64 = 0l
		var u64 : uint64 = 0ul
		var f : float = 0f
		var d : double = 0d

		var i8i8 = (int(i8) + int(i8))
		var i8u8 = (int(i8) + int(u8))
		var i16i16 = (int(i16) + int(i16))
		var i16u16 = (int(i16) + int(u16))
		var ii = (i + i)
		var uu = (u + u)
		var ui = (int64(i) + int64(u))
		var i32i32 = (i32 + i32)
		var i32u32 = (int64(i32) + int64(u32))
		var i64i64 = (i64 + i64)
		var u64u64 = (u64 + u64)

		var fd = (double(f) + d)

	def static whileLoop : void
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

	def static forLoop : void
		for i in range(0,10)
			print("{i}\n")
		if true // for loop
			var i : int = 0
			var j : int = 12
			while ((i + j) < 100)
				print("i={i} j={j}\n")
				i++
				j--
		for i in range(0,10)
			if (i == 5)
				break
			print("{i}\n")

		if true // for loop
			var i = 1
			while (i != 3)
				print("{i}\n")
				i += 2
		if true // for loop
			var i = 1
			while (i != 3)
				print("{i}\n")
				i = (i + 2)

		for i in range(1,3)
			print("{i}\n")
		for i in range(1,3)
			print("{i}\n")
		for i in range(1,3)
			print("{i}\n")
		for i in range(1,3)
			print("{i}\n")
		for i in range(1,3)
			print("{i}\n")
		for i in range(1,3)
			print("{i}\n")
		for i in range(1,3)
			print("{i}\n")

	def static doWhileLoop : void
		var a = 1
		var _temp_1_doWhileCond_ = true
		while _temp_1_doWhileCond_
			++a
			_temp_1_doWhileCond_ = (a != 10)
		var _temp_2_doWhileCond_ = true
		while _temp_2_doWhileCond_
			if true
				--a
				if (a == 5)
					break
			finally
				_temp_2_doWhileCond_ = (a != 0)

	def static arrayTypes : void
		var b : array<int> /*int[]*/ <- newArray(10)
		var bb : array<array<int>> /*int[][]*/ <- newArray(20, -1)
		var c : array<array<int>> /*int[,]*/ <- newArray(10, 20)
		var d : array<int> /*int[]*/ <- [{auto 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10}]
		var dd : array<int> /*int[]*/ <- newInitArray(-1, [{auto 0; 1; 2; 4; 5}])

	def static forEachLoop : void
		var b : array<int> /*int[]*/ <- [{auto 1; 2; 3; 4}]
		for t in b
			print("{t}\n")

	def static switchCase : void
		var i = 13
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
				if (i == 0)
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

	static private POTToText : table<int; string>? <- new {{14 => "16K"; 15 => "32K"; 16 => "64K"; 17 => "128K"; 18 => "256K"; 19 => "512K"; 20 => "1M"; 21 => "2M"; 22 => "4M"; 23 => "8M"; 24 => "16M"; 25 => "32M"; 26 => "64M"; 27 => "128M"; 28 => "256M"; 29 => "512M"; 30 => "1B"; 31 => "2B"; 32 => "4B"; 33 => "8B"; 34 => "16B"; 35 => "32B"; 36 => "64B"; 37 => "128B"; 38 => "256B"; 39 => "512B"; 40 => "1024B"; }}

	def objectInit : void
		var epochStart : System::DateTime = [[System::DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)]]
		var t : Foo? <- new [[Foo()]]
		var tt : Foo? <- new [[Foo(1)]]
		t->bar()
		t->bar(1)

	def structures : void
		var f1 = [[SFoo()]]
		var f2 = [[SFoo() a = 1, ]]
		var f3 := f1
		f1 := [[SFoo]]
		f3 := f2
		print("f1 = {f1} f2 = {f2} f3 = {f3.a}\n")
		print("{f1}\n")
		var f2Str = "{f2}"
		print("{f2Str}\n")

	def lists : void
		var a <- new [{float 1f; 2f; 3f; 4f; 5f; 6f; }]
		*a |> push(7f)
		*a |> erase(0)
		*a |> remove_value(2.1f)
		*a |> push(123f, 0)
		*a |> erase(0, 1)
		var b = (*a)[0]
		print("a = {*a} size: {*a |> length()}\n")
		print("b = {b}\n")
		*a |> clear()
		var sum = 0f
		for i in *a
			sum += i

			print("i = {i}\n")
		print("sum = {sum} has 6 {*a |> has_value(6f)}, 7 index {*a |> find_index(7f)}\n")
		*a |> sort()

		var c <- new [{Foo? new [[Foo() a = 1, ]]; new [[Foo() a = 2, ]]; }]
		*c |> push(new [[Foo() a = 3, ]])
		print("c = {c}\n")

		var d <- new [{SFoo [[SFoo() a = 1, ]]; [[SFoo() a = 2, ]]; }]
		*d |> push([[SFoo() a = 3, ]])
		print("d = {*d}\n")

	def static_methods : void
		var i = Foo`next()
		print("i = {i}\n")

	def static m_in (a : int) : void
		print("{a}\n")

	def static m_out (var a : int&) : void
		a = 13

	def static testInOut : void
		var a = 0
		HelloProgram`m_out(a)
		HelloProgram`m_in(a)


class DefaultPerson
	// property FirstName : string
	def operator . FirstName : string
		return FirstName`Storage
	def operator . FirstName := ( value:string )
		FirstName`Storage = value
	private FirstName`Storage : string

class DefaultPersonWithInit
	// property FirstName : string
	def operator . FirstName : string
		return FirstName`Storage
	def operator . FirstName := ( value:string )
		FirstName`Storage = value
	private FirstName`Storage : string = "blahblah"

class Person
	// property FirstName : string
	def operator . FirstName : string
			return _firstName
	def operator . FirstName := ( value:string )
			_firstName = value

	private _firstName : string?


class PersonWithArrows
	// property FirstName : string
	def operator . FirstName : string
		return _firstName
	def operator . FirstName := ( value:string )
		_firstName = value

	private _firstName : string?


class Goo
	def static testProperty : void
		var p <- new [[Person()]]
		p.FirstName := "Test"
		p.FirstName := p.FirstName + "Me"
		if (p.FirstName == "TestMe")
			print("{p.FirstName}\n")


