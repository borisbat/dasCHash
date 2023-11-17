module test

typedef MyAction = lambda<(var value : int) : string>
typedef VoidAction = lambda<(var value : string; var flag : bool) : void>

require math
require daslib/coroutines
// using System.Console
// using System
// using System.Collections.Generic
// using System.Collections

// namespaced HelloWorld
// enum )
struct TestStruct
	x : int


class Foo
	static count : int = 13

	a : int = 13

	private b : int = 13

	static instance : Foo?

	def Foo
		instance = addr(self)

	def Foo (var A : int)
		a = A

	def bar : void
		a = 123

	def bar_int (t : int) : void
		a = t
	// static method
	def static farfar : void
		print("static method\n")

	def static next : int
		return count++

// struct - value type
struct SFoo
	a : int


class HelloProgram
	/*
          multiline comment
        */
	count : int = 13

	def static _main (var args : array<string> /*string[]*/) : void
		print("Hello, World!\n")
		print("Hello, World!\n")
		print("Hello, World!\n")

		print("Hello, World!\n")
		print("Hello, World!\n")
		print("Hello, World!\n")
		return

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
		// op1
		var a1 : int = +a
		var a2 : int = -a
		var a3 : int = ~a
		var a4 : bool = !((a == 0))
		// op2
		var c : float = (float(a) + b)
		var d : float = (float(a) - b)
		var e : float = (float(a) * b)
		var f : float = (float(a) / b)
		var g : float = (float(a) % b)
		// op2 bool
		var t1 : bool = (a == a)
		var t2 : bool = (a != a)
		var t3 : bool = (a <= a)
		var t4 : bool = (a >= a)
		var t5 : bool = (a > a)
		var t6 : bool = (a < a)
		// more bool
		var tt1 : bool = (t1 && t2)
		var tt2 : bool = (t2 || t3)
		var tt3 : bool = (t3 ^^ t4)
		// binary
		var b1 : int = (a & a)
		var b2 : int = (a | a)
		var b3 : int = (a ^ a)
		var b4 : int = (a << 1)
		var b5 : int = (a >> 1)
		// assignmetn
		a <<= 1
		a >>= 1
		a |= a
		a &= a
		a ^= a
		print("{c}\n")
		// true and false
		a4 = true
		a4 = false

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

		var i8i8 = (int(i8) + int(i8))		// int
		var i8u8 = (int(i8) + int(u8))		// int
		var i16i16 = (int(i16) + int(i16))		// int
		var i16u16 = (int(i16) + int(u16))		// int
		var ii = (i + i)		// int
		var uu = (u + u)		// uint
		var ui = (int64(i) + int64(u))		// long
		var i32i32 = (i32 + i32)		// int
		var i32u32 = (int64(i32) + int64(u32))		// long
		var i64i64 = (i64 + i64)		// long
		var u64u64 = (u64 + u64)		// ulong
		// var i64u64 = i64 + u64;  // ambiguity, does not compile
		var fd = (double(f) + d)		// goes to double
		// casts
		var dd : double = 10.0d
		var ff : float = 10.0f
		var df : double = double(10.0f)
		var ii8 : int = int(i8)
		var iu8 : int = int(u8)
		var i64i : int64 = int64(i)
		var fi : float = float(i)

		// long promotions
		var _score : int64 = 0l
		var levelTargetScore : int = 13
		var progress : float = (float(_score) / float(levelTargetScore))

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

	/*
          multiline comment 2
        */
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
		var b : array<int> /*int[]*/ <- [{ for _ in range(10); [[int]] }]
		var s = 10
		var bs : array<int> /*int[]*/ <- [{ for _ in range(s); [[int]] }]
		var bb : array<array<int>> /*int[][]*/ <- newArray(20, -1)
		var c : array<array<int>> /*int[,]*/ <- newArray(10, 20)
		var d : array<int> /*int[]*/ <- [{auto 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10}]
		var dd : array<int> /*int[]*/ <- [{int 0; 1; 2; 4; 5}]
		print("add {dd} len {dd |> length()}\n")

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
		// and the crazy case
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
		// default in the middle
		let _temp_6_switchcase_ = i
		if _temp_6_switchcase_==1
			i++
		elif _temp_6_switchcase_==2
			i--
		else
			pass

	static private POTToText : table<int; string>? = new {{14 => "16K"; 15 => "32K"; 16 => "64K"; 17 => "128K"; 18 => "256K"; 19 => "512K"; 20 => "1M"; 21 => "2M"; 22 => "4M"; 23 => "8M"; 24 => "16M"; 25 => "32M"; 26 => "64M"; 27 => "128M"; 28 => "256M"; 29 => "512M"; 30 => "1B"; 31 => "2B"; 32 => "4B"; 33 => "8B"; 34 => "16B"; 35 => "32B"; 36 => "64B"; 37 => "128B"; 38 => "256B"; 39 => "512B"; 40 => "1024B"; }}

	def strings : void
		var empty = empty("foo")
		print("{empty}")
		if empty("")
			print("empty\n")

	def objectInit : void
		var epochStart : System::DateTime = System::DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind Utc)
		var t : Foo? = new Foo()
		var tt : Foo? = new Foo(1)
		t->bar()
		t->bar_int(1)

	def structures : void
		var f1 = SFoo()
		var f2 = [[SFoo() a = 1, ]]
		var f3 := f1		// clone!
		f1 := [[SFoo]]
		f3 := f2
		print("f1 = {f1} f2 = {f2} f3 = {f3.a}\n")
		print("{f1}\n")
		var f2Str = "{f2}"
		print("{f2Str}\n")

	def lists : void
		var a = new [{float 1f; 2f; 3f; 4f; 5f; 6f; }]
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

		var c = new [{Foo? new [[Foo() a = 1, ]]; new [[Foo() a = 2, ]]; }]
		*c |> push(new [[Foo() a = 3, ]])
		print("c = {c}\n")

		var d = new [{SFoo [[SFoo() a = 1, ]]; [[SFoo() a = 2, ]]; }]
		*d |> push([[SFoo() a = 3, ]])
		print("d = {*d}\n")
		var consoleCapsLock : bool = Console |> get_caps_lock()

		var empty : array<int>? = new [[array<int>() ]]
		if (empty == null)
			empty = new [[array<int>() ]]

		*empty |> push(13)
		print("empty = {*empty}\n")

		var arr = (empty != null ? empty : new [[array<int>() ]])
		print("{arr}\n")

	def testNullCoalescing (var a : int?) : void
		var b = (a ?? 2)
		print("b = {b}\n")
		if (a == null)
			a = new [[int]]
			*a = 10

		print("a = {a}\n")

	def conditional_expression : void
		var a = 1
		var b = (((a > 0) ? 1 : 0) * ((a < 0) ? 1 : 0))

	def static_methods : void
		var i = Foo`next()
		print("{"i"} = {i}\n")

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
		return get__FirstName()
	def get__FirstName : string
		return FirstName`Storage
	def operator . FirstName := ( value:string )
		set__FirstName(value)
	def set__FirstName ( value:string ) : void
		FirstName`Storage = value
	private FirstName`Storage : string

class DefaultPersonWithInit
	// property FirstName : string
	def operator . FirstName : string
		return get__FirstName()
	def get__FirstName : string
		return FirstName`Storage
	def operator . FirstName := ( value:string )
		set__FirstName(value)
	def set__FirstName ( value:string ) : void
		FirstName`Storage = value
	private FirstName`Storage : string = "blahblah"

class Person
	// property FirstName : string
	def operator . FirstName : string
		return get__FirstName()
	def get__FirstName : string
		return _firstName
	def operator . FirstName := ( value:string )
		set__FirstName(value)
	def set__FirstName ( value:string ) : void
		_firstName = value

	private _firstName : string


class PersonWithArrows	// new style operator
	// property FirstName : string
	def operator . FirstName : string
		return get__FirstName()
	def get__FirstName : string
		return _firstName
	def operator . FirstName := ( value:string )
		set__FirstName(value)
	def set__FirstName ( value:string ) : void
		_firstName = value

	private _firstName : string


class Goo
	def static testProperty : void
		var p = new Person()
		// set first name
		p.FirstName := "Test"
		// concat
		p.FirstName := p.FirstName + "Me"
		if (p.FirstName == "TestMe")
			print("{p.FirstName}\n")
		// also null keyword
		p = null


class Shape
	// property Area : double
	def operator . Area : double
		return get__Area()
	def abstract get__Area : double
	def operator . Area := ( value:double )
		set__Area(value)
	def abstract set__Area ( value:double ) : void

	def Hi : string
		return "Hi. I am a shape."


class Square : Shape
	side : double
	//constructor
	def Square (var s : double)
		side = s

	// property Area : double
	def override get__Area : double
		return (side * side)
	def override set__Area ( value:double ) : void
		side = math::sqrt(value)

	def override Hi : string
		return "{Shape`Hi(self)}I am a square with side {side}"


class Cube : Shape
	side : double
	//constructor
	def Cube (var s : double)
		side = s

	// property Area : double
	def override get__Area : double
		return ((6d * side) * side)
	def override set__Area ( value:double ) : void
		side = math::sqrt((value / 6d))



class ContTest
	def static StaticGetEmptyCont : Cont_U?
		return new Cont_U()

	def static StaticGetCont (var size : int) : Cont_U?
		print("size = {size}\n")
		return new Cont_U()

	def GetEmptyCont : Cont_U?
		return new Cont_U()

	def GetCont (var size : int) : Cont_U?
		print("size = {size}\n")
		return new Cont_U()

	def testStaticCont : void
		var emptyCont = ContTest`StaticGetEmptyCont()
		var intCont = ContTest`StaticGetCont(10)
		var i = intCont->pop()
		print("{i}\n")

		var contTest : ContTest? = new ContTest()
		var floatCont = contTest->GetCont(2)
		print("{floatCont}\n")

	def testCont : void
		var cont = new Cont_TestStruct()
		var t = cont->pop()
		print("{t.x}\n")

		var emptyCont = GetEmptyCont()
		var intCont = GetCont(10)
		var i = intCont->pop()
		print("{i}\n")


class SubContTest : ContTest
	def testConts : void
		var cont = new Cont_TestStruct()
		var t = cont->pop()
		print("{t.x}\n")

		var emptyCont = GetEmptyCont()
		var intCont = GetCont(10)
		var i = intCont->pop()
		print("{i}\n")



class StaticPerson
	// static property FirstName : string
	def static get__FirstName : string
		return _firstName
	def static set__FirstName ( value:string ) : void
		_firstName = value

	static private _firstName : string

	def static foo : void
		StaticPerson`set__FirstName("foo")
		StaticPerson`set__FirstName(StaticPerson`get__FirstName() + "bar")
		print("{StaticPerson`get__FirstName()}\n")


enum Season
	Spring
	Summer
	Autumn
	Winter

enum public ErrorCode : uint16
	None = uint16(0)
	Unknown = uint16(1)
	ConnectionLost = uint16(100)
	OutlierReading = uint16(200)

class EnumTester
	static code : ErrorCode = ErrorCode Unknown


class Employee


	private alias : string

	private name : string

	def invokeMe (var action : lambda) : void
		if action != null
			action.Invoke()

	def invokeMyAction (var action : MyAction) : string
		return action |> invoke(10)

	def invokeVoidAction (var action : VoidAction) : void
		action |> invoke("test", true)

	def Employee (var name_ : string; var alias_ : string)
		self.name = name_
		self.alias = alias_

	def Set (var name_ : string; var alias_ : string) : void
		var a : lambda <- @ <| ()
			self.name = name_
			self.alias = alias_

		a |> invoke()

		invokeMe() <| @()
			self.name = name_
			self.alias = alias_


		var str = invokeMyAction() <| @(var val : int)
			return "{val}"

		print("{str}\n")

		var delegate1 : MyAction <- @ <| (var val : int)
			return "{val}"

		var str1 = invokeMyAction(delegate1)
		print("{str1}\n")

		invokeMyAction(@(var i : int) { return self->InvokeTest(i); })		// convert to self->InvokeTest()
		invokeMyAction() <| @(var i : int)
			return InvokeTest(i)

		invokeVoidAction() <| @(var i; var b)
			VoidInvokeTest(i, b)
		invokeVoidAction(@(var i : string; var flag : bool) { self->VoidInvokeTest(i, flag); })

	def InvokeTest (var i : int) : string
		print("{i}\n")
		return "{i}"

	def VoidInvokeTest (var i : string; var flag : bool) : void
		print("{i}\n")


class AsIsTester
	def static testit : void
		var p = new Cube(13d)
		if (p is Cube)
			print("{(p as Cube)}\n")


class TestDeclarationExpression
	def static set13 (var t : int&) : int
		t = 13
		return 13

	def static testit : void
		try
			var t : int
			var q : int
			var z = (TestDeclarationExpression`set13(t) + TestDeclarationExpression`set13(q))
			print("{t} - {q}\n")
		recover
			// (Exception e)
			print("{e}\n")

	[coroutine] def Get : void
		return false

	[coroutine] def GetNames : string
		yield "Foo"
		yield "{1}"
		yield "{true}"


class Vec_int
	x : int

	y : int

	z : int

	def Vec_int (var x_ : int; var y_ : int; var z_ : int)
		x = x_
		y = y_
		z = z_

	def toString : string
		return "({x}, {y}, {z})"

	def testGenerics : void
		var v1 = new Vec_int(1, 2, 3)
		var v2 = new Vec_float(1f, 2f, 3f)
		print("{*v1}\n")
		print("{*v2}\n")
		print("{*new Vec_bool(true, true, false)}\n")

class Vec_float
	x : float

	y : float

	z : float

	def Vec_float (var x_ : float; var y_ : float; var z_ : float)
		x = x_
		y = y_
		z = z_

	def toString : string
		return "({x}, {y}, {z})"

	def testGenerics : void
		var v1 = new Vec_int(1, 2, 3)
		var v2 = new Vec_float(1f, 2f, 3f)
		print("{*v1}\n")
		print("{*v2}\n")
		print("{*new Vec_bool(true, true, false)}\n")

class Vec_bool
	x : bool

	y : bool

	z : bool

	def Vec_bool (var x_ : bool; var y_ : bool; var z_ : bool)
		x = x_
		y = y_
		z = z_

	def toString : string
		return "({x}, {y}, {z})"

	def testGenerics : void
		var v1 = new Vec_int(1, 2, 3)
		var v2 = new Vec_float(1f, 2f, 3f)
		print("{*v1}\n")
		print("{*v2}\n")
		print("{*new Vec_bool(true, true, false)}\n")

class Cont_TestStruct
	def pop : TestStruct
		var res : TestStruct = TestStruct()
		return res

class UnusedCont_T
	def pop : T
		var res = T()
		return res

