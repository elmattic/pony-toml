use "ponytest"

actor Main is TestList
  new create(env: Env) =>
    PonyTest(env, this)

  new make() =>
    None

  fun tag tests(test: PonyTest) =>
    test(_TestCommentFullLine)
    test(_TestCommentEndOfLine)
    test(_TestCommentNoNewline)

    test(_TestKeyValuePair)
    test(_TestKeyValuePairNoNewline)
    test(_TestKeyValuePairNoSpaces)
    test(_TestKeyValuePairCouple)
    test(_TestKeyValuePairCoupleWithNewline)
    test(_TestKeyValuePairNoValue)
    test(_TestKeyValuePairNoValueNewline)
    test(_TestKeyValuePairNotSameLine)
    test(_TestKeyValuePairInvalidValue)

    test(_TestKeyInvalid)
    test(_TestKeyBare)
    test(_TestKeyBareInteger)
    test(_TestKeyBareBoolean)
    test(_TestKeyDefinedTwoTimes)

    test(_TestStringBasic)
    test(_TestStringBasicEscapeChar)
    test(_TestStringBasicUnicodeChar)
    test(_TestStringBasicNotTerminated)
    test(_TestStringBasicMultiLine)
    test(_TestStringBasicInvalidEscape)
    test(_TestStringBasicInvalidUnicode)

    test(_TestIntegerZero)
    test(_TestIntegerOne)
    test(_TestIntegerPlusOne)
    test(_TestIntegerMinusOne)
    test(_TestIntegerMinValue)
    test(_TestIntegerMaxValue)
    test(_TestIntegerOutOfRangeMin)
    test(_TestIntegerOutOfRangeMax)
    test(_TestIntegerTwoDigits)
    test(_TestIntegerUnderscore)
    test(_TestIntegerUnderscoreEnd)
    test(_TestIntegerUnderscoreFollowing)
    test(_TestIntegerUnderscoreAfterPrefix)
    test(_TestIntegerLeadingZero)
    test(_TestIntegerLeadingZeros)
    test(_TestIntegerHex)
    test(_TestIntegerHex64bit)
    test(_TestIntegerHexLeadingZero)
    test(_TestIntegerHexLeadingUnderscore)
    test(_TestIntegerHexLeadingSign)
    test(_TestIntegerOctal)
    test(_TestIntegerBinary)

    test(_TestBooleanTrue)
    test(_TestBooleanFalse)

    test(_TestTableEmpty)
    test(_TestTableEmptyNewline)
    test(_TestTableOneInner)
    test(_TestTableTwoInners)
    test(_TestTableKeyInteger)
    test(_TestTableDefinedTwoTimes)

class Check
  fun apply(h: TestHelper, input: String, reference: String) =>
    let parser = Parser.from_string(input)
    match parser.parse()
    | let table: TOMLTable => h.assert_eq[String](table.string(), reference)
    | let err: Error => h.fail(err.message())
    end

class Fail
  fun apply(h: TestHelper, input: String) =>
    let parser = Parser.from_string(input)
    match parser.parse()
    | let _: Error => h.assert_true(true)
    else
      h.fail("must have failed")
    end

//
// Comment
//

class iso _TestCommentFullLine is UnitTest
  fun name(): String => "comment full-line"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      #a = 0
      """,
      """
      {
      }""")

class iso _TestCommentEndOfLine is UnitTest
  fun name(): String => "comment at the end of a line"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = 0 # friendship is magic
      """,
      """
      {
        "a": 0
      }""")

class iso _TestCommentNoNewline is UnitTest
  fun name(): String => "comment without a newline"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      #"""
      ,
      """
      {
      }""")

//
// Key/Value Pair
//

class iso _TestKeyValuePair is UnitTest
  fun name(): String => "key/value pair"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = 0
      """,
      """
      {
        "a": 0
      }""")

class iso _TestKeyValuePairNoNewline is UnitTest
  fun name(): String => "key/value pair finished without a newline"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = 0"""
      ,
      """
      {
        "a": 0
      }""")

class iso _TestKeyValuePairNoSpaces is UnitTest
  fun name(): String => "key/value pair without spaces between equals"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a=0
      """
      ,
      """
      {
        "a": 0
      }""")

class iso _TestKeyValuePairCouple is UnitTest
  fun name(): String => "key/value pair couple"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = 0
      b = 1
      """
      ,
      """
      {
        "a": 0,
        "b": 1
      }""")

class iso _TestKeyValuePairCoupleWithNewline is UnitTest
  fun name(): String => "key/value pair couple with a newline between them"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = 0

      b = 1
      """
      ,
      """
      {
        "a": 0,
        "b": 1
      }""")

class iso _TestKeyValuePairNoValue is UnitTest
  fun name(): String => "key/value pair without a value"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = """)

class iso _TestKeyValuePairNoValueNewline is UnitTest
  fun name(): String => "key/value pair without a value but a newline"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a =
      """)

class iso _TestKeyValuePairNotSameLine is UnitTest
  fun name(): String => "key/value pair not on the same line"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a =
      0
      """)

class iso _TestKeyValuePairInvalidValue is UnitTest
  fun name(): String => "key/value pair with an invalid value"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = doh
      """)

//
// Keys
//

class iso _TestKeyInvalid is UnitTest
  fun name(): String => "key invalid"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      & = 0
      """)

class iso _TestKeyBare is UnitTest
  fun name(): String => "key bare"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      aA0_- = 0
      """
      ,
      """
      {
        "aA0_-": 0
      }""")

class iso _TestKeyBareInteger is UnitTest
  fun name(): String => "key bare that is an integer"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      42 = 0
      """
      ,
      """
      {
        "42": 0
      }""")

class iso _TestKeyBareBoolean is UnitTest
  fun name(): String => "key bare that is a boolean"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      true = 0
      """
      ,
      """
      {
        "true": 0
      }""")

class iso _TestKeyDefinedTwoTimes is UnitTest
  fun name(): String => "key defined two times"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = 0
      a = 1
      """)

//
// String
//

class iso _TestStringBasic is UnitTest
  fun name(): String => "string basic"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = "hello world"
      """,
      """
      {
        "a": "hello world"
      }""")

class iso _TestStringBasicEscapeChar is UnitTest
  fun name(): String => "string basic with an escape char"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = "\n"
      """,
      """
      {
        "a": "
      "
      }""")

class iso _TestStringBasicUnicodeChar is UnitTest
  fun name(): String => "string basic with an unicode char"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = "I \u2665 Pony"
      """,
      """
      {
        "a": "I ♥ Pony"
      }""")

class iso _TestStringBasicNotTerminated is UnitTest
  fun name(): String => "string basic that is not terminated"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = "hello world
      """)

class iso _TestStringBasicMultiLine is UnitTest
  fun name(): String => "string basic that is multi-line"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = "hello
      world"
      """)

class iso _TestStringBasicInvalidEscape is UnitTest
  fun name(): String => "string basic with invalid escape sequence"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = "hello \sworld"
      """)

class iso _TestStringBasicInvalidUnicode is UnitTest
  fun name(): String => "string basic with invalid unicode char"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = "hello \uD800world"
      """)

//
// Integer
//

class iso _TestIntegerZero is UnitTest
  fun name(): String => "integer decimal 0"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = 0
      """,
      """
      {
        "a": 0
      }""")

class iso _TestIntegerOne is UnitTest
  fun name(): String => "integer decimal 1"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = 1
      """,
      """
      {
        "a": 1
      }""")

class iso _TestIntegerPlusOne is UnitTest
  fun name(): String => "integer decimal +1"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = +1
      """,
      """
      {
        "a": 1
      }""")

class iso _TestIntegerMinusOne is UnitTest
  fun name(): String => "integer decimal -1"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = -1
      """,
      """
      {
        "a": -1
      }""")

class iso _TestIntegerMinValue is UnitTest
  fun name(): String => "integer decimal min value"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = -9223372036854775808
      """,
      """
      {
        "a": -9223372036854775808
      }""")

class iso _TestIntegerMaxValue is UnitTest
  fun name(): String => "integer decimal max value"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = 9223372036854775807
      """,
      """
      {
        "a": 9223372036854775807
      }""")

class iso _TestIntegerOutOfRangeMin is UnitTest
  fun name(): String => "integer decimal out of range min"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = -9223372036854775809
      """)

class iso _TestIntegerOutOfRangeMax is UnitTest
  fun name(): String => "integer decimal out of range max"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = 9223372036854775808
      """)

class iso _TestIntegerTwoDigits is UnitTest
  fun name(): String => "integer decimal with two digits"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = 10
      """,
      """
      {
        "a": 10
      }""")

class iso _TestIntegerUnderscore is UnitTest
  fun name(): String => "integer decimal with underscores"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = 10_000_000
      """,
      """
      {
        "a": 10000000
      }""")

class iso _TestIntegerUnderscoreEnd is UnitTest
  fun name(): String => "integer decimal with underscore at the end"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = 1_
      """)

class iso _TestIntegerUnderscoreFollowing is UnitTest
  fun name(): String => "integer decimal with underscores following each other"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = 1__0
      """)

class iso _TestIntegerUnderscoreAfterPrefix is UnitTest
  fun name(): String => "integer with underscore after base prefix"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = 0X_DEAD_BEEF
      """)

class iso _TestIntegerLeadingZero is UnitTest
  fun name(): String => "integer decimal with one leading zero"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = 01
      """)

class iso _TestIntegerLeadingZeros is UnitTest
  fun name(): String => "integer decimal with two leading zeros"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = 001
      """)

class iso _TestIntegerHex is UnitTest
  fun name(): String => "integer hexadecimal"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = 0xDEADBEEF
      """,
      """
      {
        "a": 3735928559
      }""")

class iso _TestIntegerHex64bit is UnitTest
  fun name(): String => "integer hexadecimal 64-bit"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = 0xffffffffffffffff
      """,
      """
      {
        "a": -1
      }""")

class iso _TestIntegerHexLeadingZero is UnitTest
  fun name(): String => "integer hexadecimal with a leading zero"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = 0x0DEFACED
      """,
      """
      {
        "a": 233811181
      }""")

class iso _TestIntegerHexLeadingUnderscore is UnitTest
  fun name(): String => "integer hexadecimal with a leading underscore"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = Ox_baad_food
      """)

class iso _TestIntegerHexLeadingSign is UnitTest
  fun name(): String => "integer hexadecimal with leading positive sign"

  fun apply(h: TestHelper) =>
    Fail(h,
      """
      a = +Oxbaadfood
      """)

class iso _TestIntegerOctal is UnitTest
  fun name(): String => "integer octal"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = 0o755
      """,
      """
      {
        "a": 493
      }""")

class iso _TestIntegerBinary is UnitTest
  fun name(): String => "integer octal"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = 0b11010110
      """,
      """
      {
        "a": 214
      }""")

//
// Boolean
//

class iso _TestBooleanTrue is UnitTest
  fun name(): String => "boolean true value"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = true
      """
      ,
      """
      {
        "a": true
      }""")

class iso _TestBooleanFalse is UnitTest
  fun name(): String => "boolean false value"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      a = false
      """
      ,
      """
      {
        "a": false
      }""")

//
// Table
//

class iso _TestTableEmpty is UnitTest
  fun name(): String => "table that is empty"

  fun apply(h: TestHelper) =>
    Check(h,
      ""
      ,
      """
      {
      }""")

class iso _TestTableEmptyNewline is UnitTest
  fun name(): String => "table that is empty but with a newline"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      """
      ,
      """
      {
      }""")

class iso _TestTableOneInner is UnitTest
  fun name(): String => "table with one inner table"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      [A]
      a = 0
      b = 1
      """
      ,
      """
      {
        "A": {
          "a": 0,
          "b": 1
        }
      }""")

class iso _TestTableTwoInners is UnitTest
  fun name(): String => "table with two inner tables"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      [A]
      a = 0
      b = 1
      [B]
      a = 0
      b = 1
      """
      ,
      """
      {
        "A": {
          "a": 0,
          "b": 1
        },
        "B": {
          "a": 0,
          "b": 1
        }
      }""")

class iso _TestTableKeyInteger is UnitTest
  fun name(): String => "table with an integer key"

  fun apply(h: TestHelper) =>
    Check(h,
      """
      [42]
      a = 0
      b = 1
      """
      ,
      """
      {
        "42": {
          "a": 0,
          "b": 1
        }
      }""")

class iso _TestTableDefinedTwoTimes is UnitTest
  fun name(): String => "table defined more than once"

    fun apply(h: TestHelper) =>
    Fail(h,
      """
      [a]
      b = 1

      [a]
      c = 2
      """)