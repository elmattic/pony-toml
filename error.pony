use "files"

primitive UnderscoreAfterBasePrefixNotAllowed
fun string(): String iso^ =>
  "underscores are not allowed between the base prefix and the value".string()

primitive UnderscoreNotSurroundedByDigits
fun string(): String iso^ =>
  "underscores must be surrounded by at least one digit on each side".string()

primitive DecimalNumberExpected
fun string(): String iso^ =>
  "decimal number expected right after the sign".string()

primitive LeadingZerosInDecimalNotAllowed
fun string(): String iso^ =>
  "leading zeros are not allowed in decimal numbers".string()

primitive SignPrefixWithBasePrefixNotAllowed
fun string(): String iso^ =>
  "only decimal numbers may be prefixed with a plus or minus sign".string()

primitive TooLargeToBeRepresentedIn64Bit
fun string(): String iso^ =>
  "too large to be represented in the 64 bit (signed long) range".string()

type IntegerError is
  ( UnderscoreAfterBasePrefixNotAllowed
  | UnderscoreNotSurroundedByDigits
  | DecimalNumberExpected
  | LeadingZerosInDecimalNotAllowed
  | SignPrefixWithBasePrefixNotAllowed
  | TooLargeToBeRepresentedIn64Bit )

primitive UnterminatedString
fun string(): String iso^ =>
  "unterminated string, did you forget the ‘\"’?".string()

primitive MultiLineStringNotAllowed
fun string(): String iso^ =>
  ("basic or literal strings can't span over multiple lines, " +
  "use their multi-line counterpart").string()

primitive InvalidEscapeSequence
fun string(): String iso^ => "invalid escape sequence".string()

primitive InvalidUnicodeEscapeCode
fun string(): String iso^ => "invalid unicode escape code".string()

type StringError is
  ( UnterminatedString
  | MultiLineStringNotAllowed
  | InvalidEscapeSequence
  | InvalidUnicodeEscapeCode )

class val InvalidChar
  let _value: U8

  new val create(value: U8 val) =>
    _value = value

  fun string(): String iso^ =>
    var str: String ref = String()
    str.push_utf32(U32.from[U8](_value))
    ("invalid character ‘" + str + "’").string()

type LexerError is
  ( IntegerError
  | StringError
  | InvalidChar )

primitive KeyOrTableDefinedMoreThanOnce
fun string(): String iso^ =>
  "you cannot define any key or table more than once".string()

primitive ValueIsNotATable
fun string(): String iso^ =>
  "a value in current dotted key is not a table".string()

primitive ArrayHasMixDataTypes
fun string(): String iso^ =>
  "data types may not be mixed in arrays".string()

class val UnexpectedToken
  let _value: _Token
  let _expected: String

  new val create(token: _Token, values: String) =>
    _value = token
    _expected = values

  fun string(): String iso^ =>
    ("expecting " + _expected + " but get " + _value.string()).string()

type ParserError is
  ( KeyOrTableDefinedMoreThanOnce
  | ValueIsNotATable
  | ArrayHasMixDataTypes
  | UnexpectedToken )

class Error
  let _lexer: _Lexer ref
  let code: (LexerError | ParserError)

  new create(code': (LexerError | ParserError), lexer: _Lexer ref) =>
    _lexer = lexer
    code = code'

  fun ref hint(): String =>
    (let line: USize, let column: USize) = _lexer.current()
    let blame =
      match _lexer.file_or_string
      | let str: String =>
        try
          str.split("\n")(line)?
        else
          // unreachable
          ""
        end
      | let file: File ref =>
        let offset = _lexer.start_position - ISize.from[USize](file.position())
        file.seek(offset)
        var i: USize = 0
        while i != line do
          try
            file.line()?
          end
          i = i + 1
        end
        try
          file.line()?
        else
          // unreachable
          ""
        end
      end
    let spaces: Array[U8 val] val = recover
      Array[U8 val].init(' ', column)
    end
    "\n" + blame + "\n" + String.from_array(spaces) + "^"

  fun ref message(): String =>
    let path =
      match _lexer.file_or_string
      | let file: File ref => file.path.path
      | let _: String => "string"
      end
    (let line: USize, let column: USize) = _lexer.current()
    "Error:\n" +
    path + ":" + (line + 1).string() + ":" + (column + 1).string() + ": " +
    code.string() + hint()
