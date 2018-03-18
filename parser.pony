use "assert"
use "collections"
use "files"
use "itertools"

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

trait _Keyable
  fun key(): _BareKey val

class val _BareKey is _Keyable
  let name: String val

  new val create(name': String val) =>
    name = name'

  new val from_integer(integer: _Integer) =>
    name = integer.value.string()

  new val from_boolean(boolean: _Bool) =>
    name = boolean.value.string()

  new val from_string(str: _String) =>
    name = str.value

  fun string(): String iso^ =>
    ("a bare key (" + name + ")").string()

  fun key(): _BareKey =>
    _BareKey(name)

class val _DottedKey
  let names: Array[String]

  new val create(names': Array[String] iso) =>
    names = consume names'

  fun string(): String iso^ =>
    ("a dotted key").string()

type _Key is (_BareKey | _DottedKey)

class val _Integer is _Keyable
  let value: I64

  new val create(value': I64) =>
    value = value'

  fun string(): String iso^ =>
    ("an integer (" + value.string() + ")").string()

  fun key(): _BareKey val =>
    _BareKey(value.string())

class val _String is _Keyable
  let value: String

  new val create(value': String) =>
    value = value'

  fun string(): String iso^ =>
    ("a string (\"" + value.string() + "\")").string()

  fun key(): _BareKey val =>
    _BareKey(value)

class val _Bool is _Keyable
  let value: Bool

  new val create(value': Bool) =>
    value = value'

  fun string(): String iso^ =>
    ("a boolean (" + value.string() + ")").string()

  fun key(): _BareKey val =>
    _BareKey(value.string())

primitive _End         fun string(): String iso^ => "EOF".string()
primitive _LeftSquare  fun string(): String iso^ => "‘[’".string()
primitive _RightSquare fun string(): String iso^ => "‘]’".string()
primitive _Equals      fun string(): String iso^ => "‘=’".string()
primitive _Dot         fun string(): String iso^ => "‘.’".string()
primitive _Comma       fun string(): String iso^ => "‘,’".string()
primitive _Whitespace  fun string(): String iso^ => "a whitespace".string()
primitive _Newline     fun string(): String iso^ => "a newline".string()

type _Token is
  ( _Integer
  | _String
  | _Bool
  | _Key
  | _End
  | _LeftSquare
  | _RightSquare
  | _Equals
  | _Dot
  | _Comma
  | _Whitespace
  | _Newline )

primitive _Hex
primitive _Octal
primitive _Binary

type _BasePrefix is (_Hex | _Octal | _Binary)

class _Lexer
  let _block_size: USize = 4096
  var _block: Array[U8] = Array[U8]()
  var _position: USize = 0
  var _current: (USize, USize) = (0, 0)
  var _next: (USize, USize) = _current
  var file_or_string: (File ref | String)
  var start_position: ISize

  new from_file(file: File ref) =>
    file_or_string = file
    start_position = ISize.from[USize](file.position())

  new from_string(string: String) =>
    file_or_string = string
    start_position = 0

  fun current(): (USize, USize) => _current

  fun is_newline(value: U8): Bool =>
    (value == '\n')

  fun is_whitespace(value: U8): Bool =>
    (value == ' ') or (value == '\t') or is_newline(value)

  fun is_comment(value: U8): Bool =>
    (value == '#')

  fun is_quote(value: U8): Bool =>
    (value == '"') or (value == '\'')

  fun is_escape_char(value: U8): Bool =>
    (value == '\\')

  fun is_sign(value: U8): Bool =>
    (value == '+') or (value == '-')

  fun is_alpha(value: U8): Bool =>
    ((value >= 'a') and (value <= 'z')) or ((value >= 'A') and (value <= 'Z'))

  fun is_decimal(value: U8): Bool =>
    (value >= '0') and (value <= '9')

  fun is_hex(value: U8): Bool =>
    ((value >= '0') and (value <= '9')) or
    ((value >= 'a') and (value <= 'f')) or ((value >= 'A') and (value <= 'F'))

  fun is_octal(value: U8): Bool =>
    (value >= '0') and (value <= '7')

  fun is_binary(value: U8): Bool =>
    (value == '0') or (value == '1')

  fun is_dash_or_underscore(value: U8): Bool =>
    (value == '-') or (value == '_')

  fun is_key_char(value: U8): Bool =>
    is_alpha(value) or is_decimal(value) or is_dash_or_underscore(value)

  fun ref next_block(file: File ref): Bool =>
      let block = file.read(_block_size)
      _block = consume block
      _position = 0
      _block.size() != 0

  fun ref peep_char(): (U8 | None) =>
    try
      match file_or_string
      | let file: File => None
        if _position == _block.size() then
          if not next_block(file) then
            return None
          end
        end
        _block(_position)?
      | let string: String =>
        if _position < string.size() then
          string(_position)?
        else
          None
        end
      end
    else
      // unreachable
      None
    end

  fun ref next_char(): (U8 | None) =>
    let value = peep_char()
    _position = _position + 1
    _current = _next
    _next = (_current._1, _current._2 + 1)
    value

  fun ref lex_base_prefix(cc: U8): (_BasePrefix | None) =>
    if cc == '0' then
      match peep_char()
      | 'x' | 'X' => next_char(); _Hex
      | 'o' | 'O' => next_char(); _Octal
      | 'b' | 'B' => next_char(); _Binary
      else
        None
      end
    else
      None
    end

  fun ref lex_integer_base(
    cc: U8,
    is_from_base: {(U8): Bool},
    to_base: {(U64, U8): U64},
    lex_decimal: Bool = false)
    : (U64 | LexerError)
  =>
    var value: U64 = if lex_decimal then U64.from[U8](cc - '0') else 0 end
    var last_underscore: Bool = false
    while true do
      match peep_char()
      | let nc: U8 if is_from_base(nc) =>
        next_char()
        if not lex_decimal or (cc != '0') then
          let previous = value
          value = to_base(value, nc)
          if (previous != 0) and (value <= previous) then
            return TooLargeToBeRepresentedIn64Bit
          end
          last_underscore = false
        else
          return LeadingZerosInDecimalNotAllowed
        end
      | let nc: U8 if nc == '_' =>
          next_char()
        if value == 0 then
          return UnderscoreAfterBasePrefixNotAllowed
        elseif not last_underscore then
          last_underscore = true
          continue
        else
          return UnderscoreNotSurroundedByDigits
        end
      | let _: U8 | None => break
      end
    end
    if last_underscore then
      UnderscoreNotSurroundedByDigits
    else
      value
    end

  fun to_hex(value: U64, cc: U8): U64 =>
    (value * 16) + (U64.from[U8](cc - '0') and 15) +
    if (cc >= 'A') or (cc >= 'a') then 9 else 0 end

  fun to_hex_u32(value: U32, cc: U8): U32 =>
    (value * 16) + (U32.from[U8](cc - '0') and 15) +
    if (cc >= 'A') or (cc >= 'a') then 9 else 0 end

  fun to_octal(value: U64, cc: U8): U64 =>
    (value * 8) + U64.from[U8](cc - '0')

  fun to_binary(value: U64, cc: U8): U64 =>
    (value * 2) + (U64.from[U8](cc - '0') and 1)

  fun to_decimal(value: U64, cc: U8): U64 =>
    (value * 10) + U64.from[U8](cc - '0')

  fun ref lex_integer(cc: U8, sign_prefix: Bool = false, is_pos: Bool = true)
    : (_Integer | LexerError)
  =>
    let base_prefix = lex_base_prefix(cc)
    let result =
      match base_prefix
      | _Hex => lex_integer_base(0, this~is_hex(), this~to_hex())
      | _Octal => lex_integer_base(0, this~is_octal(), this~to_octal())
      | _Binary => lex_integer_base(0, this~is_binary(), this~to_binary())
      | None => lex_integer_base(cc, this~is_decimal(), this~to_decimal(), true)
      end
    match result
    | let value: U64 =>
      match base_prefix
      | None =>
        let max_value: U64 = U64.from[I64](I64.max_value())
        if is_pos then
          if value <= max_value then
            _Integer(I64.from[U64](value))
          else
            TooLargeToBeRepresentedIn64Bit
          end
        else
          if value <= (max_value + 1) then
            _Integer(-I64.from[U64](value))
          else
            TooLargeToBeRepresentedIn64Bit
          end
        end
      | let _: _BasePrefix if not sign_prefix => _Integer(I64.from[U64](value))
      else
        SignPrefixWithBasePrefixNotAllowed
      end
    | let err: LexerError => err
    end

  fun ref lex_key_or_boolean(cc: U8): (_Key | _Bool) =>
    let str: String = recover
      var temp: String ref = String()
      temp.push(cc)
      while true do
        match peep_char()
        | let nc: U8 if is_key_char(nc) => next_char(); temp.push(nc)
        | let _: U8 | None => break
        end
      end
      temp
    end
    match str
    | "true"  => _Bool(true)
    | "false" => _Bool(false)
    else
      _BareKey(str)
    end

  fun ref is_valid_scalar(codepoint: U32): Bool =>
    ((codepoint >= 0) and (codepoint <= 0xD7FF)) or
    ((codepoint >= 0xE000) and (codepoint <= 0x10FFFF))

  fun ref lex_unicode_char(is_16bit: Bool): (U32 | LexerError) =>
    var codepoint: U32 = 0
    var i: USize = 0
    while true do
      match next_char()
      | let nc: U8 if is_hex(nc) => codepoint = to_hex_u32(codepoint, nc)
      | None => return InvalidUnicodeEscapeCode
      end
      i = i + 1
      if (is_16bit and (i == 4)) or (i == 8) then
        break
      end
    end
    if is_valid_scalar(codepoint) then
      codepoint
    else
      InvalidUnicodeEscapeCode
    end

  fun ref lex_escape_sequence(): (U32 | LexerError) =>
    match next_char()
    | 'b' => U32(0x0008)
    | 't' => U32(0x0009)
    | 'n' => U32(0x000A)
    | 'f' => U32(0x000C)
    | 'r' => U32(0x000D)
    | '"' => U32(0x0022)
    | '\\' => U32(0x005C)
    | 'u' => lex_unicode_char(where is_16bit = true)
    | 'U' => lex_unicode_char(false)
    else
      InvalidEscapeSequence
    end

  fun ref lex_triple_quote(quote: U8): (Bool | U8 | LexerError) =>
    // returns true when a triple quote is matched, false for an empty basic or
    // literal string, otherwise returns the second char from the triplet, e.g.
    // """ => true
    // ''' => true
    // "", => false
    // ''a => false
    // 'a' => a
    // '"" => "
    match next_char()
    | quote =>
      match peep_char()
      | quote => next_char(); true
      | let _: U8 => quote
      | None => false
      end
    | let nc: U8 => nc
    | None => UnterminatedString
    end

  fun ref lex_string(quote: U8): (_String | LexerError) =>
    /*
    // FIXME: This does not typecheck, we need more investigation
    match lex_triple_quote(quote)
    | true => lex_string_body('\0', quote, true)
    | false => _String("")
    | let nc: U8 => lex_string_body(nc, quote, false)
    | let err: LexerError => err
    end
    */
    var is_multiline: Bool = false
    var cc: U8 = '\0'
    match lex_triple_quote(quote)
    | true => is_multiline = true
    | false => return _String("")
    | let nc: U8 => cc = nc
    | let err: LexerError => return err
    end
    lex_string_body(cc, quote, is_multiline)

  fun ref lex_string_body(cc: U8, quote: U8, is_multiline: Bool)
    : (_String | LexerError)
  =>
    let str: String = recover
      let is_basic = quote == '"'
      var read_next: Bool = is_multiline
      var x: U8 = cc
      var trim_first_newline = true
      var trim_whitespace = false
      var temp: String ref = String()
      while true do
        match if read_next then next_char() else x end
        | let nc: U8 if is_basic and is_escape_char(nc) =>
          match peep_char()
          | None => UnterminatedString
          | let pc: U8 if is_newline(pc) =>
            next_char()
            trim_whitespace = true
          else
            match lex_escape_sequence()
            | let value: U32 => temp.push_utf32(value)
            | let err: LexerError => return err
            end
          end
        | quote =>
          if is_multiline then
            match lex_triple_quote(quote)
            | true => break // end of string
            | false => temp.push(quote); temp.push(quote)
            | let nc: U8 =>
              temp.push(quote)
              read_next = false
              x = nc
              continue
            | let err: LexerError => return err
            end
          else
            // end of string
            break
          end
        | let nc: U8 if is_whitespace(nc) and trim_whitespace => None
        | let nc: U8 if is_newline(nc) =>
          if is_multiline then
            if (temp.size() == 0) and trim_first_newline then
              trim_first_newline = false
            else
              temp.push(nc)
            end
          else
            return MultiLineStringNotAllowed
          end
        | let nc: U8 => trim_whitespace = false; temp.push(nc)
        | None => return UnterminatedString
        end
        read_next = true
      end
      temp
    end
    _String(str)

  fun ref lex_comment(): _Token =>
    while true do
      match next_char()
      | let nc: U8 if nc == '\n' =>
        _next = (_next._1 + 1, 0)
        return _Newline
      | None =>
        return _End
      end
    end
    // unreachable
    _End

  fun ref lex_symbol(cc: U8): (_Token | LexerError) =>
    match cc
    | '[' => _LeftSquare
    | ']' => _RightSquare
    | '=' => _Equals
    | '.' => _Dot
    | ',' => _Comma
    | ' ' | '\t' => _Whitespace
    | '\n' =>
      _next = (_next._1 + 1, 0)
      _Newline
    else
      InvalidChar(cc)
    end

  fun ref next(): (_Token | LexerError) =>
    while true do
      let token =
        match next_char()
        | let nc: U8 if is_decimal(nc) => lex_integer(nc)
        | let nc: U8 if is_sign(nc) =>
          match next_char()
          | let nnc: U8 if is_decimal(nnc) => lex_integer(nnc, true, nc == '+')
          | let _: U8 | None => DecimalNumberExpected
          end
        | let nc: U8 if is_key_char(nc) => lex_key_or_boolean(nc)
        | let nc: U8 if is_quote(nc) => lex_string(nc)
        | let nc: U8 if is_comment(nc) => lex_comment()
        | let nc: U8 => lex_symbol(nc)
        | None => _End
        end
      match token
      | _Whitespace => continue
      else
        return token
      end
    end
    // unreachable
    _End

class val _Appender
fun append_with_indent(value: TOMLValue box, indent: String, result: String ref)
  : None
=>
  match value
  | let str: String box =>
    result.append("\"")
    result.append(str.string())
    result.append("\"")
  | let table: TOMLTable box => table._append_with_indent(indent, result)
  | let array: TOMLArray box => array._append_with_indent(indent, result)
  else
    result.append(value.string())
  end

class TOMLTable
  var map: Map[String, TOMLValue]

  new create() =>
    map = Map[String, TOMLValue]()

  fun string(): String iso^ =>
    var result: String ref = String()
    _append_with_indent("", result)
    result.string()

  fun _append_with_indent(indent: String, result: String ref): String iso^ =>
    let tab = "  "
    var i: USize = 0
    result.append("{\n")
    for pair in map.pairs() do
      result.append(indent)
      result.append(tab)
      result.append("\"")
      result.append(pair._1.string())
      result.append("\"")
      result.append(": ")
      _Appender.append_with_indent(pair._2, indent + tab, result)
      if i < (map.size() - 1) then
        result.append(",")
      end
      result.append("\n")
      i = i + 1
    end
    result.append(indent)
    result.append("}")
    result.string()

class TOMLArray
  var array: Array[TOMLValue]

  fun string(): String iso^ =>
    var result: String ref = String()
    _append_with_indent("", result)
    result.string()

  fun _append_with_indent(indent: String, result: String ref): String iso^ =>
    let tab = "  "
    var i: USize = 0
    result.append("[\n")
    for value in array.values() do
      result.append(indent)
      result.append(tab)
      _Appender.append_with_indent(value, indent + tab, result)
      if i < (array.size() - 1) then
        result.append(",")
      end
      result.append("\n")
      i = i + 1
    end
    result.append(indent)
    result.append("]")
    result.string()

  new create() =>
    array = Array[TOMLValue]

type TOMLValue is
  ( I64
  | Bool
  | String
  | TOMLTable
  | TOMLArray )

class Parser
  var _lexer: _Lexer
  var _table_stack: Array[TOMLTable] ref = Array[TOMLTable]()
  var _parsing_done: Bool = false
  var _error: (None | Error) = None
  let _root_level: USize = 1 // using _table_stack.size() do NOT work
  var _ahead: ((_Token | LexerError) | None) = None

  new from_file(file: File ref) =>
    _lexer = _Lexer.from_file(file)
    _table_stack.push(TOMLTable.create())

  new from_string(string: String) =>
    _lexer = _Lexer.from_string(string)
    _table_stack.push(TOMLTable.create())

  fun ref _next_token(): (_Token | LexerError) =>
    match _ahead
    | None => _lexer.next()
    | let token: (_Token | LexerError) =>
      _ahead = None
      token
    end

  fun ref _look_ahead(): (_Token | LexerError) =>
    match _ahead
    | None =>
      let token = _lexer.next()
      _ahead = token
      token
    else
      try
        Assert(false, "only one token look ahead is possible")?
      end
      _End
    end

  fun ref _error_expected(result: (_Token | LexerError), values: String)
    : Error
  =>
    match result
    | let err: LexerError => Error(err, _lexer)
    | let token: _Token =>
      Error(UnexpectedToken(token, values), _lexer)
    end

  fun ref _error_expected_if(
    result: (_Token | LexerError),
    expected: Array[_Token])
    : (None | Error)
  =>
    let is_equal = {(lhs: _Token, rhs: _Token): Bool =>
      match lhs
      | let _: _Key => match rhs | let _: _Key => true else false end
      | let _: _Integer => match rhs | let _: _Integer => true else false end
      | let _: _String => match rhs | let _: _String => true else false end
      | let _: _Bool => match rhs | let _: _Bool => true else false end
      | _End => match rhs | _End => true else false end
      | _LeftSquare => match rhs | _LeftSquare => true else false end
      | _RightSquare => match rhs | _RightSquare => true else false end
      | _Equals => match rhs | _Equals => true else false end
      | _Dot => match rhs | _Dot => true else false end
      | _Comma => match rhs | _Comma => true else false end
      | _Whitespace => match rhs | _Whitespace => true else false end
      | _Newline => match rhs | _Newline => true else false end
      end
    }
    match result
    | let token: _Token =>
      if Iter[_Token](expected.values()).any({(t) => is_equal(t, token)}) then
        None
      else
        let values = (Iter[_Token](expected.values()))
          .fold[String]("", {(str, tok) =>
            str + (if str != "" then ", " else "" end) + tok.string() })
        _error_expected(token, values)
      end
    | let err: LexerError => Error(err, _lexer)
    end

  fun ref _pop_table(): None =>
    try
      Assert(_table_stack.size() > 1)?
      _table_stack.pop()?
    end

  fun ref _pop_tables(): None =>
    while _table_stack.size() != _root_level do
      _pop_table()
    end

  fun ref _push_table(key: _BareKey, is_dotted: Bool = false)
    : (None | ParserError)
  =>
    try
      Assert(_table_stack.size() > 0, "_table_stack must not be empty")?
      let table = _table_stack(_table_stack.size() - 1)?
      if table.map.contains(key.name) then
        if is_dotted then
          match table.map(key.name)?
          | let this_table: TOMLTable => _table_stack.push(this_table)
          else
            ValueIsNotATable
          end
        else
          KeyOrTableDefinedMoreThanOnce
        end
      else
        let new_table = TOMLTable.create()
        table.map.insert(key.name, new_table)?
        _table_stack.push(new_table)
        None
      end
    end

  fun ref _push_table_rec(dotted: _DottedKey, index: USize)
    : (None | ParserError)
  =>
    try
      if index < (dotted.names.size() - 1) then
        let bare = _BareKey(dotted.names(index)?)
        match _push_table(bare, true)
        | None => _push_table_rec(dotted, index + 1)
        | let err: ParserError => return err
        end
      else
        let bare = _BareKey(dotted.names(index)?)
        _push_table(bare)
      end
    else
      // unreachable
      None
    end

  fun ref _parse_table(): (None | Error) =>
    let token = _next_token()
    match token
    | let keyable: _Keyable val =>
      let result =
        match _parse_key(keyable.key(), true)
        | let bare: _BareKey =>
          _pop_tables()
          _push_table(bare)
        | let dotted: _DottedKey =>
          _pop_tables()
          _push_table_rec(dotted, 0)
        end
      match result
      | None => None
      | let err: ParserError => Error(err, _lexer)
      end
    else
      _error_expected(token, "the table name")
    end

  fun ref _insert_value_rec(dotted: _DottedKey, value: TOMLValue, index: USize)
    : (None | ParserError)
  =>
    try
      let bare = _BareKey(dotted.names(index)?)
      if index < (dotted.names.size() - 1) then
        match _push_table(bare, true)
        | None =>
          match _insert_value_rec(dotted, value, index + 1)
          | None => _pop_table()
          | let err: ParserError => err
          end
        | let err: ParserError => err
        end
      else
        _insert_value(bare, value)
      end
    else
      // unreachable
      None
    end

  fun ref _insert_value(key: _Key, value: TOMLValue): (None | ParserError) =>
    match key
    | let bare: _BareKey =>
      try
        Assert(_table_stack.size() > 0, "_table_stack must not be empty")?
        let table = _table_stack(_table_stack.size() - 1)?
        if table.map.contains(bare.name) then
          KeyOrTableDefinedMoreThanOnce
        else
          table.map.insert(bare.name, value)?
          None
        end
      end
    | let dotted: _DottedKey =>
      _insert_value_rec(dotted, value, 0)
    end

  fun ref _parse_array(): (TOMLArray | Error) =>
    let is_equal = {(lhs: TOMLValue, rhs: TOMLValue): Bool =>
      match lhs
      | let _: I64 => match rhs | let _: I64 => true else false end
      | let _: Bool => match rhs | let _: Bool => true else false end
      | let _: String => match rhs | let _: String => true else false end
      | let _: TOMLTable => match rhs | let _: TOMLTable => true else false end
      | let _: TOMLArray => match rhs | let _: TOMLArray => true else false end
      end
    }
    let arr: TOMLArray = TOMLArray.create()
    var first: (TOMLValue | None) = None
    while true do
      match _look_ahead()
      | _RightSquare => _next_token(); break
      | let err: LexerError => Error(err, _lexer)
      else
        match _parse_value()
        | let value: TOMLValue =>
          match first
          | None => first = value
          | let lhs: TOMLValue =>
            if not is_equal(lhs, value) then
              return Error(ArrayHasMixDataTypes, _lexer)
            end
          end
          arr.array.push(value)
          let token = _next_token()
          match _error_expected_if(token, [_Comma; _RightSquare])
          | None =>
            match token
            | _RightSquare => break
            end
          | let err: Error => return err
          end
        | let err: Error => return err
        end
      end
    end
    arr

  fun ref _parse_value(): (TOMLValue | Error) =>
    let token = _next_token()
    match token
    | let int: _Integer => int.value
    | let bool: _Bool => bool.value
    | let str: _String => str.value
    | _LeftSquare => _parse_array()
    else
      _error_expected(token, "a valid TOML value")
    end

  fun ref _parse_key(bare: _BareKey, is_table: Bool = false): (_Key | Error) =>
    let names: Array[String] iso = recover Array[String] end
    names.push(bare.name)
    while true do
      let token = _next_token()
      match token
      | _Dot =>
        let token2 = _next_token()
        match token2
        | let keyable: _Keyable val => names.push(keyable.key().name)
        else
          return _error_expected(token2, "a bare or quoted key")
        end
      | _Equals if not is_table => break
      | _RightSquare if is_table => break
      else
        let terminator = if is_table then "a right bracket" else "equals" end
        return _error_expected(token, "a dot or " + terminator)
      end
    end
    let dotted: Bool = names.size() > 1
    if dotted then _DottedKey(consume names) else bare end

  fun ref _parse_key_value(bare: _BareKey): (None | Error) =>
    match _parse_key(bare)
    | let key: _Key =>
      match _parse_value()
      | let value: TOMLValue =>
        match _insert_value(key, value)
        | None => _error_expected_if(_next_token(), [_Newline; _End])
        | let err: ParserError => Error(err, _lexer)
        end
      | let err: Error => err
      end
    | let err: Error => err
    end

  fun ref _parse_top_level(): (None | Error) =>
    var result: (None | Error) = None
    while true do
      let token = _next_token()
      result =
        match token
        | let keyable: _Keyable val => _parse_key_value(keyable.key())
        | _LeftSquare => _parse_table()
        | _Newline => continue
        | _End => break
        else
          _error_expected(token, "a table, a key/value pair or a newline")
        end
      match result
      | let _: Error => break
      end
    end
    result

  fun ref parse(): (TOMLTable | Error) =>
    if not _parsing_done then
      _error = _parse_top_level()
      _parsing_done = true
    end
    match _error
    | let _: None =>
      try
        _table_stack(0)?
      else
        // unreachable
        TOMLTable.create()
      end
    | let err: Error => err
    end

actor _Main
  new create(env: Env) =>
    let parser = Parser.from_string(
      """
      # This is a TOML document.

      title = "TOML Example"

      [owner]
      name = "Tom Preston-Werner"

      [database]
      server = "192.168.1.1"
      ports = [ 8001, 8001, 8002 ]
      connection_max = 5000
      enabled = true

      [servers]

        # Indentation (tabs and/or spaces) is allowed but not required
        [servers.alpha]
        ip = "10.0.0.1"
        dc = "eqdc10"

        [servers.beta]
        ip = "10.0.0.2"
        dc = "eqdc10"
      """)
    match parser.parse()
    | let doc: TOMLTable =>
      env.out.print(doc.string()) // JSON like output
      try
        match doc.map("database")?
        | let database: TOMLTable =>
          env.out.print(database.map("connection_max")?.string())
        end
      end
    | let err: Error => env.out.print(err.message())
    end
