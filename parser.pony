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

primitive MultiLineBasicStringNotAllowed
fun string(): String iso^ =>
  "basic strings can't span over multiple lines, use multi-line basic".string()

primitive InvalidEscapeSequence
fun string(): String iso^ => "invalid escape sequence".string()

primitive InvalidUnicodeEscapeCode
fun string(): String iso^ => "invalid unicode escape code".string()

type StringError is
  ( UnterminatedString
  | MultiLineBasicStringNotAllowed
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

class val UnexpectedToken
  let _value: _Token
  let _expected: String

  new val create(token: _Token, values: String) =>
    _value = token
    _expected = values

  fun string(): String iso^ =>
    ("expecting " + _expected + " but get " + _value.string()).string()

type ParserError is ( KeyOrTableDefinedMoreThanOnce | UnexpectedToken )

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

primitive _Bare
primitive _Quotted
primitive _Dotted

type _KeyUsage is (_Bare | _Quotted | _Dotted)

class val _Key
  let value: String val
  let usage: _KeyUsage val

  new val create(value': String val, usage': _KeyUsage val) =>
    value = value'
    usage = usage'

  fun string(): String iso^ =>
    ("a key (" + value + ")").string()

class val _Integer
  let value: I64

  new val create(value': I64) =>
    value = value'

  fun string(): String iso^ =>
    ("an integer (" + value.string() + ")").string()

class val _String
  let value: String

  new val create(value': String) =>
    value = value'

  fun string(): String iso^ =>
    ("a string (\"" + value.string() + "\")").string()

class val _Bool
  let value: Bool

  new val create(value': Bool) =>
    value = value'

  fun string(): String iso^ =>
    ("a boolean (" + value.string() + ")").string()

primitive _End         fun string(): String iso^ => "EOF".string()
primitive _LeftSquare  fun string(): String iso^ => "‘[’".string()
primitive _RightSquare fun string(): String iso^ => "‘]’".string()
primitive _Equals      fun string(): String iso^ => "‘=’".string()
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

  fun is_whitespace(value: U8): Bool =>
    (value == ' ') or (value == '\t')

  fun is_newline(value: U8): Bool =>
    (value == '\n')

  fun is_comment(value: U8): Bool =>
    (value == '#')

  fun is_quote(value: U8): Bool =>
    (value == '"')

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
    base: {(U64, U8): U64},
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
          value = base(value, nc)
          if value < previous then
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

  fun hex(value: U64, cc: U8): U64 =>
    (value * 16) + (U64.from[U8](cc - '0') and 15) +
    if (cc >= 'A') or (cc >= 'a') then 9 else 0 end

  fun hex_u32(value: U32, cc: U8): U32 =>
    (value * 16) + (U32.from[U8](cc - '0') and 15) +
    if (cc >= 'A') or (cc >= 'a') then 9 else 0 end

  fun octal(value: U64, cc: U8): U64 =>
    (value * 8) + U64.from[U8](cc - '0')

  fun binary(value: U64, cc: U8): U64 =>
    (value * 2) + (U64.from[U8](cc - '0') and 1)

  fun decimal(value: U64, cc: U8): U64 =>
    (value * 10) + U64.from[U8](cc - '0')

  fun ref lex_integer(cc: U8, sign_prefix: Bool = false, is_pos: Bool = true)
    : (_Integer | LexerError)
  =>
    let base_prefix = lex_base_prefix(cc)
    let result =
      match base_prefix
      | _Hex => lex_integer_base(0, this~is_hex(), this~hex())
      | _Octal => lex_integer_base(0, this~is_octal(), this~octal())
      | _Binary => lex_integer_base(0, this~is_binary(), this~binary())
      | None => lex_integer_base(cc, this~is_decimal(), this~decimal(), true)
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
      _Key(str, _Bare)
    end

  fun ref is_valid_scalar(codepoint: U32): Bool =>
    ((codepoint >= 0) and (codepoint <= 0xD7FF)) or
    ((codepoint >= 0xE000) and (codepoint <= 0x10FFFF))

  fun ref lex_unicode_char(is_16bit: Bool): (U32 | LexerError) =>
    var codepoint: U32 = 0
    var i: USize = 0
    while true do
      match next_char()
      | let nc: U8 if is_hex(nc) => codepoint = hex_u32(codepoint, nc)
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

  fun ref lex_basic_string(): (_String | LexerError) =>
    let str: String = recover
      var temp: String ref = String()
      while true do
        match next_char()
        | let nc: U8 if is_escape_char(nc) =>
          match lex_escape_sequence()
          | let value: U32 => temp.push_utf32(value)
          | let err: LexerError => return err
          end
        | let nc: U8 if is_quote(nc) => break
        | let nc: U8 if is_newline(nc) => return MultiLineBasicStringNotAllowed
        | let nc: U8 => temp.push(nc)
        | None => return UnterminatedString
        end
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
    | ' ' | '\t' => _Whitespace
    | '\n' =>
      _next = (_next._1 + 1, 0)
      _Newline
    else
      InvalidChar(cc)
    end

  fun ref next(): (_Token | LexerError) =>
    let token =
      match next_char()
      | let nc: U8 if is_decimal(nc) => lex_integer(nc)
      | let nc: U8 if is_sign(nc) =>
        match next_char()
        | let nnc: U8 if is_decimal(nnc) => lex_integer(nnc, true, nc == '+')
        | let _: U8 | None => DecimalNumberExpected
        end
      | let nc: U8 if is_key_char(nc) => lex_key_or_boolean(nc)
      | let nc: U8 if is_quote(nc) => lex_basic_string()
      | let nc: U8 if is_comment(nc) => lex_comment()
      | let nc: U8 => lex_symbol(nc)
      | None => _End
      end
    match token
    | _Whitespace => next() // TODO: don't rely on tco, use a loop instead
    else
      token
    end

class TOMLTable
  var map: Map[String, TOMLValue]

  new create() =>
    map = Map[String, TOMLValue]()

  fun string(): String iso^ =>
    _string_with_indent("")

  fun _string_with_indent(indent_str: String): String iso^ =>
    let tab = "  "
    var result: String ref = String()
    var i: USize = 0
    result.append("{\n")
    for pair in map.pairs() do
      result.append(indent_str)
      result.append(tab)
      result.append("\"")
      result.append(pair._1.string())
      result.append("\"")
      result.append(": ")
      match pair._2
      | let str: String =>
        result.append("\"")
        result.append(str.string())
        result.append("\"")
      | let table: TOMLTable box =>
        result.append(table._string_with_indent(indent_str + tab))
      else
        result.append(pair._2.string())
      end
      if i < (map.size()-1) then
        result.append(",")
      end
      result.append("\n")
      i = i + 1
    end
    result.append(indent_str)
    result.append("}")
    result.string()

class TOMLArray
  var array: Array[TOMLValue]

  fun string(): String iso^ =>
    "".string()

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

  new from_file(file: File ref) =>
    _lexer = _Lexer.from_file(file)
    _table_stack.push(TOMLTable.create())

  new from_string(string: String) =>
    _lexer = _Lexer.from_string(string)
    _table_stack.push(TOMLTable.create())

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
    // only pop if top of the stack is not the root table
    if _table_stack.size() > 1 then
      try
        _table_stack.pop()?
      end
    end

  fun ref _push_table(key: String): (None | ParserError) =>
    try
      Assert(_table_stack.size() > 0, "_table_stack must not be empty")?
      let table = _table_stack(_table_stack.size() - 1)?
      if table.map.contains(key) then
        KeyOrTableDefinedMoreThanOnce
      else
        let new_table = TOMLTable.create()
        table.map.insert(key, new_table)?
        _table_stack.push(new_table)
        None
      end
    end

  fun ref _pop_and_push(key: String): (None | Error) =>
    _pop_table()
    match _push_table(key)
    | None => _error_expected_if(_lexer.next(), [_RightSquare])
    | let err: ParserError => Error(err, _lexer)
    end

  fun ref _parse_table(): (None | Error) =>
    let token = _lexer.next()
    match token
    | let key: _Key =>
      match key.usage
      | _Bare => _pop_table()
      end
      match _push_table(key.value)
      | None => _error_expected_if(_lexer.next(), [_RightSquare])
      | let err: ParserError => Error(err, _lexer)
      end
    | let int: _Integer => _pop_and_push(int.value.string())
    | let bool: _Bool => _pop_and_push(bool.value.string())
    else
      _error_expected(token, "the table name")
    end

  fun ref _parse_equals(): (None | Error) =>
    let token = _lexer.next()
    match token
    | _Equals => None
    else
      _error_expected(token, "‘=‘")
    end

  fun ref _parse_value(): (TOMLValue | Error) =>
    let token = _lexer.next()
    match token
    | let int: _Integer => int.value
    | let bool: _Bool => bool.value
    | let str: _String => str.value
    else
      _error_expected(token, "a valid TOML value")
    end

  fun ref _insert_value(key: _Key, value: TOMLValue): (None | ParserError) =>
    try
      Assert(_table_stack.size() > 0, "_table_stack must not be empty")?
      let table = _table_stack(_table_stack.size() - 1)?
      if table.map.contains(key.value) then
        KeyOrTableDefinedMoreThanOnce
      else
        table.map.insert(key.value, value)?
        None
      end
    end

  fun ref _parse_key_value(key: _Key): (None | Error) =>
    match _parse_equals()
    | None =>
      match _parse_value()
      | let value: TOMLValue =>
        match _insert_value(key, value)
        | None => _error_expected_if(_lexer.next(), [_Newline; _End])
        | let err: ParserError => Error(err, _lexer)
        end
      | let err: Error => err
      end
    | let err: Error => err
    end

  fun ref _parse_top_level(): (None | Error) =>
    var result: (None | Error) = None
    while true do
      let token = _lexer.next()
      result =
        match token
        | let key: _Key => _parse_key_value(key)
        | let int: _Integer => _parse_key_value(_Key(int.value.string(), _Bare))
        | let bool: _Bool => _parse_key_value(_Key(bool.value.string(), _Bare))
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
      connection_max = 5000
      enabled = true
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
