use "assert"
use "collections"
use "files"
use "itertools"

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
  | F64
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
      | let _: _Float => match rhs | let _: _Float => true else false end
      | let _: _String => match rhs | let _: _String => true else false end
      | let _: _Bool => match rhs | let _: _Bool => true else false end
      | _End => match rhs | _End => true else false end
      | _LeftBracket => match rhs | _LeftBracket => true else false end
      | _RightBracket => match rhs | _RightBracket => true else false end
      | _LeftBrace => match rhs | _LeftBrace => true else false end
      | _RightBrace => match rhs | _RightBrace => true else false end
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
        | let err: ParserError => err
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
      | let _: F64 => match rhs | let _: F64 => true else false end
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
      | _RightBracket => _next_token(); break
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
          match _error_expected_if(token, [_Comma; _RightBracket])
          | None =>
            match token
            | _RightBracket => break
            end
          | let err: Error => return err
          end
        | let err: Error => return err
        end
      end
    end
    arr

  fun ref _parse_inline_table(): (TOMLTable | Error) =>
    let new_table = TOMLTable.create()
    _table_stack.push(new_table)
    while true do
      let token = _next_token()
      match token
      | let keyable: _Keyable val =>
        match _parse_key_value(keyable.key())
        | None =>
          let token2 = _next_token()
          match _error_expected_if(token2, [_Comma; _RightBrace])
          | None =>
            match token2
            | _RightBrace => break
            end
          | let err: Error => return err
          end
        | let err: Error => return err
        end
      | _RightBrace => break
      | _Newline => return Error(NewlineInInlineTable, _lexer)
      else
        return _error_expected(token, "key value pairs or ‘}’")
      end
    end
    try
      _table_stack.pop()?
    else
      // unreachable
      None
    end
    new_table

  fun ref _parse_value(): (TOMLValue | Error) =>
    let token = _next_token()
    match token
    | let int: _Integer => int.value
    | let flt: _Float => flt.value
    | let bool: _Bool => bool.value
    | let str: _String => str.value
    | _LeftBracket => _parse_array()
    | _LeftBrace => _parse_inline_table()
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
      | _RightBracket if is_table => break
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
        | None => None
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
        | let keyable: _Keyable val =>
          match _parse_key_value(keyable.key())
          | None => _error_expected_if(_next_token(), [_Newline; _End])
          | let err: Error => err
          end
        | _LeftBracket => _parse_table()
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
