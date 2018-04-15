use "assert"
use "files"

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

class val _FloatEncoder
  let sign_offset: U64 = 63
  let exp_offset: U64 = 52
  let exp_bias: U64 = 1023
  let inf_bits: U64 =
    0b0_11111111111_0000000000000000000000000000000000000000000000000000
  let nan_bits: U64 =
    0b0_11111111111_1000000000000000000000000000000000000000000000000000

  fun encode_int_part(big_int: _BigUInt, big_int_size: USize, big_int_clz: U64)
    : (U64, U64)
  =>
    let int_part_left_aligned: U64 =
      try
        let nolz: U64 = big_int.values(big_int_size - 1)? << big_int_clz
        let msb_bits: U64 = nolz << 1
        let lsb_bits: U64 =
          if big_int_size > 1 then
            big_int.values(big_int_size - 2)? >> (64 - big_int_clz - 1)
          else
            0
          end
        msb_bits or lsb_bits
      else
        // unreachable
        0
      end
    // compute integer part bits
    let int_part_offset: U64 = 64 - exp_offset
    let int_part_bits: U64 = int_part_left_aligned >> int_part_offset
    let avail_bits_i64 = exp_offset.i64() - ((64 - big_int_clz.i64()) - 1)
    let avail_bits: U64 = avail_bits_i64.max(0).u64()
    (int_part_bits, avail_bits)

  fun encode_float_part(
    int_part_gtz: Bool,
    avail_bits: USize,
    exponent: I64,
    integer: Array[U8])
    : (U64, U64)
  =>
    let float_part = _BigUInt.create("0")
    let fpot = _BigUInt.create("5")
    let scratch = _BigUInt.empty()
    let start: ISize = integer.size().isize() + exponent.isize()
    var float_part_bits: U64 = 0
    var float_part_clz: U64 = 0
    var write_state: (Bool | None) = if int_part_gtz then true else false end
    var i: USize = 0
    var j: ISize = 0
    while i < avail_bits do
      let index: ISize = start + j
      let value: U64 =
        if index < 0 then
          0
        elseif index < integer.size().isize() then
          try
            let cc = integer(index.usize())?
            (cc - '0').u64()
          else
            // unreachable
            0
          end
        else
          0
        end
      float_part.add_u64(value)
      let bit_on: Bool =
        if float_part.ge(fpot) then
          float_part.sub(fpot)
          true
        else
          false
        end
      match write_state
      | false if bit_on => write_state = None
      | false => float_part_clz = float_part_clz + 1
      | None => write_state = true
      | true => None
      end
      // TODO: check IEEE 754 standard regarding rounding
      let rounds_up: Bool =
        if i == (avail_bits - 1) then
          float_part.gtz()
        else
          false
        end
      match write_state
      | true =>
        if bit_on or rounds_up then
          let offset: USize = (avail_bits - 1 - i)
          float_part_bits = float_part_bits or (0x1 << offset.u64())
        end
        i = i + 1
      | false | None => None
      end
      float_part.mul_u64(10, scratch)
      fpot.mul_u64(5, scratch)
      j = j + 1
    end
    (float_part_bits, float_part_clz)

  fun encode(positive: Bool, exponent: I64, integer: Array[U8]): F64 =>
    let big_int = _BigUInt("0")
    let scratch = _BigUInt.empty()
    let num_digits: ISize = integer.size().isize() + exponent.isize()
    if num_digits > 0 then
      var i: USize = 0
      while i < num_digits.usize() do
        try
          let value: U64 =
            if i < integer.size() then
              let cc: U8 = integer(i)?
              (cc - '0').u64()
            else
              0
            end
          big_int.mul_u64(10, scratch)
          big_int.add_u64(value)
        else
          // unreachable
          None
        end
        i = i + 1
      end
    end
    let big_int_size: USize = big_int.values.size()
    let big_int_clz: U64 = big_int.clz()
    let int_part_gtz: Bool = big_int.gtz()
    (let int_part_bits: U64, let avail_bits: U64) =
      if int_part_gtz then
        encode_int_part(big_int, big_int_size, big_int_clz)
      else
        (0, exp_offset)
      end
    // compute float part bits
    (let float_part_bits: U64, let float_part_clz: U64) =
      if avail_bits > 0 then
        encode_float_part(int_part_gtz, avail_bits.usize(), exponent, integer)
      else
        (0x0, 0)
      end
    // fraction bits
    let frac_bits = int_part_bits or float_part_bits
    // exponent bits
    let exp: I64 =
      if int_part_gtz then
        ((64 - big_int_clz.i64()) - 1) +
        if big_int_size > 1 then (big_int_size.i64() - 1) * 64 else 0 end
      else
        -(float_part_clz.i64() + 1)
      end
    let exp_bits: U64 = (exp.u64() + exp_bias) << exp_offset
    // sign bits
    let sign_bit: U64 = encode_sign_bit(positive)
    // assemble all bits together
    let float_bits: U64 = (sign_bit or exp_bits) or frac_bits
    F64.from_bits(float_bits)

  fun encode_sign_bit(positive: Bool): U64 =>
    if positive then 0 else 0x1 << sign_offset end

  fun inf(positive: Bool): F64 =>
    let sign_bit: U64 = encode_sign_bit(positive)
    F64.from_bits(sign_bit or inf_bits)

  fun nan(positive: Bool): F64 =>
    F64.from_bits(nan_bits)

class val _Float
  let value: F64

  new val create(value': F64) =>
    value = value'

  fun string(): String iso^ =>
    ("a float (" + value.string() + ")").string()

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

primitive _End          fun string(): String iso^ => "EOF".string()
primitive _LeftBracket  fun string(): String iso^ => "‘[’".string()
primitive _RightBracket fun string(): String iso^ => "‘]’".string()
primitive _LeftBrace    fun string(): String iso^ => "‘{’".string()
primitive _RightBrace   fun string(): String iso^ => "‘}’".string()
primitive _Equals       fun string(): String iso^ => "‘=’".string()
primitive _Dot          fun string(): String iso^ => "‘.’".string()
primitive _Comma        fun string(): String iso^ => "‘,’".string()
primitive _Whitespace   fun string(): String iso^ => "a whitespace".string()
primitive _Newline      fun string(): String iso^ => "a newline".string()

type _Token is
  ( _Integer
  | _Float
  | _String
  | _Bool
  | _Key
  | _End
  | _LeftBracket
  | _RightBracket
  | _LeftBrace
  | _RightBrace
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

  fun ref lex_integer_base(
    cc: U8,
    is_base: {(U8): Bool},
    to_base: {(U64, U8): U64},
    decimal: Bool = false)
    : (U64 | LexerError)
  =>
    var value: U64 = if decimal then U64.from[U8](cc - '0') else 0 end
    var last_underscore: Bool = true
    while true do
      match peep_char()
      | let nc: U8 if is_base(nc) =>
        next_char()
        if not decimal or (cc != '0') then
          let previous = value
          value = to_base(value, nc)
          if (previous != 0) and (value <= previous) then
            return TooLargeToBeRepresentedIn64Bit
          end
          last_underscore = false
        else
          return LeadingZerosNotAllowed
        end
      | '_' if not last_underscore => next_char(); last_underscore = true
      | '_' => next_char(); return UnderscoreNotSurroundedByDigits
      | let _: U8 | None => break
      end
    end
    // TODO: add corresponding test
    if (value > 9) and last_underscore then
      UnderscoreNotSurroundedByDigits
    else
      value
    end

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

  fun ref lex_number(nc: U8) : (_Integer | _Float | LexerError) =>
    let result =
      match lex_base_prefix(nc)
      | _Hex => lex_integer_base(0, this~is_hex(), this~to_hex())
      | _Octal => lex_integer_base(0, this~is_octal(), this~to_octal())
      | _Binary => lex_integer_base(0, this~is_binary(), this~to_binary())
      | None => None
      end
    match result
    | let value: U64 => _Integer(value.i64())
    | None => lex_float_or_decimal(nc, true)
    | let err: LexerError => err
    end

  fun ref lex_sign(): Bool =>
    match peep_char()
    | '-' => next_char(); false
    | '+' => next_char(); true
    | let _: U8 | None => true
    end

  fun ref i64(value: U64, positive: Bool): (I64 | None) =>
    let in_bounds: Bool =
      if positive then
        value <= I64.max_value().u64()
      else
        value <= -I64.min_value().u64()
      end
    if in_bounds then
      if positive then value.i64() else -value.i64() end
    end

  fun ref lex_decimal(nc: U8, positive: Bool): (_Integer | LexerError) =>
    match lex_integer_base(nc, this~is_decimal(), this~to_decimal(), true)
    | let value: U64 =>
      match i64(value, positive)
      | let signed: I64 => _Integer(signed)
      | None => TooLargeToBeRepresentedIn64Bit
      end
    | let err: LexerError => err
    end

  fun ref lex_float_exponent(): (_Integer | LexerError) =>
    let positive: Bool = lex_sign()
    match next_char()
    | let nc: U8 if is_decimal(nc) => lex_decimal(nc, positive)
    else
      return ExponentPartExpected
    end

  fun ref lex_float_or_decimal(nc: U8, positive: Bool)
    : (_Float | _Integer | LexerError)
  =>
    let data = Array[U8]()
    var leading_zero = (nc == '0')
    var last_underscore = false
    var decimal_point = false
    var exponent: I64 = 0
    var explicit_exp: Bool = false
    data.push(nc)
    while true do
      match peep_char()
      | let pc: U8 if is_decimal(pc) =>
        next_char()
        if not leading_zero then
          data.push(pc)
          last_underscore = false
          leading_zero = false
          if decimal_point then
            exponent = exponent - 1
          end
        else
          return LeadingZerosNotAllowed
        end
      | '_' if not last_underscore => next_char(); last_underscore = true
      | '_' => next_char(); return UnderscoreNotSurroundedByDigits
      | '.' if not decimal_point =>
        next_char()
        leading_zero = false
        decimal_point = true
      | '.' => break
      | 'e' | 'E' =>
        explicit_exp = true
        next_char()
        match lex_float_exponent()
        | let int: _Integer => exponent = exponent + int.value
        | let err: LexerError => return err
        end
      | let _: U8 | None => break // FIXME: don't forget the "let _: U8" part
      end
    end
    if last_underscore then
      return UnderscoreNotSurroundedByDigits
    end
    if decimal_point or explicit_exp then
      _Float(_FloatEncoder.encode(positive, exponent, data))
    else
      // TODO: use lex_decimal via push/pop lexer state?
      var value: U64 = 0
      for i in data.values() do
        value = to_decimal(value, i)
      end
      match i64(value, positive)
      | let signed: I64 => _Integer(signed)
      | None => TooLargeToBeRepresentedIn64Bit
      end
    end

  fun ref lex_signed_number(nc: U8): (_Integer | _Float | LexerError) =>
    let positive = (nc == '+')
    match peep_char()
    | let pc: U8 if is_decimal(pc) =>
      next_char()
      lex_float_or_decimal(pc, positive)
    | let pc: U8 if is_alpha(pc) =>
      next_char()
      match lex_key_boolean_or_float(pc, positive)
      | let flt: _Float => flt
      | let _: _BareKey | let _: _Bool => SignedValueExpected
      end
    | let _: U8 | None => SignedValueExpected
    end

  fun ref lex_key_boolean_or_float(cc: U8, positive: Bool = true)
    : (_BareKey | _Bool | _Float)
  =>
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
    | "inf" => _Float(_FloatEncoder.inf(positive))
    | "nan" => _Float(_FloatEncoder.nan(positive))
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
    | '[' => _LeftBracket
    | ']' => _RightBracket
    | '{' => _LeftBrace
    | '}' => _RightBrace
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
        | let nc: U8 if is_decimal(nc) => lex_number(nc)
        | let nc: U8 if is_sign(nc) => lex_signed_number(nc)
        | let nc: U8 if is_key_char(nc) => lex_key_boolean_or_float(nc)
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
