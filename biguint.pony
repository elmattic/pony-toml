class _BigUInt
  let values: Array[U64]

  new ref create(decimal: String) =>
    values = Array[U64]()
    let scratchpad = _BigUInt.empty()
    var i: USize = 0
    while i < decimal.size() do
      try
        let cc: U8 = decimal(i)?
        let value: U64 = (cc - '0').u64()
        mul_u64(10, scratchpad)
        add_u64(value)
        i = i + 1
      else
        // unreachable
        None
      end
    end

  new ref empty() =>
    values = Array[U64]()

  fun ref copy(other: _BigUInt): None =>
    values.clear()
    var i: USize = 0
    while i < other.values.size() do
      try
        values.push(other.values(i)?)
      else
        // unreachable
        None
      end
      i = i + 1
    end

  fun ref clear(): None =>
    values.clear()

  fun ref string(): String =>
    let result: String iso = recover String() end
    let quot: _BigUInt = _BigUInt.empty()
    let dividend: _BigUInt = _BigUInt.empty()
    dividend.copy(this)
    while true do
      if dividend.gtz() then
        quot.clear()
        let rem: U64 = dividend.divmod_u64(10, quot)
        dividend.copy(quot)
        let cc: U8 = '0' + rem.u8()
        result.push(cc)
      else
        break
      end
    end
    result.reverse_in_place()
    result

  fun ref clz(): U64 =>
    if values.size() > 0 then
      try
        let high: U64 = values(values.size() - 1)?
        high.clz()
      else
        // unreachable
        0
      end
    else
      // undefined
      0
    end

  fun ref add_u64(value: U64): None =>
    _add_rec(0, value)

  fun ref _add_rec(index: USize, value: U64): None =>
    try
      if index < values.size() then
        (let result: U64, let carry: Bool) = values(index)?.addc(value)
        values(index)? = result
        if carry then
          _add_rec(index + 1, 1)
        end
      elseif value != 0 then
        values.push(value)
      else
        // do nothing
        None
      end
    else
      // unreachable
      None
    end

  fun ref sub(other: _BigUInt): None =>
    var index: USize = 0
    while index < other.values.size() do
      try
        let low: U64 = other.values(index)?
        _sub_rec(index, low)
        index = index + 1
      else
        // unreachable
        None
      end
    end
    // trim leading zeros but keep at least one 64-bit word in values array
    while values.size() > 1 do
      try
        let high: U64 = values(values.size() - 1)?
        if high == 0 then
          values.pop()?
        else
          break
        end
      else
        // unreachable
        None
      end
    end

  fun ref _sub_rec(index: USize, value: U64): None =>
    try
      let low = values(index)?
      (let result: U64, let carry: Bool) = low.subc(value)
      values(index)? = result
      if carry then
        _sub_rec(index + 1, 1)
      end
    else
      // unreachable
      None
    end

  fun ref _mul128(lhs: U64, rhs: U64): (U64, U64) =>
    let result: U128 = lhs.u128() * rhs.u128()
    let low: U64 = (result and 0xffff_ffff_ffff_ffff).u64()
    let high: U64 = (result >> 64).u64()
    (high, low)

  fun ref mul_u64(value: U64, scratchpad: _BigUInt): None =>
    scratchpad.clear()
    _mul_rec(0, value, 0, scratchpad)
    copy(scratchpad)

  fun ref _mul_rec(index: USize, value: U64, extra: U64, result: _BigUInt)
    : None
  =>
    try
      result._add_rec(index, extra)
      if index < values.size() then
        let current: U64 = values(index)?
        (let high: U64, let low: U64) = _mul128(current, value)
        result._add_rec(index, low)
        _mul_rec(index + 1, value, high, result)
      else
        // do nothing
        None
      end
    else
      // unreachable
      None
    end

  fun ref divmod_u64(value: U64, result: _BigUInt): U64 =>
    let rem: U128 = _divmod_rec(values.size() - 1, value.u128(), 0, result)
    result.values.reverse_in_place()
    rem.u64()

  fun ref _divmod_rec(index: USize, value: U128, extra: U128, result: _BigUInt)
    : U128
  =>
    try
      let high: U64 = values(index)?
      let dividend: U128 = (extra << 64) or high.u128()
      if dividend >= value then
        (let quot: U128, let rem: U128) = dividend.divmod(value)
        result.values.push(quot.u64())
        if index == 0 then
          rem
        else
          _divmod_rec(index - 1, value, rem, result)
        end
      else
        if result.values.size() > 0 then
          result.values.push(0)
        end
        if index == 0 then
          dividend
        else
          _divmod_rec(index - 1, value, dividend, result)
        end
      end
    else
      // unreachable
      0
    end

  fun ref gtz(): Bool =>
    let size = values.size()
    if size > 1 then
      true
    elseif size == 1 then
      try
        values(size - 1)? > 0
      else
        // unreachable
        false
      end
    else
      false
    end

  fun ref ge(other: _BigUInt): Bool =>
    let size = values.size()
    let other_size = other.values.size()
    if size > other_size then
      true
    elseif size == other_size  then
      try
        values(size - 1)? >= other.values(size - 1)?
      else
        // unreachable
        false
      end
    else
      false
    end