
type
  uoffset* = uint32           # offset in to the buffer
  soffset* = int32            # offset from start of table, to a vtable
  voffset* = int16            # offset from start of table to value
  Primitive* = SomeNumber | char | bool

proc raw_add_offset*(buffer: var seq[uint8], x: uoffset): uoffset =
  ## Appends an offset to a byte buffer.
  let bmk = buffer.len
  setlen(buffer, buffer.len + uoffset.sizeof)
  var y = cast[ptr uoffset](addr buffer[bmk])
  y[] = x                       # XXX assumes little-endian
  return bmk.uoffset

proc raw_add_string*(buffer: var seq[uint8], str: string): uoffset =
  ## Appends a string to a byte buffer.
  discard buffer.raw_add_offset(buffer.len.uoffset)
  let bmk = buffer.len
  setlen(buffer, buffer.len + str.len + 1)
  movemem(cast[pointer](addr buffer[bmk]), cast[pointer](unsafeaddr str[0]), str.len)
  buffer[bmk+str.len] = 0
  return bmk.uoffset

proc raw_add*[T:Primitive](buffer: var seq[uint8], x: T): uoffset =
  ## Append some primitive nim type to a byte buffer.
  let bmk = buffer.len
  setlen(buffer, buffer.len + T.sizeof)
  var y = cast[ptr T](addr buffer[bmk])
  y[] = x                       # XXX assumes little-endian
  return bmk.uoffset

proc raw_add_inline*[T](buffer: var seq[uint8], largest_member_size_bytes: uint, x: T): uoffset =
  ## Append some object inline to the buffer; used for internal serialization, or when blitting structs.
  let padding = buffer.len %% largest_member_size_bytes.int
  let bmk = buffer.len + padding
  setlen(buffer, bmk + T.sizeof)
  movemem(cast[pointer](addr buffer[bmk]), cast[pointer](unsafeaddr x), T.sizeof)
  return bmk.uoffset

when isMainModule:
  import unittest

  test "Adding offsets doesn't boom":
    var buff: seq[uint8] = @[]
    discard buff.raw_add_offset(32)

  test "Adding strings doesn't boom":
    var buff: seq[uint8] = @[]
    discard buff.raw_add_string("exploded kittens")

  test "Adding bytes doesn't boom":
    var buff: seq[uint8] = @[]
    discard buff.raw_add(4'u8)
    check buff.len == 1

  test "Adding tuples doesn't boom":
    var buff: seq[uint8] = @[]
    discard buff.raw_add_inline(int.sizeof.uint, (foo: 3, bar: 6))
    check buff.len == (int.sizeof * 2)
