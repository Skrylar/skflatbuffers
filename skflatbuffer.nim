
type
  uoffset* = uint32           ## offset in to the buffer
  soffset* = int32            ## offset from start of table, to a vtable
  voffset* = int16            ## offset from start of table to value

  SomeOffset* = uoffset | soffset | voffset
  Primitive* = SomeOffset | SomeNumber | char | bool ## for generics

  Vtable* = object ## helper when dealing with schemas at runtiem
    struct_size*: int           ## how big is the structure?
    offsets*: seq[soffset]      ## how many (and what) are the offsets?

template add*(self: var Vtable; T: typed) =
  inc self.struct_size, T.sizeof
  if self.offsets == nil:
    newseq(self.offsets, 0)
  self.offsets.add(0)           # just assume the value isn't set

template add*(self: var Vtable; where: voffset; T: typed) =
  inc self.struct_size, T.sizeof
  if self.offsets == nil:
    newseq(self.offsets, 0)
  self.offsets.add(where)

proc raw_add*[T:Primitive](buffer: var seq[uint8], x: T): uoffset =
  ## Append some primitive nim type to a byte buffer.
  let bmk = buffer.len
  setlen(buffer, buffer.len + T.sizeof)
  var y = cast[ptr T](addr buffer[bmk])
  y[] = x                       # XXX assumes little-endian
  return bmk.uoffset

proc raw_add_string*(buffer: var seq[uint8], str: string): uoffset =
  ## Appends a string to a byte buffer.
  discard buffer.raw_add(buffer.len.uoffset)
  let bmk = buffer.len
  setlen(buffer, buffer.len + str.len + 1)
  movemem(cast[pointer](addr buffer[bmk]), cast[pointer](unsafeaddr str[0]), str.len)
  buffer[bmk+str.len] = 0
  return bmk.uoffset

proc raw_add_inline*[T](buffer: var seq[uint8], largest_member_size_bytes: uint, x: T): uoffset =
  ## Append some object inline to the buffer; used for internal serialization, or when blitting structs.
  let padding = buffer.len %% largest_member_size_bytes.int
  let bmk = buffer.len + padding
  setlen(buffer, bmk + T.sizeof)
  movemem(cast[pointer](addr buffer[bmk]), cast[pointer](unsafeaddr x), T.sizeof)
  return bmk.uoffset

proc raw_add*(buffer: var seq[uint8]; vt: Vtable): uoffset =
  let bmk = buffer.len
  let offset_count = if vt.offsets != nil: vt.offsets.len else: 0
  let vtsize = (voffset.sizeof * (2 + offset_count))

  # XXX we are making sure the cap is preallocated
  setlen(buffer, bmk + vtsize)
  setlen(buffer, bmk)

  discard raw_add(buffer, vtsize.voffset)
  discard raw_add(buffer, vt.struct_size)

  # now we need the space again :/
  setlen(buffer, bmk + vtsize)

  # blit offsets
  movemem(cast[pointer](addr buffer[bmk + (voffset.sizeof * 2)]), cast[pointer](unsafeaddr vt.offsets[0]), voffset.sizeof * vt.offsets.len)

when isMainModule:
  import unittest

  test "Adding offsets doesn't boom":
    var buff: seq[uint8] = @[]
    discard buff.raw_add(32.uoffset)
    discard buff.raw_add(32.soffset)
    discard buff.raw_add(32.voffset)

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

  test "Can create a dynamic vtable":
    var vt = Vtable()
    vt.add(int32)
    vt.add(uint32)
    check vt.struct_size == (uint32.sizeof + int32.sizeof)

    var buff: seq[uint8] = @[]
    discard buff.raw_add(vt)
