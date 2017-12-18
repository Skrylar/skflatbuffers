
import math

type
  uoffset* = uint32           ## offset in to the buffer
  soffset* = int32            ## offset from start of table, to a vtable
  voffset* = int16            ## offset from start of table to value

  Primitive* = SomeNumber | char | bool ## for generics

  Vtable* = object ## helper when dealing with schemas at runtiem
    struct_size*: int           ## how big is the structure?
    offsets*: seq[soffset]      ## how many (and what) are the offsets?

const
  ENotEnoughBytes = "Not enough bytes to read from."
  MaxStringReadSize = 8192 # safety against lazy coders and ram limits

# NOTE: the offset from the start of a struct is actually inverse; the offset is how many bytes BEHIND the struct to find the vtable, so a negative value goes forward

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

template `[]=`*(self: var Vtable; index: int; offset: voffset) =
  self.offsets[index] = offset

proc scrub*(self: var Vtable) =
  ## Sets the offsets to zero, as though the fields still exist but hold no values.
  for i in 0..self.offsets.high:
    self.offsets[i] = 0.soffset

proc raw_add*[T:Primitive](buffer: var seq[uint8], x: T): uoffset =
  ## Append some primitive nim type to a byte buffer.
  let bmk = buffer.len
  setlen(buffer, buffer.len + T.sizeof)
  var y = cast[ptr T](addr buffer[bmk])
  y[] = x                       # XXX assumes little-endian
  return bmk.uoffset

proc raw_add*(buffer: var seq[uint8], str: string): uoffset =
  ## Appends a string to a byte buffer, including zero terminal.
  let bmk = buffer.len
  setlen(buffer, buffer.len + str.len + 1)
  movemem(cast[pointer](addr buffer[bmk]), cast[pointer](unsafeaddr str[0]), str.len)
  buffer[bmk+str.len] = 0
  return bmk.uoffset

proc add*(buffer: var seq[uint8]; str: string): uoffset =
  ## Adds a string to a flat buffer, including the length prefix.
  result = raw_add(buffer, str.len.uoffset)
  discard raw_add(buffer, str)

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

proc raw_read*[T:Primitive](buffer: seq[uint8]; offset: int): T =
  # XXX no idea if -d:release turns off bounds checks; in the event that it might, we need to make sure to intentionally bounds check as we are in the danger zone here
  if offset < 0 or (offset + T.sizeof) > buffer.high:
    raise new_exception(IndexError, ENotEnoughBytes)
  movemem(cast[pointer](unsafeaddr result), cast[pointer](unsafeaddr buffer[offset]), T.sizeof)

proc read_string*(buffer: seq[uint8]; offset: int; max_size: int = MaxStringReadSize): string =
  # read string length
  var strlen = raw_read[uoffset](buffer, offset).int
  if (offset + uoffset.sizeof + strlen) > buffer.len:
    raise new_exception(IndexError, ENotEnoughBytes)

  # apply the safety
  strlen = min(strlen.int, max_size.int)

  result = newstring(strlen)
  movemem(cast[pointer](unsafeaddr result[0]), cast[pointer](unsafeaddr buffer[offset + uoffset.sizeof]), strlen)

proc vtcount*(buffer: seq[uint8]; offset: int): int =
  ## Calculates the number of offsets in the vtable at offset.
  if offset + (voffset.sizeof * 2) > buffer.len:
    raise new_exception(IndexError, ENotEnoughBytes)

  var vtable_size = raw_read[voffset](buffer, offset)
  result = ((vtable_size /% voffset.sizeof).int) - 2

when isMainModule:
  import unittest, streams

  #var junitfile = newfilestream("skflatbuffer.xml", fmwrite)
  #var junit = newJUnitOutputFormatter(junitfile)
  #addoutputformatter(junit)

  suite "Basic crash tests":
    test "Adding offsets doesn't boom":
        var buff: seq[uint8] = @[]
        discard buff.raw_add(32.uoffset)
        discard buff.raw_add(32.soffset)
        discard buff.raw_add(32.voffset)

    test "Adding strings doesn't boom":
        var buff: seq[uint8] = @[]
        discard buff.raw_add("exploded kittens")

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

    suite "Reading":
      test "Strings":
        var buff: seq[uint8] = @[]
        discard buff.add("pine cones")

        checkpoint "written string"

        check raw_read[uoffset](buff, 0) == 10
        check read_string(buff, 0) == "pine cones"

      test "VTable Element Counts":
        var vt = Vtable()
        vt.add(int32)
        vt.add(uint32)

        check vt.offsets.len == 2

        checkpoint "created virtual table"

        var buff: seq[uint8] = @[]
        discard buff.raw_add(vt)

        checkpoint "serialized vtable"

        check vtcount(buff, 0) == 2

  #junit.close()
  #junitfile.flush()
  #junitfile.close()
