
import math

type
  uoffset* = uint32           ## offset in to the buffer
  soffset* = int32            ## offset from start of table, to a vtable
  voffset* = int16            ## offset from start of table to value

  Primitive* = SomeNumber | char | bool ## for generics

  Vtable* = object ## helper when dealing with schemas at runtiem
    struct_size*: int           ## how big is the structure?
    offsets*: seq[voffset]      ## how many (and what) are the offsets?

const
  ENotEnoughBytes = "Not enough bytes to read from."
  ENotInVtable = "VTable does not contain this entry."
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

template `[]=`*(self: Vtable; index: int; offset: voffset) =
  self.offsets[index] = offset

proc scrub*(self: var Vtable) =
  ## Sets the offsets to zero, as though the fields still exist but hold no values.
  for i in 0..self.offsets.high:
    self.offsets[i] = 0.voffset

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

  result = raw_add[voffset](buffer, vtsize.voffset).uoffset
  discard raw_add[voffset](buffer, (vt.struct_size + soffset.sizeof).voffset)

  # now we need the space again :/
  setlen(buffer, bmk + vtsize)

  # blit offsets
  movemem(cast[pointer](addr buffer[bmk + (voffset.sizeof * 2)]), cast[pointer](unsafeaddr vt.offsets[0]), voffset.sizeof * offset_count)

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

proc resolve*(buffer: seq[uint8]; offset, element: int): int =
  ## Assuming a table is at 'offset', follow the vtable pointer.  Try to find the numbered element (starting from zero) in the vtable.  Returns the buffer location the desired value can be read.
  var vloc = offset - raw_read[soffset](buffer, offset) # yes, this offset is subtracted; read the spec
  var count = vtcount(buffer, vloc)
  if element < 0 or element >= count:
    raise new_exception(IndexError, ENotInVtable)
  let soff = raw_read[voffset](buffer, vloc + (voffset.sizeof * (2 + element)))
  result = offset + soff

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

      test "Resolving Element Positions":
        var vt = Vtable()
        vt.add(int32)
        vt.add(int32)

        # its possible to calculate this in advance, although the
        # values CAN be literally anywhere.  there is no requirement
        # for the layout of your tables make any sense whatsoever.
        vt[0] = soffset.sizeof.voffset
        vt[1] = (soffset.sizeof + int32.sizeof).voffset

        var buff: seq[uint8] = @[]
        discard buff.raw_add(vt)

        check buff.len == 8

        checkpoint "wrote vtable"

        let tpos = raw_add(buff, buff.len.soffset) # write offset to vtable
        let b1 = raw_add(buff, 42.int32)   # and both values
        let b2 = raw_add(buff, 24.int32)

        check b1 == 12
        check b2 == 16

        check buff.len == 20

        check resolve(buff, tpos.int, 0) == b1.int
        check resolve(buff, tpos.int, 1) == b2.int

        expect(IndexError):
          discard resolve(buff, tpos.int, 2)

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
