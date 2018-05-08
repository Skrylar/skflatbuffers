
import streams, skmmutils, macros

static:
  assert cuint.sizeof == int32.sizeof
  assert culong.sizeof == int64.sizeof

type
  WireType* {.pure.} = enum
    Varint = 0
    Sixtyfour
    LengthDelimited
    StartGroup # deprecated
    EndGroup # deprecated
    Thirtytwo

# SECTION
# Zigzag encoding

# NOTE: Has to be done in raw C for some godforsaken reason. Adapted
# from a suggestion by Araq.

proc zigzag* (n: int32): uint32 =
  {.emit: [result, " = (", n, " >> 31) ^ (", n, " << 1);"].}

proc zigzag* (n: int64): uint64 =
  {.emit: [result, " = (", n, " >> 63) ^ (", n, " << 1);"].}

# https://stackoverflow.com/a/48924178/201526
proc unzigzag* (n: uint64): int64 =
  {.emit: [result, " = (", n, " >> 1) ^ -(", n, " & 0x1);"]}

# SECTION
# Estimating the size of base payloads

proc pbPayloadSize* (self: int64): int =
  var accum = cast[uint64](self)
  while accum > 127'u64:
    inc result
    accum = accum shr 7

proc pbPayloadSize* (self: uint64): int =
  var accum = self
  while accum > 0x7F'u64:
    inc result
    accum = accum shr 7

template pbPayloadSize* (self: int): int =
  pbPayloadSize(self.int64)
template pbPayloadSize* (self: int8): int =
  pbPayloadSize(self.int64)
template pbPayloadSize* (self: int16): int =
  pbPayloadSize(self.int64)
template pbPayloadSize* (self: int32): int =
  pbPayloadSize(self.int64)
template pbPayloadSize* (self: char): int =
  pbPayloadSize(self.int64)

template pbPayloadSize* (self: uint): int =
  pbPayloadSize(self.uint64)
template pbPayloadSize* (self: uint8): int =
  pbPayloadSize(self.uint64)
template pbPayloadSize* (self: uint16): int =
  pbPayloadSize(self.uint64)
template pbPayloadSize* (self: uint32): int =
  pbPayloadSize(self.uint64)

template pbFixedPayloadSize*(self: int64): int =
  int64.sizeof
template pbFixedPayloadSize*(self: uint64): int =
  uint64.sizeof
template pbFixedPayloadSize*(self: int32): int =
  int32.sizeof
template pbFixedPayloadSize*(self: uint32): int =
  uint32.sizeof
template pbFixedPayloadSize*(self: int16): int =
  int32.sizeof
template pbFixedPayloadSize*(self: uint16): int =
  uint32.sizeof
template pbFixedPayloadSize*(self: int8): int =
  int32.sizeof
template pbFixedPayloadSize*(self: uint8): int =
  uint32.sizeof
template pbFixedPayloadSize*(self: char): int =
  uint32.sizeof
template pbPayloadSize*(self: float64): int =
  float64.sizeof
template pbPayloadSize*(self: float32): int =
  float32.sizeof

proc pbPayloadSize* (self: string): int =
  if self != nil:
    return pbPayloadSize(self.len) + self.len
  else:
    return pbPayloadSize(0'i64)

proc pbPayloadSize* [T](self: openarray[T]): int =
  # total size of 
  if self.len == 0:
    return pbPayloadSize(0'i64)
  else:
    for peasant in self:
      inc result, pbPayloadSize(peasant)
    inc result, pbPayloadSize(result)

# SECTION
# Protocol buffer serialization of things

proc writePB*[T] (s: Stream; self: openarray[T]) =
  s.write(pbPayloadSize(self))
  for field in self:
    s.writePB(field)

proc writePBVarint* (s: Stream; varint: int64) =
  var accum = cast[uint64](varint)
  while accum > 127'u64:
    s.write((0x80'u8 + (accum and 0x7F).uint8).uint8)
    accum = accum shr 7
  s.write(accum.uint8)

proc writePBVarint* (s: Stream; varint: uint64) =
  var accum = varint
  while accum > 0x7F'u64:
    s.write((0x80'u8 + (accum and 0x7F).uint8).uint8)
    accum = accum shr 7
  s.write(accum.uint8)

template writePBTag* (s: Stream; tag: int64; typ: WireType) =
  s.writePBVarint((tag shl 3) + typ.ord)

proc writePB* (s: Stream; str: string) =
  if str != nil:
    s.writePBVarint(str.len)
    s.write(str)
  else:
    s.writePBVarint(0'i64)

proc writePB* (s: Stream; data: pointer; size: int) =
  if (data != nil) and (size > 0):
    s.writePBVarint(size)
    s.writeData(data, size)
  else:
    s.writePBVarint(0'i64)

template writePBTagged* (s: Stream; tag: int64; value: string) =
  s.writePBTag(tag, WireType.LengthDelimited)
  s.writePB(value)

template writePBTagged* (s: Stream; tag: int64; value: float32) =
  s.writePBTag(tag, WireType.Thirtytwo)
  s.write(value)

template writePBTagged* (s: Stream; tag: int64; value: float64) =
  s.writePBTag(tag, WireType.Sixtyfour)
  s.write(value)

template writePBTagged* (s: Stream; tag: int64; value: int64) =
  s.writePBTag(tag, WireType.Varint)
  s.writePBVarint(value)

template writePBTagged* (s: Stream; tag: int64; value: int8) =
  s.writePBTagged(tag, value.int64)
template writePBTagged* (s: Stream; tag: int64; value: int16) =
  s.writePBTagged(tag, value.int64)
template writePBTagged* (s: Stream; tag: int64; value: int32) =
  s.writePBTagged(tag, value.int64)

template writePBTagged* (s: Stream; tag: int64; value: uint64) =
  s.writePBTag(tag, WireType.Varint)
  s.writePBVarint(value)

template writePBTagged* (s: Stream; tag: int64; value: uint8) =
  s.writePBTagged(tag, value.uint64)
template writePBTagged* (s: Stream; tag: int64; value: uint16) =
  s.writePBTagged(tag, value.uint64)
template writePBTagged* (s: Stream; tag: int64; value: uint32) =
  s.writePBTagged(tag, value.uint64)

template writePBTaggedFixed* (s: Stream; tag: int64; value: int64) =
  s.writePBTag(tag, WireType.Sixtyfour)
  s.write(value)

template writePBTaggedFixed* (s: Stream; tag: int64; value: uint64) =
  s.writePBTag(tag, WireType.Sixtyfour)
  s.write(value)

template writePBTaggedFixed* (s: Stream; tag: int64; value: int32) =
  s.writePBTag(tag, WireType.Thirtytwo)
  s.write(value)

template writePBTaggedFixed* (s: Stream; tag: int64; value: uint32) =
  s.writePBTag(tag, WireType.Thirtytwo)
  s.write(value)

template writePBTaggedFixed* (s: Stream; tag: int64; value: int16) =
  s.writePBTag(tag, WireType.Thirtytwo)
  s.write(value.int32)

template writePBTaggedFixed* (s: Stream; tag: int64; value: uint16) =
  s.writePBTag(tag, WireType.Thirtytwo)
  s.write(value.uint32)

template writePBTaggedFixed* (s: Stream; tag: int64; value: int8) =
  s.writePBTag(tag, WireType.Thirtytwo)
  s.write(value.int32)

template writePBTaggedFixed* (s: Stream; tag: int64; value: uint8) =
  s.writePBTag(tag, WireType.Thirtytwo)
  s.write(value.uint32)

template writePB*(s: Stream; value: int) =
  s.writePBVarint(value)

# SECTION
# Protocol buffer reading of things

proc readPBVarint* (s: Stream): int64 =
  var head = s.readUint8()
  var i = 0
  while head > 127'u8:
    result = result + ((head and 0x7F'u8).int64 shl i)
    head = s.readUint8()
    inc i, 7
  result = result + ((head and 0x7F'u8).int64 shl i)

proc readPBTag* (s: Stream): tuple[tag: int64; typ: WireType] =
  var x = readPBVarint(s)

# SECTION
# Pragmas

## Key number for fields; this is the same as the key in a .proto file.
template pbkey*(key: uint) {.pragma.}

# SECTION
# Automatic code generation

proc get_pbkey(x: NimNode): NimNode =
  # Looks for the pbkey pragma and extracts the key for that field.

  expectKind(x, nnkPragma)
  for p in x:
    if p.kind == nnkExprColonExpr:
      if p[0].kind == nnkSym:
        if $p[0] == "pbkey":
          expectKind(p[1], nnkIntLit)
          return p[1]
  return nil

macro autoProtobufFor* (x: typed): untyped =
  expectKind(x, nnkSym)

  var istream = newIdentNode("stream")
  var sstream = bindSym("Stream")
  var iself = newIdentNode("self")
  var iwrite = newIdentNode("write")
  var iwritepb = newIdentNode("writePB")
  var isizepb = newIdentNode("pbPayloadSize")
  var iwritePBTagged = newIdentNode("writePBTagged")
  var iwritePBTag = newIdentNode("writePBTag")
  var iinc = newIdentNode("inc")
  var iresult = newIdentNode("result")
  var ikey = newIdentNode("key")
  var i64 = newIdentNode("int64")
  var delimtag = newDotExpr(bindSym("WireType"), newIdentNode("LengthDelimited"))

  var writetaggedbody = newStmtList()
  var writebody = newStmtList()
  var sizebody = newStmtList()
  
  writetaggedbody.add newCall(iwritePBTag, istream, ikey, delimtag)
  writetaggedbody.add newCall(iwritepb, istream, newCall(isizepb, iself))
  writetaggedbody.add newCall(iwritepb, istream, iself)

  for field, typ, pragmas in fields(x.symbol.getImpl()):
    # get tag for encoding this field
    let key = get_pbkey(pragmas)

    if key == nil:
      # TODO can we issue a compiler hint/warning that this field cannot be
      # exported as it has no tag?
      continue

    let selfDotField = newDotExpr(iself, field)

    # put the write call in proc
    let call = newCall(iwritePBTagged, istream, newDotExpr(key, i64), selfDotField)
    writebody.add call

    let selfDotField2 = newDotExpr(iself, field)

    let call2 = newCall(iinc, iresult, newCall(isizepb, selfDotField2))
    sizebody.add call2

  var writetaggedproc = newProc(postfix(iwritePBTagged, "*"),
    [newEmptyNode(), newIdentDefs(istream, sstream), newIdentDefs(ikey, i64), newIdentDefs(iself, x)],
    writetaggedbody)

  var writeproc = newProc(postfix(iwritepb, "*"),
    [newEmptyNode(), newIdentDefs(istream, sstream), newIdentDefs(iself, x)],
    writebody)

  var sizeproc = newProc(postfix(isizepb, "*"),
    [newIdentNode("int"), newIdentDefs(iself, x)],
    sizebody)

  result = newStmtList([sizeproc, writeproc, writetaggedproc])
  debugecho repr result

when isMainModule:
  import unittest

  suite "Basic encoding checks":
    test "Positive varint (1)":
      var s = newStringStream()
      s.writePBVarint(1'i64)
      check s.data[0] == 1.char

    test "Positive varint (300)":
      var s = newStringStream()
      s.writePBVarint(300'i64)
      check s.data[0] == 0xAC.char
      check s.data[1] == 0x02.char

    test "Zigzag (64-bit)":
      check zigzag(0'i64) == 0'u64
      check zigzag(-1'i64)  == 1'u64
      check zigzag(1'i64) == 2'u64
      check zigzag(-2'i64)  == 3'u64
      check zigzag(2147483647'i64)  == 4294967294'u64
      check zigzag(-2147483648'i64) == 4294967295'u64

    test "Zigzag (32-bit)":
      check zigzag(0'i32) == 0'u32
      check zigzag(-1'i32)  == 1'u32
      check zigzag(1'i32) == 2'u32
      check zigzag(-2'i32)  == 3'u32

    test "Un-Zigzag (64-bit)":
      check unzigzag(zigzag(0'i64)) == 0'i64
      check unzigzag(zigzag(-1'i64)) == -1'i64
      check unzigzag(zigzag(1'i64)) == 1'i64
      check unzigzag(zigzag(-2'i64)) == -2'i64
      check unzigzag(zigzag(2147483647'i64)) == 2147483647'i64
      check unzigzag(zigzag(-2147483648'i64)) == -2147483648'i64

    test "Un-Zigzag (32-bit)":
      check unzigzag(zigzag(0'i32)) == 0'i32
      check unzigzag(zigzag(-1'i32)) == -1'i32
      check unzigzag(zigzag(1'i32)) == 1'i32
      check unzigzag(zigzag(-2'i32)) == -2'i32

  suite "Basic encode-decode checks":
    test "Varints":
      var s = newStringStream()
      s.writePBVarint(300'i64)
      s.setPosition(0)
      check s.readPBVarint() == 300

  type
    ThingDoer = object
      name* {.pbkey: 1.}: string
      i {.pbkey: 2}, j {.pbkey: 3.}: int32
      w {.pbkey: 4.}: string
      x {.pbkey: 5.}, y* {.pbkey: 10.}: int32

    Bucket = object
      left* {.pbkey: 1.}, right* {.pbkey: 2.}: ThingDoer

  autoProtobufFor(ThingDoer)
  autoProtobufFor(Bucket)

  suite "Automated serialization":
    test "ThingDoer":
      var s = newFileStream("thingdoer.proto.bin", fmWrite)
      var x = ThingDoer()

      x.name = "Bob Dole"
      x.y = 500

      s.writePB(x)

    test "Recursive":
      var s = newFileStream("recursive.proto.bin", fmWrite)
      var x = Bucket()

      x.left.name = "Bob Ross"
      x.right.name = "Bob Dole"

      s.writePB(x)