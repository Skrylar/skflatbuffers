
import math, macros, typetraits, streams

type
  uoffset* = uint32           ## offset in to the buffer
  soffset* = int32            ## offset from start of table, to a vtable
  voffset* = int16            ## offset from start of table to value

  Primitive* = SomeNumber | char | bool ## for generics

  Vtable* = object ## helper when dealing with schemas at runtiem
    struct_size*: int           ## how big is the structure?
    embed_size*: int
    alignment_size*: int        ## largest size of inlined thing
    offsets*: seq[voffset]      ## how many (and what) are the offsets?

# NOTE: the offset from the start of a struct is actually inverse; the offset is how many bytes BEHIND the struct to find the vtable, so a negative value goes forward

# SECTION
# Creating virtual tables

proc flatbuffer_embedded_sizeof_str*(typ: string): int =
  ## Checks the size of this datatype when inlined in to the flat buffer. A
  ## size of zero means the type cannot be inlined. Due to bugs with generics
  ## in 0.18, this is a hard coded table of known-good types.
  case typ
  of "int8": return int8.sizeof
  of "int16": return int16.sizeof
  of "int32": return int32.sizeof
  of "int64": return int64.sizeof
  of "uint8": return uint8.sizeof
  of "uint16": return uint16.sizeof
  of "uint32": return uint32.sizeof
  of "uint64": return uint64.sizeof
  of "byte": return byte.sizeof
  of "char": return char.sizeof
  of "float32": return float32.sizeof
  of "float64": return float64.sizeof
  else: return 0

macro flatbuffer_embedded_sizeof*(typ: typed): int =
  ## Checks the size of this datatype when inlined in to the flat buffer. A
  ## size of zero means the type cannot be inlined. Due to bugs with generics
  ## in 0.18, this is a hard coded table of known-good types.
  return flatbuffer_embedded_sizeof_str($typ)

template add*(self: var Vtable; typ: typed) =
  ## Convenience template for adding a type to a virtual table.
  add(self, flatbuffer_embedded_sizeof(typ))

proc add*(self: var Vtable; embed_size: int) =
  ## Less convenient procedure for adding to a virtual table, including size of embedded value.

  inc self.struct_size, soffset.sizeof
  
  # technically something that was not embedded is instead pointed to, so the pointer itself is embedded.
  if embed_size == 0:
    inc self.embed_size, uint16.sizeof
  else:
    inc self.embed_size, embed_size

  self.alignment_size = max(self.alignment_size, embed_size)
  if self.offsets == nil:
    newseq(self.offsets, 0)
  self.offsets.add(0)

template `[]=`*(self: Vtable; index: int; offset: voffset) =
  ## Easy way to set the offset of a written value.
  self.offsets[index] = offset

proc scrub*(self: var Vtable) =
  ## Sets the offsets to zero, as though the fields still exist but hold no values.
  for i in 0..self.offsets.high:
    self.offsets[i] = 0.voffset

macro vtable_for*(something: typed): VTable =
  var rs = newIdentNode("result")
  var add = newIdentNode("add")
  var rdot = newDotExpr(rs, add)
  var vt = bindSym("VTable")
  var soz = bindSym("flatbuffer_embedded_sizeof")
  var holder = newAssignment(rs, newCall(vt))

  var list = newStmtList()
  list.add(holder)

  var tdef = something.symbol.getImpl()

  for field in tdef[2][2]:
    list.add(newCall(rdot, newCall(soz, field[1])))

  var fn = newProc(newEmptyNode(), [vt], list, nnkLambda)
  result = newCall(fn)

# SECTION
# Writing datums

proc writeFB* (s: Stream; str: string) =
  s.write(str.len.uint32)
  s.write(str)
  s.write(0.uint8) # implicit null terminator; required by spec

proc writeFB* (s: Stream; b: pointer; bs: int) =
  s.write(bs.uint32)
  s.writeData(b, bs)
  s.write(0.uint8) # implicit null terminator; required by spec

# SECTION
# Reading datums

proc readFBString* (s: Stream): string =
  ## NB: This procedure does not involve a read limit; specially crafted
  ## inputs could request all available memory be allocated.
  
  var slen = s.readUint32().int
  result = s.readStr(slen).string
  discard s.readUint8() # implicit null terminator; required by spec

proc readFBData* (s: Stream; b: pointer; bs: int): int =
  ## NB: You may want to use `peekUint32` to determine the size of the data
  ## actually on the wire. XXX does not yet handle attempts to read more or
  ## less than is available intelligently.

  assert bs.uint32 == s.readUint32() # we don't care about the field size here
  result = s.readData(b, bs)
  discard s.readUint8() # implicit null terminator; required by spec

# SECTION
# Writing virtual tables

proc writeFB* (s: Stream; table: ref VTable) =
  ## Writes a virtual table to an output stream.

  s.write(((table.offsets.len + 2) * uint16.sizeof).uint16) # size of vtable, including size field and embed field
  s.write((table.embed_size + soffset.sizeof).uint16) # size of embedded data, including vtable offset
  for field in table.offsets:
    s.write(field.uint16)

proc writeEmbeddedFB* (stream: Stream; value: int8) =
  ## Boilerplate for writing a int8 during flat buffer serialization.
  stream.write(value)
proc writeEmbeddedFB* (stream: Stream; value: uint8) =
  ## Boilerplate for writing a uint8 during flat buffer serialization.
  stream.write(value)
proc writeEmbeddedFB* (stream: Stream; value: int16) =
  ## Boilerplate for writing a int16 during flat buffer serialization.
  stream.write(value)
proc writeEmbeddedFB* (stream: Stream; value: uint16) =
  ## Boilerplate for writing a uint16 during flat buffer serialization.
  stream.write(value)
proc writeEmbeddedFB* (stream: Stream; value: int32) =
  ## Boilerplate for writing a int32 during flat buffer serialization.
  stream.write(value)
proc writeEmbeddedFB* (stream: Stream; value: uint32) =
  ## Boilerplate for writing a uint32 during flat buffer serialization.
  stream.write(value)
proc writeEmbeddedFB* (stream: Stream; value: int64) =
  ## Boilerplate for writing a int64 during flat buffer serialization.
  stream.write(value)
proc writeEmbeddedFB* (stream: Stream; value: uint64) =
  ## Boilerplate for writing a uint64 during flat buffer serialization.
  stream.write(value)
proc writeEmbeddedFB* (stream: Stream; value: float32) =
  ## Boilerplate for writing a float32 during flat buffer serialization.
  stream.write(value)
proc writeEmbeddedFB* (stream: Stream; value: float64) =
  ## Boilerplate for writing a float64 during flat buffer serialization.
  stream.write(value)

# SECTION
# Reading virtual tables

proc readFB_VTable* (s: Stream; table: ref VTable) =
  # Reads a virtual table from the stream, matching metadata against the
  # supplied table. You must provide a virtual table which already knows how
  # many fields should be read. This procedure takes care of checking expected
  # field count against known field count, possibly throwing an error. It also
  # takes care of loading field offsets.

  # gather size of the table to read, and embedded data length
  var size = s.readUint16().int # TODO maybe bother someone if these don't match expectation
  discard s.readUint16() # TODO maybe bother someone if these don't match expectation
  # infer number of fields by offset count
  var fieldCount = (size - (uint16.sizeof * 2)) /% uint16.sizeof
  # now read offsets
  for i in 0..<min(fieldCount, table.offsets.len):
    table.offsets[i] = s.readUint16().voffset

# SECTION
# Automatic serialization

iterator fields(victim: NimNode): tuple[name, sym: NimNode] =
  ## Iterates fields of a NimNode, which represent a type definition.

  # sanity tests
  expectKind(victim, nnkTypeDef)
  expectKind(victim[2], nnkObjectTy)
  expectKind(victim[2][2], nnkRecList)

  for i in 0..<victim[2][2].len:
    let here = victim[2][2][i]
    expectKind(here, nnkIdentDefs)

    let sym = here[here.len-2]

    for name in here:
      case name.kind
      of nnkIdent: # normal field
        yield (name: name, sym: sym)
      of nnkPostfix: # public field, `foo*`
        yield (name: name[1], sym: sym)
      of nnkPragmaExpr: # field with pragma, `foo {.bar.}`
        case name[0].kind
          of nnkIdent:
            yield (name: name, sym: sym)
          of nnkPostfix:
            yield (name: name[0][1], sym: sym)
          else: break
      else: break

macro autoFlatbuffersFor* (x: typed): untyped =
  expectKind(x, nnkSym)

  var istream = newIdentNode("stream")
  var sstream = bindSym("Stream")
  var swrite = newIdentNode("write")
  var swritefb = newIdentNode("writeFB")
  var swritefbembed = newIdentNode("writeEmbeddedFB")
  var sgetPosition = newIdentNode("getPosition")
  var iself = newIdentNode("self")
  var sint16 = newIdentNode("int16")
  var sint32 = newIdentNode("int32")

  var writebody = newStmtList()

  var marks = newStmtList()
  var minus = newIdentNode("-")

  var vtable_size = int16.sizeof * 2
  var table_size = int32.sizeof
  for name, sym in fields(x.symbol.getImpl()):
    let embed_size = flatbuffer_embedded_sizeof_str($sym)
    inc vtable_size, int16.sizeof
    if embed_size > 0:
      inc table_size, embed_size
    else:
      inc table_size, int16.sizeof

  # iterate fields and write everything we cannot embed
  for name, sym in fields(x.symbol.getImpl()):
    let embed_size = flatbuffer_embedded_sizeof_str($sym)
    if embed_size == 0:
      let marker = newLetStmt(gensym(), newCall(sgetPosition, istream))
      marks.add(marker)
      writebody.add(marker)
      writebody.add(newCall(swritefb, istream, newDotExpr(iself, name)))

  # write the pointer to our vtable 
  let table_marker = gensym()
  writebody.add(newLetStmt(table_marker, newCall(sgetPosition, istream)))
  writebody.add(newCall(swrite, istream, newDotExpr(newLit(-table_size), sint32)))

  # iterate fields and write references to unembedded data, and any data which
  # has been embedded
  var y = 0
  for name, sym in fields(x.symbol.getImpl()):
    let embed_size = flatbuffer_embedded_sizeof_str($sym)
    if embed_size > 0:
      writebody.add(newCall(swritefbembed, istream, newDotExpr(iself, name)))
    else:
      writebody.add(newCall(swrite, istream, newDotExpr(newCall(minus, marks[y][0][0], table_marker), sint16)))
      inc y

  # now we must manufacture the vtable for this object
  writebody.add(newCall(swrite, istream, newDotExpr(newLit(vtable_size), sint16)))
  writebody.add(newCall(swrite, istream, newDotExpr(newLit(table_size), sint16)))
 
  y = soffset.sizeof # skip the pointer to vtable
  for name, sym in fields(x.symbol.getImpl()):
    let embed_size = flatbuffer_embedded_sizeof_str($sym)
    writebody.add(newCall(swrite, istream, newDotExpr(newLit(y), sint16)))
    if embed_size > 0:
      inc y, embed_size
    else:
      inc y, int16.sizeof

  var writeproc = newProc(swritefb,
    [newEmptyNode(), newIdentDefs(istream, sstream), newIdentDefs(iself, x)],
    writebody)

  return writeproc

when isMainModule:
  import unittest, streams

  type
    ThingDoer = object
      name*: string
      i, j: int32
      w: string
      x, y*: int32

  autoFlatbuffersFor(ThingDoer)

  #var junitfile = newfilestream("skflatbuffer.xml", fmwrite)
  #var junit = newJUnitOutputFormatter(junitfile)
  #addoutputformatter(junit)

  suite "Automatic serialization":
    test "ThingDoer":
      var s = newFileStream("thingdoer.bin", fmWrite)

      var x = ThingDoer()
      x.name = "Test Object"
      x.w = "<(^_^<)"
      x.y = 500'i32

      s.writeFB(x)

  #junit.close()
  #junitfile.flush()
  #junitfile.close()
