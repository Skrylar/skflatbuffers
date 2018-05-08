
import macros 

iterator fields*(victim: NimNode): tuple[name, sym, pragmas: NimNode] =
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
        yield (name: name, sym: sym, pragmas: nil)
      of nnkPostfix: # public field, `foo*`
        yield (name: name[1], sym: sym, pragmas: nil)
      of nnkPragmaExpr: # field with pragma, `foo {.bar.}`
        case name[0].kind
          of nnkIdent:
            yield (name: name[0], sym: sym, pragmas: name[1])
          of nnkPostfix:
            yield (name: name[0][1], sym: sym, pragmas: name[1])
          else: break
      else: break
