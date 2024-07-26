import std/strutils
import std/tables
import std/sequtils

type
  ElementType = enum
    E_WORD
    E_INTEGER
    E_LIST
  Element = ref object
    case eType: ElementType
    of E_WORD:
      wVal: string
    of E_INTEGER:
      iVal: int
    of E_LIST:
      lVal: seq[Element]

proc `$`(x: Element): string =
  case x.eType:
    of E_WORD:
      "E:" & x.wVal
    of E_INTEGER:
      "E:" & $x.iVal
    of E_LIST:
      "E:[" & x.lVal.mapIt($it).join(" ") & "]"

proc mkWordElement(wVal: string): Element = Element(eType: E_WORD, wVal: wVal)
proc mkIntegerElement(iVal: int): Element = Element(eType: E_INTEGER, iVal: iVal)
proc mkListElement(lVal: seq[Element]): Element = Element(eType: E_LIST, lVal: lVal)
proc isWordElement(x: Element): bool = x.eType == E_WORD
proc isIntegerElement(x: Element): bool = x.eType == E_INTEGER
proc isListElement(x: Element): bool = x.eType == E_LIST
proc isWordElementOf(x: Element, w: string): bool = x.isWordElement and x.wVal == w

proc errorWithReason(x: string): void =
  raise newException(ValueError, x)

proc isWhite(x: char): bool = " \b\r\n\t".contains(x)
proc isDigit(x: char): bool =
  let o = x.ord
  '0'.ord <= o and o <= '9'.ord

proc skipWhite(x: string): string =
  var i = 0
  let lenx = x.len
  while i < lenx and x[i].isWhite:
    i += 1
  x[i..<lenx]

proc parseMulti(x: string): (seq[Element], string)
proc parseSingle(x: string): (Element, string) =
  let x = x.skipWhite
  let lenx = x.len
  if lenx <= 0: return (nil, x)
  if x[0] == '[':
    let z = x[1..<lenx].parseMulti
    let zz = z[1].skipWhite
    let zzlen = zz.len
    if zzlen <= 0: errorWithReason("right bracket required but not found")
    if zz[0] != ']': errorWithReason("right bracket required but not found")
    return (mkListElement(z[0]), zz[1..<zzlen])
  elif x[0] == '&':
    return (mkWordElement("&"), x[1..<lenx])
  elif x[0] == '@':
    return (mkWordElement("@"), x[1..<lenx])
  elif x[0] == '#':
    return (mkWordElement("#"), x[1..<lenx])
  elif x[0] == '~':
    return (mkWordElement("~"), x[1..<lenx])
  elif x[0].isDigit:
    var i = 0
    while i < lenx and x[i].isDigit:
      i += 1
    let ns = x[0..<i]
    let rs = x[i..<lenx]
    return (mkIntegerElement(ns.parseInt), rs)
  elif x[0] == ']':
    errorWithReason("extra right bracket")
  else:
    var i = 0
    while i < lenx and not x[i].isWhite and x[i] != ']':
      i += 1
    let ns = x[0..<i]
    let rs = x[i..<lenx]
    return (mkWordElement(ns), rs)

proc parseMulti(x: string): (seq[Element], string) =
  var res: seq[Element] = @[]
  var z = x
  while z.len > 0 and z[0] != ']':
    z = z.skipWhite
    let e = z.parseSingle
    res.add(e[0])
    z = e[1].skipWhite
  return (res, z)

type
  # the reason why seq is not suitable here is that in the most direct style
  # of metacircular evaluator the structure of env is implicitly a tree, e.g.
  # two closure could've had two different env that are built from the same env.
  Env = ref object
    page: TableRef[string, Value]
    parent: Env
  ValueType = enum
    V_INTEGER
    V_BOOL
    V_CLOSURE
    V_PRIMITIVE
    V_WORD
    V_LIST
  Value = ref object
    case vType: ValueType
    of V_INTEGER:
      iVal: int
    of V_BOOL:
      bVal: bool
    of V_CLOSURE:
      cenv: Env      
      carglist: seq[string]
      cbody: seq[Element]
    of V_PRIMITIVE:
      parity: int
      pbody: proc (x: seq[Element], e: Env): Value
    of V_WORD:
      wVal: string
    of V_LIST:
      lVal: seq[Value]

proc `$`(x: Value): string =
  if x == nil: return "nil"
  case x.vType:
    of V_INTEGER:
      "V:" & $x.iVal
    of V_BOOL:
      "V:" & $x.bVal
    of V_CLOSURE:
      "V:<CLOSURE>"
    of V_PRIMITIVE:
      "V:<PRIMITIVE>"
    of V_WORD:
      "V:" & $x.wVal
    of V_LIST:
      "V:[" & x.lVal.mapIt($it).join(" ") & "]"
      
proc `$`(x: Env): string = $x.page & (if x.parent == nil:
                                        ""
                                      else:
                                        "::" & $x.parent)
      


proc mkEnv(page: TableRef[string, Value], parent: Env): Env = Env(page: page, parent: parent)
proc mkEnv(page: TableRef[string, Value]): Env = Env(page: page, parent: nil)

proc fromEnv(x: string, e: Env): Value =
  var subj = e
  while subj != nil:
    let tp = subj.page
    if tp.hasKey(x): return tp[x]
    subj = subj.parent
  return nil

proc mkIntegerValue(iVal: int): Value = Value(vType: V_INTEGER, iVal: iVal)
proc mkBoolValue(bVal: bool): Value = Value(vType: V_BOOL, bVal: bVal)
proc mkClosureValue(cenv: Env, carglist: seq[string], cbody: seq[Element]): Value = Value(vType: V_CLOSURE, cenv: cenv, carglist: carglist, cbody: cbody)
proc mkPrimitiveValue(parity: int, pbody: proc (x: seq[Element], e: Env): Value): Value = Value(vType: V_PRIMITIVE, parity: parity, pbody: pbody)
proc mkWordValue(wVal: string): Value = Value(vType: V_WORD, wVal: wVal)
proc mkListValue(lVal: seq[Value]): Value = Value(vType: V_LIST, lVal: lVal)

proc quoteAsValue(x: Element): Value =
  case x.eType:
    of E_WORD:
      mkWordValue(x.wVal)
    of E_INTEGER:
      mkIntegerValue(x.iVal)
    of E_LIST:
      mkListValue(x.lVal.mapIt(it.quoteAsValue))
      
proc evalMulti(x: seq[Element], e: Env): Value
proc evalSingle(x: Element, e: Env): Value =
  case x.eType:
    of E_WORD:
      if x.wVal == "true":
        mkBoolValue(true)
      elif x.wVal == "false":
        mkBoolValue(false)
      else:
        x.wVal.fromEnv(e)
    of E_INTEGER:
      mkIntegerValue(x.iVal)
    of E_LIST:
      x.lVal.evalMulti(e)
proc applyClosure(x: Value, arglist: seq[Value], e: Env): Value
proc evalMulti(x: seq[Element], e: Env): Value =
  var lastValue: Value = nil
  var i = 0
  let lenx = x.len
  let latestPage = e.page
  while i < lenx:
    if x[i].isWordElementOf("def"):
      if i + 3 >= lenx: errorWithReason("def form invalid")
      let name = x[i+1]
      let arglist = x[i+2]
      let body = x[i+3]
      if name.eType != E_WORD: errorWithReason("def form invalid: first arg must be word")
      if arglist.eType != E_LIST: errorWithReason("def form invalid: second arg must be list")
      var argnamelist: seq[string] = @[]
      for k in arglist.lVal:
        if k.eType == E_WORD:
          argnamelist.add(k.wVal)
        else:
          errorWithReason("def form invalid: arg list must consist of words only")
      let cbody = if body.eType != E_LIST:
                    @[body]
                  else:
                    body.lVal
      let v = mkClosureValue(e, argnamelist, cbody)
      latestPage[name.wVal] = v
      lastValue = v
      i += 4
      continue

    let current = x[i].evalSingle(e)
    if current == nil: errorWithReason("error while eval")
    case current.vType:
      of V_CLOSURE:
        let arity = current.carglist.len
        if lenx-1 < arity: errorWithReason("arg required")
        let args = x[i+1..<i+1+arity].mapIt(it.evalSingle(e))
        let r = current.applyClosure(args, e)
        lastValue = r
        i = i+1+arity
      of V_PRIMITIVE:
        let arity = current.parity
        lastValue = current.pbody(x[i+1..<i+1+arity], e)
        i = i+1+arity
      else:
        lastValue = current
        i += 1
  return lastValue
proc applyClosure(x: Value, arglist: seq[Value], e: Env): Value =
  assert x.vType == V_CLOSURE
  let newEnvPage = newTable[string, Value]()
  var i = 0
  let argnames = x.carglist
  let arity = argnames.len
  while i < arity:
    let argName = argnames[i]
    let arg = arglist[i]
    newEnvPage[argName] = arg
    i += 1
  return x.cbody.evalMulti(mkEnv(newEnvPage, x.cenv))


proc handleUnquote(x: Element, e: Env): Value =
  case x.eType:
    of E_WORD:
      mkWordValue(x.wVal)
    of E_INTEGER:
      mkIntegerValue(x.iVal)
    of E_LIST:
      var evalNext = false
      var r: seq[Value] = @[]
      for k in x.lVal:
        if evalNext:
          r.add(k.evalSingle(e))
          evalNext = false
        elif k.eType == E_WORD and k.wVal == "~":
          evalNext = true
        else:
          r.add(k.handleUnquote(e))
      mkListValue(r)
  
let rootEnvTable: TableRef[string, Value] = newTable[string, Value]()
rootEnvTable["abc"] = mkIntegerValue(998)
rootEnvTable["+"] = mkPrimitiveValue(2,
                                       proc (x: seq[Element], e: Env): Value =
                                         mkIntegerValue(x[0].evalSingle(e).iVal + x[1].evalSingle(e).iVal))
rootEnvTable["&"] = mkPrimitiveValue(1,
                                     proc (x: seq[Element], e: Env): Value =
                                       x[0].wVal.fromEnv(e))
rootEnvTable["@"] = mkPrimitiveValue(1,
                                     proc (x: seq[Element], e: Env): Value =
                                       x[0].quoteAsValue)
rootEnvTable["#"] = mkPrimitiveValue(
  1,
  proc (x: seq[Element], e: Env): Value =
    x[0].handleUnquote(e))
rootEnvTable["not"] = mkPrimitiveValue(1,
                                       proc (x: seq[Element], e: Env): Value = mkBoolValue(not x[0].evalSingle(e).bVal))
rootEnvTable["and"] = mkPrimitiveValue(2,
                                       proc (x: seq[Element], e: Env): Value = mkBoolValue(x[0].evalSingle(e).bVal and x[1].evalSingle(e).bVal))
rootEnvTable["or"] = mkPrimitiveValue(2,
                                       proc (x: seq[Element], e: Env): Value = mkBoolValue(x[0].evalSingle(e).bVal or x[1].evalSingle(e).bVal))
rootEnvTable["all"] = mkPrimitiveValue(
  1,
  proc (x: seq[Element], e: Env): Value =
    let z = x[0]
    if z.eType != E_LIST: errorWithReason("value error: not a list")
    var r = true
    for k in z.lVal:
      r = r and k.evalSingle(e).bVal
    mkBoolValue(r))
rootEnvTable["any"] = mkPrimitiveValue(
  1,
  proc (x: seq[Element], e: Env): Value =
    let z = x[0]
    if z.eType != E_LIST: errorWithReason("value error: not a list")
    var r = false
    for k in z.lVal:
      r = r or k.evalSingle(e).bVal
    mkBoolValue(r))
rootEnvTable["if"] = mkPrimitiveValue(
  3,
  proc (x: seq[Element], e: Env): Value =
    let c = x[0].evalSingle(e).bVal
    let b = if c: x[1] else: x[2]
    b.evalSingle(e))

let rootEnv = mkEnv(rootEnvTable)
  
when isMainModule:
  # echo "def a [x y] [add x y]".parseMulti[0].evalMulti(rootEnv)
  # echo "3 4 5 6 7 @[add abc abc] def a [x y] [add x y] a 3 4".parseMulti[0].evalMulti(rootEnv)
  # echo "if false 3 [add abc abc]".parseMulti[0].evalMulti(rootEnv)
  echo "3 4 5 + 6 7 [&add abc abc]".parseMulti[0]
  
