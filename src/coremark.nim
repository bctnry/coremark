import std/strutils
import std/tables
import std/sequtils

type
  WordType* = enum
    E_WORD
    E_QUOTE
  Word* = ref object
    case eType*: WordType
    of E_WORD:
      wVal*: string
    of E_QUOTE:
      qVal*: seq[Word]

proc `$`*(x: Word): string =
  case x.eType:
    of E_WORD:
      x.wVal
    of E_QUOTE:
      "[ " & x.qVal.mapIt($it).join(" ") & " ]"

proc isUpperCaseAlpha(x: char): bool {.inline.} = 'A' <= x and x <= 'Z'
proc mkWordWord(wVal: string): Word = Word(eType: E_WORD, wVal: wVal)
proc mkQuoteWord(qVal: seq[Word]): Word = Word(eType: E_QUOTE, qVal: qVal)
proc isWordWord(x: Word): bool = x.eType == E_WORD
proc isQuoteWord(x: Word): bool = x.eType == E_QUOTE
proc isWordWordOf(x: Word, w: string): bool = x.isWordWord and x.wVal == w
proc isValueCall(x: Word): bool = x.eType == E_WORD and x.wVal.startsWith("$")
proc errorWithReason(x: string): void =
  raise newException(ValueError, x)
proc wordWordToString(x: Word): string =
  if not x.isWordWord: errorWithReason("Wrong value - wordWordToString with non-word word")
  return x.wVal

type
  Macro = (seq[string], seq[Word])
  Subst = TableRef[string, Word]

proc populate(x: seq[Word], args: seq[string], subst: Subst): seq[Word] =
  var res: seq[Word] = @[]
  for k in x:
    if k.isWordWord and k.wVal in args:
      if subst.hasKey(k.wVal): res.add(subst[k.wVal])
      else: res.add(k)
    elif k.isQuoteWord:
      res.add(mkQuoteWord(k.qVal.populate(args, subst)))
    else:
      res.add(k)
  return res

var
  globalActive: seq[Word] = @[]
  globalNeutral: seq[Word] = @[]
  dict: TableRef[string, Macro] = newTable[string, Macro]()

type
  PrimitiveImplementation* = proc (head: Word, args: seq[Word]): seq[Word]
  
var
  primitiveList = newTable[string, (int, PrimitiveImplementation)]()

proc isPrimitive(x: Word): bool =
  return x.isWordWord and primitiveList.hasKey(x.wVal)
  
proc getArityRequirement(x: Word): int =
  if not x.isWordWord: errorWithReason("Wrong value - getArityRequirement on non-word word")
  if x.isPrimitive: return primitiveList[x.wVal][0]
  if not dict.hasKey(x.wVal): errorWithReason("Cannot find definition for " & x.wVal)
  return dict[x.wVal][0].len

proc getValue(x: Word): seq[Word] =
  if not x.isWordWord: errorWithReason("Wrong value - getValue on non-word word")
  let realName = x.wVal.substr(1)
  if not dict.hasKey(realName): errorWithReason("Cannot find definition for " & x.wVal)
  return dict[realName][1]

primitiveList["def"] = (
  3, proc (head: Word, args: seq[Word]): seq[Word] {.closure.} =
         let argword = args[^2]
         if not argword.isQuoteWord: errorWithReason("Wrong value - arg list isn't a list")
         let arglst = argword.qVal.mapIt(it.wordWordtoString)
         let body = if args[^3].isWordWord: @[args[^3]] else: args[^3].qVal
         dict[args[^1].wordWordToString] = (arglst, body)
         return @[]
)
primitiveList["if"] = (
  3, proc (head: Word, args: seq[Word]): seq[Word] {.closure.} =
         if args[^1].isWordWordOf("true"): return args[^2].qVal
         else: return args[^3].qVal
)
primitiveList["<="] = (
  2, proc (head: Word, args: seq[Word]): seq[Word] {.closure.} =
         return @[(if args[^1].wVal.parseInt <= args[^2].wVal.parseInt:
                     mkWordWord("true")
                   else:
                     mkWordWord("false"))]
)
primitiveList["nth"] = (
  2, proc (head: Word, args: seq[Word]): seq[Word] {.closure.} =
         return @[args[^2].qVal[args[^1].wVal.parseInt]]
)
primitiveList["+"] = (
  2, proc (head: Word, args: seq[Word]): seq[Word] {.closure.} =
         return @[mkWordWord($(args[^1].wVal.parseInt + args[^2].wVal.parseInt))]
)
primitiveList["-"] = (
  2, proc (head: Word, args: seq[Word]): seq[Word] {.closure.} =
         return @[mkWordWord($(args[^1].wVal.parseInt - args[^2].wVal.parseInt))]
)
primitiveList["*"] = (
  2, proc (head: Word, args: seq[Word]): seq[Word] {.closure.} =
         return @[mkWordWord($(args[^1].wVal.parseInt * args[^2].wVal.parseInt))]
)

proc call(head: Word, args: seq[Word]): seq[Word] =
  if head.isPrimitive: return primitiveList[head.wVal][1](head, args)
  else:
    var subst = newTable[string, Word]()
    let z = head.wordWordToString
    let m = dict[z]
    for i in 0..<m[0].len:
      let k = m[0][i]
      subst[k] = args[^(i+1)]
    return m[1].populate(m[0], subst)

proc run(): void =
  var currentArity: seq[(int, int)] = @[]
  while (currentArity.len > 0) or globalActive.len > 0:
    if currentArity.len > 0 and currentArity[^1][0] >= currentArity[^1][1]:
      var args: seq[Word] = @[]
      if globalNeutral.len < currentArity[^1][0]: errorWithReason("Wrong value - not enough arguments")
      for _ in 0..<currentArity[^1][0]: args.add(globalNeutral.pop())
      var head = globalNeutral.pop()
      discard currentArity.pop()
      var res = call(head, args)
      while res.len > 0: globalActive.add(res.pop())
      continue
    if globalActive.len <= 0: break
    let word = globalActive.pop()
    if word.isValueCall:
      let wordValue = getValue(word)
      globalNeutral.add(mkQuoteWord(wordValue))
      if currentArity.len > 0: currentArity[^1][0] += 1
    elif word.isQuoteWord:
      globalNeutral.add(word)
      if currentArity.len > 0: currentArity[^1][0] += 1
    elif word.isWordWord and (dict.hasKey(word.wordWordToString) or word.isPrimitive):
      currentArity.add((0, getArityRequirement(word)))
      globalNeutral.add(word)
    else:
      globalNeutral.add(word)
      if currentArity.len > 0: currentArity[^1][0] += 1

proc isWhite(x: char): bool = x in " \t\r\n"
proc isBrackets(x: char): bool = x in "[]|"
      
proc parse*(x: string): seq[Word] =
  var res: seq[Word] = @[]
  var i = 0
  var stk: seq[seq[Word]] = @[]
  var currentRun: seq[Word] = @[]
  var cnt: int = 0
  while i < x.len:
    if x[i] == '[':
      stk.add(currentRun)
      currentRun = @[]
      cnt += 1
      i += 1
    elif x[i] == ']':
      if cnt > 1:
        let z = mkQuoteWord(currentRun)
        currentRun = stk.pop()
        currentRun.add(z)
        cnt -= 1
      elif cnt == 1:
        let z = mkQuoteWord(currentRun)
        currentRun = @[]
        res.add(z)
        cnt -= 1
      i += 1
    elif x[i].isWhite:
      i += 1
    elif x[i] == ';':
      while i < x.len and x[i] != '\n': i += 1
      if i < x.len: i += 1
    else:
      var ii = i
      while ii < x.len and (not x[ii].isWhite) and (not x[ii].isBrackets): ii += 1
      let word = mkWordWord(x.substr(i, ii-1))
      if cnt > 0: currentRun.add(word)
      else: res.add(word)
      i = ii
  return res
  
when isMainModule:

  var x = parse("""
def hanoi [n from via to] [
  if <= n 0 [] [
    if <= n 1 [[from to]] [
      hanoi - n 1 from to via
      [from to]
      hanoi - n 1 via from to
    ]
  ]
]
hanoi 3 A B C
""")

  while x.len > 0:
    globalActive.add(x.pop())
  run()
  echo globalNeutral.mapIt($it).join(" ")


