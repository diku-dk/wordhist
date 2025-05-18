import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/diku-dk/containers/hashkey"
import "lib/github.com/diku-dk/containers/hashmap"
import "lib/github.com/diku-dk/containers/hashset"
import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/containers/array"

module type slice = {
  type elem
  type slice
  val mk : (i: i64) -> (n: i64) -> slice
  val unmk : slice -> (i64, i64)
  val get [n] : slice -> [n]elem -> ?[k].[k]elem
}

module mk_slice (E: {type elem}) : slice with elem = E.elem = {
  type elem = E.elem
  type slice = {i: i64, n: i64}
  def mk i n = {i, n}
  def unmk {i, n} = (i, n)
  def get {i, n} xs = xs[i:i + n]
}

def arreq [n] [m] 't (eq: t -> t -> bool) (x: [n]t) (y: [m]t) =
  n == m
  && (loop (ok, i) = (true, 0)
      while ok && i < n do
        (ok && (x[i] `eq` y[i]), i + 1)).0

module mk_slice_key
  (S: slice)
  (E: {
    val (==) : S.elem -> S.elem -> bool
    val word : S.elem -> u64
  })
  : hashkey
    with ctx = []S.elem
    with key = S.slice = {
  type i = u64
  type key = S.slice
  type~ ctx = ?[l].[l]S.elem

  def m : i64 = 1

  def eq (xctx: []S.elem) (x: key) (yctx: []S.elem) (y: key) =
    arreq (E.==) (S.get x xctx) (S.get y yctx)

  def hash (ctx: []S.elem) (a: [m]u64) (x: key) : u64 =
    loop v = 0
    for x' in S.get x ctx do
      let x = a[0] * (v ^ E.word x')
      let x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9
      let x = (x ^ (x >> 27)) * 0x94d049bb133111eb
      let y = (x ^ (x >> 31))
      in y
}

module engine = xorshift128plus
module word = mk_slice {type elem = u8}

module wordkey = mk_slice_key word {
  def (==) = (u8.==)
  def word = u64.u8
}

module wordarray = mk_array wordkey engine
module wordmap = mk_hashmap wordkey engine

-- | A unique identifier for a word.
type sym = i32

-- | A unique symbol that represents nothing.
def nosym : sym = -1

type char = u8

def isnt_space (x: char) = x >= 'A' && x <= 'Z' || x >= 'a' && x <= 'z' || x >= '0' && x <= '9'
def is_space (x: char) = !(isnt_space x)

def (&&&) f g x = (f x, g x)

type word = word.slice

def words [n] (s: [n]char) : []word =
  segmented_scan (+) 0 (map is_space s) (map (isnt_space >-> i64.bool) s)
  |> (id &&& rotate 1)
  |> uncurry zip
  |> zip (indices s)
  |> filter (\(i, (x, y)) -> (i == n - 1 && x > 0) || x > y)
  |> map (\(i, (x, _)) -> word.mk (i - x + 1) x)

def exscan [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a) : [n]a =
  scan op ne (map2 (\i a -> if i == 0 then ne else a)
                   (indices as)
                   (rotate (-1) as))

def packwords [n] [k] (s: [n]char) (words: [k]word) : ?[m].([m]char, [k]word) =
  let (_, sizes) = unzip (map word.unmk words)
  let offsets = exscan (+) 0 sizes
  in ( expand (word.unmk >-> (.1)) (\w i -> (word.get w s)[i]) words
     , map2 (\o n -> word.mk o n) offsets sizes
     )

type~ symtab =
  ?[m].{ wmap: wordmap.map [m] sym
       , ws: [m]word
       , heap: ?[n].[n]char
       }

entry symtab_new [n] (s: [n]char) : symtab =
  let ws = words s
  let rng = engine.rng_from_seed [1]
  let (_rng, ws) = wordarray.dedup s rng ws
  let (heap, ws) = packwords s ws
  let [w] (wmap: wordmap.map [w] i32) =
    wordmap.from_array heap (zip ws (map i32.i64 (indices ws)))
  in {wmap, ws = sized w ws, heap}

def symtab_member [n] (st: symtab) (s: [n]char) : bool =
  wordmap.member s (word.mk 0 n) st.wmap

def symtab_lookup [n] (st: symtab) (w: [n]char) : sym =
  match wordmap.lookup w (word.mk 0 n) st.wmap
  case #some x -> x
  case #none -> nosym

entry symtab_sym2word (st: symtab) (x: sym) : ?[k].[k]char =
  word.get st.ws[x] st.heap

entry symtab_extend [n] (st: symtab) (s: [n]char) : symtab =
  let rng = engine.rng_from_seed [1]
  let (s, new_ws) =
    s
    |> words
    |> filter (\w -> wordmap.not_member s w st.wmap)
    |> wordarray.dedup s rng
    |> (.1)
    |> packwords s
  let heap = st.heap ++ s
  let shift w =
    let (i, n) = word.unmk w
    in word.mk (i + length st.heap) n
  let new_ws = map shift new_ws
  let new_syms = map (+ (i32.i64 (wordmap.size st.wmap))) (map i32.i64 (indices new_ws))
  let (old_ws, old_syms) = unzip (wordmap.to_array st.wmap)
  let ws = old_ws ++ new_ws
  let syms = old_syms ++ new_syms
  let kvs = zip ws syms
  let [w] (wmap: wordmap.map [w] i32) = wordmap.from_array heap kvs
  in {wmap, ws = sized w ws, heap}

entry symtab_size (st: symtab) : i64 = wordmap.size st.wmap

entry wordhist [n] (st: symtab) (s: [n]char) : []i32 =
  let syms = map (\w -> i64.i32 (symtab_lookup st (word.get w s))) (words s)
  in hist (+) 0 (length st.ws) syms (map (const 1) syms)
