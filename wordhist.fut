import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/diku-dk/containers/key"
import "lib/github.com/diku-dk/containers/hashmap"
import "lib/github.com/diku-dk/containers/hashset"
import "lib/github.com/diku-dk/containers/slice"
import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/containers/array"

module engine = xorshift128plus

module wordkey = mk_slice_key u8key

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

type word = slice.slice u8

def words [n] (s: [n]char) : []word =
  segmented_scan (+) 0 (map is_space s) (map (isnt_space >-> i64.bool) s)
  |> (id &&& rotate 1)
  |> uncurry zip
  |> zip (indices s)
  |> filter (\(i, (x, y)) -> (i == n - 1 && x > 0) || x > y)
  |> map (\(i, (x, _)) -> slice.mk (i - x + 1) x)

def exscan [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a) : [n]a =
  scan op ne (map2 (\i a -> if i == 0 then ne else a)
                   (indices as)
                   (rotate (-1) as))

def packwords [n] [k] (s: [n]char) (words: [k]word) : ?[m].([m]char, [k]word) =
  let (_, sizes) = unzip (map slice.unmk words)
  let offsets = exscan (+) 0 sizes
  in ( expand (slice.unmk >-> (.1)) (\w i -> (slice.get w s)[i]) words
     , map2 (\o n -> slice.mk o n) offsets sizes
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
  wordmap.member s (slice.mk 0 n) st.wmap

def symtab_lookup [n] (st: symtab) (w: [n]char) : sym =
  match wordmap.lookup w (slice.mk 0 n) st.wmap
  case #some x -> x
  case #none -> nosym

entry symtab_sym2word (st: symtab) (x: sym) : ?[k].[k]char =
  slice.get st.ws[x] st.heap

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
    let (i, n) = slice.unmk w
    in slice.mk (i + length st.heap) n
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
  let syms = map (\w -> i64.i32 (symtab_lookup st (slice.get w s))) (words s)
  in hist (+) 0 (length st.ws) syms (map (const 1) syms)
