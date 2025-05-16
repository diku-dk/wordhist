fun word8ArrayToString (arr: Word8Array.array) : string =
  let
    val len = Word8Array.length arr
    fun build i =
      if i = len then []
      else Char.chr (Word8.toInt (Word8Array.sub (arr, i))) :: build (i + 1)
  in
    String.implode (build 0)
  end


fun word8VectorToArray (v: Word8Vector.vector) : Word8Array.array =
  let
    val len = Word8Vector.length v
    val arr = Word8Array.array (len, Word8.fromInt 0)
    fun copy i =
      if i < len then
        (Word8Array.update (arr, i, Word8Vector.sub (v, i)); copy (i + 1))
      else
        ()
  in
    copy 0;
    arr
  end

fun readFileAsWord8Array (fname: string) : Word8Array.array =
  word8VectorToArray (BinIO.inputAll (BinIO.openIn fname))

fun main () =
  let
    val cfg =
      (Wordhist.Config.cache (SOME "wordhist.cache")) Wordhist.Config.default
    val ctx = Wordhist.Context.new cfg
    val fname = List.nth (CommandLine.arguments (), 0)
    val str = readFileAsWord8Array fname
    val str_arr =
      Wordhist.Word8Array1.new ctx (Word8ArraySlice.full str)
        (Word8Array.length str)
    val st = Wordhist.Entry.symtab_new ctx str_arr
    val h = Wordhist.Entry.wordhist ctx (st, str_arr)
    fun printSym (sym, k) =
      print
        (word8ArrayToString (Wordhist.Word8Array1.values
           (Wordhist.Entry.symtab_sym2word ctx (st, Int32.fromInt sym))) ^ ": "
         ^ Int32.toString k ^ "\n")
  in
    Int32Array.appi printSym (Wordhist.Int32Array1.values h);
    Wordhist.Context.free ctx
  end

val () = main ()
