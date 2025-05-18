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

fun err s = TextIO.output (TextIO.stdErr, s)

fun withContext f =
  let
    val cacheFileName = "wordhist.cache"
    val cfg = Wordhist.Config.cache (SOME cacheFileName) Wordhist.Config.default
    val ctx = Wordhist.Context.new cfg
  in
    (f ctx before Wordhist.Context.free ctx)
    handle e => (Wordhist.Context.free ctx; raise e)
  end

fun readFileAsWord8Array1 ctx fname =
  let
    val str = readFileAsWord8Array fname
  in
    Wordhist.Word8Array1.new ctx (Word8ArraySlice.full str)
      (Word8Array.length str)
  end

fun cmdWordHist fname =
  withContext (fn ctx =>
    let
      val str_arr = readFileAsWord8Array1 ctx fname
      val st = Wordhist.Entry.symtab_new ctx str_arr
      val h = Wordhist.Entry.wordhist ctx (st, str_arr)
      fun printSym (sym, k) =
        let
          val arr = Wordhist.Entry.symtab_sym2word ctx (st, Int32.fromInt sym)
        in
          print
            (word8ArrayToString (Wordhist.Word8Array1.values arr) ^ ": "
             ^ Int32.toString k ^ "\n");
          Wordhist.Word8Array1.free arr
        end
    in
      Int32Array.appi printSym (Wordhist.Int32Array1.values h)
    end)

fun cmdSymTabNew fname =
  withContext (fn ctx =>
    let
      val str_arr =
        Wordhist.Word8Array1.new ctx
          (Word8ArraySlice.full (Word8Array.fromList [])) 0
      val st = Wordhist.Entry.symtab_new ctx str_arr
      val () = Wordhist.Word8Array1.free str_arr
    in
      BinIO.output
        ( BinIO.openOut fname
        , Word8Array.vector (Wordhist.Opaque.symtab.store st)
        )
    end)

fun cmdSymTabExtend st_fname words_fname =
  withContext (fn ctx =>
    let
      val st = Wordhist.Opaque.symtab.restore ctx
        (Word8ArraySlice.full (readFileAsWord8Array st_fname))
      val str_arr = readFileAsWord8Array1 ctx words_fname
      val st' = Wordhist.Entry.symtab_extend ctx (st, str_arr)
      val () = Wordhist.Word8Array1.free str_arr
      val () = Wordhist.Opaque.symtab.free st
    in
      BinIO.output
        ( BinIO.openOut st_fname
        , Word8Array.vector (Wordhist.Opaque.symtab.store st')
        )
    end)

fun cmdSymTabShow st_fname =
  withContext (fn ctx =>
    let
      val st = Wordhist.Opaque.symtab.restore ctx
        (Word8ArraySlice.full (readFileAsWord8Array st_fname))
      fun printSym sym =
        let
          val arr = Wordhist.Entry.symtab_sym2word ctx (st, Int32.fromInt sym)
        in
          print (word8ArrayToString (Wordhist.Word8Array1.values arr) ^ "\n");
          Wordhist.Word8Array1.free arr
        end

      val n = Wordhist.Entry.symtab_size ctx st
      fun printSyms i =
        if i = n then () else (printSym i; printSyms (i + 1))
    in
      printSyms 0;
      Wordhist.Opaque.symtab.free st
    end)


fun main () =
  (case CommandLine.arguments () of
     ["wordhist", file] => cmdWordHist file
   | ["symtab-new", file] => cmdSymTabNew file
   | ["symtab-extend", st_fname, words_fname] =>
       cmdSymTabExtend st_fname words_fname
   | ["symtab-show", st_fname] => cmdSymTabShow st_fname
   | _ => (err "Bad options\n"; OS.Process.exit OS.Process.failure))
  handle Wordhist.Error e => err ("Futhark error:\n" ^ e)

val () = main ()
