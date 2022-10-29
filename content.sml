structure HttpContentStream :
sig
  type InputContent
  val init: int -> InputContent
  val add:  InputContent -> string -> unit
  val done: InputContent -> TextIO.instream
end
=
struct

type input_size = int

datatype Input = InputString of string list ref | InputFile of (TextIO.instream * TextIO.outstream)

datatype InputContent = InputContent of input_size ref * Input ref

val maxSizeForString = 16 * 1024


fun initS () = InputString (ref [])

fun initF () =
  let
    val file = OS.FileSys.tmpName ()
    val i    = TextIO.openIn  file
    val out  = TextIO.openOut file
  in
    OS.FileSys.remove file;
    InputFile (i, out)
  end

fun init size = InputContent (ref 0, ref ( (if size <= maxSizeForString then initS else initF) ()))


fun add (InputContent (size_ref, input_ref)) s = (
  size_ref := (!size_ref) + String.size s;
  case !input_ref of
      InputString sl_ref => (
        sl_ref := s::(!sl_ref);
        if !size_ref > maxSizeForString
        then (
          case initF () of
               (inputContent as InputFile (_, out)) => (TextIO.output (out, String.concat (List.rev (!sl_ref))); input_ref := inputContent)
             | _ => ()
        )
        else ()
      )
    | InputFile (i, out) => TextIO.output (out, s)
)


fun done (InputContent (size_ref, input_ref)) =
  case !input_ref of
      InputString sl_ref => TextIO.openString (String.concat (List.rev (!sl_ref)))
    | InputFile (i, out) => (TextIO.closeOut out; i)



fun test () =
  let
    val ic  = init 3
    val _   = add ic "Hello, "
    val _   = add ic "Input Content.\n"
    val ics = done ic
  in
    print (TextIO.inputAll ics);
    TextIO.closeIn ics
  end

(* val _ = test () *)
end



structure HttpContent =
struct

structure CS = HttpContentStream

exception HttpBadContent


fun readContent needStop read timeout socket cl buf =
  let
    val ic = CS.init cl

    fun doit cl buf =
      let
        val s = String.size buf
      in
        if cl <= s
        then (
          CS.add ic (String.substring (buf, 0, cl));
          String.substring (buf, cl, s - cl)
        )
        else (
          CS.add ic buf;
          case read timeout socket of
              ""  => if needStop () then "" else raise HttpBadContent
            | buf => doit (cl - s) buf
        )
      end

      val buf = doit cl buf

      val ics = CS.done ic
  in
    (SOME ics, buf)
  end


exception HttpBadChunks

local

val maxChunkSizeEntity = 1024

fun getChunkSize t =
  let
    fun parser i getc strm =
      case getc strm of
          NONE => NONE
        | SOME (#"\r", strm) => parser (i + 1) getc strm
        | SOME (#"\n", strm) => SOME ((i + 1), strm)
        | SOME (c, strm)     => parser (i + 1) getc strm
  in
    case StringCvt.scanString (parser 0) t of NONE => NONE | SOME i =>
    case StringCvt.scanString (Int.scan StringCvt.HEX) t of NONE => NONE | SOME n => SOME (n, i)
  end



fun readChunkData needStop read timeout socket buf n i ic =
 let
   val s = String.size buf
   (* val _ = print ("n=" ^ (Int.toString n) ^ " i=" ^ (Int.toString i) ^ " s=" ^ (Int.toString s) ^ " buf: " ^ buf ^ "\n") *)
 in
   if s >= i + n + 1
   then
     let
       val data = String.substring (buf, i, n)
       val c = String.sub (buf, i + n)
     in
       if String.size data = 0 then () else CS.add ic data;
       if c = #"\r"
       then (
         if s >= i + n + 2
         then (if String.sub (buf, i + n + 1) = #"\n" then String.substring (buf, i + n + 2, s - i - n - 2) else raise HttpBadChunks )
         else (case read timeout socket of "" => if needStop () then "" else raise HttpBadChunks | b => readChunkData needStop read timeout socket ((String.substring (buf, i + n + 1, s - i - n - 1)) ^ b) 0 0 ic)
       )
       else if c = #"\n"
       then String.substring (buf, i + n + 1, s - i - n - 1)
       else if n = 0 then String.substring (buf, i, s - i) (* it is delete "\r\n" after size *)
       else raise HttpBadChunks
     end
   else
     let
       val data = String.substring (buf, i, s - i)
     in
       if String.size data = 0 then () else CS.add ic data;
       case read timeout socket of "" => if needStop () then "" else raise HttpBadChunks | buf => readChunkData needStop read timeout socket buf (n - s + i) 0 ic
     end
 end


in


fun readChunkes needStop read timeout socket buf =
  let
    val ic = CS.init (String.size buf)

    fun doit buf =
      case getChunkSize buf of
          NONE => (
            if String.size buf > maxChunkSizeEntity then raise HttpBadChunks else
            case read timeout socket of
                "" => if needStop () then "" else raise HttpBadChunks
              | b  => doit (buf ^ b)
          )
        | SOME (n, i) =>
          let
            val buf = readChunkData needStop read timeout socket buf n i ic
          in
            if n = 0
            then buf
            else doit buf
          end

    val buf = doit buf

    val ics = CS.done ic
  in
    (SOME ics, buf)
  end


fun test () =
  let

    fun needStop () = false

    val timeout = NONE

    fun test chunks content tail =
      let
        val bufs = ref chunks
        val socket = "ToDo"

        fun read timeout socket =
          let
            val buf = hd (!bufs)
          in
            bufs := tl (!bufs);
            buf
          end

        val (icsOption, buf) = readChunkes needStop read timeout socket ""
        val ics = valOf icsOption
        val inputContent = TextIO.inputAll ics
        val _ = TextIO.closeIn ics
      in
        if inputContent = content andalso buf = tail then print "OK\n" else print "ERROR\n"
      end

  in
    test ["0\r\n\r\n"] "" "";
    test ["5\r\n", "Hello\r\n", "0\r\n\r\n"] "Hello" "";
    test ["5\r\n", "Hell", "o\r\n", "0\r\n\r\n", ""] "Hello" "";
    test ["5 chunk-extension \r\n", "Hello\r\n", "0\r\n\r\n"] "Hello" "";
    test ["5\r\nHello\r\n", "A\r\n, Plack!\r\n\r\n", "0\r\n\r\n", "", ""] "Hello, Plack!\r\n" "";
    test ["5\r\nHello\r\nA\r\n, Plack!\r\n\r\n0\r\n\r\nXXXX\n"] "Hello, Plack!\r\n" "XXXX\n";

    test ["5\r\nHello", "\r\n0\r\n\r\n", "", ""] "Hello" "";
    test ["5\r\nHello\r", "\n0\r\n\r\n", "", ""] "Hello" "";
    ()
  end

end

(* val _ = test () *)
end
