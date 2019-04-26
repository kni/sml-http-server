structure HttpServer =
struct

datatype ('c, 'd) Env = Env of {
  requestMethod   : string,
  requestURI      : string,
  pathInfo        : string,
  queryString     : string,
  serverProtocol  : string,
  headers         : (string * string) list,
  input           : TextIO.instream option,
  workerHookData  : 'c option,
  connectHookData : 'd option
}


datatype Response =
    ResponseSimple  of   string * (string * string) list * string
  | ResponseDelayed of ((string * (string * string) list * string) -> bool) -> bool
  | ResponseStream  of ((string * (string * string) list) -> (string -> bool)) -> bool


datatype ('c, 'd) settings = Settings of {
  handler      : ('c, 'd) Env -> Response,
  port         : int,
  host         : string,
  acceptQueue  : int,
  workers      : int,
  maxRequests  : int,
  reuseport    : bool,
  workerHook   : ((unit -> 'c) * ('c -> unit)) option,
  connectHook  : ((unit -> 'd) * ('d -> unit)) option,
  logger       : string -> unit,
  timeout      : Time.time option
}

val needStop = NetServer.needStop


val maxHeadersSize = 8 * 1024
val chunksize = 64 * 1024


local

  fun write timeout socket text = NetServer.write (socket, text, timeout)

  fun doHeaders [] = ""
    | doHeaders headers = (String.concatWith "\r\n" (List.map (fn (a, b) => (a ^ ": " ^ b)) headers)) ^ "\r\n"

  fun doResponseSimple timeout socket keepAliveHeader (code, headers, body) =
    let
      val contentLength = String.size body
    in
      if contentLength = 0
      then write timeout socket ("HTTP/1.1 " ^ code ^ "\r\n" ^
          (if keepAliveHeader then "Connection: keep-alive\r\n" else "") ^
          (doHeaders headers) ^
          "\r\n"
        )
      else write timeout socket ("HTTP/1.1 " ^ code ^ "\r\n" ^
          (if keepAliveHeader then "Connection: keep-alive\r\n" else "") ^
          (doHeaders headers) ^
          "Content-Length: " ^ (Int.toString contentLength) ^ "\r\n" ^
          "\r\n" ^
          body
        )
    end

in
  fun doResponse timeout socket keepAliveHeader (ResponseSimple (code, headers, body)) = doResponseSimple timeout socket keepAliveHeader (code, headers, body)
    | doResponse timeout socket keepAliveHeader (ResponseDelayed f) = f (doResponseSimple timeout socket keepAliveHeader)
    | doResponse timeout socket keepAliveHeader (ResponseStream f) =
      let
        fun writer t =
          let
            val length = String.size t
         in
           if length = 0 then write timeout socket "0\r\n\r\n" else
           write timeout socket ((Int.fmt StringCvt.HEX length) ^ "\r\n" ^ t ^ "\r\n")
         end

        fun doit (code, headers) = (
            write timeout socket ("HTTP/1.1 " ^ code ^ "\r\n" ^
              (if keepAliveHeader then "Connection: keep-alive\r\n" else "") ^
              (doHeaders headers) ^
              "Transfer-Encoding: chunked\r\n" ^
              "\r\n"
            );
            writer
          )
      in
        f doit
      end
end


fun findPairValue _ [] = NONE
  | findPairValue x ((k,v)::ks) = if k = x then SOME v else findPairValue x ks


fun findConnectionHeader headers = case findPairValue "connection" headers of NONE => NONE | SOME v => SOME (String.map Char.toLower v)

fun isPersistent "HTTP/1.0" headers = (case findConnectionHeader headers of SOME "keep-alive" => (true, true)   | _ => (false, false))
  | isPersistent protocol   headers = (case findConnectionHeader headers of SOME "close"      => (false, false) | _ => (true, false))


exception HttpBadRequest


fun run (Settings settings) =
  let

    val timeout = #timeout settings
    val logger  = #logger  settings

    fun handler (workerHookData, connectHookData) socket =
      let

        fun read timeout socket = NetServer.read (socket, chunksize, timeout)

        fun readContent socket cl buf = HttpContent.readContent needStop read timeout socket cl buf
          handle HttpContent.HttpBadContent => raise HttpBadRequest | exc => raise exc

        fun readChunkes socket buf = HttpContent.readChunkes needStop read timeout socket buf
          handle HttpContent.HttpBadChunks => raise HttpBadRequest | exc => raise exc


        fun doit buf =
          case HttpHeaders.parse buf of
               NONE => (
                 if String.size buf > maxHeadersSize
                 then (doResponse timeout socket false (ResponseSimple ("413", [], "Entity Too Large\r\n")); ())
                 else case read timeout socket of "" => () | b => doit (buf ^ b)
               )
             | SOME (method, uri, path, query, protocol, headers, buf) =>
                 let
                   val (persistent, keepAliveHeader) = isPersistent protocol headers

                   val (inputContent, buf) =
                     if method = "POST" orelse method = "PUT"
                     then (
                       if findPairValue "expect" headers = SOME "100-continue"
                       then doResponse timeout socket keepAliveHeader (ResponseSimple ("100 Continue", [], "")) else true;

                       case findPairValue "content-length" headers of
                         SOME cl => (
                           case Int.fromString cl of
                               NONE => raise HttpBadRequest
                             | SOME cl => readContent socket cl buf
                         )
                       | NONE => (
                           if findPairValue "transfer-encoding" headers = SOME "chunked"
                           then readChunkes socket buf
                           else (NONE, buf)
                         )
                     )
                     else (NONE, buf)

                   val env = Env {
                     requestMethod   = method,
                     requestURI      = uri,
                     pathInfo        = path,
                     queryString     = query,
                     serverProtocol  = protocol,
                     headers         = headers,
                     input           = inputContent,
                     workerHookData  = workerHookData,
                     connectHookData = connectHookData
                   }

                   val res = (#handler settings) env handle exc => ResponseSimple ("500", [], "Internal server error\r\n")
                   val _ = case inputContent of NONE => () | SOME inputContent => TextIO.closeIn inputContent
                 in
                   doResponse timeout socket keepAliveHeader res;
                   if persistent then doit buf else ()
                 end


      in
        logger "HELLO, socket.";

        (doit "" handle
            HttpBadRequest => (doResponse timeout socket false (ResponseSimple ("400", [], "Bad Request\r\n")); ())
          | exc as OS.SysErr (s, SOME e) => if OS.errorName e = "ECONNRESET" orelse OS.errorName e = "connreset" then logger ("ERROR ECONNRESET: " ^ s ^ "\n") else raise exc
          | exc => raise exc);

        logger "BY, socket."
      end

  in
    logger "Start.";
    NetServer.run (NetServer.Settings {
      handler      = handler,
      port         = (#port        settings),
      host         = (#host        settings),
      acceptQueue  = (#acceptQueue settings),
      workers      = (#workers     settings),
      maxRequests  = (#maxRequests settings),
      reuseport    = (#reuseport   settings),
      workerHook   = (#workerHook  settings),
      connectHook  = (#connectHook settings),
      logger       = logger
    })
  end

end
