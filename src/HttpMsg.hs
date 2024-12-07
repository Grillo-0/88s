module HttpMsg
  ( HttpVer (..),
    Method (..),
    RequestUri (..),
    StatusCode (..),
    getReasonPhrase,
    RequestLine (..),
    StatusLine (..),
    GeneralHeader (..),
    GeneralHeaderFields (..),
    RequestHeader (..),
    RequestHeaderFields (..),
    ResponseHeader (..),
    ResponseHeaderFields (..),
    EntityHeader (..),
    EntityHeaderFields (..),
    Body,
    Msg (..),
  )
where

import Data.List (intercalate)

data HttpVer = HttpVer Int Int

instance Show HttpVer where
  show (HttpVer major minor) = "HTTP/" ++ show major ++ "." ++ show minor

data Method
  = Options
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Trace
  | Connect
  | ExtensionMethod String

instance Show Method where
  show method = case method of
    Options -> "OPTIONS"
    Get -> "GET"
    Head -> "HEAD"
    Post -> "POST"
    Put -> "PUT"
    Delete -> "DELETE"
    Trace -> "TRACE"
    Connect -> "CONNECT"
    (ExtensionMethod a) -> show a

data Path = Path [String]

instance Show Path where
  show (Path paths_bits) = "/" ++ intercalate "/" paths_bits

data RequestUri
  = NoParticular
  | Absolute Path
  | AbsPath Path
  | Autorithy String

instance Show RequestUri where
  show req_uri = case req_uri of
    NoParticular -> "*"
    Absolute uri -> show uri
    AbsPath uri -> show uri
    Autorithy uri -> show uri

data StatusCode
  = S100
  | S101
  | S200
  | S201
  | S202
  | S203
  | S204
  | S205
  | S206
  | S300
  | S301
  | S302
  | S303
  | S304
  | S305
  | S307
  | S400
  | S401
  | S402
  | S403
  | S404
  | S405
  | S406
  | S407
  | S408
  | S409
  | S410
  | S411
  | S412
  | S413
  | S414
  | S415
  | S416
  | S417
  | S500
  | S501
  | S502
  | S503
  | S504
  | S505
  | ExtensionCode Int String

instance Show StatusCode where
  show code = case code of
    S101 -> "101"
    S100 -> "100"
    S200 -> "200"
    S201 -> "201"
    S202 -> "202"
    S203 -> "203"
    S204 -> "204"
    S205 -> "205"
    S206 -> "206"
    S300 -> "300"
    S301 -> "301"
    S302 -> "302"
    S303 -> "303"
    S304 -> "304"
    S305 -> "305"
    S307 -> "307"
    S400 -> "400"
    S401 -> "401"
    S402 -> "402"
    S403 -> "403"
    S404 -> "404"
    S405 -> "405"
    S406 -> "406"
    S407 -> "407"
    S408 -> "408"
    S409 -> "409"
    S410 -> "410"
    S411 -> "411"
    S412 -> "412"
    S413 -> "413"
    S414 -> "414"
    S415 -> "415"
    S416 -> "416"
    S417 -> "417"
    S500 -> "500"
    S501 -> "501"
    S502 -> "502"
    S503 -> "503"
    S504 -> "504"
    S505 -> "505"
    (ExtensionCode ext_code _) -> show ext_code

getReasonPhrase :: StatusCode -> String
getReasonPhrase code = case code of
  S100 -> "Continue"
  S101 -> "Switching Protocols"
  S200 -> "OK"
  S201 -> "Created"
  S202 -> "Accepted"
  S203 -> "Non-Authoritative Information"
  S204 -> "No Content"
  S205 -> "Reset Content"
  S206 -> "Partial Content"
  S300 -> "Multiple Choices"
  S301 -> "Moved Permanently"
  S302 -> "Found"
  S303 -> "See Other"
  S304 -> "Not Modified"
  S305 -> "Use Proxy"
  S307 -> "Temporary Redirect"
  S400 -> "Bad Request"
  S401 -> "Unauthorized"
  S402 -> "Payment Required"
  S403 -> "Forbidden"
  S404 -> "Not Found"
  S405 -> "Method Not Allowed"
  S406 -> "Not Acceptable"
  S407 -> "Proxy Authentication Required"
  S408 -> "Request Time-out"
  S409 -> "Conflict"
  S410 -> "Gone"
  S411 -> "Length Required"
  S412 -> "Precondition Failed"
  S413 -> "Request Entity Too Large"
  S414 -> "Request-URI Too Large"
  S415 -> "Unsupported Media Type"
  S416 -> "Requested range not satisfiable"
  S417 -> "Expectation Failed"
  S500 -> "Internal Server Error"
  S501 -> "Not Implemented"
  S502 -> "Bad Gateway"
  S503 -> "Service Unavailable"
  S504 -> "Gateway Time-out"
  S505 -> "HTTP Version not supported"
  (ExtensionCode _ phrase) -> phrase

httpUnlines :: [String] -> String
httpUnlines = foldMap (\s -> s ++ "\r\n")

data RequestLine = RequestLine Method RequestUri HttpVer

instance Show RequestLine where
  show (RequestLine method req_uri ver) = httpUnlines [unwords [show method, show req_uri, show ver]]

data StatusLine = StatusLine HttpVer StatusCode

instance Show StatusLine where
  show (StatusLine ver code) = httpUnlines [unwords [show ver, show code, getReasonPhrase code]]

data GeneralHeaderFields
  = CacheControl
  | Connection
  | Date
  | Pragma
  | Trailer
  | TransferEncoding
  | Upgrade
  | Via
  | Warning

instance Show GeneralHeaderFields where
  show _ = "TODO: GeneralHeaderFields"

data GeneralHeader = GeneralHeader [GeneralHeaderFields]

instance Show GeneralHeader where
  show (GeneralHeader fields) = concat $ map show fields

data RequestHeaderFields
  = Accept
  | AcceptCharset
  | AcceptEncoding
  | AcceptLanguage
  | Authorization
  | Expect
  | From
  | Host
  | IfMatch
  | IfModifiedSince
  | IfNoneMatch
  | IfRange
  | IfUnmodifiedSince
  | MaxForwards
  | ProxyAuthorization
  | Range
  | Referer
  | TE
  | UserAgent

instance Show RequestHeaderFields where
  show _ = "TODO: RequestHeaderFields"

data RequestHeader = RequestHeader [RequestHeaderFields]

instance Show RequestHeader where
  show (RequestHeader fields) = concat $ map show fields

data ResponseHeaderFields
  = AcceptRanges
  | Age
  | ETag
  | Location
  | ProxyAuthenticate
  | RetryAfter
  | Server
  | Vary
  | WwwAuthenticate

instance Show ResponseHeaderFields where
  show _ = "TODO: ResponseHeaderFields"

data ResponseHeader = ResponseHeader [ResponseHeaderFields]

instance Show ResponseHeader where
  show (ResponseHeader fields) = concat $ map show fields

data EntityHeaderFields
  = Allow
  | ContentEncoding
  | ContentLanguage
  | ContentLength
  | ContentLocation
  | ContentMd5
  | ContentRange
  | ContentType
  | Expires
  | LastModified
  | ExtensionHeader

instance Show EntityHeaderFields where
  show _ = "TODO: EntityHeaderFields"

data EntityHeader = EntityHeader [EntityHeaderFields]

instance Show EntityHeader where
  show (EntityHeader fields) = concat $ map show fields

type Body = String

data Msg
  = Request RequestLine GeneralHeader RequestHeader EntityHeader Body
  | Response StatusLine GeneralHeader ResponseHeader EntityHeader Body

instance Show Msg where
  show (Request rl gh rh eh body) = (httpUnlines $ filter (not . null) [show rl, show gh, show rh, show eh, ""]) ++ body
  show (Response sl gh rh eh body) = (httpUnlines $ filter (not . null) [show sl, show gh, show rh, show eh, ""]) ++ body
