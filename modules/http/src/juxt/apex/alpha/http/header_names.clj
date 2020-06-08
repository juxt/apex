;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.http.header-names
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))

(def header-canonical-case
  {"a-im" "A-IM",
   "accept" "Accept",
   "accept-additions" "Accept-Additions",
   "accept-charset" "Accept-Charset",
   "accept-datetime" "Accept-Datetime",
   "accept-encoding" "Accept-Encoding",
   "accept-features" "Accept-Features",
   "accept-language" "Accept-Language",
   "accept-patch" "Accept-Patch",
   "accept-post" "Accept-Post",
   "accept-ranges" "Accept-Ranges",
   "age" "Age",
   "allow" "Allow",
   "alpn" "ALPN",
   "alt-svc" "Alt-Svc",
   "alt-used" "Alt-Used",
   "alternates" "Alternates",
   "apply-to-redirect-ref" "Apply-To-Redirect-Ref",
   "authentication-control" "Authentication-Control",
   "authentication-info" "Authentication-Info",
   "authorization" "Authorization",
   "c-ext" "C-Ext",
   "c-man" "C-Man",
   "c-opt" "C-Opt",
   "c-pep" "C-PEP",
   "c-pep-info" "C-PEP-Info",
   "cache-control" "Cache-Control",
   "cal-managed-id" "Cal-Managed-ID",
   "caldav-timezones" "CalDAV-Timezones",
   "cdn-loop" "CDN-Loop",
   "cert-not-after" "Cert-Not-After",
   "cert-not-before" "Cert-Not-Before",
   "close" "Close",
   "connection" "Connection",
   "content-base" "Content-Base",
   "content-disposition" "Content-Disposition",
   "content-encoding" "Content-Encoding",
   "content-id" "Content-ID",
   "content-language" "Content-Language",
   "content-length" "Content-Length",
   "content-location" "Content-Location",
   "content-md5" "Content-MD5",
   "content-range" "Content-Range",
   "content-script-type" "Content-Script-Type",
   "content-style-type" "Content-Style-Type",
   "content-type" "Content-Type",
   "content-version" "Content-Version",
   "cookie" "Cookie",
   "cookie2" "Cookie2",
   "dasl" "DASL",
   "date" "Date",
   "dav" "DAV",
   "default-style" "Default-Style",
   "delta-base" "Delta-Base",
   "depth" "Depth",
   "derived-from" "Derived-From",
   "destination" "Destination",
   "differential-id" "Differential-ID",
   "digest" "Digest",
   "early-data" "Early-Data",
   "etag" "ETag",
   "expect" "Expect",
   "expect-ct" "Expect-CT",
   "expires" "Expires",
   "ext" "Ext",
   "forwarded" "Forwarded",
   "from" "From",
   "getprofile" "GetProfile",
   "hobareg" "Hobareg",
   "host" "Host",
   "http2-settings" "HTTP2-Settings",
   "if" "If",
   "if-match" "If-Match",
   "if-modified-since" "If-Modified-Since",
   "if-none-match" "If-None-Match",
   "if-range" "If-Range",
   "if-schedule-tag-match" "If-Schedule-Tag-Match",
   "if-unmodified-since" "If-Unmodified-Since",
   "im" "IM",
   "include-referred-token-binding-id"
   "Include-Referred-Token-Binding-ID",
   "keep-alive" "Keep-Alive",
   "label" "Label",
   "last-modified" "Last-Modified",
   "link" "Link",
   "location" "Location",
   "lock-token" "Lock-Token",
   "man" "Man",
   "max-forwards" "Max-Forwards",
   "memento-datetime" "Memento-Datetime",
   "meter" "Meter",
   "mime-version" "MIME-Version",
   "negotiate" "Negotiate",
   "odata-entityid" "OData-EntityId",
   "odata-isolation" "OData-Isolation",
   "odata-maxversion" "OData-MaxVersion",
   "odata-version" "OData-Version",
   "opt" "Opt",
   "optional-www-authenticate" "Optional-WWW-Authenticate",
   "ordering-type" "Ordering-Type",
   "origin" "Origin",
   "oscore" "OSCORE",
   "overwrite" "Overwrite",
   "p3p" "P3P",
   "pep" "PEP",
   "pep-info" "Pep-Info",
   "pics-label" "PICS-Label",
   "position" "Position",
   "pragma" "Pragma",
   "prefer" "Prefer",
   "preference-applied" "Preference-Applied",
   "profileobject" "ProfileObject",
   "protocol" "Protocol",
   "protocol-info" "Protocol-Info",
   "protocol-query" "Protocol-Query",
   "protocol-request" "Protocol-Request",
   "proxy-authenticate" "Proxy-Authenticate",
   "proxy-authentication-info" "Proxy-Authentication-Info",
   "proxy-authorization" "Proxy-Authorization",
   "proxy-features" "Proxy-Features",
   "proxy-instruction" "Proxy-Instruction",
   "public" "Public",
   "public-key-pins" "Public-Key-Pins",
   "public-key-pins-report-only" "Public-Key-Pins-Report-Only",
   "range" "Range",
   "redirect-ref" "Redirect-Ref",
   "referer" "Referer",
   "replay-nonce" "Replay-Nonce",
   "retry-after" "Retry-After",
   "safe" "Safe",
   "schedule-reply" "Schedule-Reply",
   "schedule-tag" "Schedule-Tag",
   "sec-token-binding" "Sec-Token-Binding",
   "sec-websocket-accept" "Sec-WebSocket-Accept",
   "sec-websocket-extensions" "Sec-WebSocket-Extensions",
   "sec-websocket-key" "Sec-WebSocket-Key",
   "sec-websocket-protocol" "Sec-WebSocket-Protocol",
   "sec-websocket-version" "Sec-WebSocket-Version",
   "security-scheme" "Security-Scheme",
   "server" "Server",
   "set-cookie" "Set-Cookie",
   "set-cookie2" "Set-Cookie2",
   "setprofile" "SetProfile",
   "slug" "SLUG",
   "soapaction" "SoapAction",
   "status-uri" "Status-URI",
   "strict-transport-security" "Strict-Transport-Security",
   "sunset" "Sunset",
   "surrogate-capability" "Surrogate-Capability",
   "surrogate-control" "Surrogate-Control",
   "tcn" "TCN",
   "te" "TE",
   "timeout" "Timeout",
   "topic" "Topic",
   "trailer" "Trailer",
   "transfer-encoding" "Transfer-Encoding",
   "ttl" "TTL",
   "upgrade" "Upgrade",
   "urgency" "Urgency",
   "uri" "URI",
   "user-agent" "User-Agent",
   "variant-vary" "Variant-Vary",
   "vary" "Vary",
   "via" "Via",
   "want-digest" "Want-Digest",
   "warning" "Warning",
   "www-authenticate" "WWW-Authenticate",
   "x-content-type-options" "X-Content-Type-Options",
   "x-frame-options" "X-Frame-Options"})

(defn create-header-map []
  (->>
   (str/split (slurp "https://www.iana.org/assignments/message-headers/perm-headers.csv") #"\n")
   (map (fn [line] (str/split line #",")) )
   (filter (fn [[_ _ protocol]] (= protocol "http")))
   (map first)
   (sort)
   (map (juxt str/lower-case identity))
   (into (sorted-map))))

(defn- wrap-headers-normalize-case-response [response]
  (update response :headers set/rename-keys header-canonical-case))

(defn wrap-headers-normalize-case
  "Turn headers into their normalized case. Eg. user-agent becomes User-Agent."
  [h]
  (fn
    ([req]
     (wrap-headers-normalize-case-response (h req)))
    ([req respond raise]
     (h
      req
      (fn [response]
        (respond (wrap-headers-normalize-case-response response)))
      raise))))
