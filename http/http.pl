%% <module> HTTP facts
%
% @author Douglas S. Green
% @license GPL

:- module(http, [
        http_code/2
    ]
).

%! http_code(?Code:int, ?Desc:string)
%! See https://en.wikipedia.org/wiki/List_of_HTTP_status_codes
http_code(100, "Continue").
http_code(101, "Switching Protocols").
http_code(102, "Processing").
http_code(103, "Early Hints").
http_code(110, "Response is Stale").
http_code(111, "Revalidation Failed").
http_code(112, "Disconnected Operation").
http_code(113, "Heuristic Expiration").
http_code(199, "Miscellaneous Warning").
http_code(200, "OK").
http_code(201, "Created").
http_code(202, "Accepted").
http_code(203, "Non-Authoritative Information").
http_code(204, "No Content").
http_code(205, "Reset Content").
http_code(206, "Partial Content").
http_code(207, "Multi-Status").
http_code(208, "Already Reported").
http_code(214, "Transformation Applied").
http_code(226, "IM Used").
http_code(299, "Miscellaneous Persistent Warning").
http_code(300, "Multiple Choices").
http_code(301, "Moved Permanently").
http_code(302, "Found").
http_code(303, "See Other").
http_code(304, "Not Modified").
http_code(305, "Use Proxy").
http_code(306, "Switch Proxy").
http_code(307, "Temporary Redirect").
http_code(308, "Permanent Redirect").
http_code(400, "Bad Request").
http_code(401, "Unauthorized").
http_code(402, "Payment Required").
http_code(403, "Forbidden").
http_code(404, "Not Found").
http_code(404, "error on Wikimedia").
http_code(405, "Method Not Allowed").
http_code(406, "Not Acceptable").
http_code(407, "Proxy Authentication Required").
http_code(408, "Request Timeout").
http_code(409, "Conflict").
http_code(410, "Gone").
http_code(411, "Length Required").
http_code(412, "Precondition Failed").
http_code(413, "Payload Too Large").
http_code(414, "URI Too Long").
http_code(415, "Unsupported Media Type").
http_code(416, "Range Not Satisfiable").
http_code(417, "Expectation Failed").
http_code(418, "I'm a teapot").
http_code(419, "Page Expired").
http_code(420, "Enhance Your Calm").
http_code(420, "Method Failure").
http_code(421, "Misdirected Request").
http_code(422, "Unprocessable Entity").
http_code(423, "Locked").
http_code(424, "Failed Dependency").
http_code(425, "Too Early").
http_code(426, "Upgrade Required").
http_code(428, "Precondition Required").
http_code(429, "Too Many Requests").
http_code(430, "Request Header Fields Too Large").
http_code(431, "Request Header Fields Too Large").
http_code(440, "Login Time-out").
http_code(444, "No Response").
http_code(449, "Retry With").
http_code(450, "Blocked by Windows Parental Controls").
http_code(451, "Redirect").
http_code(451, "Unavailable For Legal Reasons").
http_code(460, "Client closed the connection with the load balancer before the idle timeout period elapsed.").
http_code(463, "The load balancer received an X-Forwarded-For request header with more than 30 IP addresses.").
http_code(494, "Request header too large").
http_code(495, "SSL Certificate Error").
http_code(496, "SSL Certificate Required").
http_code(497, "HTTP Request Sent to HTTPS Port").
http_code(498, "Invalid Token").
http_code(499, "Client Closed Request").
http_code(499, "Token Required").
http_code(500, "Internal Server Error").
http_code(501, "Not Implemented").
http_code(502, "Bad Gateway").
http_code(503, "Service Unavailable").
http_code(504, "Gateway Timeout").
http_code(505, "HTTP Version Not Supported").
http_code(506, "Variant Also Negotiates").
http_code(507, "Insufficient Storage").
http_code(508, "Loop Detected").
http_code(509, "Bandwidth Limit Exceeded").
http_code(510, "Not Extended").
http_code(511, "Network Authentication Required").
http_code(520, "Web Server Returned an Unknown Error").
http_code(521, "Web Server Is Down").
http_code(522, "Connection Timed Out").
http_code(523, "Origin Is Unreachable").
http_code(524, "A Timeout Occurred").
http_code(525, "SSL Handshake Failed").
http_code(526, "Invalid SSL Certificate").
http_code(527, "Railgun Error").
http_code(529, "Site is overloaded").
http_code(530, "Error 530 is returned along with a 1xxx error.]").
http_code(530, "Site is frozen").
http_code(561, "Unauthorized").
http_code(598, "(Informal convention) Network read timeout error").
http_code(599, "Network Connect Timeout Error").
http_code(_, "Unknown HTTP code").
