Modifications to existing modules:
 + base64 encode/decode for streams and/or files
 + update XML sax parser clojure interface
New Modules planned:
 + Hash/checksum library (SHA1, CRC32, MD5, etc)

Thinking about it:
 + utility module common macros/functions (e.g. switch) ; prefer not to do this unless writing other modules becomes syntactically hard
 + Network modues:
  + sockets 
  + network protocol implementations (HTTP/SMTP/POP/IMAP) may overlap or require J2EE
 + Data modules
  + JSON encoder/decoder
  + generic data structures/algorithms (e.g. red black tree, heap, etc ala SML/CAML may require Functors?)
  + mime encoding/decoding
  + Wrapper/front end to HD5 (ala pytables may be difficult do to soft typing)
 + Wrappers
  + Wrapper for apache commons IO 
  + Wrapper for apache commons calendar/date functions
 + lex/yacc port (seperate project?)



  
