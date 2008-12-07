Modifications to existing modules:
 + update XML sax parser clojure interface

Thinking about it:
 + Network modues:
  + sockets 
  + network protocol implementations (HTTP/SMTP/POP/IMAP) may overlap or require J2EE
 + Data modules
  + JSON encoder/decoder
  + generic data structures/algorithms (e.g. red black tree, heap, etc ala SML/CAML may require Functors?)
  + mime encoding/decoding
  + Wrapper/front end to HD5 (ala pytables may be difficult do to soft typing)
 + Wrappers
  + apache commons IO 
  + apache commons calendar/date functions
  + apache Math and/or Jama
 + lex/yacc port (seperate project?)
