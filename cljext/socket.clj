;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File     : socket.clj
;; Function : Socket library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Send comments or questions to code at freshlime dot org
;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (c) 2008, J. Bester
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * The name of the authors names of its contributors may be used to 
;;       endorse or promote products derived from this software without
;;       specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ``AS IS'' AND ANY
;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns cljext.socket
    (:refer-clojure)
    (:import [java.net Socket]
	     [java.io BufferedReader InputStreamReader InputStream OutputStream])
    )


(defstruct socket-connection
  :socket
  :input
  :input-reader
  :output)

;;(new BufferedReader (new InputStreamReader (.getInputStream socket)))

(defn connect 
  ([hostname port]
   (let [socket (new Socket hostname port)]
     (struct-map socket-connection
		 :socket 
		 :input (.getInputStream socket)
		 :output (.getOutputStream socket)
		 ))))

(defn close
  ([conn]
   (.close (:socket conn))))

(defn closed?
  ([conn]
   (.isClosed (:socket conn))))

(defn connected?
  ([conn]
   (.isConnected (:socket conn))))

(defmacro switch
  ([value & cases]
   (let [sym (gensym)]
     `(let [~sym ~value]
	(cond 
	 ~@(apply concat 
		  (for [case (partition 2 cases)]
		    `((= ~sym ~(first case)) ~(second case)))))))))

(def +keep-alive+ 'keep-alive)
(def +oob-inline+ 'oob-line)
(def +receive-buffer-size+ 'receive-buffer-size)
(def +reuse-address+ 'reuse-address)
(def +send-buffer-size+ 'send-buffer-size )
(def +so-linger+ 'so-linger )
(def +so-timeout+ 'so-timeout)
(def +tcp-no-delay+ 'tcp-no-delay)
(def +crlf+ "\r\n")

(defn set-socket-option
  ([conn option value & values]
   (let [socket (:socket conn)]
     (switch option
	     +keep-alive+ (.setKeepAlive socket (boolean value))
	     +oob-inline+ (.setOOBInline socket (boolean value))
	     +receive-buffer-size+ (.setReceiveBufferSize socket (int value))
	     +reuse-address+ (.setReuseAddress socket (boolean value))
	     +send-buffer-size+ (.setSendBufferSize socket (boolean value))
	     +so-linger+ (.setSoLinger socket (boolean value) (int (first values)))
	     +so-timeout+ (.setTimeout socket (int value))
	     +tcp-no-delay+ (.setTcpNoDelay socket (boolean value))
	     )))

(defn get-socket-option
  ([conn option]
   (let [socket (:socket conn)]
     (switch option
	     +keep-alive+ (.getKeepAlive socket)
	     +oob-inline+ (.getOOBInline socket)
	     +receive-buffer-size+ (.getReceiveBufferSize socket)
	     +reuse-address+ (.getReuseAddress socket)
	     +send-buffer-size+ (.getSendBufferSize socket)
	     +so-linger+ (.getSoLinger socket)
	     +so-timeout+ (.getTimeout socket)
	     +tcp-no-delay+ (.getTcpNoDelay socket)
	     ))))


(defn socket-read
  ([conn max-size]
   (let [buffer (make-array Byte/TYPE 10)
	 count (.read (:input conn) buffer)]
     (list count buffer))))

(defn socket-read-string
  ([conn max-size]
   (let [buffer (make-array Byte/TYPE max-size)
	 count (.read (:input conn) buffer)]
     (new String buffer 0 count))))

(defn socket-write
  ([conn data]
   (if (string? data)
     (.write (:output conn) (.getBytes data))
     (.write (:output conn) data))))

(defn socket-writeln
  ([conn data]
   (if (string? data)
     (do
       (let [out (:output conn)]
	 (.write out (.getBytes data))
	 (.write out (.getBytes +crlf+))))
     (throw (new IllegalArgumentException "Argument must be string")))))


(let [cnn (cljext.socket/connect "www.cnn.com" 80)]
  (do
    (cljext.socket/socket-writeln cnn "GET / HTTP/1.0")
    (cljext.socket/socket-writeln cnn "Host: www.cnn.com")
    (cljext.socket/socket-writeln cnn "")
    (println (cljext.socket/socket-read-string cnn 1024))
    (println (cljext.socket/socket-read-string cnn 1024))))
  
(defn resolve-host-ipaddr 
  ([hostname]
     (let [hosts (java.net.InetAddress/getAllByName hostname)]
       (map (memfn getHostAddress) hosts))))

(defn resolve-ipaddr-host
  ([ipaddr]
     (do
;;       (let [ip (into-array Byte/TYPE (map (fn [x] (byte (Integer/valueOf x))) (.split ipaddr "[.]")))]
	 (map (memfn getHostName) (java.net.InetAddress/getAllByName ipaddr)))))