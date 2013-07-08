;; This library currently depends on Sam Aaron's "serial-port" lib. It doesn't
;; reuse much of it, and might be better off rewriting those functions. In
;; particular, this library is designed to allow both synchronous (blocking)
;; and asychronous (i.e. listen and handle) read operations, whereas the
;; "serial-port" base lib only considers the latter. Here are a few other
;; enhancements that would be nice:
;;
;; - On the Port record type, implement java.io.Closeable so that it can be
;; used with Clojure's 'with-open' macro.
;; - Use a protocol to allow 'write' to take a seq of bytes, or a byte array,
;; and perhaps an int, string, etc.
;; - Create a 'with-handler' macro for 'read' that stores the current handler,
;; executes the body with a specified handler, restores the original handler,
;; and returns the result of the body. This will allow mixed mode operation
;; (synchronous and asynchronous).
;; - Give all functions reasonable return values for a more functional style.
;; - Eliminate reflection on handlers (type InputStreams), as these can get
;; called frequently.

(ns serialio.core
  "Core functions for communicating with a serial device"
  (:refer-clojure :exclude [read])
  (:require [clojure.string :as str]
            [serial-port :as ser])
  (:import  [java.io InputStream]))

;; ## Constants

(def read-timeout
  "How long to wait for a command to return in milliseconds"
  3000)

;; ## Utility functions

(defn read-stream
  "Returns the sequence of available bytes from the input stream"
  [in]
  (doall (repeatedly (.available in) #(.read in))))

(defn add-port-id
  "Adds the path to the available ports via the 'gnu.io.rxtx.SerialPorts'
  system property. Once this property is set, no scanning is performed;
  only the specified ports are available."
  [path]
  (let [prop "gnu.io.rxtx.SerialPorts"
        paths (apply sorted-set
                     (-> (str (System/getProperty prop))
                         (str/split #":")
                         (conj path)))]
    (System/setProperty prop (str/join ":" paths))))

;; ## Core functions

;; Serial communication using [RXTX](http://rxtx.qbang.org) is asynchronous by
;; default, and synchronous send-and-receieve requires a bit more coordination.
;;
;; To receive data notifications, we register a listener, which starts a new
;; monitor thread on which the handler is called when data is received. For
;; asynchronous reads, this is all we need. However, if we want to send a
;; message and return the response synchronously, we need to coordinate the
;; sending thread and the listener/handler thread.
;;
;; We could simply dispense with the listener and use polling to avoid this, but
;; at the cost of efficiency. What we want is a way to make the handler
;; subordinate to the sending thread within a specific context. We do this by
;; separating the handler from the listener:
;;
;; - Store the input handler in an atom.
;; - Register a listener that, when input is received, derefs the atom
;; and invokes the current handler on the data.
;; - When a blocking read is needed, return a promise, and use a handler that
;; delivers it on input received.

(defn open
  "Opens a port with the specified baud rate. If the connected device will send
  data on its own, a handler function may be speficied, which takes an input
  stream as its lone argument."
  ([path baud]
     (open path baud (constantly nil)))
  ([path baud handler]
     (let [a (atom handler)]
       (doto (-> (ser/open path baud)
                 (assoc :handler a))
         (ser/listen (fn [in] (@a in)))))))

(defn read
  "Performs a synchronous read from the port, blocking until data is received
  or the specified timeout (in milliseconds) has elapsed"
  [port timeout]
  (let [resp (promise)
        handler (fn [in] (deliver resp (read-stream in)))]
    (reset! (:handler port) handler)
    (deref resp timeout nil)))

(defn write
  "Writes the data to the port"
  [port bytes]
  (ser/write port bytes))

(defn exec
  "Sends the specified data, and synchronously returns the response data"
  ([port bytes]
     (exec port bytes read-timeout))
  ([port bytes timeout]
     (write port bytes)
     (read port timeout)))

(defn close
  "Closes a port and removes its event listener"
  [port]
  (ser/close port))

(defn on-data
  "Calls the handler for each data received event"
  [port handler]
  (reset! (:handler port) handler))
