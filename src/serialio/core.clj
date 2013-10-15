;; Provides:
;;
;; - Simple, flexible handler use for received data
;; - Synchronous, asynchronous, and mixed mode use
;; - Dynamic manipulation of available ports
;; - Sane error messages when opening a port fails
(ns serialio.core
  "Core functions for communicating with a serial device"
  (:refer-clojure :exclude [read])
  (:require [clojure.string :as str])
  (:import  (clojure.lang Sequential)
            (gnu.io CommPortIdentifier
                    NoSuchPortException
                    PortInUseException
                    SerialPort
                    SerialPortEvent
                    SerialPortEventListener
                    UnsupportedCommOperationException)
            (java.io Closeable InputStream)))

;; This uses [RXTX](http://rxtx.qbang.org) version 2.2, and for available port
;; manipulation, leverages speicific behavior of that library.


;;; ## Constants

;; Default timeout for port opening and blocking reads
(def default-timeout 3000)


;;; ## Available ports
;; The RXTX driver will first look for an explicit list of defined ports, and
;; if none exists, will scan the host for ports. Ports are defined using the
;; "gnu.io.rxtx.SerialPorts" property, either via a "gnu.io.rxtx.properties"
;; file in "java.ext.dirs" or programmatically. This lets us manipulate the list
;; of available ports at runtime.

(def ^:private path-sep (System/getProperty "path.separator"))

(defn available-ports
  "Returns a list of available port names. If ports have been registered using
  'add-ports', the list is returned as is; otherwise, a new scan of the host is
  performed."
  []
  (if-let [v (System/getProperty "gnu.io.rxtx.SerialPorts")]
    (seq (.split v path-sep))
    (map #(.getName %)
         (enumeration-seq (CommPortIdentifier/getPortIdentifiers)))))

(defn add-ports
  "Adds the paths as available ports. Once this property is set, no additional
  port scanning is performed; only the registered ports are available."
  [& paths]
  (let [v (distinct (apply conj (available-ports) paths))]
    (System/setProperty "gnu.io.rxtx.SerialPorts" (str/join path-sep v))
    v))

(defn reset-ports
  "Clears added ports, and restores the default port scanning behavior."
  []
  (System/clearProperty "gnu.io.rxtx.SerialPorts")
  (available-ports))


;;; ## Ports and listeners
;; The trickiest bit of setting up a port is managing incoming data flexibly. To
;; receive data, we register a listener, which starts a new monitor thread on
;; which the handler is called when data is received. This gives us basic
;; asynchronous reads, invoking the same handler each time. However, if we want
;; to send a message and return the response synchronously, we need to
;; coordinate the sending thread and the listener/handler thread.
;;
;; What we want is a way to make the handler subordinate to the sending thread
;; within a specific context. We do this by separating the handler from the
;; listener:
;;
;; - Store the handler function in an atom.
;; - Register a listener that, when data is received, derefs the atom and
;; invokes the current handler on the data.
;; - When a blocking read is needed, return a promise, and use a handler that
;; delivers it on data received.
;;
;; With this approach, the listener is an implementation detail, and the API
;; deals only with handlers, which can be changed easily.

;; Port type implements Closeable for "with-open" support
(declare close)
(defrecord Port [path device handler]
  Closeable
  (close [this] (close this)))

;; Helper function for throwing exceptions with useful messages
(defn- err [msg & params]
  (throw (Exception. (apply format msg params))))

(defn open
  "Opens a port with the specified baud rate and options. If the port will
  listen for incoming data, a handler function may be speficied, which takes an
  InputStream as its lone argument."
  ([path baud] (open path baud (constantly nil)))
  ([path baud handler] (open path baud 8 1 0 handler))
  ([path baud data-bits stop-bits parity handler]
     (try
       (let [handler (atom handler)
             port-id (CommPortIdentifier/getPortIdentifier path)
             device  ^SerialPort (.open port-id path default-timeout)]
         (doto device
           (.setSerialPortParams baud data-bits stop-bits parity)
           (.notifyOnDataAvailable true)
           (.addEventListener (reify SerialPortEventListener
                                (serialEvent [_ event]
                                  (when (= (.getEventType event)
                                           SerialPortEvent/DATA_AVAILABLE)
                                    (@handler (.getInputStream device)))))))
         (Port. path device handler))
       (catch NoSuchPortException e
         (err "'%s' is not a known port. See function 'add-ports'." path))
       (catch PortInUseException e
         (err "'%s' is already in use." path))
       (catch UnsupportedCommOperationException e
         (err "'%s' does not support connections at %d baud." path baud)))))

(defn close
  "Closes a port and removes its event listener"
  [port]
  (doto (:device port)
    (.removeEventListener)
    (.close)))


;;; ## Reading data
;; Asychronous reads, and synchronous (blocking) reads

;; ### Handlers
;; Handlers are functions that take a single InputStream argument and define how
;; received data is processed.

(defn on-data
  "Updates the handler to be called on each data received event"
  [port handler]
  (reset! (:handler port) handler))

(defn on-bytes
  [port handler]
  (on-data port (fn [^InputStream in]
                  (handler (doall (repeatedly (.available in) #(.read in)))))))


;; ### Blocking reads
;; This stores the existing handler, uses a read-specific handler to deliver
;; the response promise, then restores the original handler.

(defn read
  "Performs a synchronous read from the port, blocking until data is received
  or the specified timeout (in milliseconds) has elapsed"
  [port timeout]
  (let [resp (promise)
        orig (deref (:handler port))
        _    (on-bytes port (partial deliver resp))
        ret  (deref resp timeout nil)
        _    (on-data port orig)]
    ret))


;;; ## Writing data

;; Allow write operations on various data types
(defprotocol Writable (to-bytes ^bytes [this]))
(extend-protocol Writable
  Sequential (to-bytes [this] (byte-array (map byte this)))
  Number (to-bytes [this] (byte-array 1 (byte this)))
  String (to-bytes [this] (.getBytes this))
  Object (to-bytes [this] this))

(defn write
  "Writes the data to the port and returns the bytes written"
  [port data]
  (let [bytes (to-bytes data)
        device ^SerialPort (:device port)]
    (.write (.getOutputStream device) bytes)
    bytes))

(defn exec
  "Sends the specified data, and synchronously returns the response data"
  ([port data]
     (exec port data default-timeout))
  ([port data timeout]
     (write port data)
     (read port timeout)))
