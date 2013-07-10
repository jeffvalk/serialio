;; - Synchronous, asynchronous, and mixed mode use
;;
;; - Sane error messages when opening a port fails


;; A few other enhancements possibilities:
;;
;; - Use a protocol to allow 'write' to take a seq of bytes, or a byte array,
;; and perhaps an int, string, etc.
;; - Create a 'with-handler' macro for 'read' that stores the current handler,
;; executes the body with a specified handler, restores the original handler,
;; and returns the result of the body. This will allow mixed mode operation
;; (synchronous and asynchronous).

(set! *warn-on-reflection* true)

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

;; ## Constants

;; Default timeout for port opening and blocking reads
(def default-timeout 3000)

;; ## Available ports
;; The RXTX driver will first look for an explicit list of defined ports, and
;; if none exists, will scan the host for ports. Ports are defined using the
;; "gnu.io.rxtx.SerialPorts" property, either via a "gnu.io.rxtx.properties"
;; file in "java.ext.dirs" or programmatically. This lets us manipulate the list
;; of available ports at runtime.

(def ^:private path-sep (System/getProperty "path.separator"))

(defn available-ports
  "Returns a list of available port names. By default, this scans the host
  machine. If ports are registered using the 'gnu.io.rxtx.SerialPorts' system
  property, either by calling 'add-ports' or setting this directly, only
  registered ports are available."
  []
  (if-let [v (System/getProperty "gnu.io.rxtx.SerialPorts")]
    (seq (.split v path-sep))
    (map #(.getName %)
         (enumeration-seq (CommPortIdentifier/getPortIdentifiers)))))

(defn add-ports
  "Registers the paths as available ports via the 'gnu.io.rxtx.SerialPorts'
  system property. Once this property is set, no port scanning is performed;
  only the registered ports are available."
  [& paths]
  (let [pv (System/getProperty "gnu.io.rxtx.SerialPorts" "")
        nv (str/join path-sep (disj (into (apply sorted-set paths)
                                          (.split pv path-sep))
                                    ""))]
    (System/setProperty "gnu.io.rxtx.SerialPorts" nv)
    nv))

(defn reset-ports
  "Clears the 'gnu.io.rxtx.SerialPorts' system property. This restores
  default port scanning behavior."
  []
  (System/clearProperty "gnu.io.rxtx.SerialPorts")
  nil)

;; ## Utility functions

;; Allow write operations on various data types
(defprotocol ByteData (to-bytes ^bytes [this]))
(extend-protocol ByteData
  Sequential (to-bytes [this] (byte-array (map byte this)))
  Number (to-bytes [this] (byte-array 1 (byte this)))
  String (to-bytes [this] (.getBytes this))
  Object (to-bytes [this] this))

(defn read-stream
  "Returns the sequence of available bytes from the input stream"
  [^InputStream in]
  (doall (repeatedly (.available in) #(.read in))))

(defn- err
  "Convience function for throwing exceptions"
  [msg & params]
  (throw (Exception. (apply format msg params))))

;; ## Ports and listeners

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

(defn close
  "Closes a port and removes its event listener"
  [port]
  (doto (:device port)
    (.removeEventListener)
    (.close)))

;; Make port Closeable for 'with-open' support
(defrecord Port [path device handler]
  Closeable
  (close [this] (close this)))

(defn open
  "Opens a port with the specified baud rate. If the connected device will send
  data on its own, a handler function may be speficied, which takes an input
  stream as its lone argument."
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
         (err "'%s' is not defined. See function 'add-ports'." path))
       (catch PortInUseException e
         (err "'%s' is already in use." path))
       (catch UnsupportedCommOperationException e
         (err "'%s' does not support connections at %d baud." path baud)))))

;; ## Handlers

(defn on-data
  "Updates the handler to be called on each data received event"
  [port handler]
  (reset! (:handler port) handler))

;; ## Reading and writing data

(defn read
  "Performs a synchronous read from the port, blocking until data is received
  or the specified timeout (in milliseconds) has elapsed"
  [port timeout]
  (let [resp (promise)]
    (on-data port (fn [in] (deliver resp (read-stream in))))
    (deref resp timeout nil)))

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
