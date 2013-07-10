(ns serialio.core-test
  (:refer-clojure :exclude [read])
  (:require [clojure.test :refer :all]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [serialio.core :refer :all])
  (:import [java.io File]))

(def ports (atom nil))

;; The socat command used here creates two connected PTYs, outputs their
;; paths to stderr, and then waits for a command line interrupt. We need to
;; (a) background this process, (b) get the PTY paths it connects, and (c) get
;; its PID so we can shut it down.

;; The clojure.java.shell/sh function waits for the process to finish before
;; returning stdout and stderr, so we can't run the socat command directly;
;; instead we run it as a bash command. When we do this, though, (a) the bash
;; command (sometimes) returns before socat has written the PTY paths to stderr,
;; and (b) bash process doesn't return while its command's stdout/stderr are
;; sending it output. Both of these snags can be addressed by redirecting
;; command output to a temp file, waiting for it to be written, then reading it.
(defn pty-setup
  []
  (let [cmd  "socat -d -d pty,raw,echo=0 pty,raw,echo=0"
        tmp  (.getPath (doto (File/createTempFile "socat" ".log")
                         .deleteOnExit))
        proc (sh/sh "bash" "-c"
                    (str cmd " > " tmp " 2>&1 & echo $!"))]
    (Thread/sleep 100)
    {:pid  (str/trim (:out proc))
     :ptys (take 2 (map (comp last #(str/split % #" "))
                        (str/split-lines (slurp tmp))))
     :baud 115200}))

(defn pty-fixture
  [f]
  (let [{:keys [pid ptys baud] :as socat} (pty-setup)]
    (println socat)
    (try
      (do
        (apply add-ports ptys)
        (with-open [pty1 (open (first ptys) baud)
                    pty2 (open (second ptys) baud)]
          (reset! ports [pty1 pty2])
          (f)))
      (finally (sh/sh "kill" pid)))))

(use-fixtures :once pty-fixture)

(deftest echo
  (let [[master slave] @ports]
    (on-data slave (fn [in]
                     (write slave (read-stream in))))
    (let [msg (.getBytes "Hi there!")
          resp (exec master msg)]
      (println "Sent:     " (seq msg))
      (println "Received: " resp)
      (is (= (seq msg)
             (seq (exec master msg)))))))


(comment
  ;; Some manual tests...

  ;; Open a PTY connected to a terminal stdio:
  ;;  socat -d -d -,raw,echo=0,escape=0x0f pty,raw,echo=0

  (def path "/dev/pts/6")
  (add-port-id path)

  (def port (open path 115200)) ; baud for pty
  (close port)

  (write port (.getBytes "Helloooo!\n"))
  (println (exec port (.getBytes "Say something!\n")))

  (println (read port 6000))
  )
