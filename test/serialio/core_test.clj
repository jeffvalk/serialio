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
  (assert (zero? (:exit (sh/sh "which" "socat")))
          "Tests require package 'socat'.")
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

(use-fixtures :each pty-fixture)

(defn- rand-bytes [n] (repeatedly n #(rand-int 128)))

(deftest master-slave
  (let [[master slave] @ports
        exec (partial exec master)
        echo (partial write slave)]
    (on-bytes slave echo)
    (testing "Master/slave ports"
      (testing "with a long message"
        (let [m (rand-bytes 2048)]
          (is (= m (exec m)))))
      (testing "read timeout with no data"
        (is (nil? (read master 100))))
      (testing "with varied data types"
        (let [l (rand-bytes 32), a (to-bytes l)
              v (vec l), s (String. a), n 42]
          (is (= l (exec l)) "list")
          (is (= v (exec v)) "vector")
          (is (= (seq a) (seq (exec a))) "bytes")
          (is (= s (String. (to-bytes (exec s)))) "string")
          (is (= n (first (exec n))) "number"))))))

(deftest bidirectional
  (let [[peer1 peer2] @ports
        echo (fn [port] (partial write port))]
    (on-bytes peer1 (echo peer1))
    (on-bytes peer2 (echo peer2))
    (testing "Bidirectional send and receive"
      (let [a (rand-bytes 128)
            b (rand-bytes 256)]
        (is (= a (exec peer1 a)))
        (is (= a (exec peer2 a)))
        (is (= b (exec peer1 b)))
        (is (= b (exec peer2 b)))))))

(deftest adding-ports
  (testing "Repeatedly add and open ports"
    (let [v :ok!
          f (->> (constantly v)
                 (partial pty-fixture)
                 (partial pty-fixture)
                 (partial pty-fixture))]
      (is (= v (f))))))


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
