(ns nbtdb.core
  (:gen-class)
  (:require
   [clojure.java.io :as io]
   [clojure.tools.cli :refer [parse-opts]]
   [clojure.string :as str])
  (:import
   (java.nio.channels FileChannel)
   (java.nio ByteBuffer)
   (java.nio.file StandardOpenOption)
   java.util.zip.GZIPInputStream
   java.util.zip.GZIPOutputStream))

(def cli-options
  [["-h" "--help" "Display help"]
   ["-i" "--interactive" "Interactive shell"]])

(defn print-byte
  [byte]
  (if (< 31 byte 127)
    (printf "0x%02x '%c'\n" byte byte)
    (printf "0x%02x\n" byte)))

(defn print-number
  [number]
  (printf "0x%x / %d\n" number number))

(defn print-float
  [number]
  (printf "%a / %g\n" number number))

(defn array-loader
  [array-maker value-getter value-setter nbt-type]
  (fn [^ByteBuffer stream]
    (let [l (.getInt stream) data (array-maker l)]
      (dotimes [n l]
        (value-setter data n (value-getter stream)))
      (with-meta (vec data) {:list false :nbt nbt-type}))))

(defn load-nbt-byte-array
  [^ByteBuffer stream]
  (let [l (.getInt stream) data (byte-array l)]
    (.get stream data)
    data))

(defn load-nbt-string
  [^ByteBuffer stream]
  (let [l (.getShort stream) data (byte-array l)]
    (.get stream data)
    (String. data)))

(declare load-value load-named-tag value-loader)

(defn load-list [^ByteBuffer stream]
  (let [t (.get stream) loader (value-loader t) l (.getInt stream) s (repeatedly l #(loader stream))]
    (doall s)
    (with-meta s {:list true :nbt t})))

(defn load-compound [^ByteBuffer stream]
  (loop [values {}]
    (let [[t name value] (load-named-tag stream)]
      (if (> t 0)
        (recur (assoc values name value))
        values))))

(defn load-named-tag
  [^ByteBuffer stream]
  (let
   [type (.get stream)]
    (if (> type 0)
      [type (load-nbt-string stream) (load-value type stream)]
      [type "" nil])))

(declare value-printer)

(defn print-compound
  [compound prefix maxdepth]
  (let [ks (keys compound) vs (vals compound) subprefix (str prefix "│ ") n (count ks)]
    (printf "compound[%d]\n" n)
    (when (and (> n 0) (> maxdepth 0))
      (loop [ks ks vs vs]
        (let [k (first ks) v (first vs) nk (rest ks) nv (rest vs)]
          (if (empty? nk)
            (do
              (printf "%s└%s: " prefix k)
              (value-printer v (str prefix "  ") (dec maxdepth)))
            (do
              (printf "%s├%s: " prefix k)
              (value-printer v subprefix (dec maxdepth))
              (recur nk nv))))))))

(defn print-list
  [list prefix maxdepth]
  (let [t (:nbt (meta list)) list? (:list (meta list)) subprefix (str prefix "│ ") n (count list)]
    (printf "%s[%d] %d\n" (if list? "list" "array") n t)
    (when (and (> n 0) (> maxdepth 0))
      (loop [list list]
        (let [v (first list) list (rest list)]
          (if (empty? list)
            (do
              (printf "%s└" prefix)
              (value-printer v (str prefix "  ") (dec maxdepth)))
            (do
              (printf "%s├" prefix)
              (value-printer v subprefix (dec maxdepth))
              (recur list))))))))

(defn print-string
  [s]
  (printf "\"%s\"\n" s))

(defn value-printer
  [v prefix maxdepth]
  (condp = (type v)
    clojure.lang.PersistentVector (print-list v prefix maxdepth)
    clojure.lang.PersistentArrayMap (print-compound v prefix maxdepth)
    clojure.lang.PersistentHashMap (print-compound v prefix maxdepth)
    clojure.lang.LazySeq (print-list v prefix maxdepth)
    java.lang.Byte (print-byte v)
    java.lang.Short (print-number v)
    java.lang.Integer (print-number v)
    java.lang.Long (print-number v)
    java.lang.Float (print-float v)
    java.lang.Double (print-float v)
    java.lang.String (print-string v)
    (println "unknown type" (type v))))

(deftype NBT-Type [name load print])

(def NBT-Types
  [; 0 end
   (NBT-Type. "end" (fn [^ByteBuffer stream] nil) (fn [] (print "end")))
   ; 1 byte
   (NBT-Type. "byte" (fn [^ByteBuffer stream] (.get stream))
              print-byte)
   ; 2 short
   (NBT-Type. "short" (fn [^ByteBuffer stream] (.getShort stream))
              print-number)
   ; 3 int
   (NBT-Type. "int" (fn [^ByteBuffer stream] (.getInt stream))
              print-number)
   ; 4 long
   (NBT-Type. "long" (fn [^ByteBuffer stream] (.getLong stream))
              print-number)
   ; 5 float
   (NBT-Type. "float" (fn [^ByteBuffer stream] (.getFloat stream))
              print-float)
   ; 6 double
   (NBT-Type. "double" (fn [^ByteBuffer stream] (.getDouble stream))
              print-float)
   ; 7 byte-array
   (NBT-Type. "byte-array" load-nbt-byte-array print-list)
   ; 8 string
   (NBT-Type. "string" load-nbt-string print-string)
   ; 9 list
   (NBT-Type. "list" load-list print-list)
   ; 10 compound
   (NBT-Type. "compound" load-compound print-compound)
   ; 11 int-array
   (NBT-Type. "int-array" (array-loader int-array #(.getInt %) aset-int 3) print-list)
   ; 12 long-array
   (NBT-Type. "long-array" (array-loader long-array #(.getLong %) aset-long 4) print-list)])

(defn value-loader [t] (let [nbt (get NBT-Types (int t))] (.load nbt)))

(defn load-value
  [t ^ByteBuffer stream]
  (let [nbt (get NBT-Types (int t))]
    (let [load (.load nbt)] (load stream))))

(defn load-nbt-file [name]
  (let [data (with-open [gz (-> name io/input-stream GZIPInputStream.)
                         output (new java.io.ByteArrayOutputStream)]
               (io/copy gz output)
               (. ByteBuffer (wrap (.toByteArray output))))
        [_ _ value] (load-named-tag data)]
    value))

; parse-words divides a line on spaces, but allows quoting; double quotes
; prevent spaces from breaking words, backslashes prevent anything from
; anything. state:
; 0 unquoted
; 1 quoted
; 2 backslash-unquoted
; 3 backslash-quoted
(defn parse-words [original]
  (loop [input original current nil words [] state 0]
    (if (empty? input)
      (condp < state
        1 [nil "unterminated backslash"]
        0 [nil "unterminated quote"]
        [(if current (conj words current) words) nil])
      (let [c (get input 0) input (subs input 1)]
        (cond
          (>= state 2) (recur input (str current c) words (- state 2))
          (= c \") (recur input current words (bit-xor state 1))
          (= c \\) (recur input current words (bit-or state 2))
          (and (= c \space) (= state 0)) (recur input nil (if current (conj words current) words) state)
          :else (recur input (str current c) words state))))))

(defn parse [input]
  (let [[words error] (parse-words input)]
    (cond
      error (fn [value] (println "error:" error) value)
      (empty? words) (fn [value] value)
      :else (let [cmd (first words) args (next words)]
              (condp = cmd
                "ls" (fn [value] (value-printer value "" 1) value)
                (fn [value] (println "cmd:" cmd "args:" (str/join ", " args))
                  value))))))

(defn process [value input]
  (let [op (parse input)]
    (op value)))

(defn run-nbt-shell [data]
  (loop [value data]
    (print "> ")
    (flush)
    (let [input (read-line)]
      (if input
        (recur (process value input))
        (println "Goodbye.")))))

(defn -main
  [& args]
  (let [cli-data (parse-opts args cli-options) opts (:options cli-data) files (:arguments cli-data) nfiles (count files)]
    (cond
      (or (:help opts) (not (== 1 nfiles))) (println "usage: nbtdb [-i] file.nbt")
      :else (let [nbt-data (load-nbt-file (get files 0))]
              (cond
                (:interactive opts) (run-nbt-shell nbt-data)
                :else (value-printer nbt-data "" 99))))))
