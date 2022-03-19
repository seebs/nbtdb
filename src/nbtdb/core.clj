(ns nbtdb.core
  (:gen-class)
  (:require
   [clojure.java.io :as io])
  (:import
   (java.nio.channels FileChannel)
   (java.nio ByteBuffer)
   (java.nio.file StandardOpenOption)
   java.util.zip.GZIPInputStream
   java.util.zip.GZIPOutputStream))

(defn load-nbt-string
  [^ByteBuffer stream]
  (let [l (.getShort stream) data (byte-array l)]
    (.get stream data)
    (String. data)))

(defn load-nbt-array [f ^ByteBuffer stream t]
  (let [l (.getInt stream) s (repeatedly l f)]
    (doall s)
    (with-meta s {:list false :nbt t})))

(defn load-nbt-byte-array
  [^ByteBuffer stream]
  (let [l (.getInt stream) data (byte-array l 1)]
    (.get stream data)
    data))

(defn load-int-array
  [^ByteBuffer stream]
  (load-nbt-array #(.getInt stream) stream 3))

(defn load-long-array
  [^ByteBuffer stream]
  (load-nbt-array #(.getLong stream) stream 4))

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

(defn value-loader
  [type]
  (condp = type
    1 #(.get %)
    2 #(.getShort %)
    3 #(.getInt %)
    4 #(.getLong %)
    5 #(.getFloat %)
    6 #(.getDouble %)
    7 load-nbt-byte-array
    8 load-nbt-string
    9 load-list
    10 load-compound
    11 load-int-array
    12 load-long-array
    #(prn "oops" type %)))

(defn load-value
  [t ^ByteBuffer stream]
  (let [v ((value-loader t) stream)]
    v))

(defn load-named-tag
  [^ByteBuffer stream]
  (let
   [type (.get stream)]
    (if (> type 0)
      [type (load-nbt-string stream) (load-value type stream)]
      [type "" nil])))

(declare value-printer)

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

(defn print-compound
  [compound prefix]
  (let [k (keys compound) v (vals compound) subprefix (str prefix "│ ") n (count k)]
    (printf "compound[%d]\n" n)
    (loop [k k v v n n]
      (if (> n 1)
        (do
          (printf "%s├%s: " prefix (first k))
          (value-printer (first v) subprefix)
          (recur (rest k) (rest v) (dec n)))
        (when (> n 0)
          (do
            (printf "%s└%s: " prefix (first k))
            (value-printer (first v) (str prefix "  "))))))))

(defn print-list
  [list prefix]
  (let [t (:nbt (meta list)) list? (:list (meta list)) subprefix (str prefix "│ ") n (count list)]
    (printf "%s[%d] %d\n" (if list? "list" "array") n t)
    (loop [list list n n]
      (if (> n 1)
        (do
          (printf "%s├" prefix)
          (value-printer (first list) subprefix)
          (recur (rest list) (dec n)))
        (when (> n 0)
          (do
            (printf "%s└" prefix)
            (value-printer (first list) (str prefix "  ")) (str prefix "  ")))))))

(defn print-string
  [s]
  (printf "\"%s\"\n" s))

(defn value-printer
  [v prefix]
  (condp = (type v)
    clojure.lang.PersistentVector (println prefix "list or array")
    clojure.lang.PersistentArrayMap (print-compound v prefix)
    clojure.lang.PersistentHashMap (print-compound v prefix)
    clojure.lang.LazySeq (print-list v prefix)
    java.lang.Byte (print-byte v)
    java.lang.Short (print-number v)
    java.lang.Integer (print-number v)
    java.lang.Long (print-number v)
    java.lang.Float (print-float v)
    java.lang.Double (print-float v)
    java.lang.String (print-string v)
    (println "unknown type" (type v))))

(defn -main
  [& args]
  (let [data (with-open [gz (-> "resources/sample.dat" io/input-stream GZIPInputStream.)
                         output (new java.io.ByteArrayOutputStream)]
               (io/copy gz output)
               (. ByteBuffer (wrap (.toByteArray output))))]
    (let [[_ _ value] (load-named-tag data)]
      (value-printer value ""))))
