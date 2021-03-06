(ns nbtdb.core
  (:gen-class)
  (:require
   [clojure.java.io :as io]
   [clojure.tools.cli :refer [parse-opts]]
   [clojure.string :as str])
  (:import
   (java.nio ByteBuffer)
   java.util.zip.GZIPInputStream
   java.util.zip.GZIPOutputStream))

(def cli-options
  [["-h" "--help" "Display help"]
   ["-i" "--interactive" "Interactive shell"]
   ["-e" "--exec CMD" "Execute command"
    :multi true
    :default []
    :update-fn conj]])

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
  (printf "%g / %a\n" number number))

(declare load-value save-value load-named-tag save-named-tag value-loader value-saver find-nbt-type NBT-Types)

(defn array-loader
  [array-maker value-getter value-setter nbt-type]
  (fn [^ByteBuffer stream]
    (let [l (.getInt stream) data (array-maker l)]
      (dotimes [n l]
        (value-setter data n (value-getter stream)))
      (with-meta (vec data) {:list false :nbt nbt-type}))))

(defn array-saver
  [data value-putter nbt-type]
    (fn [^ByteBuffer stream]
      (.writeInt stream (alength data))
      (.writeByte stream nbt-type)
      (doseq [v data] (value-putter stream v))))

(defn load-nbt-byte-array
  [^ByteBuffer stream]
  (let [l (.getInt stream) data (byte-array l)]
    (.get stream data)
    data))

(defn save-nbt-byte-array
  [^ByteBuffer stream v]
  (.writeInt stream (alength v))
  (.write stream v 0 (alength v)))

(defn save-nbt-string
  [^ByteBuffer stream v]
  (.writeShort stream (.length v))
  (.writeBytes stream v))

(defn load-nbt-string
  [^ByteBuffer stream]
  (let [l (.getShort stream) data (byte-array l)]
    (.get stream data)
    (String. data)))

(defn save-list
  [^ByteBuffer stream data]
  (let [t (:nbt (meta data)) saver (value-saver t) l (count data)]
    (.writeByte stream t)
    (.writeInt stream l)
    (doseq [v data] (saver stream v))))

(defn load-list [^ByteBuffer stream]
  (let [t (.get stream) loader (value-loader t) l (.getInt stream) s (repeatedly l #(loader stream))]
    (with-meta (vec s) {:list true :nbt t})))

(defn load-compound [^ByteBuffer stream]
  (loop [values {}]
    (let [[t name value] (load-named-tag stream)]
      (if (> t 0)
        (recur (assoc values name value))
        values))))

(defn save-compound
  [^ByteBuffer stream data]
  (doseq [[k v] (seq data)]
    (save-named-tag stream k v))
  (.writeByte stream 0))

(defn load-named-tag
  [^ByteBuffer stream]
  (let
   [type (.get stream)]
    (if (> type 0)
      [type (load-nbt-string stream) (load-value type stream)]
      [type "" nil])))

(defn save-named-tag
  [^ByteBuffer stream name v]
  (let [t (find-nbt-type v)]
    (.writeByte stream t)
    (when (> t 0)
      (save-nbt-string stream name)
      (save-value t stream v))))

(declare value-printer)

(defn print-compound
  [compound prefix maxdepth]
  (let [ks (keys compound) vs (vals compound) subprefix (str prefix "??? ") n (count ks)]
    (printf "compound[%d]\n" n)
    (when (and (> n 0) (> maxdepth 0))
      (loop [ks ks vs vs]
        (let [[k & nk] ks [v & nv] vs]
          (if (empty? nk)
            (do
              (printf "%s???%s: " prefix k)
              (value-printer v (str prefix "  ") (dec maxdepth)))
            (do
              (printf "%s???%s: " prefix k)
              (value-printer v subprefix (dec maxdepth))
              (recur nk nv))))))))

(defn print-list
  [list prefix maxdepth]
  (let [t (:nbt (meta list)) list? (:list (meta list)) subprefix (str prefix "??? ") n (count list)]
    (printf "%s[%d] %s\n" (if list? "list" "array") n (.name (get NBT-Types (int t))))
    (when (and (> n 0) (> maxdepth 0))
      (loop [list list k 0]
        (let [[v & list] list]
          (if (empty? list)
            (do
              (printf "%s???[%d]: " prefix k)
              (value-printer v (str prefix "  ") (dec maxdepth)))
            (do
              (printf "%s???[%d]: " prefix k)
              (value-printer v subprefix (dec maxdepth))
              (recur list (inc k)))))))))

(defn print-string
  [s]
  (printf "\"%s\"\n" s))

(defn value-printer
  [v prefix maxdepth]
  (cond
    (map? v) (print-compound v prefix maxdepth)
    (vector? v) (print-list v prefix maxdepth)
    (float? v) (print-float v)
    (= (type v) java.lang.Byte) (print-byte v)
    (number? v) (print-number v)
    (string? v) (print-string v)
    :else (println "unknown type" (type v))))

(defn find-nbt-type
  "Find the NBT type of an item"
  [data]
  (cond
    (nil? data) 0
    (string? data) 8
    (map? data) 10
    (vector? data) (if (:list (meta data)) 9 (case (:nbt (meta data)) 1 7 3 11 4 12))
    (double? data) 6
    (float? data) 5
    ; but how to disambiguate byte/short/int/long
    (= (type data) java.lang.Byte) 1
    (= (type data) java.lang.Short) 2
    (= (type data) java.lang.Integer) 3
    (= (type data) java.lang.Long) 4
    ))

(deftype NBT-Type [name load save print])

(def NBT-Types
  [; 0 end
   (NBT-Type.
     "end"
    (fn [^ByteBuffer _] nil)
    (fn [^ByteBuffer _ _] nil)
    (fn [] (print "end")))
   ; 1 byte
   (NBT-Type.
    "byte"
    (fn [^ByteBuffer stream] (.get stream))
    (fn [^ByteBuffer stream v] (.writeByte stream v))
    print-byte)
   ; 2 short
   (NBT-Type.
    "short"
    (fn [^ByteBuffer stream] (.getShort stream))
    (fn [^ByteBuffer stream v] (.writeShort stream v))    
    print-number)
   ; 3 int
   (NBT-Type.
    "int"
    (fn [^ByteBuffer stream] (.getInt stream))
    (fn [^ByteBuffer stream v] (.writeInt stream v))
    print-number)
   ; 4 long
   (NBT-Type.
    "long"
    (fn [^ByteBuffer stream] (.getLong stream))
    (fn [^ByteBuffer stream v] (.writeLong stream v))      
    print-number)
   ; 5 float
   (NBT-Type.
    "float"
    (fn [^ByteBuffer stream] (.getFloat stream))
    (fn [^ByteBuffer stream v] (.writeFloat stream v))
    print-float)
   ; 6 double
   (NBT-Type. 
    "double"
    (fn [^ByteBuffer stream] (.getDouble stream))
    (fn [^ByteBuffer stream v] (.writeDouble stream v))    
    print-float)
   ; 7 byte-array
   (NBT-Type. 
    "byte-array" 
    load-nbt-byte-array
    save-nbt-byte-array
    print-list)
   ; 8 string
   (NBT-Type.
    "string"
    load-nbt-string
    save-nbt-string
    print-string)
   ; 9 list
   (NBT-Type.
    "list"
    load-list
    save-list
    print-list)
   ; 10 compound
   (NBT-Type.
    "compound"
    load-compound
    save-compound
    print-compound)
   ; 11 int-array
   (NBT-Type. 
    "int-array" 
    (array-loader int-array #(.getInt %) aset-int 3)
    (array-saver int-array #(.writeInt %1 %2) 4)
    print-list)
   ; 12 long-array
   (NBT-Type. 
    "long-array" 
    (array-loader long-array #(.getLong %) aset-long 4)
    (array-saver long-array #(.writeLong %1 %2) 4)
    print-list)])

(defn value-loader [t] (let [nbt (get NBT-Types (int t))] (.load nbt)))
(defn value-saver [t] (let [nbt (get NBT-Types (int t))] (.save nbt)))

(defn load-value
  [t ^ByteBuffer stream]
  (let [nbt (get NBT-Types (int t)) load (.load nbt)]
    (load stream)))

(defn save-value
  [t ^ByteBuffer stream v]
  (let [nbt (get NBT-Types (int t)) save (.save nbt)]
    (save stream v)))

(defn load-nbt-file [name]
  (let [data (with-open [gz (-> name io/input-stream GZIPInputStream.)
                         output (new java.io.ByteArrayOutputStream)]
               (io/copy gz output)
               (. ByteBuffer (wrap (.toByteArray output))))
        [_ _ value] (load-named-tag data)]
    value))

(defn save-nbt-file [name data]
  (with-open [output (-> name io/output-stream GZIPOutputStream. java.io.DataOutputStream.)]
    (save-named-tag output "" data)))

(defn state-from-data [data]
  {:tree data
   :nodes (list data)
   :path []})

; parse-words divides a line on spaces, but allows quoting; double quotes
; prevent spaces from breaking words, backslashes prevent anything from
; anything.
(defn parse-words
  "divides a line on spaces, yielding space-separated words, but supporting
  double-quote and backslash"
  [original]
  (loop [input original current nil words [] quoting false backslash false]
    (if (empty? input)
      (cond
        backslash [nil "unterminated backslash"]
        quoting [nil "unterminated quote"]
        :else [(if current (conj words current) words) nil])
      (let [c (get input 0) input (subs input 1)]
        (cond
          backslash (recur input (str current c) words quoting false)
          (= c \") (recur input current words (not quoting) backslash)
          (= c \\) (recur input current words quoting true)
          (and (Character/isSpace c) (not quoting)) (recur input nil (if current (conj words current) words) quoting backslash)
          :else (recur input (str current c) words quoting backslash))))))

(defn cd-node-path [state node path]
  (-> state (update :nodes conj node) (update :path conj path)))

(defn cd-in-vector
  "tries to cd into an array or list within val"
  [state path]
  (let [index (if (string? path) (parse-long path) path) node (get (first (:nodes state)) index)]
    (cond
      node (cd-node-path state node index)
      :else (do (println "path not found") state))))

(defn cd-in-compound
  "tries to cd into a compound within val"
  [state path]
  (let [node (get (first (:nodes state)) path)]
    (cond
      node (cd-node-path state node path)
      :else (do (println "path not found") state))))

(defn cd-to-path
  "yields a possibly-updated state and may print an error"
  [state path]
  (let [node (first (:nodes state))]
    (cond
      (= path "..")
      (if (> (count (:path state)) 0)
        (assoc state :nodes (rest (:nodes state)) :path (butlast (:path state)))
        state)

      (map? node)
      (cd-in-compound state path)

      (vector? node)
      (cd-in-vector state path)

      :else
      (do (println "not on a list/array/compound") state))))

(defn cmd-cd [state _ args]
  (cond
    (> 1 (count args)) (do (println "too many args") state)
    (= 0 (count args)) (do (println "need an arg") state)
    :else (let [path (first args)]
            (cd-to-path state path))))

(defn cmd-load [state _ args]
  (if (= (count args) 1)
  (state-from-data (load-nbt-file (first args)))
    (do (println "usage: load <file>") state)))

(defn cmd-save [state _ args]
  (if (= (count args) 1)
    (save-nbt-file (first args) (:tree state))
    (do (println "usage: save <file>") state)))

; a command has a name, a parse-opts style option list, and a function.
(defrecord command [opts func])

(defn ro-cmd [opts func]
  (->command opts (fn [state o a] (func state o a) state)))

(defn cmd-ls [state _ _]
  (value-printer (first (:nodes state)) "" 1))

(defn cmd-pwd [state _ _]
  (print "/")
  (println (str/join "/" (:path state)))
  (println "node len" (count (:nodes state)))
  (println "current node" (first (:nodes state))))

(defn cmd-error [state name]
  (println "unknown command:" name)
  state)

(defn cmd-show [state opts _]
  (value-printer (first (:nodes state)) "" (:depth opts)))

(defn cmd-cmp [state _ args]
  (let [other (load-nbt-file (first args))]
    (if (= (:tree state) other)
      (println "same")
      (println "different"))))

(defn rm-in-vector
  "tries to remove a path from an array or list within val"
  [node path]
  (let [index (parse-long path) md (meta node)]
    (if (> (count node) index -1)
        (with-meta (into (subvec node 0 index) (subvec node (inc index))) md)
        (do (printf "%d out of range\n" index) node))))

(defn rm-in-compound
  "tries to remove a path from a compound within val"
  [node path]
  (if (contains? node path)
    (dissoc node path)
    (do (printf "no entry %s\n" path) node)))

(defn rm-node
  "remove an entry from a node"
  [node path]
  (cond
     (map? node)
     (rm-in-compound node path)

     (vector? node)
     (rm-in-vector node path)

     :else
     (do (println "not on a list/array/compound:" (type node)) node)))

(defn refollow-path [tree path]
  (reduce cd-to-path (state-from-data tree) path))

(defn do-rm [state arg]
  (let [removed (rm-node (first (:nodes state)) arg)
        tree (update-in (:tree state) (:path state) (fn [_] removed))]
        (refollow-path tree (:path state))))

(defn cmd-rm [state _ args]
  (if (= (count args) 1)
    (do-rm state (first args))
    (println "usage: rm <node>")))

(def commands {"cd" (->command [] cmd-cd)
               "rm" (->command [] cmd-rm)
               "ls" (ro-cmd [] cmd-ls)
               "pwd" (ro-cmd [] cmd-pwd)
               "load" (->command [] cmd-load)
               "save" (ro-cmd [] cmd-save)
               "cmp" (ro-cmd [] cmd-cmp)
               "show" (ro-cmd [["-d" "--depth CMD" "max depth" :default 99 :parse-fn #(Integer/parseInt %)]] cmd-show)})

(defn parse [input]
  (let [[words error] (parse-words input)]
    (cond
      error (fn [state] (println "error:" error) state)
      (empty? words) (fn [state] state)
      :else (let [[cmd-name & args] words cmd (get commands cmd-name)]
              (if cmd
                (let [cli-data (parse-opts args (get cmd :opts)) {opts :options args :arguments} cli-data]
                  (fn [state] ((get cmd :func) state opts args)))
                (fn [state] (cmd-error state cmd-name)))))))

(defn process [state input]
  (let [op (parse input)]
    (op state)))

(defn run-nbt-shell [state]
  (loop [state state]
    (print "> ")
    (flush)
    (let [input (read-line)]
      (if input
        (recur (process state input))
        (println "Goodbye.")))))

(defn run-nbt-cmds [cmds state]
  (when (first cmds) (recur (next cmds) (process state (first cmds)))))

(defn -main
  [& args]
  (let [cli-data (parse-opts args cli-options) {opts :options files :arguments} cli-data nfiles (count files)]
    (cond
      (or (:help opts) (not (== 1 nfiles))) (println "usage: nbtdb [-i] file.nbt")
      :else (let [nbt-data (state-from-data (load-nbt-file (get files 0)))]
              (cond
                (:interactive opts) (run-nbt-shell nbt-data)
                (> (count (:exec opts)) 0) (run-nbt-cmds (:exec opts) nbt-data)
                :else (value-printer (:tree nbt-data) "" 99))))))
