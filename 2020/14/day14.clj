(ns day14)

(require '[clojure.pprint :refer (cl-format)])

(defn dbg [x]
  ;; (println (let [ss (.toString x)]
  ;;            (subs ss 0 (min (count ss) 500))))
  x)

(defn parse-line [line]
  (if-let [m (re-matches #"mask = ([X01]+)" line)]
    (list "mask" (second m))
    (if-let [m (re-matches #"mem\[([0-9]+)\] = ([0-9]+)" line)]
      (list "mem" (map #(Integer/parseInt %) (rest m))))))

(defn read-instructions [input-buffer]
  (map parse-line (line-seq (java.io.BufferedReader. input-buffer))))

(defn part1 [instructions]
  (defn apply-mask [n mask]
    (defn fiddle-bit [integer [idx what]]
      (case what
        \0 (bit-clear integer idx)
        \1 (bit-set integer idx)
        \X (if (bit-test integer idx)
             (bit-set integer idx)
             (bit-clear integer idx))))
    (->> mask
         reverse
         (map vector (range))
         (reduce fiddle-bit n)))
  (defn step [[memory mask] [what payload]]
    (case what
      "mask" (list memory payload)
      "mem" (let [[address value] payload]
              (list (assoc memory address (apply-mask value mask))
                    mask))))
  (->> instructions
       (reduce step (list {} nil))
       ;; dbg
       first
       vals
       (reduce +)))

(defn number->binary-string [n padding]
  (cl-format nil (format "~%d,'0',B" padding) n))

(defn possible-addresses [address]
  (defn f [[s filler] c]
    (if (= \X c)
      [(str s (first filler)) (rest filler)]
      [(str s c) filler]))
  (let [num-x (count (filter #(= \X %) address))]
    (for [i (range (Math/pow 2 num-x))]
      (as-> address v
        (reduce f ["" (number->binary-string i num-x)] v)
        (first v)
        (Integer/parseInt v 2)))))

(defn part2 [instructions]
  (defn apply-mask [n mask]
    (defn fiddle-bit [integer [idx what]]
      (case what
        \0 (bit-clear integer idx)
        \1 (bit-set integer idx)
        \X (if (bit-test integer idx)
             (bit-set integer idx)
             (bit-clear integer idx))))
    (->> mask
         reverse
         (map vector (range))
         (reduce fiddle-bit n)))
  (defn step [[memory mask] [what payload]]
    (case what
      "mask" (list memory payload)
      "mem" (let [[address value] payload
                  straddr (number->binary-string address (count mask))
                  plugged (dbg (reduce str (map (fn [[a b]] (if (or (= \1 b) (= \X b)) b a)) (map vector straddr mask))))
                  possible (possible-addresses plugged)]
              (dbg (list (reduce (fn [m p] (assoc m p (dbg value))) memory possible) mask)))))
  (->> instructions
       (reduce step (list {} nil))
       dbg
       first
       vals
       (reduce +)))

(def instructions (read-instructions *in*))
(println (part1 instructions))
(println (part2 instructions))

;; Local Variables:
;; compile-command: "clojure-1.8 day14.clj < sample.txt"
;; End:
