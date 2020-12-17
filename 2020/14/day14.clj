(ns day14)

(defn parse-line [line]
  (if-let [m (re-matches #"mask = ([X01]+)" line)]
    (list "mask" (second m))
    (if-let [m (re-matches #"mem\[([0-9]+)\] = ([0-9]+)" line)]
      (list "mem" (map #(Integer/parseInt %) (rest m))))))

(defn part1 [instructions]
  (defn apply-mask [n mask]
    (reduce
     (fn [integer bit]
       ;; (println integer bit)
       (let [idx (first bit)
             what (second bit)]
         (case what
           \0 (bit-clear integer idx)
           \1 (bit-set integer idx)
           \X (if (bit-test integer idx) (bit-set integer idx) (bit-clear integer idx)))))
     n (map vector (range) (reverse mask))))
  (reduce + (first (reduce
                     (fn [acc x]
                       (let [memory (first acc)
                             mask (second acc)]

                         (case (first x)
                           "mask" (list memory (second x))
                           "mem" (let [address  (first (second x))
                                       value (second (second x))]
                                   (list (assoc memory address (apply-mask value mask)) mask)))))
                     (list (vec (repeat 100000 0)) nil) instructions))))

(def instructions
  (map parse-line (line-seq (java.io.BufferedReader. *in*))))

;; (doseq [i instructions]
  ;; (println i))
(println (part1 instructions))

;; Local Variables:
;; compile-command: "clojure-1.8 day14.clj < sample.txt"
;; End:
