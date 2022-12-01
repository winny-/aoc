(ns day01)

(import '(java.io BufferedReader))
(import '(java.lang Integer))

(defn map-parseInt [xs]
  (map #(Integer/parseInt %) xs))

(def food-by-elf
  (map map-parseInt
       (filter #(not= [""] %)
               (map #(into [] %)
                    (partition-by empty?
                                  (line-seq (java.io.BufferedReader. *in*)))))))

(def calories-by-elf
  (map #(reduce + %) food-by-elf))

(prn (reduce max calories-by-elf))
(prn (reduce + (take 3 (sort > calories-by-elf))))
