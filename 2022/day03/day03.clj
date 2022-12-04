(ns day03)

(import '(java.io BufferedReader))
(require '[clojure.string :as str])
(require '[clojure.main :as main])
(require '[clojure.set :as set])

(defn split-backpack [backpack]
  (->> backpack
       (split-at (quot (count backpack) 2))
       (map #(into [] %))
       (into [])))

(def backpacks
  (with-open [rdr (clojure.java.io/reader (first *command-line-args*))]
    (->> rdr
         line-seq
         (into []))))

(defn find-common-item [splitted]
  (->> splitted
       (map set)
       (apply set/intersection)
       first))

(defn lookup [ch]
  (def i (int ch))
  (+ 1
     (if (< i (int \a))
       (+ 26 (- i (int \A)))
       (- i (int \a)))))

(prn (->> backpacks
          (map split-backpack)
          (map find-common-item)
          (map lookup)
          (reduce +)))

(defn my-chunk [num xs]
  (loop [acc '()
         remaining xs]
    (if (empty? remaining)
      (reverse acc)
      (recur (cons (take num remaining) acc)
             (drop num remaining)))))

(prn (->> backpacks
          (my-chunk 3)
          (map #(map set %))
          (map #(apply set/intersection %))
          (map first)
          (map lookup)
          (reduce +)))
