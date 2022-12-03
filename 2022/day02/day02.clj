(ns day02)

(import '(java.io BufferedReader))
(require '[clojure.string :as str])
(require '[clojure.main :as main])
(require '[clojure.set :as set])

(def BASE-MOVES
  {"A" :rock,
   "B" :paper,
   "C" :scissors})

(def MOVES1
  (merge BASE-MOVES
         {"X" :rock,
          "Y" :paper,
          "Z" :scissors}))

(def POINTS
  {:rock 1,
   :paper 2,
   :scissors 3})

(def POSSIBLE (set (keys POINTS)))

(def RULES
  {#{:rock :paper} :paper,
   #{:rock :scissors} :rock,
   #{:paper :scissors} :scissors})

(def RULES-REV
  (set/map-invert RULES))

(defn lookup-moves1 [game]
  (map #(MOVES1 %) game))

(defn lookup-moves2 [[opponent outcome]]
  (def o (MOVES1 opponent))
  (def ret
    [o (if (= "Y" outcome)
         o
         (let [to-win (first (set/difference (RULES-REV o) (set [o])))]
           (if (= "X" outcome)
             to-win
             (first (set/difference POSSIBLE (set [o to-win]))))))])
  ;; (prn ret)
  ret)

(defn score-game [game]
  (let [s (set game)]
    (+ (POINTS (second game))
       (cond (= (count s) 1) 3
             (= (RULES s) (second game)) 6
             true 0))))

(def games
  (with-open [rdr (clojure.java.io/reader (first *command-line-args*))]
    (->> rdr
         line-seq
         (map #(str/split % #" "))
         (into []))))

(defn play [games lookup-moves]
  (->> games
       (map lookup-moves)
       (map score-game)
       (reduce +)))

(prn (play games lookup-moves1))

(prn (play games lookup-moves2))
