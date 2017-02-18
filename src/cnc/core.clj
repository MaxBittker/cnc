(ns cnc.core
  (:gen-class)
  (require
    [clojure.string :as str]))

(def size 100)
(def file-name "out.2obj")

(defn write-out [data]
  (do
    (println "writing to: " file-name)
    (spit file-name data)))


(def vlines
  (partition 2
    (apply concat
      (map
        (fn [y] [[y 0] [y size]])
        (range 10 size 10)))))

(def hlines
    (map
      (fn [[[ax ay] [bx by]]]
       [[ay ax] [by bx]])
      vlines))

(def grid
  (apply concat
    (map vector hlines vlines)))

(defn sorted-set-to-vstring [s]
  (str/join "\n"
    (map
      (fn [[x y]] (str "v " x " " y))
     s)))

(defn point-to-index [pmap point]
  (get pmap point))

(defn line-sec [pmap line]
  (let [indexes (map
                  (fn [point] (get pmap point))
                  line)]
    (str "e " (first indexes) "\n"
         "l " (str/join " " indexes))))


(defn lines-to-2obj [lines]
  (let [points (apply sorted-set (apply concat lines))
        vsec (sorted-set-to-vstring points)
        pmap (zipmap points (iterate inc 1))
        lsecs (str/join "\n" (map #(line-sec pmap %) lines))]
    (str vsec "\n" lsecs)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (do
    (println "starting:")
    (let [data (lines-to-2obj grid)]
      (write-out data))))
