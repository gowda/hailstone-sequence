(ns hailstone-sequence.core
  (:use [clojure.tools.cli :only (cli)]))

(defn hailstone [number]
  (let [next (fn [n]
               (if (even? n)
                 (/ n 2)
                 (+ 1 (* n 3))))]
    (if (= number 1)
      [number]
      (concat [number] (hailstone (next number))))))

(defn longest-until [limit]
  (reduce (fn [longest hail-s]
            (if (> (count hail-s)
                   (count longest))
              hail-s
              longest))
          '()
          (map hailstone (range 1 (inc limit)))))

(defn longest [l1 l2]
  (if (> (count l1)
         (count l2))
    l1
    l2))

(defn plongest-until [limit pcount]
  (let [fns (map (fn [beg]
                   (fn []
                     (reduce longest
                             '()
                             (map hailstone (range beg (inc limit) pcount)))))
                 (doall (range 1 (inc pcount))))]
    (reduce longest (apply pcalls fns))))

(defn- parse-cmdline [args]
  (let [[options rest-args help-banner]
        (cli args
             ["-t" "--threads" :parse-fn #(Integer. %) :default 1]
             ["-l" "--limit" :parse-fn #(Integer. %) :default 100]
             ["-h" "--help" :default false :flag true])]
    (if (:help options)
      (do
        (println help-banner)
        nil)
      options)))
              
(defn -main [& args]
  (if-let [options (parse-cmdline args)]
    (do
      (println (str "Counting hailstone sequences upto " (:limit options) "."))
      (if (= (:threads options) 1)
        (println (longest-until (:limit options)))
        (println (plongest-until (:limit options) (:threads options)))))))
