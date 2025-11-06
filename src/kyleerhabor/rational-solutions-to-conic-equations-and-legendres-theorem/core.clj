#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns kyleerhabor.rational-solutions-to-conic-equations-and-legendres-theorem.core
  (:require
   [clojure.math :as math]
   [clojure.math.numeric-tower :as tower]
   [clojure.string :as string]
   [clojure.tools.cli :as cli]))

(defn gcd [a b c]
  (tower/gcd (tower/gcd a b) c))

(defn square-free-2 [n d]
  {:n (/ n (math/pow d 2))
   :d d})

;; TODO: Rename.
(defn whole-number? [n]
  (= n (math/floor n)))

(defn square-free [n]
  (->>
    (range (math/floor (math/sqrt n)) 0 -1)
    (map #(square-free-2 n %))
    (filter #(whole-number? (:n %)))
    first))

(defn divides? [a b]
  (zero? (mod b a)))

;; TODO: Rename.
(defn transform [{p :d
                  :keys [a]}
                 {:keys [b]}
                 {:keys [c]}]
  (if (and
        (divides? p b)
        (divides? p c))
    {:a (/ a p)
     :b (/ b p)
     :c (* p c)}
    {:a a
     :b b
     :c c}))

;; Generates primes.
;;
;; https://clojuredocs.org/clojure.core/lazy-seq#example-542692d3c026201cdc326ff1
(defn sieve [s]
  (cons
    (first s)
    (lazy-seq (sieve (filter
                       #(not= 0 (mod % (first s)))
                       (rest s))))))

;; 12X² + 18Y² + 27Z² = 0
(def a 12)
(def b 18)
(def c 27)
(def a 2)
(def b 3)
(def c -6)
(defn progress [p a b c]
  (cond
    (and (divides? p a) (divides? p b))
    ;; p does not divide c. Verify whether or not we need to swap.
    (and (divides? p b) (divides? p c))
    ;; p does not divide a, so swap a and c.
    (and (divides? p a) (divides? p c))
    ;; p does not divide b, so swap b and c.
    ))

(defn theorem [a b c]
  (let [;; Before: aX^2 + bY^2 = cZ^2
        ;; After:
        ;;   aX^2 + bY^2 - cZ^2 = 0
        ;;   aX^2 + bY^2 + c'Z^2 = 0 where c' = -c
        c (- c)
        ;; Corollary 3.
        gcd (gcd a b c)
        a (square-free (/ a gcd))
        b (square-free (/ b gcd))
        c (square-free (/ c gcd))
        x (loop [a a
                 b b
                 c c]
            (if (= 1.0 (:d (square-free (* (:n a) (:n b) (:n c)))))
              {:a a
               :b b
               :c c}
              (map #(or
                      (and (divides? % (:n a)) (divides? % (:n b)))
                      (and (divides? % (:n b)) (divides? % (:n c)))
                      (and (divides? % (:n a)) (divides? % (:n c)))) (take-while #(<= % (/ (max (:n a) (:n b) (:n c)) 2)) (sieve (iterate inc 2))))))]
    x))

(def cli-options
  [["-h" "--help"]
   ["-a" "--a NUMBER" nil
    :parse-fn parse-long
    :missing "a is required"]
   ["-b" "--b NUMBER" nil
    :parse-fn parse-long
    :missing "b is required"]
   ["-c" "--c NUMBER" "Should probably be negative"
    :parse-fn parse-long
    :missing "c is required"]])

(defn -main [& args]
  (let [{:keys [options summary errors]
         {:keys [a b c]} :arguments} (cli/parse-opts args cli-options)
        s (cond
            (:help options) summary
            errors (string/join "\n" errors)
            :else (theorem a b c))]
    (println s)))

(comment
  (def args ["-a" "1" "-b" "2" "-c" "3"])
  (def args ["-a" "1" "-b" "2"])
  (def args ["-h"]))
