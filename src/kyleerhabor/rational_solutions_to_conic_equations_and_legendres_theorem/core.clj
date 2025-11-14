(ns kyleerhabor.rational-solutions-to-conic-equations-and-legendres-theorem.core
  (:require
   [clojure.math :as math]
   [clojure.math.numeric-tower :as tower]
   [clojure.string :as string]
   [clojure.tools.cli :as cli]))

(defn gcd [a b c]
  (tower/gcd (tower/gcd a b) c))

(defn square [n]
  (* n n))

(defn remove-square-factor [n d]
  {:n (/ n (square d))
   :d d})

(defn whole-number? [n]
  ;; There must be a more efficient way of doing this.
  (= n (math/floor n)))

(defn square-free [n]
  ;; Corollary 2. Suppose d^2 | a. Then the equation aX^2 + bY^2 + cZ^2 = 0 has a non-trivial Z-solution if and only if
  ;; (a/d^2)X^2 + bY^2 + cZ^2 = 0 does.
  (let [n' (abs n)
        s (->>
            (range (math/floor (math/sqrt n')) 0 -1)
            (map #(remove-square-factor n' %))
            (filter #(whole-number? (:n %)))
            first)]
    (update s :n * (math/signum n))))

(defn divides? [a b]
  (zero? (mod b a)))

;; Generates primes.
;;
;; https://clojuredocs.org/clojure.core/lazy-seq#example-542692d3c026201cdc326ff1
(defn sieve [s]
  (let [e (first s)]
    (cons e
      (lazy-seq (sieve (remove #(divides? e %) (rest s)))))))

(defn arrange-coefficients [p {:keys [a b c]}]
  (let [pa (divides? p a)
        pb (divides? p b)
        pc (divides? p c)]
    (cond
      (and pa pb) {:a a :b b :c c}
      (and pb pc) {:a c :b b :c a}
      (and pa pc) {:a a :b c :c b})))

(defn prepare-transformation [p coefs]
  {:coefficients (arrange-coefficients p coefs)
   :p p})

;; TODO: Rename.
(defn transform [p {:keys [a b c]}]
  {:a (/ a p)
   :b (/ b p)
   :c (* p c)})

(defn square-free-normal-form [{:keys [a b c]
                                :as coefs}]
  (if (= 1.0 (:d (square-free (* a b c))))
    {:a a
     :b b
     :c c}
    (let [limit (/ (max (abs a) (abs b) (abs c)) 2)
          primes (take-while #(<= % limit) (sieve (iterate inc 2)))
          permut (first (filter :coefficients (map #(prepare-transformation % coefs) primes)))
          coefs (transform (:p permut) (:coefficients permut))]
      (recur coefs))))

(defn square-mod? [n m]
  (or
    ;; I don't think this should be true for our program, but just in case...
    (zero? m)
    ;; There exists some x such that x^2 ≡ n' (mod m)
    (let [n' (mod n m)]
      (some #(= n' (mod (square %) m)) (range m)))))

(defn proposition-1 [a b]
  (let [;; Proposition 1. If Z^2 = aX^2 + bY^2 has a non-trivial Z-solution, then
        ;; (i) at least one of a and b is positive,
        ;; (ii) a is a square modulo |b|,
        ;; (iii) b is a square modulo |a|, and
        ;; (iv) −(a/d)(b/d) is a square modulo d where d is the GCD of a and b.
        a (- a)
        b (- b)
        d (tower/gcd a b)]
    (and
      (or (pos? a) (pos? b))
      (boolean (square-mod? a (abs b)))
      (boolean (square-mod? b (abs a)))
      (boolean (square-mod? (- (* (/ a d) (/ b d))) d)))))

(defn proposition-1-input [a b c]
  (let [;; Corollary 1. The equation aX^2 + bY^2 + cZ^2 = 0 has a non-trivial Z-solution if and only if acX^2 + bcY^2 + Z^2 = 0 does.
        ;;
        ;; c = 1, but we don't use it in this context.
        a (* a c)
        b (* b c)]
    (proposition-1 a b)))

(defn proposition-2 [a b c]
  ;; Proposition 2. Suppose aX^2 + bY^2 + cZ^2 = 0 has a non-trivial Z-solution where abc is square-free. then
  ;; (i) a, b, c are not all of the same sign,
  ;; (iia) −bc is a square modulo |a|,
  ;; (iib) −ac is a square modulo |b|, and
  ;; (iic) −ab is a square modulo |c|.
  (and
    (not= 3.0 (abs (+ (math/signum a) (math/signum b) (math/signum c))))
    (boolean (square-mod? (- (* b c)) (abs a)))
    (boolean (square-mod? (- (* a c)) (abs b)))
    (boolean (square-mod? (- (* a b)) (abs c)))))

(defn proposition-2-input [a b c]
  (let [;; Corollary 3. Suppose d | a and d | b. Then the equation aX^2 + bY^2 + cZ^2 = 0 has a non-trivial Z-solution
        ;; if and only if (a/d)X^2 + (b/d)Y^2 + cdZ^2 = 0 does.
        gcd (gcd a b c)
        a (square-free (/ a gcd))
        b (square-free (/ b gcd))
        c (square-free (/ c gcd))
        {:keys [a b c]} (square-free-normal-form {:a (:n a)
                                                  :b (:n b)
                                                  :c (:n c)})]
    (proposition-2 a b c)))

(defn theorem [a b c]
  (let [;; Before: aX^2 + bY^2 = cZ^2
        ;; After:
        ;;   aX^2 + bY^2 - cZ^2 = 0
        ;;   aX^2 + bY^2 + c'Z^2 = 0 where c' = -c
        c (- c)]
    (and (proposition-1-input a b c) (proposition-2-input a b c))))

(comment
  ;; 2X^2 + 3Y^2 = -6Z^2
  (def a 2)
  (def b 3)
  (def c -6)

  ;; 2X^2 + 3Y^2 = 6Z^2
  (def a 2)
  (def b 3)
  (def c 6)

  ;; X^2 + Y^2 = 5Z^2
  ;;
  ;; https://math.stackexchange.com/a/738538
  (def a 1)
  (def b 1)
  (def c 5))

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
         {:keys [a b c]} :options} (cli/parse-opts args cli-options)
        s (cond
            (:help options) summary
            errors (string/join "\n" errors)
            :else (theorem a b c))]
    (println s)))

(comment
  (-main "--a" "1" "--b" "1" "--c" "5"))
