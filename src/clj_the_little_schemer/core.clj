(ns clj-the-little-schemer.core)

(defn atom? [l]
  (not (list? l)))

(comment
  (= true
     (atom? "foo"))

  (= false
     (atom? '())))

(defn lat? [l]
  (cond (empty? l)        true
        (atom? (first l)) (recur (rest l))
        :else             false))

(comment
  ;; Non recursive version
  (defn lat? [l]
    (every? atom? l)))

(comment
  (= true
     (lat? '(jack sprat could eat no chicken fat)))

  (= false
     (lat? '(jack (sprat) could eat no chicken fat))))

(defn member? [a lat]
  (when-let [[x & xs] (seq lat)]
    (cond (= x a) true
          :else   (recur a xs))))

(comment
  ;; Non recursive version
  (defn member? [a lat]
    (boolean (some #{a} lat))))

(comment
  (= true
     (member? 'fat '(jack sprat could eat no chicken fat)))

  (nil? (member? 'fat '(jack could eat no chicken))))

(defn rember [a lat]
  (lazy-seq
   (let [[x & xs] (seq lat)]
     (cond (= a x) xs
           :else   (cons x (rember a xs))))))

(comment
  ;; Non recursive version
  (defn rember [a lat]
    (let [[x y] (split-with #(not= a %) lat)]
      (concat x (rest y)))))

(comment
  (= '(b c d f g a)
     (rember 'a '(b c d a f g a))))

(defn firsts [l]
  (lazy-seq
   (when-let [[x & xs] (seq l)]
     (cons (first x)
           (firsts xs)))))

(comment
  ;; Non recursive version
  (defn firsts [l]
    (map first l)))

(comment
  (= '(a c e)
     (firsts '((a b) (c d) (e f)))))

(defn insertR [new old lat]
  (lazy-seq
   (let [[x & xs] (seq lat)]
     (cond (= x old) (cons old (cons new xs))
           :else     (cons x (insertR new old xs))))))

(comment
  ;; Non recursive version
  (defn insertR [new old lat]
    (let [[x y] (split-with #(not= old %) lat)]
      (concat x [(first y) new] (rest y)))))

(comment
  (= '(ice cream with fudge topping for desert)
     (insertR 'topping 'fudge '(ice cream with fudge for desert))))

(defn insertL [new old lat]
  (lazy-seq
   (let [[x & xs] (seq lat)]
     (cond (= x old) (cons new lat)
           :else     (cons x (insertL new old xs))))))

(comment
  ;; Non recursive version
  (defn insertL [new old lat]
    (let [[x y] (split-with #(not= old %) lat)]
      (concat x [new] y))))

(comment
  (= '(ice cream with topping fudge for desert)
     (insertL 'topping 'fudge '(ice cream with fudge for desert))))

(defn subst [new old lat]
  (lazy-seq
   (let [[x & xs] (seq lat)]
     (cond (= x old) (cons new xs)
           :else     (cons x (subst new old xs))))))

(comment
  ;; Non recursive version
  (defn subst [new old lat]
    (let [[x y] (split-with #(not= old %) lat)]
      (concat x [new] (rest y)))))

(comment
  (= '(ice cream with topping for desert)
     (subst 'topping 'fudge '(ice cream with fudge for desert))))

(defn subst2 [new o1 o2 lat]
  (lazy-seq
   (let [[x & xs] (seq lat)]
     (cond (or (= x o1) (= x o2)) (cons new xs)
           :else                  (cons x (subst2 new o1 o2 xs))))))

(comment
  ;; Non recursive version
  (defn subst2 [new o1 o2 lat]
    (let [[x y] (split-with #(and (not= o1 %)
                                  (not= o2 %)) lat)]
      (concat x [new] (rest y)))))

(comment
  (= '(ice cream with banana for desert)
     (subst2 'banana 'fudge 'desert '(ice cream with fudge for desert)))

  (= '(ice banana with fudge for desert)
     (subst2 'banana 'fudge 'cream '(ice cream with fudge for desert))))

(defn multirember [a lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (= a x) (multirember a xs)
           :else   (cons x (multirember a xs))))))

(comment
  ;; Non recursive version
  (defn multirember [a lat]
    (remove #(= % a) lat)))

(comment
  (= '(ice cream with fudge for desert)
     (multirember 'banana '(banana ice cream with banana fudge for desert))))

(defn multiinsertR [new old lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (= x old) (cons old (cons new (multiinsertR new old xs)))
           :else     (cons x (multiinsertR new old xs))))))

(comment
  ;; Non recursive version
  (defn multiinsertR [new old lat]
    (mapcat #(if (= % old) [% new] [%]) lat)))

(comment
  (= '(vanilla ice cream with banana ice cream for desert)
     (multiinsertR 'cream 'ice '(vanilla ice with banana ice for desert))))

(defn multiinsertL [new old lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (= x old) (cons new (cons old (multiinsertL new old xs)))
           :else     (cons x (multiinsertL new old xs))))))

(comment
  ;; Non recursive version
  (defn multiinsertL [new old lat]
    (mapcat #(if (= % old) [new %] [%]) lat)))

(comment
  (= '(vanilla ice cream with banana ice cream for desert)
     (multiinsertL 'ice 'cream  '(vanilla cream with banana cream for desert))))

(defn multisubst [new old lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (= x old) (cons new (multisubst new old xs))
           :else     (cons x (multisubst new old xs))))))

(comment
  ;; Non recursive version
  (defn multisubst [new old lat]
    (map #(if (= % old) new %) lat)))

(comment
  (= '(ice cream with topping for desert)
     (multisubst 'topping 'fudge '(ice cream with fudge for desert))))

(defn o+ [n m]
  (if (zero? m)
    n
    (recur (inc n) (dec m))))

(comment
  ;; Non recursive version
  (defn o+ [n m]
    (+ n m)))

(comment
  (= 10000001
     (o+ 1 10000000)))

(defn o- [n m]
  (if (zero? m)
    n
    (recur (dec n) (dec m))))

(comment
  ;; Non recursive version
  (defn o- [n m]
    (- n m)))

(comment
  (= -9999999
     (o- 1 10000000)))

(defn tup? [t]
  (or (empty? t) (every? number? t)))

(comment
  (= false
     (tup? [1 2 3 [2 3]]))
  (= true
     (tup? [1 2 3 ])))

(defn addtup [tup]
  (loop [acc      0
         [x & xs] (seq tup)]
    (if-not x
      acc
      (recur (+ acc x) xs))))

(comment
  ;; Non recursive versions
  (defn addtup [tup]
    (apply + tup))
  (defn addtup [tup]
    (reduce + tup)))

(comment
  (= 18
     (addtup [3 5 2 8])))

(defn o* [n m]
  (loop [acc 0
         m   m]
    (if (zero? m)
      acc
      (recur (+ acc n) (dec m)))))

(comment
  ;; Non recursive versions
  (defn o* [n m]
    (* n m)))

(comment
  (= 25 (o* 5 5))
  (= 5 (o* 5 1))
  (= 0 (o* 5 0))
  (= 0 (o* 0 4)))

(defn tup+ [tup1 tup2]
  (lazy-seq
   (let [[x & xs] (seq tup1)
         [y & ys] (seq tup2)]
     (cond (empty? tup1) tup2
           (empty? tup2) tup1
           :else         (cons (+ x y) (tup+ xs ys))))))

(comment
  ;; Non recursive versions
  (defn tup+ [tup1 tup2]
    (let [[shortest longest] (sort-by count [tup1 tup2])]
      (map + longest (concat shortest (repeat 0)))))

  ;; Var args
  (defn tup+ [& tups]
    (let [[longest & rest] (sort-by count > tups)]
      (apply map + (conj (map #(concat % (repeat 0)) rest) longest)))))

(comment
  (= [2 4 6 1] (tup+ [1 2 3] [1 2 3 1]))
  (= [2 4 6 1] (tup+ [1 2 3 1]  [1 2 3])))

(defn o> [n m]
  (cond (zero? n) false
        (zero? m) true
        :else     (recur (dec n) (dec m))))

(comment
  ;; Non recursive versions
  (defn o> [n m]
    (> n m)))

(comment
  (o> 4 3)
  (o> 1333 8000)
  (o> 3 3))

(defn o< [n m]
  (cond (zero? m) false
        (zero? n) true
        :else     (recur (dec n) (dec m))))

(comment
  ;; Non recursive versions
  (defn o< [n m]
    (< n m)))

(comment
  (o< 3 4)
  (o< 8000 1333)
  (o< 3 3))

(defn o= [n m]
  (cond (zero? m) (zero? n)
        (zero? n) false
        :else     (recur (dec n) (dec m))))

(comment
  ;; Non recursive versions
  (defn o= [n m]
    (= n m)))

(comment
  (o= 3 4)
  (o= 8000 1333)
  (o= 3 3))

(defn expt [n m]
  (loop [acc 1
         m   m]
    (if (zero? m)
      acc
      (recur (* acc n) (dec m)))))

(comment
  ;; Non recursive versions
  (defn expt [n m]
    (reduce * (repeat m n))))

(comment
  (= 25 (expt 5 2))
  (= 32 (expt 2 5)))

(defn divide [n m]
  (loop [acc 0
         n   n]
    (if (< n m)
      acc
      (recur (inc acc) (- n m)))))

(comment
  ;; Non recursive versions
  (defn divide [n m]
    (/ n m)))

(comment
  (= 5 (divide 10 2))
  (= 5 (divide 30 6)))
