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
  (cond (empty? lat)      false
        (= (first lat) a) true
        :else             (recur a (rest lat))))

(comment
  ;; Non recursive version
  (defn member? [a lat]
    (boolean (some #{a} lat))))

(comment
  (= true
     (member? 'fat '(jack sprat could eat no chicken fat)))

  (= false
     (member? 'fat '(jack could eat no chicken))))

(defn rember [a lat]
  (lazy-seq
   (cond (empty? lat)      lat
         (= a (first lat)) (rest lat)
         :else             (cons (first lat) (rember a (rest lat))))))

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
   (cond (empty? l) l
         :else      (cons (first (first l))
                          (firsts (rest l))))))

(comment
  ;; Non recursive version
  (defn firsts [l]
    (map first l)))

(comment
  (= '(a c e)
     (firsts '((a b) (c d) (e f)))))
