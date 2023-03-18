(ns the-little-clojurer.core
  (:require [the-little-clojurer.core :refer :all]))

;; ---------------------------------------------------------------------------
;; Preface
;; ---------------------------------------------------------------------------

(defn atom? [x]
  (not (coll? x)))

;; ---------------------------------------------------------------------------
;; 1. Toys
;; ---------------------------------------------------------------------------

; car is first
; cdr is rest
; null? is empty?
; eq? is =

;; ---------------------------------------------------------------------------
;; 2. Do It, Do It Again, and Again, and Again...
;; ---------------------------------------------------------------------------

(defn lat?
  "Test if l is a list of atoms"
  [l]
  (cond
    (empty? l) true
    (atom? (first l)) (lat? (rest l))
    :else false))

(defn member?
  [a lat]
  (cond
    (empty? lat) false
    :else (or (= a (first lat))
              (member? a (rest lat)))))

;; ---------------------------------------------------------------------------
;; 3. Cons the Magnificent
;; ---------------------------------------------------------------------------

(defn rember
  "Removes the first occurence of the atom a from lat"
  [a lat]
  (cond
    (empty? lat) ()
    (= a (first lat)) (rest lat)
    :else (cons (first lat) (rember a (rest lat)) )))

(defn firsts [l]
  (cond
    (empty? l) ()
    :else (cons (first (first l)) (firsts (rest l)))))

(defn insertR
  "Inserts the atom new to the right of the first occurrence of the atom old in lat"
  [new old lat]
  (cond
    (empty? lat) ()
    (= old (first lat)) (cons old (cons new (rest lat)))
    :else (cons (first lat) (insertR new old (rest lat)))))

(defn insertL
  "Inserts the atom new to the left of the first occurrence of the atom old in lat"
  [new old lat]
  (cond
    (empty? lat) ()
    (= old (first lat)) (cons new lat)
    :else (cons (first lat) (insertL new old (rest lat)))))

(defn subst
  "Replaces the first occurence of old in the lat with new"
  [new old lat]
  (cond
    (empty? lat) ()
    (= old (first lat)) (cons new (rest lat))
    :else (cons (first lat) (subst new old (rest lat)))))

(defn subst2
  "Replaces either the first occurence of o1 or the first occurrence of o2 by new"
  [new o1 o2 lat]
  (cond
    (empty? lat) ()
    (= o1 (first lat)) (cons new (rest lat))
    (= o2 (first lat)) (cons new (rest lat))
    :else (cons (first lat) (subst2 new o1 o2 (rest lat)))))

(defn multirember
  "Removes all the occurences of the atom a from lat"
  [a lat]
  (cond
    (empty? lat) ()
    (= a (first lat)) (multirember a (rest lat))
    :else (cons (first lat) (multirember a (rest lat)) )))

(defn multiinsertR
  "Inserts the atom new to the right of the occurrences of the atom old in lat"
  [new old lat]
  (cond
    (empty? lat) ()
    (= old (first lat)) (cons old
                              (cons new
                                    (multiinsertR new old (rest lat))))
    :else (cons (first lat)
                (multiinsertR new old (rest lat)))))

(defn multiinsertL
  "Inserts the atom new to the left of the occurrences of the atom old in lat"
  [new old lat]
  (cond
    (empty? lat) ()
    (= old (first lat)) (cons new
                              (cons old
                                    (multiinsertL new old (rest lat))))
    :else (cons (first lat)
                (multiinsertL new old (rest lat)))))

(defn multisubst
  "Replaces the all the occurences of old in the lat with new"
  [new old lat]
  (cond
    (empty? lat) ()
    (= old (first lat)) (cons new
                              (multisubst new old (rest lat)))
    :else (cons (first lat)
                (multisubst new old (rest lat)))))

;; ---------------------------------------------------------------------------
;; 4. Numbers Games
;; ---------------------------------------------------------------------------

(defn add1 [n]
  (+ n 1))

(defn sub1 [n]
  (- n 1))

(defn o+ [n m]
  (cond
    (zero? m) n
    :else (add1 (o+ n (sub1 m)))))

(defn o- [n m]
  (cond
    (zero? m) n
    :else (sub1 (o- n (sub1 m)))))

(defn tup? [l]
  (cond
    (empty? l) true
    (number? (first l)) (tup? (rest l))
    :else false))

(defn addtup [tup]
  (cond
    (empty? tup) 0
    :else (o+ (first tup) (addtup (rest tup)))))

(defn o* [n m]
  (cond
    (zero? m) 0
    :else (o+ n (o* n (sub1 m)))))


(defn tup1+ [tup1 tup2]
  (cond
    (and (empty? tup1) (empty? tup2)) '()
    :else (cons (o+ (first tup1) (first tup2))
                (tup1+ (rest tup1) (rest tup2))) ))

(defn tup+ [tup1 tup2]
  (cond
    (empty? tup1) tup2
    (empty? tup2) tup1
    :else (cons (o+ (first tup1) (first tup2))
                (tup+ (rest tup1) (rest tup2))) ))

(defn o> [n m]
  (cond
    (zero? n) false
    (zero? m) true
    :else (o> (sub1 n) (sub1 m))))

(defn o< [n m]
  (cond
    (zero? m) false
    (zero? n) true
    :else (o< (sub1 n) (sub1 m))))

(defn o= [n m]
  (cond
    (o> n m) false
    (o< n m) false
    :else true))

(defn exp [n m]
  (cond
    (zero? m) 1
    :else (o* n (exp n (sub1 m)))
    ))

(defn quotient [n m]
  (cond
    (o< n m) 0
    :else (add1 (quotient (o- n m) m))))

(defn length
  "Returns the length of a list"
  [lat]
  (cond
    (empty? lat) 0
    :else (add1 (length (rest lat)))))

(defn pick
  "Returns the n-th element of a list. Indexes start at 1"
  [n lat]
  (cond
    (zero? (sub1 n)) (first lat)
    :else (pick (sub1 n) (rest lat))))

(defn rempick
  "Removes the n-th element from the lat"
  [n lat]
  (cond
    (zero? (sub1 n)) (rest lat)
    :else (cons (first lat)
                (rempick (sub1 n) (rest lat)))))

(defn no-nums
  "Removes the numbers from lat"
  [lat]
  (cond
    (empty? lat) '()
    (number? (first lat)) (no-nums (rest lat))
    :else (cons (first lat)
                (no-nums (rest lat)))))

(defn all-nums
  "Extract a tup from a lat using all the numbers"
  [lat]
  (cond
    (empty? lat) '()
    (number? (first lat)) (cons (first lat) (all-nums (rest lat)))
    :else (no-nums (rest lat))))

(def equan =)

(defn occur
  "Counts the number of times an atom a appears in a lat"
  [a lat]
  (cond
    (empty? lat) 0
    (= a (first lat)) (add1 (occur a (rest lat)))
    :else (occur a (rest lat))))

(defn one? [n]
  (= n 1))

;; ---------------------------------------------------------------------------
;; 5. Oh My Gawd: It's Full of Stars
;; ---------------------------------------------------------------------------

(defn rember* [a l]
  (cond
    (empty? l) '()
    (atom? (first l)) (cond
                        (= a (first l)) (rember* a (rest l))
                        :else (cons (first l)
                                    (rember* a (rest l))))
    :else (cons (rember* a (first l))
                (rember* a (rest l))) ))

(defn insertR*
  "Inserts the atom new to the right of old everywhere"
  [new old l]
  (cond
    (empty? l) '()
    (atom? (first l)) (cond
                        (= old (first l)) (cons old
                                                (cons new
                                                      (insertR* new old (rest l))))
                        :else (cons (first l)
                                    (insertR* new old (rest l))))
    :else (cons (insertR* new old (first l))
                (insertR* new old (rest l))) ))

;; The First Commandment
;;
;; When recurring on a list of atoms, lat, ask two questions about it: (empty? lat) and :else.
;; When recurring on a number, n, ask two questions about it: (zero? n) and :else.
;; When recurring on a list of S-expressions, l, ask three questions about it:
;; (empty? l), (atom? (first l)) and :else.

(defn occur*
  "Counts the number of times an atom a appears in a S-exp"
  [a l]
  (cond
    (empty? l) 0
    (atom? (first l)) (cond
                        (= a (first l)) (add1 (occur* a (rest l)))
                        :else (occur* a (rest l)))
    :else (o+ (occur* a (first l))
             (occur* a (rest l))) ))

(defn subst* [new old l]
  (cond
    (empty? l) l
    (atom? (first l)) (cond
                        (= old (first l)) (cons new
                                                (subst* new old (rest l)))
                        :else (cons (first l)
                                    (subst* new old (rest l))))
    :else (cons (subst* new old (first l))
                (subst* new old (rest l)))) )

(defn insertL* [new old l]
  (cond
    (empty? l) '()
    (atom? (first l)) (cond
                        (= old (first l)) (cons new
                                                (cons old
                                                      (insertL* new old (rest l))))
                        :else (cons (first l)
                                    (insertL* new old (rest l))))
    :else (cons (insertL* new old (first l))
                (insertL* new old (rest l))) ))

(defn member* [a l]
  (cond
    (empty? l) false
    (atom? (first l)) (or (= a (first l))
                          (member* a (rest l)))
    :else (or (member* a (first l))
              (member* a (rest l))) ))

(defn leftmost
  "Finds the leftmost atom in a non-empty list of S-expressions that does not contain the empty list"
  [l]
  (cond
    (atom? (first l)) (first l)
    :else (leftmost (first l)) ))

(defn eqlist1?
  "True if the two lists are equal"
  [l1 l2]
  (cond
    (and (empty? l1) (empty? l2)) true
    (and (empty? l1) (atom? (first l2))) false
    (empty? l1) false

    (and (atom? (first l1)) (empty? l2)) false
    (and (atom? (first l1)) (atom? (first l2))) (and (= (first l1) (first l2)) (eqlist1? (rest l1) (rest l2)))
    (atom? (first l1)) false

    ;; (first l1) is a list
    (empty? l2) false
    (atom? (first l2)) false
    :else (and (eqlist1? (first l1) (first l2))
               (eqlist1? (rest l1) (rest l2))) ))

(defn eqlist?
  "True if the two lists are equal"
  [l1 l2]
  (cond
    (and (empty? l1) (empty? l2)) true
    (or (empty? l1) (empty? l2)) false

    (and (atom? (first l1))
         (atom? (first l2))) (and (= (first l1) (first l2)) (eqlist? (rest l1) (rest l2)))

    (or (atom? (first l1))
        (atom? (first l2))) false
    :else (and (eqlist? (first l1) (first l2))
               (eqlist? (rest l1) (rest l2))) ))

(defn rember2
  "Removes the first occurence of the S-expr from a list of S-expr"
  [s l]
  (cond
    (empty? l) ()
    (= (first l) s) (rest l)
    :else (cons (first l)
                (rember2 s (rest l))) ))

;; ---------------------------------------------------------------------------
;; 6. Shadows
;; ---------------------------------------------------------------------------

(defn numbered1? [aexp]
  (cond
    (atom? aexp)  (number? aexp)
    (or (= (first (rest aexp)) '+)
        (= (first (rest aexp)) 'x)
        (= (first (rest aexp)) '↑)) (and (numbered1? (first aexp))
                                         (numbered1? (first (rest (rest aexp)))))
    :else false))

(defn numbered? [aexp]
  (cond
    (atom? aexp)  (number? aexp)
    :else (and (numbered? (first aexp))
               (numbered? (first (rest (rest aexp))))) ))

(defn value [aexp]
 (cond
    (atom? aexp)  aexp
    (= (first (rest aexp)) '+) (+ (value (first aexp)) (value (first (rest (rest aexp)))))
    (= (first (rest aexp)) 'x) (* (value (first aexp)) (value (first (rest (rest aexp)))))
    (= (first (rest aexp)) '↑) (exp (value (first aexp)) (value (first (rest (rest aexp))))) ))

(defn prefix-1st-sub-exp
  [aexp]
  (first (rest aexp)))

(defn prefix-2nd-sub-exp
  [aexp]
  (first (rest (rest aexp))))

(defn prefix-operator
  [aexp]
  (first aexp))

(defn prefix-value [aexp]
  (cond
    (atom? aexp)  aexp
    (= (prefix-operator aexp) '+) (+ (prefix-value (prefix-1st-sub-exp aexp))
                                     (prefix-value(prefix-2nd-sub-exp aexp)))
    (= (prefix-operator aexp) 'x) (* (prefix-value (prefix-1st-sub-exp aexp))
                                     (prefix-value (prefix-2nd-sub-exp aexp)))
    (= (prefix-operator aexp) '↑) (exp (prefix-value (prefix-1st-sub-exp aexp))
                                       (prefix-value (prefix-2nd-sub-exp aexp))) ))

;; representations of numbers with empty lists

(defn sero? [n]
  (empty? n))

(defn edd1
  "Like add1"
  [n]
  (cons '() n))

(defn zub1
  "Like sub1"
  [n]
  (rest n))

(defn z+
  "Add two numbers represented as empty lists"
  [n m]
  (cond
    (sero? m) n
    :else (edd1 (z+ n (zub1 m)))) )

;; ---------------------------------------------------------------------------
;; 7. Friends and Relations
;; ---------------------------------------------------------------------------

(defn set? [lat]
  (cond
    (empty? lat) true
    (member? (first lat) (rest lat)) false
    :else (set? (rest lat))))

(defn makeset [lat]
  (cond
    (empty? lat) '()
    (member? (first lat) (rest lat))  (makeset (rest lat))
    :else (cons (first lat) (makeset (rest lat)))))

(defn makeset2 [lat]
  (cond
    (empty? lat) '()
    :else (cons (first lat)
                (makeset2
                 (multirember (first lat) (rest lat))))))

(defn subset? [set1 set2]
  (cond
    (empty? set1) true
    :else (and (member? (first set1) set2)
               (subset? (rest set1) set2))))

(defn eqset? [set1 set2]
  (and (subset? set1 set2)
       (subset? set2 set1)))

(defn intersect? [set1 set2]
  (cond
    (empty? set1) false
    :else (or (member? (first set1) set2)
              (intersect? (rest set1) set2))))

(defn intersect [set1 set2]
  (cond
    (empty? set1) '()
    (member? (first set1) set2)  (cons (first set1) (intersect (rest set1) set2))
    :else (intersect (rest set1) set2)))

(defn union [set1 set2]
  (cond
    (empty? set1) set2
    (member? (first set1) set2) (union (rest set1) set2)
    :else (cons (first set1) (union (rest set1) set2))))

(defn set-diff
  "Returns all the atoms of set1 that are not in set2"
  [set1 set2]
  (cond
    (empty? set1) '()
    (member? (first set1) set2) (set-diff (rest set1) set2)
    :else (cons (first set1)
                (set-diff (rest set1) set2)) )  )

(defn intersectall [l-set]
  (cond
    (empty? (rest l-set)) (first l-set)
    :t (intersect (first l-set)
                  (intersectall  (rest l-set))) ))
(defn a-pair? [x]
  (cond
    (atom? x) false
    (empty? x) false
    (empty? (rest x)) false
    (empty? (rest (rest x))) true
    :else false     ))

(defn build
  "Builds a pair"
  [a b]
  (cons a (cons b '())))

(defn third [lst]
  (first (rest (rest lst))))

;; a rel (relation) is a set of pairs

(defn fun?
  "Tests if the keys of the given rel form a set. A fun is a finite function."
  [rel]
  (set? (firsts rel)))

(defn revpair
  "Reverses a pair"
  [p]
  (build (second p)(first p)))

(defn revrel
  "Reverses a rel"
  [rel]
  (cond
    (empty? rel) '()
    :else (cons (revpair (first rel))
                (revrel (rest rel)))))

(defn fullfun?
  [rel]
  "Tests if the inverse of rel is a finite function."
  (fun? (revrel rel)))

(defn seconds
  "Accepts a list of S-exp and return a list of the second element of each."
  [l]
  (cond
    (empty? l) l
    :else (cons (second (first l)) (seconds (rest l)))))

;; ---------------------------------------------------------------------------
;; 8. Lambda the Ultimate
;; ---------------------------------------------------------------------------
