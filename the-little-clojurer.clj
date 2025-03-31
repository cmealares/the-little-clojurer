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

(defn rember-f1
  [test? a l]
  (cond
    (empty? l) '()
    (test? a (first l)) (rest l)
    :else (cons (first l)
                (rember-f1 test? a (rest l)))))

(defn eq?-c [a]
  (fn [x] (= x a)))

;; currying

(defn rember-f
  "The curryfied version of rember-f"
  [test?]
  (fn [a l]
    (cond
      (empty? l) '()
      (test? a (first l)) (rest l)
      :else (cons (first l)
                  ((rember-f test?) a (rest l)))) ))

(defn insertL-f
  "Inserts the atom new to the left of the first occurrence of the atom old in lat"
  [test?]
  (fn insert2 [new old lat] ;; clojure allows to name this local function
    (cond
      (empty? lat) ()
      (test? old (first lat)) (cons new lat)
      :else (cons (first lat) (insert2 new old (rest lat))))))

(defn insertR-f
  "Inserts the atom new to the right of the first occurrence of the atom old in lat"
  [test?]
  (fn insert2
    [new old lat]
    (cond
      (empty? lat) ()
      (test? old (first lat)) (cons old (cons new (rest lat)))
      :else (cons (first lat) (insert2 new old (rest lat))))))


;; insert-g inserts either at the left or the right

(defn seqL [new old l]
  (cons new (cons old l)))

(defn seqR [new old l]
  (cons old (cons new l)))


(defn insert-g [seq]
  (fn [new old lat]
    (cond
      (empty? lat) ()
      (= old (first lat)) (seq new old (rest lat))
      :else (cons (first lat) ((insert-g seq) new old (rest lat))))))

(defn seqS [new old lat]
  (cons new lat))

(defn seqrem
  "When given this function, insert-g behaves like rember"
  [new old lat]
  lat)

(defn atom-to-function [x]
  (cond
    (= x '+) +
    (= x 'x) *
    :else exp ))

(defn value2 [aexp]
 (cond
    (atom? aexp)  aexp
    :else
    ((atom-to-function (prefix-operator aexp))
     (value2 (prefix-1st-sub-exp aexp))
     (value2 (prefix-2nd-sub-exp aexp)) )))

(defn multirember-f
  "Removes all the occurences of the atom a from lat"
  [test?]
  (fn mrm [a lat]
    (cond
      (empty? lat) ()
      (test? a (first lat)) (mrm a (rest lat))
      :else (cons (first lat) (mrm a (rest lat)) ))))


(defn multiremberT
  "Removes from lat all the atoms for which test? is true"
  [test? lat]
  (cond
    (empty? lat) ()
    (test? (first lat)) (multiremberT test? (rest lat))
    :else (cons (first lat) (multiremberT test? (rest lat)) )))

;; Here comes the difficult part

;; !!! co = continuation or collector !!!

(defn multirember&co
  "Atoms of lat that are = to a are collected in one list ls2; the others are collected in a list ls1.
  Finally, it calls (col ls1 ls2)"
  [a lat col]
  (cond
    (empty? lat)        (col '() '())
    (= (first lat) a)   (multirember&co a
                                        (rest lat)
                                        (fn [newlat seen]
                                          (col newlat
                                               (cons (first lat) seen))))
    :else               (multirember&co a
                                        (rest lat)
                                        (fn [newlat seen]
                                          (col (cons (first lat) newlat)
                                               seen)))))

(defn a-friend [x y]
  (empty? y))

(defn last-friend [x y]
  (length x))


(defn multiinsertLR
  "Inserts the atom new to the left of oldL and to the right of oldR in lat,
   if oldL and oldR are different."
  [new oldL oldR lat]
  (cond
    (empty? lat) ()
    (= oldL (first lat)) (cons new
                              (cons oldL
                                    (multiinsertLR new oldL oldR (rest lat))))
    (= oldR (first lat)) (cons oldR
                               (cons new
                                     (multiinsertLR new oldL oldR (rest lat))))
    :else (cons (first lat)
                (multiinsertLR new oldL oldR (rest lat)))))

;; Now transform multiinsertLR into the continuation version
;; which accepts a collector. It will call the collector on the new lat,
;; the number of left insertions and the number of right insertions.

(defn multiinsertLR&co
  "Inserts new to the left of oldL and to the right of oldR
   in lat if oldR and oldL are different."
  [new oldL oldR lat col]
  (cond
    (empty? lat)            (col '() 0 0)
    (= (first lat) oldL)    (multiinsertLR&co new oldL oldR
                                              (rest lat)
                                              (fn [newlat L R]
                                                (col (cons new
                                                           (cons oldL newlat))
                                                     (add1 L)
                                                     R)))

    (= (first lat) oldR)    (multiinsertLR&co new oldL oldR
                                              (rest lat)
                                              (fn [newlat L R]
                                                (col (cons oldR
                                                           (cons new newlat))
                                                     L
                                                     (add1 R))))

    :else                   (multiinsertLR&co new oldL oldR
                                              (rest lat)
                                              (fn [newlat L R]
                                                (col (cons (first lat) newlat)
                                                 L
                                                 R))) ))


(defn evens-only*
  "Removes all odd numbers from a list of nested numbers"
  [l]
  (cond
    (empty? l) '()
    (atom? (first l)) (cond
                        (even? (first l)) (cons (first l) (evens-only* (rest l)))
                        :else (evens-only* (rest l)))
    :else (cons (evens-only* (first l))
                (evens-only* (rest l)))))

(defn evens-only*&co
  "Collects the odd numbers from a list of nested numbers,
  the product of the even numbers and the sum of the odd numbers.
  Then calls col on the results."
  [l col]
  (cond
    (empty? l)
    (col '() 1 0)

    (atom? (first l))
    (cond
      (even? (first l)) (evens-only*&co (rest l)
                                        (fn [newl p s]
                                          (col (cons (first l) newl)
                                               (* p (first l))
                                               s)))
      :else             (evens-only*&co (rest l)
                                        (fn [newl p s]
                                          (col newl
                                               p
                                               (+ s (first l))))) )

    :else
    (evens-only*&co (first l)
                    (fn [al ap as]
                      (evens-only*&co (rest l)
                                      (fn [dl dp ds]
                                        (col (cons al dl)
                                             (* ap dp)
                                             (+ dp ds)))))) ))

(defn the-last-friend [newl product sum]
  (cons sum
        (cons product
              newl)))



;; ---------------------------------------------------------------------------
;; 9. …and Again, and Again, and Again,…
;; ---------------------------------------------------------------------------

(defn keep-looking [a sorn lat]
  (cond
    ;; unnatural recursion: it does not recur on a part of lat
    (number? sorn) (keep-looking a (pick sorn lat) lat)
    :else (= sorn a)))

;; A partial function: it may never stop"
(defn looking [a lat]
  (keep-looking a (pick 1 lat) lat))

;; A function that never reaches its goal
(defn eternity [x]
  (eternity x))

(defn shift [pair]
  "Takes a pair whose first component is a pair and builds a pair by shifting the second part of the first component into the second component"
  (build (first (first pair))
         (build (second (first pair))
                (second pair))))

(defn align [pora]
  (cond
    (atom? pora) pora
    (a-pair? (first pora)) (align (shift pora))
    ;; (shift pora) is no smaller than pora, hence we are not closer to the goal
    :else (build (first pora)
                 (align (second pora)))))

(defn length* [pora]
  "Counts the atoms in a pora (pair or atom)"
  (cond
    (atom? pora)  1
    :else (+
           (length* (first pora))
           (length* (second pora)))))

(defn weight* [pora]
  "Measure the length of the argument of align"
  (cond
    (atom? pora)  1
    :else (+ (* 2 (weight* (first pora)))
             (weight* (second pora)))))

(defn shuffle [pora]
  (cond
    (atom? pora) pora
    (a-pair? (first pora))  (shuffle (revpair pora))
    :else  (build (first pora)
                  (shuffle (second pora)))))

(defn A [n m]
  (cond
    (zero? n) (add1 m)
    (zero? m) (A (sub1 n) 1)
    :else (A (sub1 n)
             (A n (sub1 m)))))

;; writing length with lambdas only (no defn)

;; length0
(fn [l]
  (cond
    (empty? l) 0
    :else (add1 (eternity (rest l)))))

;; length<=1
(fn [l]
  (cond
    (empty? l) 0
    :else (add1
           ((fn [l]
              (cond
                (empty? l) 0
                :else (add1 (eternity (rest l))))) (rest l) ) )))

;; here is a function that creates length0
((fn [length]
   (fn [l]
     (cond
       (empty? l) 0
       :else (add1 (length (rest l))))))
 eternity)

;; we can name this function as mk-length
((fn [mk-length]
   (mk-length eternity))
 (fn [length]
   (fn [l]
     (cond
       (empty? l) 0
       :else (add1 (length (rest l)))))) )

;; we can pass mk-length to mk-length instead of eternity
;; and this is still length0
((fn [mk-length]
   (mk-length mk-length))
 (fn [length]
   (fn [l]
     (cond
       (empty? l) 0
       :else (add1 (length (rest l)))))) )

;; we could even use mk-length instead of length
((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   (fn [l]
     (cond
       (empty? l) 0
       :else (add1 (mk-length (rest l)))))) )

;; now we can use the mk-length argument to create a recursive call
;; and this gives length<=1
((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   (fn [l]
     (cond
       (empty? l) 0
       :else (add1 ((mk-length eternity)
                    (rest l)))))) )

;; we can keep passing mk-length to itself
;; and this gives length!
((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   (fn [l]
     (cond
       (empty? l) 0
       :else (add1 ((mk-length mk-length)
                    (rest l)))))) )

;; how to extract a function that looks like length?
;; extract the application of mk-length to itself and call it length
;; stack overflow
; ((fn [mk-length]
;    (mk-length mk-length))
;  (fn [mk-length]
;    ((fn [length]
;       (fn [l]
;         (cond
;           (empty? l) 0
;           :else (add1 (length (rest l))))))
;     (mk-length mk-length))) ) ;; because this never returns

;; we can fix it by turning the application of mk-length to itself in the last correct version
;; into a function
((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   (fn [l]
     (cond
       (empty? l) 0
       :else (add1
              ((fn [x]
                 ((mk-length mk-length) x))
               (rest l)))))) )

;; then move out the new function
;; and we get length back
((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   ((fn [length]
      (fn [l]
        (cond
          (empty? l) 0
          :else (add1 (length (rest l))))))
    (fn [x]
      ((mk-length mk-length) x)) )) )

;; extract the function that looks like length and give it a name
((fn [le]
   ((fn [mk-length]
      (mk-length mk-length))
    (fn [mk-length]
      (le (fn [x]
            ((mk-length mk-length) x)) )) ))
 (fn [length]
   (fn [l]
     (cond
       (empty? l) 0
       :else (add1 (length (rest l))))))
)

;; applicative order Y combinator
(defn Y [le]
  ((fn [f] (f f))
   (fn [f]
     (le (fn [x] ((f f) x))))))

;; define length with the Y combinator
(defn ylength [x]
  ((Y (fn [length]
        (fn [l]
          (cond
            (empty? l) 0
            :else (add1 (length (rest l))))))) x))

;; ---------------------------------------------------------------------------
;; 10. What is the value of all this?
;; ---------------------------------------------------------------------------
