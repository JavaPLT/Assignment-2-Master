;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
;; #reader(lib "htdp-advanced-reader.ss" "lang")((modname HW02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

;; Problem 1:
;; Given 
(define-struct BTMNode (key value left right))   ;; abbreviated name for BinaryTreeMapNode
;; A (BTMap-of alpha) is either
;; * empty, or
;; * (make-BTMNode key value left right) where
;;   * key is a number;
;;   * value is an alpha;
;;   * left and right are of type (BTMap-of alpha)


;; Generic Template for any function f with primary argument a of type (BTMap of alpha)
#|
 (define (f ... abtm ...)
   (cond [(empty? abtm) ...]
         [(BTMNode
               ... (BTMNode-key abtm)
               ... (BTMNode-value abtm)
               ... (f ... (BTMNode-left abtm) ...) 
               ... (f ... (BTMNode-right abtm) ...) 
               ... ]))
|#


;; An (BSTMap-of alpha) is an (BTMap-of alpha) that is either:
;; * empty, or
;; * a BTMNode (make-BTMNode k v l r) where every key in l is < k and every key in r is > k (implying key values are unique)


;;Main function: getBSTMap

;; Type and Contract:
;; getBSTMap: number BSTMap -> symbol | false
;; Contract: (getBSTMap n abstm) returns the symbol s in the BTMNode matching n if such a BTMNode exists and false otherwise.
;; Note: Since abstm is ordered as indicated in the definition of the type (BSTMap-of alpha), this search is easy to perform.

;; Examples 
(check-expect (getBSTMap 1 (make-BTMNode 2 'x empty empty)) false)
(check-expect (getBSTMap 2 (make-BTMNode 2 'x empty empty)) 'x)
(check-expect (getBSTMap 2 empty) false)
(check-expect (getBSTMap 3 (make-BTMNode 2 'x  (make-BTMNode 1 'o empty empty)  (make-BTMNode 4 't  (make-BTMNode 3 'a empty empty) empty))) 'a)


;; Template Instantiation
#|
 (define getBSTMap num tree )
   (cond [(empty? tree) ...]
         [(BTMNode
               ... (BTMNode-key tree)
               ... (getBSTMap num (BTMNode-left tree) ...) 
               ... (getBSTMap num (BTMNode-right tree) ...) 
               ... ]))
|#

;; Code
(define (getBSTMap num tree)
  (cond
    [(empty? tree) #false]
    [else 
     (cond
       [(= (BTMNode-key tree) num)
        (BTMNode-value tree)]
       [(< (BTMNode-key tree) num)
        (getBSTMap num (BTMNode-right tree))]
       [(> (BTMNode-key tree) num)
        (getBSTMap num (BTMNode-left tree))])]))


 
;; Analysis: searching trees vs. searching lists
;;<##your brief (a few lines) discussion comparing the
;; efficiency of the two tasks goes here##>
#|
Searching trees is generally much more efficent than searching trees.
When searching a list, because each element n of the list must linearly be
checked, the search time is O(n).
Searching a binary search tree however has a search time of O(h) and h is always less
than if not equal to the amount of elements n.
|#




;; **Problem 2**
;; Data Defintions
;; A list-of-symbols is either
;;   * empty, or
;;   * (cons s los) where s is a symbol, and los is a list-of-symbols
;;
;; Examples:
;; empty
;; (cons 1 empty)
;; (cons "x" (cons 4 empty))


;; A list-of-numbers is either
;;    empty, or
;;    (cons n lon) where n is a number, and lon is a list-of-numbers
;;
;; Examples:
;; empty
;; (cons 1 empty)
;; (cons 1 (cons 4 empty))
;;

;; Template for list-of-symbols
#|
(define (f-los ... a-los ...)
    [(empty? a-los) ...]
    [(cons? a-los) ... (first a-los) ...
                   ... (f-los ... (rest a-los) ...) ... ]))
|#

;; Template for list-of-numbers
#| (define (f-lon ... lon ...)
      (if (empty? lon) ...
          ( ... (first lon) ... f-lon (rest lon) ... )
|#


;; Auxilary function: cross-help

;; Type and Contract:
;; cross-help: number (list-of symbol) -> list-of number-symbol-pair
;; Contract: given a number num and a (list-of symbol) los, (cross-help n los)
;; returns a list of all possible pairs (n, s) s is a member of los.

;; Examples:
(check-expect (cross-help 1 '(a b c)) '((1 a) (1 b) (1 c)))
(check-expect (cross-help 2 '(a)) '((2 a)))
(check-expect (cross-help 2 '()) '())

;; Template Instantiation:
#| (define (cross-help ... los ...)
      (if (empty? loa) ...
          ( ... (cross-help num (first lon)) ... (cross-help num (rest lon)) ... )
|#

;; Code:
(define (cross-help num los)
  (cond
   [(empty? los)
        empty]
   [(cons? los)
        (cons (list num (first los)) (cross-help num (rest los)))]))

;; Main function: cross

;; Type and Contract:
;; cross: (list-of number) (list-of symbol) -> list-of number-symbol-pair
;; Contract: given a (list-of number) lon and a (list-of symbol) los,
;; (cross lon los) returns a list of all possible pairs (n, s)
;; where n is a member of lon and s is a member of los.

;; Examples:
(check-expect (cross '(1 2) '(a b c)) '((1 a) (1 b) (1 c) (2 a) (2 b) (2 c)))
(check-expect (cross '(1 1) '(a b c)) '((1 a) (1 b) (1 c) (1 a) (1 b) (1 c)))
(check-expect (cross '() '(a b c)) '())
(check-expect (cross '(4 4) '()) '())
(check-expect (cross '(1) '(a)) '((1 a)))

;; Template Instantiation
#|
(define (cross lon los)
    [(empty? lon) ...]
    [(cons? lon) ....f-los... (first lon) ...
                   ... (f-los ... (rest lon) ...) ... ]))
|#

;; Code:
(define (cross lon los)
  (cond
   [(empty? lon)
        empty]
   [(cons? lon)
        (append (cross-help(first lon) los) (cross (rest lon) los))]))
   




;; Problem 3
;; Auxilary function: merge-help
;; Type and Contract:
;; merge-help: (ascending) (not-empty) (list-of number)  (ascending) (list-of number) ->
;; (ascending) (list-of number)
;; Contract: given two arguments lon1 and lon2 of type (ascending) (list-of number),
;; (merge lon1 lon2) returns the list containing all elements in
;; both lists in ascending order

;; Examples:
(check-expect (merge-help (list 1 2 3) empty) (list 1 2 3))
(check-expect (merge-help (list 1 2 3) (list 7 8 9)) (list 1 2 3 7 8 9))
(check-expect (merge-help (list 1) (list 7 8 9)) (list 1 7 8 9))

;; Template Instantiation:
#| (define (merge-help lon1 lon2)
      (if (empty? lon2) ...
          ( ... f-lon (first lon1) ... f-lon (rest lon2) ... )
|#

;; Code
(define (merge-help lon1 lon2) 
  (cond
    [(empty? lon2) lon1]
       [(< (first lon1) (first lon2))
        (cons (first lon1) (merge (rest lon1) lon2))]
       [else 
        (cons (first lon2) (merge lon1 (rest lon2)))]))


;; Main function: merge

;; Type and Contract: 
;; merge: (ascending)(list-of number)  (ascending) (list-of number) ->
;; (ascending) (list-of number)
;; Contract: given two arguments lon1 and lon2 of type (ascending) (list-of number),
;; (merge-help lon1 lon2) returns the list containing all elements in
;; both lists in ascending order


;; Examples
(check-expect (merge (list 1 2 3) empty) (list 1 2 3))
(check-expect (merge empty empty) empty)
(check-expect (merge empty (list 1 2 3)) (list 1 2 3))
(check-expect (merge (list 1 2 3) (list 7 8 9)) (list 1 2 3 7 8 9))

;; Template Instantiation for merge
#| (define (merge lon1 lon2)
      (if (empty? lon1) ...
          ( ... (first lon1) ... f-lon (rest lon2) ... )
|#


;; Code
(define (merge lon1 lon2) 
  (cond
   [(empty? lon1)
     lon2]
    [(cons? lon1)
     (merge-help lon1 lon2)
     ]))


;; Problem 4
;; The Fibonacci function fib is defined by the recursion equation:
;;   fib(n) = fib(n-1) + fib(n-2) for n > 1
;; where fib(0) = fib(1) = 1.
;; The following Racket program computes fib(n) using this definition:

(define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))

;; Auxilary function: fibHelp

;; fibHelp: nat nat nat -> nat
;; Contract: (fibHelp k fn-k-1 fn-k-2) returns (fib n) provided
;; that fn-k-1 = (fib (- n k 1)), fn-k-2 = (fib (- n k 2))

;; Examples
(check-expect (fibHelp 0 1 1) 2)
(check-expect (fibHelp 0 2 1) 3)
(check-expect (fibHelp 1 (fib 9) (fib 8)) (fib 11))

;; Template Instantiation
#|
(define (fibHelp k A B)
  (if (...)
      .....
      (fibHelp ...))
  )
|#

;; Code
(define (fibHelp k A B)
  (if (eq? k 0)
      (+ A B)
      (fibHelp (- k 1) (+ B A) A))
  )


;; Main function: fastFib

;; Type and Contract:
;; fastFib: nat -> nat
;; Contract: (fastFub n) returns (fib n)

;; Examples
(check-expect (fastFib 1) (fib 1))
(check-expect (fastFib 11) (fib 11))
(check-expect (fastFib 4) (fib 4))


;; Code
(define (fastFib n)
  (fibHelp (- n 1) 1 0))
