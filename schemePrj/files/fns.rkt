;;-*- mode: scheme; -*-
;:set filetype=scheme
;;======================================================================================================
;;Return a 2-element list containing the 2 roots of the quadratic
;;equation a*x^2 + b*x + c = 0 using the classical formula for the
;;roots of a quadratic equation.  The first element in the returned list
;;should use the positive square-root of the discriminant, the second
;;element should use the negative square-root of the discriminant.
;;======================================================================================================
(define (quadratic-roots a b c)
  (if (= a 0) 'Wrong-Input 						;base condiction check
    (let ((rootValue (- (* b b) (* 4 a c))))				;calculate root value
      (let ((denom (+ b (* (if (>= b 0) 1 -1) (sqrt rootValue)))))	;multiply with -1 or +1
	(list (/ denom -2 a) (/ (* -2 c) denom)))			;divide denominator
      (list								;building list start
	(/ (- (sqrt rootValue) b) 2 a)					;build list 1st element
	(/ (+ (sqrt rootValue) b) -2 a))				;list 2nd element
    )									;end let 
  )									;end if
)									;end define
;;======================================================================================================
;;Return the list resulting by multiplying each element of `list` by `x`.
;;======================================================================================================
(define (mul-list list x)						;start block
  (if (null? list) '()							;check if list is null
      (cons (* x (car list)) (mul-list (cdr list) x))			;multiply x with first element
  )									;end if
)									;end block
;;======================================================================================================
;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.
;;======================================================================================================
(define (sum-lengths list)						;start sum-length
  (cond 								;check cond block start
    ((null? list) 0)							;base condition if list is null
    (else 								;start else
      (+ (length (car list)) (sum-lengths (cdr list)))			;perform addition of sublist	
    )									;end else
  )									;end cond block	
)									;end sum-length
;;======================================================================================================
;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  The computation should reflect the traditional
;;representation of the polynomial.
;;======================================================================================================
(define (poly-eval coeffs x)						;start poly-eval function
  (if (null? coeffs) 0							;base condition for null
    (+ (* (car coeffs) (expt x (- (length coeffs) 1))) (poly-eval (cdr coeffs) x))	;perform cltn
  )									;end if
)									;end function
;;======================================================================================================
;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x using Horner's method.
;;======================================================================================================
(define (poly-eval-horner coeffs x)					;start poly-eval-horner()
  (letrec 								;start letrec
    ([aux-poly-eval-horner 						;recurusive func define	
      (lambda (acc coeffs x)						;variables
      	(if (null? coeffs) acc						;base condition check
	  (aux-poly-eval-horner (+ (* acc x) (car coeffs)) (cdr coeffs) x)	;calculation 
	))])								;end block
	(aux-poly-eval-horner 0 coeffs x)				;1st call to rec. func.
  )									;end letrec block
)									;end define block
;;======================================================================================================
;;Return count of occurrences equal? to x in exp
;;======================================================================================================
(define (count-occurrences exp x)					;count-occurences() start
  (if (not (pair? exp))							;base condition chk - not pair?
    0									;return 0 if true
    (if (equal? x (car exp))						;else block, start if
      (+ (count-occurrences (car exp) x) (count-occurrences (cdr exp) x) 1)	;tracing instances
      (+ (count-occurrences(car exp) x) (count-occurrences(cdr exp) x)))))	;end of block of ()
;;======================================================================================================
;;Return result of evaluating arith expression over Scheme numbers
;;with fully parenthesized prefix binary operators 'add, 'sub, 'mul
;;and 'div.
;;======================================================================================================
(define (eval-arith exp)						;start eval-arith() block
  (cond 								;start of cond block
    ((number? exp) exp)							;check if first element is num
    ((equal? (list-ref exp 0) 'add) (+ (eval-arith(list-ref exp 1)) (eval-arith(list-ref exp 2))))
    									;perform add if 'add'
    ((equal? (list-ref exp 0) 'sub) (- (eval-arith(list-ref exp 1)) (eval-arith(list-ref exp 2))))
    									;peform sub if 'sub'
    ((equal? (list-ref exp 0) 'mul) (* (eval-arith(list-ref exp 1)) (eval-arith(list-ref exp 2))))
    									;perform mul if 'mul'
    ((equal? (list-ref exp 0) 'div) (/ (eval-arith(list-ref exp 1)) (eval-arith(list-ref exp 2))))
    									;perform div if 'div'
  )									;end of cond block
)									;end of function
;;======================================================================================================
;;Given a proper-list list of proper-lists, return sum of lengths of
;;all the contained lists.  Must be tail-recursive.
;;======================================================================================================
(define (sum-lengths-tr list)						;start sum-length-tr()
  (letrec ([calc-length 						;letrec start block
	     (lambda (len list)						;lambda with 2 parameters
	       (if (null? list) 0					;base condition check
		 (+ (length (car list) (calc-length (cdr list))))))])	;working logic of calculation
    (calc-length 0 list)						;first recursive call
  )									;end letrec
)									;end function block
;;======================================================================================================
;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  Must be tail-recursive.
;;======================================================================================================
(define (poly-eval-tr coeffs x)						;poly-eval-tr() start
  (letrec 								;letrec
    ([aux-poly-eval-tr 							;auxillary function
       (lambda (acc coeffs x)						;lambda with 3 parameters
	 (if (null? coeffs) acc						;base condition
	   (aux-poly-eval-tr (+ (* acc x) (car coeffs)) (cdr coeffs) x)	;perform calculation
	   ))])								;end of auxillary block
    (aux-poly-eval-tr 0 coeffs x)					;1st rec call
    )
    )
;;======================================================================================================
;;Return the list resulting by multiplying each element of `list` by `x`.
;;Cannot use recursion, can use one or more of `map`, `foldl`, or `foldr`.
;;======================================================================================================
(define (mul-list-2 list x)						;mul-list-2() function start
  (map (lambda (list) (* x list)) list)					;use of map() on list
)									;end of mul-list-2()
;;======================================================================================================
;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.  Cannot use recursion, can use
;;one or more of `map`, `foldl`, or `foldr`.
;;======================================================================================================
(define (sum-lengths-2 list)						;sum-length-2() start
  (if (null? list) 0							;base condition check
    (foldl + 0 (map length list))					;use of fold and map on list
  )									;end if
)									;end sum-length-2()
;:======================================================================================================
;;=================================== * End of the file * ==============================================
;;======================================================================================================
