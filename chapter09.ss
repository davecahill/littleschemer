(load "chapter08.ss")

; Chapter 9 - ...and Again, and Again, and Again...

; Assuming a valid lat, where each number is less than
; length of lat (no "out of bounds" errors)
(define looking
	(lambda (a lat)
		(keep-looking a (pick 1 lat) lat)))

; sorn = symbol or number
(define keep-looking
	(lambda (a sorn lat)
		(cond
			((number? sorn)
				(keep-looking a (pick sorn lat) lat))
			(else (eq? sorn a)))))


; shift takes a pair whose first component is a pair and builds
; a pair by shifting the second part of the first component
(define shift
	(lambda (pair)
		(build (first (first pair))
			(build (second (first pair)) (second pair)))))

; align
; REM ; a-pair - is x a list containing 2 S-expressions?
; pora - pair or atom i guess?
; hmm - i think you end up with (a (b (c (d (e))))) etc
;
(define align
	(lambda (pora)
		(cond
			((atom? pora) pora)
			((a-pair? (first pora)) (align (shift pora)))
			(else (build (first pora)
				(align (second pora)))))))



(define A
	(lambda (n m)
		(cond
			((zero? n) (add1 m))
			((zero? m) (A (sub1 n) 1))
			(else (A (sub1 n)
				(A n (sub1 m)))))))

(define eternity
	(lambda (x)
		(eternity x)))

((lambda (mk-length)
	(mk-length eternity))
 (lambda (length)
 	(lambda (l)
 		(cond
 			((null? l) 0)
 			(else (add1 (length (cdr l))))))))

; Lots more functions in this chapter, most are intentionally
; broken as they lead up to explaining the Y-combinator
; including a few in this file for future reference

((lambda (mk-length)
	(mk-length (mk-length eternity)))
 (lambda (length)
 	(lambda (l)
 		(cond
 			((null? l) 0)
 			(else (add1 (length (cdr l))))))))


((lambda (mk-length)
	(mk-length mk-length))
 (lambda (mk-length)
 	(lambda (l)
 		(cond
 			((null? l) 0)
 			(else (add1 ((mk-length mk-length) (cdr l))))))))

; This one causes maximum recursion depth error - commenting out!
; ((lambda (mk-length)
; 	(mk-length mk-length))
;  (lambda (mk-length)
;  	((lambda (length)
;  		(lambda (l)
;  			(cond
;  				((null? l) 0)
;  				(else (add1 (length (cdr l)))))))
;  				(mk-length mk-length))))

((lambda (mk-length)
	(mk-length mk-length))
 (lambda (mk-length)
 	(lambda (l)
 		(cond
 			((null? l) 0)
 			(else (add1
 				((lambda (x) ((mk-length mk-length) x))
 				(cdr l))))))))

((lambda (mk-length)
	(mk-length mk-length))
 (lambda (mk-length)
 	((lambda (length)
 		(lambda (l)
 			(cond
 				((null? l) 0)
 				(else (add1 (length (cdr l)))))))
 				(lambda (x) ((mk-length mk-length) x)))))



((lambda (le)
	((lambda (mk-length)
		(mk-length mk-length))
 	(lambda (mk-length)
 		(le (lambda (x)
 			((mk-length mk-length) x))))))
 		(lambda (length)
 			(lambda (l)
 				(cond
 					((null? l) 0)
 					(else (add1 (length (cdr l))))))))


(lambda (le)
	((lambda (mk-length)
		(mk-length mk-length))
 	(lambda (mk-length)
 		(le (lambda (x)
 			((mk-length mk-length) x))))))

(lambda (length)
 	(lambda (l)
 		(cond
 		((null? l) 0)
 		(else (add1 (length (cdr l)))))))

; And now, ladies and gents, the applicative-order Y combinator:

(define Y
	(lambda (le)
		((lambda (f) (f f))
	 	(lambda (f)
	 		(le (lambda (x) ((f f) x)))))))














