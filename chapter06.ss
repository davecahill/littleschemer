(load "chapter05.ss")

; Chapter 6 - Shadows

; requires an a-expression as input
; a-expression is either number (atom)
; or list with 3 items, (number operator number)

; for this version, keep in the operator validity check
(define oldnumbered?
	(lambda (aexp)
	(cond
		((atom? aexp) (number? aexp))
		(else
			(and
				(or
					(eq? (car (cdr aexp)) '+)
					(eq? (car (cdr aexp)) '*)
					(eq? (car (cdr aexp)) '^))
				(oldnumbered? (car aexp))
				(oldnumbered? (car (cdr (cdr aexp)))))))))

; remove operator validity check, as in book simplified version
(define numbered?
	(lambda (aexp)
	(cond
		((atom? aexp) (number? aexp))
		(else
			(and
				(numbered? (car aexp))
				(numbered? (car (cdr (cdr aexp)))))))))

; value should assume numbered expressions
; also assumes nexps use only one of the three operators

; in inner (all numeric), no else specified
; guessing that's a prob, but what would i put there? put 0 for now?
; (define value
; 	(lambda (nexp)
; 	(cond
; 		((number? nexp) nexp)
; 		((and (number? (car nexp)) (number? (car (cdr (cdr nexp)))))
; 			(cond
; 				((eq? (car (cdr nexp)) '+)
; 					(specialplus (car nexp) (car (cdr (cdr nexp)))))
; 				((eq? (car (cdr nexp)) '*)
; 					(specialmult (car nexp) (car (cdr (cdr nexp)))))
; 				((eq? (car (cdr nexp)) '^)
; 					(power (car nexp) (car (cdr (cdr nexp)))))
; 				(else 0)))
; 		((number? car nexp)
; 			(value (cons (car nexp) (cons (car (cdr nexp))
; 				(value (car (cdr (cdr nexp))))))))
; 		(else
; 			(value (cons (value (car nexp)) (cons (car (cdr nexp))
; 				(car (cdr (cdr nexp))))))))))

; second try - answer to else is, again, assuming correct input
; assume input will have correct operators (one of the supported ones)
; also, splitting out the "both sides are numbers" case is not that useful
; also, book only checks atom? rather than number? as first check
; (though i'm not sure what the advantage of that is)
; (define value
; 	(lambda (nexp)
; 		(cond
; 			((atom? nexp) nexp)
; 			((eq? (car (cdr nexp)) '+)
; 				(specialplus (value (car nexp))
; 					(value (car (cdr (cdr nexp))))))
; 			((eq? (car (cdr nexp)) '*)
; 				(specialmult (value (car nexp))
; 					(value (car (cdr (cdr nexp))))))
; 			(else
; 				(power (value (car nexp))
; 					(value (car (cdr (cdr nexp)))))))))

; now assume prefix notation rather than infix
; (define value
; 	(lambda (nexp)
; 		(cond
; 			((atom? nexp) nexp)
; 			((eq? (car nexp) '+)
; 				(specialplus (value (car (cdr nexp)))
; 					(value (car (cdr (cdr nexp))))))
; 			((eq? (car nexp) '*)
; 				(specialmult (value (car (cdr nexp)))
; 					(value (car (cdr (cdr nexp))))))
; 			(else
; 				(power (value (car (cdr nexp)))
; 					(value (car (cdr (cdr nexp)))))))))

; now use sub-expressions
; (define 1st-sub-exp
; 	(lambda (nexp)
; 		(car (cdr nexp))))

; (define 2nd-sub-exp
; 	(lambda (nexp)
; 		(car (cdr (cdr nexp)))))

; (define value
; 	(lambda (nexp)
; 		(cond
; 			((atom? nexp) nexp)
; 			((eq? (car nexp) '+)
; 				(specialplus (value (1st-sub-exp nexp))
; 					(value (2nd-sub-exp nexp))))
; 			((eq? (car nexp) '*)
; 				(specialmult (value (1st-sub-exp nexp))
; 					(value (2nd-sub-exp nexp))))
; 			(else
; 				(power (value (1st-sub-exp nexp))
; 					(value (2nd-sub-exp nexp)))))))

; now try define the process of applying a function to 2 args
(define 1st-sub-exp
	(lambda (nexp)
		(car (cdr nexp))))

(define 2nd-sub-exp
	(lambda (nexp)
		(car (cdr (cdr nexp)))))

; this one is just a renaming
(define operator
	(lambda (nexp)
		(car nexp)))

; i made up this!
(define applyfn
	(lambda (fn nexp)
		(fn (value (1st-sub-exp nexp))
		 (value (2nd-sub-exp nexp)))))

(define value
	(lambda (nexp)
		(cond
			((atom? nexp) nexp)
			((eq? (operator nexp) '+)
				(applyfn specialplus nexp))
			((eq? (operator nexp) '*)
				(applyfn specialmult nexp))
			(else
				(applyfn power nexp)))))



















