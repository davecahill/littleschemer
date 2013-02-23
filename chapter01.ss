; Chapter 1 - Toys

; define atom? as described at start of book
(define atom?
	(lambda (x)
		(and (not (pair? x)) (not (null? x)))))
