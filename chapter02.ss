(load "chapter01.ss")

; Chapter 2 - Do It, Do It Again, and Again, and Again...

; lat?
(define lat?
	(lambda (l)
		(cond
			((null? l) #t)
			((atom? l) #f)
			((atom? (car l)) (lat? (cdr l)))
			(else #f))))

; member?
(define member?
	(lambda (a lat)
		(cond
			((null? lat) #f)
			(else (or (eq? (car lat) a)
				(member? a (cdr lat)))))))






