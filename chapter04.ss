(load "chapter03.ss")

; Chapter 4: Numbers Games

(define add1
	(lambda (n)
		(+ n 1)))

(define sub1
	(lambda (n)
		(- n 1)))

; In book they use a special plus symbol
(define specialplus
	(lambda (n m)
		(cond
			((zero? m) n)
			(else (specialplus (add1 n) (sub1 m)))
			)))

; In book they use a special minus symbol
(define specialminus
	(lambda (n m)
		(cond
			((zero? m) n)
			(else (specialminus (sub1 n) (sub1 m)))
			)))

; addtup builds a number by totaling all the numbers in its argument
(define addtup
	(lambda (tup)
		(cond
			((null? tup) 0)
			(else (specialplus (car tup) (addtup (cdr tup))))
			)))

; In book they use a special minus symbol
(define specialmult
	(lambda (n m)
		(cond
			((zero? m) 0)
			(else (specialplus n (specialmult n (sub1 m))))
			)))

; tup+
(define tup+
	(lambda (tup1 tup2)
		(cond
			((null? tup1) tup2)
			((null? tup2) tup1)
			(else 
				(cons (+ (car tup1) (car tup2)) 
					(tup+ (cdr tup1) (cdr tup2)))))))

; special>
(define special>
	(lambda (n m)
		(cond
			((zero? n) #f)
			((zero? m) #t)
			(else 
				(special> (sub1 n) (sub1 m))))))

; special<
(define special<
	(lambda (n m)
		(cond
			((zero? m) #f)
			((zero? n) #t)
			(else 
				(special< (sub1 n) (sub1 m))))))

; special=
(define special1=
	(lambda (n m)
		(cond
			((and (zero? m) (zero? n)) #t)
			((or (zero? m) (zero? n)) #f)
			(else 
				(special1= (sub1 n) (sub1 m))))))

(define special2=
	(lambda (n m)
		(cond
			((or (special> n m) (special< n m)) #f)
			(else #t))))

(define power
	(lambda (n pow)
		(cond 
			((zero? pow) 1)
			(else (* n (power n (sub1 pow)))))))
























