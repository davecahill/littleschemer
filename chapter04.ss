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

; Imagine sharing n items equally among m people, and only being able
; to give them whole items. Once you got low enough that n < m,
; no one would get any more items. Before that however, you'd keep taking
; m items from your stack of one, giving one to each person, and marking
; a +1 on your "#items each person got" figure.
(define quotient
	(lambda (n m)
		(cond
			((special< n m) 0)
			(else (add1 (quotient (specialminus n m) m)))
			)))

(define d_length
	(lambda (lat)
		(cond
			((null? lat) 0)
			(else (add1 (d_length (cdr lat)))))))

; how do you give no answer?
; maybe just assume that the lat is long enough
; (that seems a common pattern so far)
(define pick
	(lambda (n lat)
		(cond
			((zero? (sub1 n)) (car lat))
			(else (pick (sub1 n) (cdr lat))))))

(define rempick
	(lambda (n lat)
		(cond
			((zero? (sub1 n)) (cdr lat))
			(else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

; prune numbers from a lat
(define no-nums
	(lambda (lat)
		(cond
			((null? lat) '())
			((number? (car lat)) (no-nums (cdr lat)))
			(else (cons (car lat) (no-nums (cdr lat)))))))

; get only numbers from a lat
(define all-nums
	(lambda (lat)
		(cond
			((null? lat) '())
			((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
			(else (all-nums (cdr lat))))))

;eqan?
(define eqan?
	(lambda (a b)
		(cond
			((and (number? a) (number? b)) (= a b))
			((and (atom? a) (atom? b)) (eq? a b))
			(else #f))))

; occur - count #times an element occurs in lat
(define occur
	(lambda (n lat)
		(cond
			((null? lat) 0)
			((eqan? n (car lat)) (add1 (occur n (cdr lat))))
			(else (occur n (cdr lat))))))

; one? n -> true if #1, false otherwise
(define one?
	(lambda (n)
		(= n 1)))

; rewrite rempick using one?
(define rempick2
	(lambda (n lat)
		(cond
			((one? n) (cdr lat))
			(else (cons (car lat) (rempick2 (sub1 n) (cdr lat)))))))






