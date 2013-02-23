(load "chapter02.ss")

; Chapter 3 - Cons The Magnificent

; rember
; should remove only first occurrence of a
(define rember
	(lambda (a lat)
		(cond
			((null? lat) '())
			((eq? (car lat) a) (cdr lat))
			(else (cons (car lat) (rember a (cdr lat)))))))

; firsts
; argument is either null list or contains non-empty lists
; return list consiting of first element of each list
(define firsts
	(lambda (l)
		(cond
			((null? l) '())
			(else (cons (car (car l)) (firsts (cdr l)))))))

; insertR
; insert new to the right of the first occurrence of old in lat
(define insertR
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? (car lat) old) (cons old (cons new (cdr lat))))
			(else (cons (car lat) (insertR new old (cdr lat)))))))

; insertL
; insert new to the left of the first occurrence of old in lat
(define insertL
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? (car lat) old) (cons new lat))
			(else (cons (car lat) (insertL new old (cdr lat)))))))

; subst
; replace first occurrence of old in lat with new
(define subst
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? (car lat) old) (cons new (cdr lat)))
			(else (cons (car lat) (subst new old (cdr lat)))))))

; subst2
; replace first occurrence of either o1 or o2 in lat with new
(define subst2
	(lambda (new o1 o2 lat)
		(cond
			((null? lat) '())
			((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
			(else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

; multirember
; should remove all occurrences of a
(define multirember
	(lambda (a lat)
		(cond
			((null? lat) '())
			((eq? (car lat) a) (multirember a (cdr lat)))
			(else (cons (car lat) (multirember a (cdr lat)))))))

; multiinsertR
; insert new to the right of the first occurrence of old in lat
(define multiinsertR
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
			(else (cons (car lat) (multiinsertR new old (cdr lat)))))))

; multiinsertL
; insert new to the left of the first occurrence of old in lat
(define multiinsertL
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
			(else (cons (car lat) (multiinsertL new old (cdr lat)))))))


; multisubst
; replace first occurrence of old in lat with new
(define multisubst
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
			(else (cons (car lat) (multisubst new old (cdr lat)))))))
