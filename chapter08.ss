(load "chapter07.ss")

; Chapter 8 - Lambda The Ultimate
(define eq?-c
	(lambda (a)
		(lambda (x)
			(eq? x a))))

(define eq?-salad
	(eq?-c 'salad))

; rember-f removes the first instance only
(define rember-f
	(lambda (test?)
		(lambda (a l)
			(cond
				((null? l) '())
				((test? a (car l)) (cdr l))
				(else (cons (car l) ((rember-f test?) a (cdr l))))))))


; insertL-f
; insert new to the left of the first occurrence of old in lat
(define insertL-f
	(lambda (test?)
		(lambda (new old lat)
			(cond
				((null? lat) '())
				((test? (car lat) old) (cons new lat))
				(else (cons (car lat)
					((insertL-f test?) new old (cdr lat))))))))

; insertR-f
; insert new to the right of the first occurrence of old in lat
(define insertR-f
	(lambda (test?)
		(lambda (new old lat)
			(cond
				((null? lat) '())
				((test? (car lat) old) (cons old (cons new (cdr lat))))
				(else (cons (car lat)
					((insertR-f test?) new old (cdr lat))))))))

; create insert-g that inserts either left or right
; OK, the below attempt was not at all what
; the book was talking about!
; (define insert-g
; 	(lambda (side)
; 		(lambda (new old lat)
; 			(cond
; 				((eq? side 'L) (cons new lat))
; 				(else (cons old (cons new (cdr lat))))))))

; l below is (cdr lat) in insertL / insertR
(define seqL
	(lambda (new old l)
		(cons new (cons old l))))

(define seqR
	(lambda (new old l)
		(cons old (cons new l))))

(define insert-g
	(lambda (seq)
		(lambda (new old l)
			((null? l) '())
			((eq? (car lat) old) (seq new old (cdr l)))
			(else (cons (car lat)
				((insert-g seq) new old (cdr lat)))))))

;redefine earlier methods more simply - this is very nice!
(define insertL
	(insert-g seqL))

(define insertR
	(insert-g seqR))

; book says using lambdas is better than naming
; redefine insertL and insertR using lambdas
(define insertL
	(insert-g
		(lambda (new old l)
			(cons new (cons old l)))))

(define insertR
	(insert-g
		(lambda (new old l)
			(cons old (cons new l)))))

; turns out subst can also be handled by insert-g:
(define seqS
	(lambda (new old l)
		(cons new l)))

(define subst
	(insert-g seqS))

; also rember, with a little tweaking:
(define seqrem
	(lambda (new old l)
		(l)))

(define rember
	(lambda (a l)
		((insert-g seqrem) #f a l)))

; this is pretty close to the dict
; {symbol:function, symbol: function} i would
; use in python...
(define atom-to-function
	(lambda (x)
		(cond
			((eq? x '+) specialplus)
			((eq? x '*) specialmult)
			(else power))))

(define value
	(lambda (nexp)
		(cond
			((atom? nexp) nexp)
			(else
				((atom-to-function (operator nexp))
					(value (1st-sub-exp nexp))
					(value (2nd-sub-exp nexp)))))))

; multirember-f
; should remove all occurrences of a
(define multirember-f
	(lambda (test?)
		(lambda (a lat)
			(cond
				((null? lat) '())
				((test? (car lat) a)
					((multirember-f test?) a (cdr lat)))
				(else (cons (car lat)
					((multirember-f test?) a (cdr lat))))))))

(define multirember-eq?
	(multirember-f eq?))

(define eq?-tuna
	(eq?-c 'tuna))


; multiremberT
(define multiremberT
	(lambda (test? lat)
		(cond
			((null? lat) '())
			((test? (car lat))
				(multiremberT test? (cdr lat)))
			(else (cons (car lat)
				(multiremberT test? (cdr lat)))))))

(define a-friend
	(lambda (x y)
		(null? y)))

(define new-friend
	(lambda (newlat seen)
		(a-friend newlat (cons 'tuna seen))))


(define multirember&co
   (lambda (a lat col)
     (cond
       ((null? lat)
        (col '() '()))
       ((eq? (car lat) a)
        (multirember&co a
                        (cdr lat)
                        (lambda (newlat seen)
                          (col newlat
                               (cons (car lat) seen)))))
       (else
        (multirember&co a
                        (cdr lat)
                        (lambda (newlat seen)
                          (col (cons (car lat) newlat)
                               seen)))))))


; multiinsertLR
; insert new to the left of oldL and right of oldR
; assuming they're different
(define multiinsertLR
	(lambda (new oldL oldR lat)
		(cond
			((null? lat) '())
			((eq? (car lat) oldL)
				(cons new (cons oldL
					(multiinsertLR new oldL oldR (cdr lat)))))
			((eq? (car lat) oldR)
				(cons oldR (cons new
					(multiinsertLR new oldL oldR (cdr lat)))))
			(else (cons (car lat)
				(multiinsertLR new oldL oldR (cdr lat)))))))

; multiinsertLR&co
; Uses col on new lat, number of left insertions, number of right insertions
(define multiinsertLR&co
	(lambda (new oldL oldR lat col)
		(cond
			((null? lat) (col '() 0 0)
			((eq? (car lat) oldL)
				(multiinsertLR&co new oldL oldR (cdr lat)
					(lambda (newlat left-count right-count)
						(col (cons new (cons oldL newlat))
							(add1 left-insertions) right-insertions))))
			((eq? (car lat) oldR)
				(multiinsertLR&co new oldL oldR (cdr lat)
					(lambda (newlat left-count right-count)
						(col (cons oldR (cons new newlat)
							left-insertions (add1 right-insertions))))))
			(else (multiinsertLR&co new oldL oldR (cdr lat)
					(lambda (newlat left-count right-count)
						(col (cons (cdr lat) newlat) left-insertions right-insertions))))))))

; maybe the outer col could cons the three args together?




















