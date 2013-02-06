(load "chapter04.ss")

; rember* remove n from l and from any lists within l
(define rember*
	(lambda (n l)
		(cond 
			((null? l) '())
			((atom? (car l))
				(cond
					((eqan? (car l) n) (rember* n (cdr l)))
					(else (cons (car l) (rember* n (cdr l))))))
			(else (cons (rember* n (car l)) (rember* n (cdr l)))))))

; style note - http://mumble.net/~campbell/scheme/style.txt
; according to the above, leaving closing brackets on their own 
; lines is unacceptable
; actually, looking at it, this seems reasonable.

; note - this book seems (so far) to prefer (eq? (car l) n) over 
; (eq? n (car l))

; insertR* insert n to the right of o in l and any lists within l
(define insertR*
	(lambda (n o l)
		(cond 
			((null? l) '())
			((atom? (car l))
				(cond
					((eqan? (car l) o) 
						(cons (car l) (cons n (insertR* n o (cdr l)))))
					(else (cons (car l) (insertR* n o (cdr l))))))
			(else (cons (insertR* n o (car l)) (insertR* n o (cdr l)))))))

; occur* - how many times does n occur in l and any lists within l
(define occur*
	(lambda (n l)
		(cond 
			((null? l) 0)
			((atom? (car l))
				(cond
					((eqan? (car l) n) (add1 (occur* n (cdr l))))
					(else (occur* n (cdr l)))))
			(else (specialplus (occur* n (car l)) (occur* n (cdr l)))))))

; subst* - replace o with n wherever it occurs in l, a list of S-expressions
(define subst*
	(lambda (n o l)
		(cond
			((null? l) '())
			((atom? (car l)) 
				(cond
					((eqan? (car l) o) (cons n (subst* n o (cdr l))))
					(else (cons (car l) (subst* n o (cdr l))))))
			(else (cons (subst* n o (car l)) (subst* n o (cdr l)))))))


; insertL* - insert n to the left of o wherever it occurs 
; in l, a list of S-expressions
(define insertL*
	(lambda (n o l)
		(cond
			((null? l) '())
			((atom? (car l))
				(cond
					((eqan? (car l) o) 
						(cons n (cons o
							(insertL* n o (cdr l)))))
					(else (cons (car l) (insertL* n o (cdr l))))))
			(else (cons (insertL* n o (car l)) 
				(insertL* n o (cdr l)))))))

; member* - return #t if a occurs anywhere in l,
; a list of S-expressions. otherwise #f
(define member*
	(lambda (a l)
		(cond
			((null? l) #f)
			((atom? (car l))
					(or (eqan? (car l) a) 
						(member* a (cdr l))))
			(else (or (member* a (car l)) (member* a (cdr l)))))))

; leftmost - finds the leftmost atom in a non-empty
; list of S-expressions (symbolic expressions) that
; does not contain the empty list.

(define leftmost
	(lambda (l)
		(cond 
			((atom? (car l)) (car l))
			(else (leftmost (car l))))))


; eqlist? determines if two lists are equal
(define eqlistold?
	(lambda (a b)
		(cond
			((and (null? a) (null? b)) #t)
			((or (null? a) (null? b)) #f)
			((and (atom? (car a)) (atom? (car b))) 
				(and (eqan? (car a) (car b)) (eqlistold? (cdr a) (cdr b))))
			((or (atom? (car a)) (atom? (car b))) #f)
			(else (and (eqlistold? (car a) (car b))
			 (eqlistold? (cdr a) (cdr b)))))))

; equal? determine if two S-expressions are equal
(define equal?
	(lambda (a b)
		(cond
			((and (atom? a) (atom? b)) (eqan? a b))
			((or (atom? a) (atom? b)) #f)
			(else (eqlist? a b)))))

; rewrite eqlist using equal?
(define eqlist?
	(lambda (a b)
		(cond
			((and (null? a) (null? b)) #t)
			((or (null? a) (null? b)) #f)
			(else (and (equal? (car a) (car b)) 
				(eqlist? (cdr a) (cdr b)))))))
