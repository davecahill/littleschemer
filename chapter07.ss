(load "chapter05.ss")

; Chapter 7 - Friends and Relations

; check if lat is a set (no repetitions)
(define set?
	(lambda (lat)
		(cond
			((null? lat) #t)
			((member? (car lat) (cdr lat)) #f)
			(else (set? (cdr lat))))))

; create a set (no repetitions) by deduplicating lat
(define makeset
	(lambda (lat)
		(cond
			((null? lat) '())
			((member? (car lat) (cdr lat)) (makeset (cdr lat)))
			(else (cons (car lat) (makeset (cdr lat)))))))

; write makeset using multirember
(define makeset
	(lambda (lat)
		(cond
			((null? lat) '())
			(else (cons (car lat)
				(makeset (multirember (car lat) (cdr lat))))))))

; subset?
(define subset?
	(lambda (set1 set2)
		(cond
			((null? set1) #t)
			((member? (car set1) set2) (subset? (cdr set1) set2))
			(else #f))))

; rewrite subset? using and...
(define subset?
	(lambda (set1 set2)
		(cond
			((null? set1) #t)
			(else (and (member? (car set1) set2)
				(subset? (cdr set1) set2))))))

; eqset? - check el from set1 in set2, then remove el from
; both and eqset?
(define eqset?
	(lambda (set1 set2)
		(cond
			((and (null? set1) (null? set2)) #t)
			((or (null? set1) (null? set2)) #f)
			(else (and (member? (car set1) set2)
				(eqset? (cdr set1) (rember (car set1) set2)))))))

; hmm - book implements this by checking a is a subset of b and b
; is a subset of a
(define eqset?
	(lambda (set1 set2)
		(and (subset? set1 set2) (subset? set2 set1))))

; anything in common between set1 and set2?
(define intersect?
	(lambda (set1 set2)
		(cond
			((null? set1) #f)
			(else (or (member? (car set1) set2)
				(intersect? (cdr set1) set2))))))


; intersect - return anything which is common between set1 and set2
(define intersect
	(lambda (set1 set2)
		(cond
			((null? set1) '())
			((member? (car set1) set2)
				(cons (car set1) (intersect (cdr set1) set2)))
			(else (intersect (cdr set1) set2)))))

; union - return the full set
(define union
	(lambda (set1 set2)
		(cond
			((null? set1) set2)
			((member? (car set1) set2)
				(union (cdr set1) set2)))
			(else (cons (car set1) (union (cdr set1) set2)))))

; difference - return items from set1 which are not in s2
(define difference
	(lambda (set1 set2)
		(cond
			((null? set1) '())
			((member? (car set1) set2)
				(difference (cdr set1) set2))
			(else (cons (car set1) (difference (cdr set1) set2))))))

; memberall? - a helper function to make the rest easier
(define memberall?
	(lambda (a l-set)
		(cond
			((null? l-set) #t)
			(else (and (member? a (car l-set))
				(memberall? a (cdr l-set)))))))

; intersectall - return items which are in all of the sets
; assume l-set consists of at least 2 sets
; (list representation of set)
(define intersectall-rec
	(lambda (set1 l-set)
		(cond
			((null? set1) '())
			((memberall? (car set1) l-set)
				(cons (car set1) (intersectall-rec (cdr set1) l-set)))
			(else (intersectall-rec (cdr set1) l-set)))))

(define intersectall
	(lambda (l-set)
		(intersectall-rec (car l-set) (cdr l-set))))































