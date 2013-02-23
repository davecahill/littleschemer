(load "chapter06.ss")

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

; intersectall - return items which are in all of the sets
; assume l-set consists of at least 2 sets
; (list representation of set)

; memberall? - a helper function to make the rest easier
(define memberall?
	(lambda (a l-set)
		(cond
			((null? l-set) #t)
			(else (and (member? a (car l-set))
				(memberall? a (cdr l-set)))))))

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

; ah, book version is even smarter and shorter! kind of a foldleft
; where at each iteration, you intersect the "intersection so far" set
; with the next set in the list
(define intersectall
	(lambda (l-set)
		(cond
			((null? (cdr l-set)) (car l-set))
			(else (intersectall
				(cons (intersect (car l-set) (car (cdr l-set)))
					(cdr (cdr l-set))))))))

; a-pair - is x a list containing 2 S-expressions?
(define a-pair?
	(lambda (x)
		(cond
			((atom? x) #f)
			((null? x) #f)
			((null? (cdr x)) #f)
			(else (null? (cdr (cdr x)))))))

(define first
	(lambda (p)
		(car p)))

(define second
	(lambda (p)
		(car (cdr p))))

(define third
	(lambda (l)
		(car (cdr (cdr l)))))

(define build
	(lambda (s1 s2)
		(cons s1 (cons s2 '()))))

(define fun?
	(lambda (rel)
		(set? (firsts rel))))

(define revrel
	(lambda (rel)
		(cond
			((null? rel) '())
			(else (cons
				(build (second (car rel)) (first (car rel)))
				(revrel (cdr rel)))))))

; revpair
(define revpair
	(lambda (p)
		(build (second p) (first p))))

; rewrite using revpair

(define revrel
	(lambda (rel)
		(cond
			((null? rel) '())
			(else (cons (revpair (car rel)) (revrel (cdr rel)))))))

(define seconds
	(lambda (l)
		(cond
			((null? l) '())
			(else (cons (second (car l)) (seconds (cdr l)))))))

; fullfun?
(define fullfun?
	(lambda (rel)
		(and (set? (firsts rel)) (set? (seconds rel)))))

; ha, nice! (we already know fun is a function)
(define one-to-one?
	(lambda (fun)
		(fun? (revrel fun))))
