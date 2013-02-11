; Chapter 1 - Toys

; define atom? as described at start of book
(define atom?
(lambda (x)
(and (not (pair? x)) (not (null? x)))))

; play with the atom? function
(atom? 'monkey)
(atom? '(monkey business))

; make a list of atoms to play with later
(define listostuff '(apple banana orange))

; get the head of the list
(car listostuff)

; get the tail of the list
(cdr listostuff)

; nested list
(define s '(a b (c)))

; empty list
(define l ())

; an atom
(define x 'hi)

(cons s l)
(cons l s)

; this isn't supposed to be possible (consing onto an atom), but it works
(cons s x)

; playing with null?
(null? ())
(null? 'monkey)

(null? '())
