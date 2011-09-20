; define atom? as described at start of book
(define atom?
(lambda (x)
(and (not (pair? x)) (not (null? x)))))

; play with the atom? function
(atom? 'monkey)
(atom? '(monkey business))

; make a list of atoms to play with later
(define listocrap '(monkey banana haha))

; get the head of the list
(car listocrap)

; get the tail of the list
(cdr listocrap)






