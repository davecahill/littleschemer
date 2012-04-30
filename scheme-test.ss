; Bart Riemens 
;http://bartriemens.wordpress.com/2010/08/22/unit-testing-in-schemelisp-roman-numeral-algorithm/

(define (string-repeat times s)
  (cond ((< times 1) "")
        ((equal? times 1) s)
        (else (string-append s
                            (string-repeat (- times 1) s)))))

(define & string-append)

(define (displayln text)
  (begin (display text)
         (newline)))

(define (show key value)
  (begin (display key)
         ;(newline)
         (display " => ")
         (displayln value)))

(define test-run-count 0)
(displayln test-run-count)
(define test-succeeded-count 0)
(define test-failed-count 0)

(define (start-testing)
  (set! test-run-count 0)
  (set! test-succeeded-count 0)
  (set! test-failed-count 0))

(define (finished-testing)
  (displayln " ===========================================")
  (display "   Number of tests: ")
  (display test-run-count)
  (display "     Succeeded: ")
  (display test-succeeded-count)
  (display "     Failed: ")
  (displayln test-failed-count)
  (displayln " =========================================="))

(define (test assumption actual predicate? expected)
  (set! test-run-count (+ test-run-count 1))
  (cond ((predicate? expected actual)
              (displayln (& "Test \"" assumption
                                  "\" Succeeded!")))
        (else (displayln (& "Test \"" assumption
                                   "\" Faild!"))
              (display "\tExpected : ")
              (displayln expected)
              (display "\tbut was  : ")
              (displayln actual))))