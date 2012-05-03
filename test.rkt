#lang racket

(require "util.rkt")
(require rackunit)

(displayln "Test if*")
(check-equal? (if* #t then (displayln "Inside then.") 1 else (displayln "Error inside else.") 0) 1)
(check-equal? (if* #f then (displayln "Error inside then.") 1 else (displayln "Inside else.") 0) 0)

(displayln "Test /= div rem vref vset! href hset! swap! push! pop! inc inc! dec dec!")

(check-equal? (/= 1 1) #f)
(check-equal? (/= 1 2) #t)
(check-equal? (div 11 2) 5)
(check-equal? (rem 11 2) 1)
(define vec (vector (vector 1 2) (vector 3 4)))
(check-equal? (vref vec 1 1) 4)
(vset! vec 1 1 5)
(check-equal? (vref vec 1 1) 5)
(define test-hash (make-hash))
(hset! test-hash 'a 'b)
(check-equal? (href test-hash 'a) 'b)
(check-equal? (href test-hash 'b #f) #f)
(check-equal? (get* test-hash 'a 'e) 'b)
(check-equal? (get* test-hash 'b 'e) 'e)
(check-equal? (href test-hash 'b) 'e)
(define a 1)
(define b 2)
(swap! a b)
(check-equal? a 2)
(check-equal? b 1)
(check-equal? (inc a) 3)
(check-equal? a 2)
(inc! a)
(check-equal? a 3)
(check-equal? (dec a) 2)
(check-equal? a 3)
(dec! a)
(check-equal? a 2)
(define lst '(2 3))
(push! 1 lst)
(check-equal? lst '(1 2 3))
(check-equal? (pop! lst) 1)
(check-equal? lst '(2 3))

(displayln "Testing dbg.")
(check-equal? (dbg (+ 1 1)) 2) ;should print "Debug: (+ 1 1) = 2"

(displayln "Testing when-let and if-let.")
(define temp #f)
(when-let (x 3) (check-equal? x 3) (set! temp #t))
(check-equal? temp #t)
(check-equal? (if-let (x 3) x #f) 3)
(check-equal? (if-let (x #f) x 20) 20)

(displayln "Testing enum.")
(enum 
 ENUM-1 1
 ENUM-2 2)

(check-equal? (enum-val ENUM-1) 1)
(check-equal? (enum-val ENUM-2) 2)

(check-equal?
 (enum-case 1
   ((ENUM-1) #t)
   ((ENUM-2) #f))
 #t)

(displayln "Test the looping macros.")

(set! lst '())
(let ([i 3])
  (while (>= i 1) (push! i lst) (dec! i)))
(check-equal? lst '(1 2 3))

(set! lst '())
(let ([i 3])
  (do-while (>= i 1) (push! i lst) (dec! i)))
(check-equal? lst '(1 2 3))

(set! lst '())
(let ([i 3])
  (do-while (>= i 4) (push! i lst) (dec! i)))
(check-equal? lst '(3))

(set! lst '())
(let ([i 3])
  (until (< i 1) (push! i lst) (dec! i)))
(check-equal? lst '(1 2 3))

(set! lst '())
(let ([i 3])
  (do-until (< i 1) (push! i lst) (dec! i)))
(check-equal? lst '(1 2 3))

(set! lst '())
(let ([i 3])
  (do-until (< i 4) (push! i lst) (dec! i)))
(check-equal? lst '(3))

(set! lst '())
(up (i 3) (push! i lst))
(check-equal? lst '(2 1 0))

(set! lst '())
(up (i 1 4) (push! i lst))
(check-equal? lst '(3 2 1))

(set! lst '())
(up (i 1 6 2) (push! i lst))
(check-equal? lst '(5 3 1))

(set! lst '())
(down (i 2) (push! i lst))
(check-equal? lst '(0 1 2))

(set! lst '())
(down (i 3 1) (push! i lst))
(check-equal? lst '(1 2 3))

(set! lst '())
(down (i 6 2 2) (push! i lst))
(check-equal? lst '(2 4 6))

(set! lst '())
(down* (i 3) (push! i lst))
(check-equal? lst '(0 1 2))

(set! lst '())
(down* (i 4 1) (push! i lst))
(check-equal? lst '(1 2 3))

(set! lst '())
(down* (i 7 2 2) (push! i lst))
(check-equal? lst '(2 4 6))

(set! lst '())
(each (i '(1 2 3)) (push! i lst))
(check-equal? lst '(3 2 1))

(set! lst '())
(repeat 3 (push! 7 lst))
(check-equal? lst '(7 7 7))

(displayln "Test point and define+")

(check-equal? (point break 10 (break 20) 30) 20)

(define+ (myfn) (10 (return 20) 30))

(check-equal? (myfn) 20)

(displayln "Test $i $f $m")

(check-equal? ($i -1 + 1 + ( 1 + 1 ) + 1 * 3 * 4 + car('(2 1)) ) 16)

(check-equal? ($f 3 (+ 1 it) (* it 4)) 16)

(check-equal? ($m (<- lst '(1 2 3)) (pop lst) (push 6 lst) (when-return #f 1) (unless-return #t 2) (when-let-return (x #f) 3)
                  (unless-let-return (x 10) 4) (when-return #t (+ x (car lst)))) 16)