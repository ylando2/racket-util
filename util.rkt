#lang racket

(provide if* /= div rem vref vset! href hset! 
         enum-case enum-val enum $i $f $m 
         dbg when-let if-let swap! push! pop!
         point define+ while do-while until do-until
         each up down repeat inc inc! dec dec!
         get* down*)
 
;;if* is for the times that "if" look ugly it add the "then" keyword and the "else" keyword
;;and no need to use begin and add more indentation inside
;;/= div rem inc! inc dec dec! add operation from common lisp
;;(inc var) add one to var, (inc! var) add one and change var, dec is the same.
;;swap! push! pop! no need to explain
;;dbg is a macro useful for debugging (dbg (+ 1 1)) => display "debug: (+ 1 1) = 2"
;;enum enum-case enum-val are macro to use enum like in the c language
;;vref and vset! are alias for vector-ref and vector-set! and they work 
;;on multi dimensional array (in scheme it is better to use one dimensional array most of the time but when you need it, those macros help)
;;href and hset! are alias for hash-ref and hash-set!
;;$i add c like math notation usful when infix get ugly (logical math expression can get very ugly fast)
;;$f is use when a series of expression are called that every one use the result of the last expression
;;$m is use to lower the indentention, it is a light weight monad
;;point is use to add break point and define+ is use to define function with a break point named return.
;;while,do-while,until,do-until,each,up,down,repeat are loop expression good for the non functional part of the program.
;;if-let and when-let help with the double indentation in using "if" or "when" inside a let expression

(define-for-syntax (split sym lst)
  (let loop ([l lst] [cur '()] [result '()])
    (if (null? l)
        (reverse (cons (reverse cur) result))
        (if (eq? (car l) sym)
            (loop (cdr l) '() (cons (reverse cur) result))
            (loop (cdr l) (cons (car l) cur) result)))))

;;a special if for complex expression, it help with indentation 
;;(if* (...) 
;;     then
;;     (...)
;;     (...)
;;     else
;;     (...)
;;     (...))

(define-syntax if*
  (lambda (obj)
    (syntax-case obj ()
      [(if* body ...)
       (let* ([cond-then-lst (split 'then (syntax->datum #'(body ...)))]
              [co (car cond-then-lst)]
              [then-else (split 'else (cadr cond-then-lst))]
              [then (car then-else)])
         (if (null? (cdr then-else))
             #`(when #,@co (begin #,@then))
             (let ([else (cadr then-else)])
               #`(if #,@co (begin #,@then) (begin #,@else)))))])))

;;Add math function from common lisp

(define-syntax /=
  (syntax-rules ()
    [(_ args ...) (not (= args ...))]))

;;quotient and remainder are too long as operators name.
(define-syntax div
  (syntax-rules ()
    [(_ args ...) (quotient args ...)]))

(define-syntax rem
  (syntax-rules ()
    [(_ args ...) (remainder args ...)]))

(define-syntax inc 
  (syntax-rules ()
    [(_ var) (+ var 1)]
    [(_ var val) (+ var val)]))

(define-syntax inc!
  (syntax-rules ()
    [(_ var) (set! var (+ var 1))]
    [(_ var val) (set! var (+ var val))]))

(define-syntax dec 
  (syntax-rules ()
    [(_ var) (- var 1)]
    [(_ var val) (- var val)]))

(define-syntax dec!
  (syntax-rules ()
    [(_ var) (set! var (- var 1))]
    [(_ var val) (set! var (- var val))]))

;;Alias for vector-set! vector-ref hash-set! hash-ref those names are too long.

;;reference to single or multi-dimensional array
(define-syntax vref
  (syntax-rules ()
    [(_ v i) (vector-ref v i)]
    [(_ v i j ...) (vref (vector-ref v i) j ...)]))

;;set a single or multi-dimensional array
(define-syntax vset!
  (syntax-rules ()
    [(_ vec i val) (vector-set! vec i val)]
    [(_ vec i j ... val) (vset! (vector-ref vec i) j ... val)]))

(define-syntax hset!
  (syntax-rules ()
    [(_ args ...) (hash-set! args ...)]))

(define-syntax href
  (syntax-rules ()
    [(_ args ...) (hash-ref args ...)]))

(define-syntax get*
  (syntax-rules ()
    [(_ hash key value) (if (hash-has-key? hash key) 
                            (hash-ref hash key)
                            (let ([val value])
                              (hash-set! hash key val)
                              val))]))

;;Macro for c style enum
;;used by:
;; (enum-case var
;;   [(ENUM-VAL) ...]
;;   [(ENUM-VAL1) ...]
;;   ...)
(define-syntax enum-case
  (lambda (x)
    (syntax-case x ()
      ((_ var case-body ...)
       #`(case var 
           #,@(map 
              (lambda (case-lst) 
                (cons 
                 (map 
                  (lambda (x) 
                    (eval x))
                  (car case-lst))
                 (cdr case-lst)))
              (syntax->datum #'(case-body ...))))))))

;;(enum-val ENUM-VAL) return the value of ENUM-VAL
(define-syntax enum-val
  (lambda (x)
    (syntax-case x ()
      [(_ val) (datum->syntax x (eval (syntax->datum #'val)))])))

;;enum declare constant numbers in a similar way to c enum
;;it is used by:
;;(enum
;;   ENUM-VAL1 1
;;   ENUM-VAL2 2
;;   ...)
(define-syntax enum
  (syntax-rules ()
    [(_ const val) (define-for-syntax const val)]
    [(_ const val rest ...) (begin (define-for-syntax const val) (enum rest ...))]))

;;Helper function for $i macro

;;Check if op is an unary operator
(define-for-syntax (unary-op? op)
  (member op '(-)))

;;Parse unary operators. 
;;Return a pair of the part that was parsed and the rest of the expression.
(define-for-syntax (uni-parse exp)
  ;;Add the unary operators ops to the expression exp
  (define (add-ops ops exp)
    (if (null? ops)
        exp
        (foldl (lambda (x res) (cons x (list res))) exp ops)))
  ;;Parse the unary operators
  (define (parse-ops exp ops)
    (cond
      [(null? exp) (error (format "~a is not a unary operator"))]
      [(unary-op? (car exp)) (parse-ops (cdr exp) (cons (car exp) ops))]
      [else (cons ops exp)]))
  ;;Parse function call,bracket,quotation,or constant values
  (define (parse-function-or-bracket exp)
    (cond 
      ;Quotation look like this: ('quote exp)
      [(or (eq? (car exp) 'quote) (eq? (car exp) 'quasiquote))
           (cons (list (car exp) (cadr exp)) '())]
      ;Bracket look like this: (( exp ) the-rest-of-the-expression ...)
      [(list? (car exp)) (cons (parse (car exp)) (cdr exp))]
      ;Function call look like this: (fn (args ...) the-rest-of-the-expression ...)
      [(and (not (null? (cdr exp))) (list? (cadr exp))) 
       (cons (cons (car exp) (map (lambda (x) (parse (list x))) (cadr exp))) (cddr exp))]
      ;just a constant or variable
      [else (cons (car exp) (cdr exp))]))
  
  (let* ([ops+rest (parse-ops exp '())]
         [ops (car ops+rest)]
         [rest (cdr ops+rest)]
         [exp+rest (parse-function-or-bracket rest)]
         [exp (car exp+rest)]
         [rest2 (cdr exp+rest)])
    (cons (add-ops ops exp) rest2))) ;;End of uni-parse


;;Return the priority of op operator
(define-for-syntax (op-priority op)
  (case op
    [(or) 0]
    [(and) 1]
    [(=) 2]
    [(/=) 3]
    [(>) 4]
    [(<) 5]
    [(>=) 6]
    [(<=) 7]
    [(+) 8]
    [(-) 9]
    [(*) 10]
    [(/) 11]
    [(div) 12]
    [(rem) 13]
    [else (error (format "~a is not a binary operator" op))]))

;;Parse the binary operator
;;Return a pair of the part that was parsed and the rest of the expression.
(define-for-syntax (bi-parse left rest)
  (if (null? rest)
      ;;Noting to parse
      (cons left rest)
      (let*
          ([op (car rest)] ;;parse the first operator
           [p (op-priority op)]
           [temp+rest (uni-parse (cdr rest))]
           [temp (car temp+rest)]
           [rest (cdr temp+rest)])
        (let loop ([temp temp] [rest rest])
          (if (null? rest)
              ;;We finish parsing
              (cons (list op left temp) '())
              ;;Put in the right position operation with higher or equal priority
              ;;If the next operatoion has a smaller priority return the part that was parsed and the rest
              (if (< (op-priority (car rest)) p) 
                  (cons (list op left temp) rest)
                  (let ([temp+rest (bi-parse temp rest)])
                    (loop (car temp+rest) (cdr temp+rest)))))))))

;;Try to parse the expression until there is noting left to parse
(define-for-syntax (parse exp)
  (let loop ([temp+rest (uni-parse exp)])
    (let ([temp (car temp+rest)] [rest (cdr temp+rest)])
      (if (null? rest)
          temp
          (loop (bi-parse temp rest))))))

;;Infix macro
(define-syntax $i
  (lambda (obj)
    (syntax-case obj ()
      (($ args ...)
       (datum->syntax obj (parse (syntax->datum #'(args ...))))))))

(define-syntax dbg
  (syntax-rules ()
    [(_ exp) (let ([val exp]) (printf "Debug: ~a = ~a~%" 'exp val) val)]))

;;A helper macro similar to -> in clojure
;;use by ($f (fn args ...) (fn2 ... it ...) ...)
;;it assign "it" with the result of the last expression
(define-syntax $f
  (lambda (x)
    (syntax-case x ()
      [(_ arg) #'arg] 
      ;;Here i use the #'args in datum->syntax scope because otherwise it break.
      [(_ args rest ...) (with-syntax ([it (datum->syntax #'args 'it)]) #'(let ([it args]) ($f rest ...)))])))

;; Haskell need monad because it is lazy.
;; Scheme need light weight monad becouse of too much indentations.
(define-syntax $m
  (syntax-rules (<- when-return unless-return when-let-return unless-let-return swap push pop)
    ;;This part help to remove the need for the let indentation 
    [(_ (<- var val) rest ...) (let ([var val]) ($m rest ...))] 
    [(_ (swap x y) rest ...) (let ([x y] [y x]) ($m rest ...))]
    [(_ (push val lst) rest ...) (let ([lst (cons val lst)]) ($m rest ...))]
    [(_ (pop lst) rest ...) (let ([lst (cdr lst)]) ($m rest ...))] 
    ;if we need the top we do ($m ... (<- top (car lst)) (pop lst) ...)
    ;;This part help with the if indentation
    [(_ (when-return cond body ...) rest ...) (if cond (begin body ...) ($m rest ...))]
    [(_ (unless-return cond body ...) rest ...) (if cond ($m rest ...) (begin body ...))]
    ;;This part help with the if-let indentation
    [(_ (when-let-return (var cond) body ...) rest ...) (let ([var cond]) (if var (begin body ...) ($m rest ...)))]
    [(_ (unless-let-return (var cond) body ...) rest ...) (let ([var cond]) (if var ($m rest ...) (begin body ...)))] 
    ;;unless-let-return look strange but it keep the result for the body
    [(_) #f]
    [(_ arg) arg]
    [(_ arg rest ...) (begin arg ($m rest ...))]))
    
(define-syntax when-let
  (syntax-rules ()
    [(_ (var cond) body ...) (let ([var cond]) (when var body ...))]))

(define-syntax if-let
  (syntax-rules ()
    [(_ (var cond) then-body else-body) (let ([var cond]) (if cond then-body else-body))]))

(define-syntax swap!
  (syntax-rules ()
    [(_ var1 var2) (let ([temp var1]) (set! var1 var2) (set! var2 temp))]))

(define-syntax push!
  (syntax-rules ()
    [(_ val lst) (set! lst (cons val lst))]))

(define-syntax pop!
  (syntax-rules ()
    [(_ lst) (let ([temp (car lst)]) (set! lst (cdr lst)) temp)]))

;;Define a breaking point
(define-syntax point 
  (syntax-rules ()
    [(_ name body ...) (call/ec (lambda (name) body ...))]))

;;Define function that we can break out with return
(define-syntax define+
  (lambda (x)
    (syntax-case x ()
      [(_ (name args ...) body ...) 
       (with-syntax ([return (datum->syntax x 'return)]) 
         #'(define (name args ...) (call/ec (lambda (return) body ...))))])))

;;Loops
(define-syntax while
  (syntax-rules ()
    [(_ cond body ...) (let loop () (when cond body ... (loop)))]))

;;run at least once
(define-syntax do-while
  (syntax-rules ()
    [(_ cond body ...) (let loop () body ... (when cond (loop)))]))

(define-syntax until 
  (syntax-rules ()
    [(_ cond body ...) (let loop () (unless cond body ... (loop)))]))

;;run at least once
(define-syntax do-until
  (syntax-rules ()
    [(_ cond body ...) (let loop () body ... (unless cond (loop)))]))

(define-syntax each
  (syntax-rules ()
    [(_ (var lst) body ...) (for-each (lambda (var) body ...) lst)]))

(define-syntax up
  (syntax-rules ()
    [(_ (var end) body ...) (let loop ([var 0]) (when (< var end) body ... (loop (+ var 1))))]
    [(_ (var start end) body ...) (let loop ([var start]) (when (< var end) body ... (loop (+ var 1))))]
    [(_ (var start end by) body ...) (let loop ([var start]) (when (< var end) body ... (loop (+ var by))))]))

(define-syntax down
  (syntax-rules ()
    [(_ (var start) body ...) (let loop ([var start]) (when (>= var 0) body ... (loop (- var 1))))]
    [(_ (var start end) body ...) (let loop ([var start]) (when (>= var end) body ... (loop (- var 1))))]
    [(_ (var start end by) body ...) (let loop ([var start]) (when (>= var end) body ... (loop (- var by))))]))

(define-syntax down*
  (syntax-rules ()
    [(_ (var start) body ...) (let loop ([var (- start 1)]) (when (>= var 0) body ... (loop (- var 1))))]
    [(_ (var start end) body ...) (let loop ([var (- start 1)]) (when (>= var end) body ... (loop (- var 1))))]
    [(_ (var start end by) body ...) (let loop ([var (- start 1)]) (when (>= var end) body ... (loop (- var by))))]))

(define-syntax repeat
  (syntax-rules ()
    [(_ times body ...) (let loop ([i times]) (when (> i 0) body ... (loop (- i 1))))]))

