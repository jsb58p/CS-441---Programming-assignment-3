#lang racket

;; Define the Either/Result type structure
(struct success (value) #:transparent)
(struct failure (message) #:transparent)

;; Helper functions similar to from-just
(define (from-success default result)
  (if (success? result)
      (success-value result)
      default))

(define (from-failure default result)
  (if (failure? result)
      (failure-message result)
      default))

;; Check if an item is in a list
(define in-list?
  (λ (x lst)
    (not (false? (member x lst)))))

;; Validate if a string is a valid identifier
(define (valid-id? id)
  (and (string? id)
       (char-alphabetic? (string-ref id 0))
       (for/and ([c (string->list id)])
         (or (char-alphabetic? c)
             (char-numeric? c)
             (char=? c #\-)
             (char=? c #\_)))))

;; Safe division that returns Either
(define (safe-div x y)
  (if (= y 0)
      (failure "Division by zero")
      (success (/ x y))))

;; State functions to manage the environment
(define empty-state '())

;; Add a variable to the state
(define (add-to-state state var-name value)
  (cons (list var-name value) state))

;; Find a variable in the state
(define (find-in-state state var-name)
  (define result (assoc var-name state))
  (if result
      (success (second result))
      (failure (format "Variable '~a' not defined" var-name))))

;; Check if a variable exists in the state
(define (var-exists? state var-name)
  (if (assoc var-name state)
      #t
      #f))

;; Remove a variable from the state
(define (remove-from-state state var-name)
  (filter (λ (entry) (not (equal? (first entry) var-name))) state))

;; Main evaluation function
(define (eval expr state)
  (cond
    ;; Handle numeric literals
    [(equal? (first expr) 'num) 
     (values (success (second expr)) state)]
    
    ;; Handle variable references
    [(equal? (first expr) 'id)
     (let ([var-name (second expr)])
       (let ([lookup (find-in-state state var-name)])
         (if (success? lookup)
             (let ([value (success-value lookup)])
               (if (eq? value 'undefined)
                   (values (failure (format "Variable '~a' is undefined" var-name)) state)
                   (values lookup state)))
             (values lookup state))))]
    
    ;; Handle arithmetic operations
    [(in-list? (first expr) '(div add sub mult))
     (let-values ([(x-result x-state) (eval (second expr) state)])
       (if (failure? x-result)
           (values x-result x-state)
           (let-values ([(y-result y-state) (eval (third expr) x-state)])
             (if (failure? y-result)
                 (values y-result y-state)
                 (let ([x (success-value x-result)]
                       [y (success-value y-result)])
                   (case (first expr)
                     [(div) (values (safe-div x y) y-state)]
                     [(add) (values (success (+ x y)) y-state)]
                     [(sub) (values (success (- x y)) y-state)]
                     [(mult) (values (success (* x y)) y-state)]))))))]
    
    ;; Handle define operation
    [(equal? (first expr) 'define)
     (let ([var-name (second expr)])
       (if (var-exists? state var-name)
           (values (failure (format "Variable '~a' already defined" var-name)) state)
           (if (= (length expr) 2)
               ;; Define without initial value
               (values (success 'ok) (add-to-state state var-name 'undefined))
               ;; Define with initial value
               (let-values ([(val-result val-state) (eval (third expr) state)])
                 (if (failure? val-result)
                     (values val-result val-state)
                     (values (success 'ok) 
                             (add-to-state val-state var-name (success-value val-result))))))))]
    
    ;; Handle assign operation
    [(equal? (first expr) 'assign)
     (let ([var-name (second expr)])
       (if (not (var-exists? state var-name))
           (values (failure (format "Cannot assign to undefined variable '~a'" var-name)) state)
           (let-values ([(val-result val-state) (eval (third expr) state)])
             (if (failure? val-result)
                 (values val-result val-state)
                 (let ([new-value (success-value val-result)]
                       [new-state (remove-from-state val-state var-name)])
                   (values (success 'ok) (add-to-state new-state var-name new-value)))))))]
    
    ;; Handle remove operation
    [(equal? (first expr) 'remove)
     (let ([var-name (second expr)])
       (if (not (var-exists? state var-name))
           (begin
             (displayln (format "Error: remove ~a: variable not defined, ignoring" var-name))
             (values (success 'ok) state))
           (values (success 'ok) (remove-from-state state var-name))))]
    
    ;; Unknown operation
    [else (values (failure (format "Unknown operation: ~a" (first expr))) state)]))

;; REPL (Read-Eval-Print Loop)
(define (repl)
  (define (loop state)
    (display "expr> ")
    (flush-output)
    (let ([input (read)])
      (cond
        ;; Exit command
        [(equal? input 'quit) 
         (displayln "Goodbye!")]
        
        ;; Handle remove operation (unquoted)
        [(and (pair? input) (equal? (car input) 'remove))
         (let-values ([(result new-state) (eval input state)])
           (printf "Result: ~a\n" 
                   (if (success? result)
                       (success-value result)
                       (format "Error: ~a" (failure-message result))))
           (printf "State: ~a\n\n" new-state)
           (loop new-state))]
        
        ;; Handle quoted expressions for define, assign, and arithmetic operations
        [(and (pair? input) (equal? (car input) 'quote))
         (let-values ([(result new-state) (eval (cadr input) state)])
           (printf "Result: ~a\n" 
                   (if (success? result)
                       (success-value result)
                       (format "Error: ~a" (failure-message result))))
           (printf "State: ~a\n\n" new-state)
           (loop new-state))]
        
        ;; Error for operations that should be quoted
        [(and (pair? input) (member (car input) '(define assign add sub mult div id num)))
         (displayln "Error: This expression should be quoted (e.g., '(define a (num 5)))")
         (loop state)]
        
        ;; Handle other unrecognized input
        [else
         (displayln "Error: Unrecognized input format")
         (loop state)])))
  
  (displayln "Expression evaluator with environment")
  (displayln "Enter 'quit' to exit")
  (displayln "Operations like define, assign, and arithmetic must be quoted: '(define a (num 5))")
  (displayln "The remove operation should NOT be quoted: (remove a)")
  (loop empty-state))

;; Test cases to verify functionality - commented out to avoid errors
#|
(define (run-tests)
  (let*-values ([(result1 state1) (eval '(num 5) empty-state)]
                [(result2 state2) (eval '(add (num 5) (mult (num 2) (num 3))) empty-state)]
                [(result3 state3) (eval '(sub (num 20) (div (add (mult (num 4) (num 5)) (num 10))(num 6))) empty-state)]
                [(result4 state4) (eval '(div (num 5) (sub (num 5) (num 5))) empty-state)])
    
    (displayln "Test cases:")
    (displayln (format "1. (num 5) => ~a" result1))
    (displayln (format "2. (add (num 5) (mult (num 2) (num 3))) => ~a" result2))
    (displayln (format "3. (sub (num 20) (div (add (mult (num 4) (num 5)) (num 10))(num 6))) => ~a" result3))
    (displayln (format "4. (div (num 5) (sub (num 5) (num 5))) => ~a" result4))
    
    ;; More test cases could be added here
    ))
|#

;; Start REPL
(repl)
