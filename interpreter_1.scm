(load "simpleParser.scm")
;use pretty big
;Griffin Saiia, Chad Johnson
;PLC Project 1

;creates the state
(define createState
  (lambda ()
    '((return true false) (() #t #f))))

(define emptyVar
  (lambda (var)
    (cons var '(()))))

;(void) abstraction - (set! x x) returns #<void> as output
;done allows me to check for that cleanly
(define done (list (void)))

(define getState
  (lambda (state key)
    (cond
      ((number? key) key)
      ((boolean? key) key)
      ((null? (car state)) (error "Variable must be assigned to before reference"))
      ((equal? (caar state) key) (caadr state))
      (else (getState (list (cdar state) (cdadr state)) key)))))

(define updateState
  (lambda (state lis)
    (cond
      ((null? (car state)) (list (cons (car lis) (car state)) (cons (cadr lis) (cadr state))))
      ((equal? (caar state) (car lis)) (list (cons (car lis) (car state)) (cons (cadr lis) (cadr state))))
      (else (list (cons (caar state) (updateState (list (cdar state) (cdadr state)))))))))
      
;state update and get

;expression evaluators
(define addHandler
  (lambda (lis)
    (+ (oEval (car lis)) (oEval (cadr lis)))))

(define subtractHandler
  (lambda (lis)
    (cond
      ((null? (cdr lis)) (- 0 (oEval (car lis))))
      (else (- (oEval (car lis)) (oEval (cadr lis)))))))

(define multiplyHandler
  (lambda (lis)
    (* (oEval (car lis)) (oEval (cadr lis)))))

(define divideHandler
  (lambda (lis)
    (quotient (oEval (car lis)) (oEval (cadr lis)))))

(define modHandler
  (lambda (lis)
    (modulo (oEval (car lis)) (oEval (cadr lis)))))

(define equalHandler
  (lambda (lis)
    (cond
      ((equal? (oEval (car lis)) (oEval (cadr lis))) #t)
      (else #f))))
(define invertBool
  (lambda (val)
    (cond
      ((eq? val #t) #f)
      (else #t))))

(define notEqualHandler
  (lambda (lis)
    (invertBool (equalHandler lis))))

(define greaterHandler
  (lambda (lis)
    (cond
      ((> (oEval (car lis)) (oEval (cadr lis))) #t)
      (else #f))))

(define lessHandler
  (lambda (lis)
    (cond
      ((< (oEval (car lis)) (oEval (cadr lis))) #t)
      (else #f))))

(define lessEqualHandler
  (lambda (lis)
    (cond
      ((or (lessHandler lis) (equalHandler lis)) #t)
      (else #f))))

(define greaterEqualHandler
  (lambda (lis)
    (cond
      ((or (greaterHandler lis) (equalHandler lis)) #t)
      (else #f))))


(define andHandler
  (lambda (lis)
    (and (oEval (car lis)) (oEval (cadr lis)))))

(define orHandler
  (lambda (lis)
    (or (oEval (car lis)) (oEval (cadr lis)))))

;
;state mutators
;
;handles if operator
(define ifHandler
  (lambda (lis)
    (cond
      ((oEval (car lis)) (oMutate (cadr lis)))
      ((not (null? (cddr lis))) (oMutate (caddr lis)))
      (else '()))))


(define assignHandler
  (lambda (lis)
    (cond
      ((equal? (getState (car lis)) done) (error "Variable cannot be assigned to before declaration"))
      ((list? (cadr lis)) (updateState (cons (car lis) (cons (oEval (cadr lis)) '()))))
      (else (updateState (cons (car lis) (cons (oEval ( cadr lis)) '())))))))


(define declareHandler
  (lambda (lis)
    (cond
      ((null? (cdr lis)) (updateState (emptyVar (car lis))))
      (else (updateState (cons (car lis) (cons (oEval (cadr lis)) '())))))))

(define returnHandler
  (lambda (lis)
    (updateState(list 'return (oEval (car lis))))))

(define whileHandler
  (lambda (lis)
    (cond
      ((equal? (oEval (car lis)) #t) (whileHandler(cdr(cons (oMutate (cadr lis)) lis))))
      (else '()))))


(define getMutator
  (lambda (operator)
    (cond
      ((eq? operator 'return) returnHandler)
      ((eq? operator 'if) ifHandler)
      ((eq? operator 'var) declareHandler)
      ((eq? operator 'while) whileHandler)
      ((eq? operator '=) assignHandler)
      (else (error "Invalid state")))))

(define getHandler
  (lambda (operator)
    (cond
      ((eq? operator '+) addHandler)
      ((eq? operator '-) subtractHandler)
      ((eq? operator '*) multiplyHandler)
      ((eq? operator '/) divideHandler)
      ((eq? operator '%) modHandler)
      ((eq? operator '==) equalHandler)
      ((eq? operator '!=) notEqualHandler)
      ((eq? operator '>) greaterHandler)
      ((eq? operator '<) lessHandler)
      ((eq? operator '>=) greaterEqualHandler)
      ((eq? operator '<=) lessEqualHandler)
      ((eq? operator '&&) andHandler)
      ((eq? operator '||) orHandler)
      ((eq? operator '!) invertBool)
      (else getState))))


(define sInterpreter
  (lambda (state parsed)
    (cond
      ((not (null? (getState 'return))) (getState 'return))
      ((null? parsed) (getState 'return))
      (else (sInterpreter (oMutate state (car parsed)) (cdr parsed) )))))))

(define oMutate
  (lambda (state lis)
    ((getMutator (car lis)) state (cdr lis))))

(define oEval
  (lambda (state lis)
    (cond
      ((null? lis) lis)
      ((not (list? lis)) (getState lis))
      ((null? (cddr lis)) ((getHandler (car lis)) (list (oEval (cadr lis)))))
      (else ((getHandler (car lis)) (list (oEval (cadr lis)) (oEval (caddr lis))))))))

(define maskReturn
  (lambda (val)
    (cond
      ((eq? val #t) 'true)
      ((eq? val #f) 'false)
      (else val))))

(define fileInterpreter
  (lambda (fileName)
    (maskReturn (sInterpreter (createState) (parser fileName)))))


;test cases

(define testIf '(if (> (* x x) y) (return (* x x))))

(define test '((var x) (= x 10) (var y (+ (* 3 x) 5)) (while (!= (% y x) 3)
  (= y (+ y 1))) (if (> x y) (return x) (if (> (* x x) y) (return (* x x)) (if (> (* x (+ x x)) y)
    (return (* x (+ x x))) (return (- y 1)))))))

(define whileTest '((var x) (= x 10) (var y (+ (* 3 x) 5)) (while (!= (% y x) 3)
  (= y (+ y 1)))))