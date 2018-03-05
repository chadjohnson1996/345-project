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

;enters a block
(define enterBlock
  (lambda (state lis)
    (cdr (oMutate (cons (createState) state) lis))))
    
;state update and get
(define getState
  (lambda (state key)
    (cond
      ((and (null? (getStateNoCheckAssign state key)) (not (equal? key 'return))) (error "Variable must be assigned to before reference"))
      (else (getStateNoCheckAssign state key)))))

;gets state without checking assignment
(define getStateNoCheckAssign
  (lambda (state key)
    (cond
      ((number? key) key)
      ((boolean? key) key)
      (else (getStateHelper state key)))))

;helper method to get the state
(define getStateHelper
  (lambda (state key)
    ((null? state) (error "Variable must be declared before reference"))
    ((null? (caar state)) (getStateHelper (cdr state) key))
    ((eq? key (caaar state)) (caadar state))
    (else (getStateHelper (cons (list (cdaar state) (cdadar state)) (cdr state)) key)))) 

(define updateState
  (lambda (state lis)
    (cond
      (((isDeclared state (car lis)) (updateHelper state lis))
       (else (declareHelper state lis))))))

(define updateHelper
  (lambda (state lis)
    ((null? state) (error "Invalid state, never should be hit"))
    ((null? (caar state)) (updateHelper (cdr state) list))
    ((eq? (car lis) (caaar state)) (cons (list () ()) (cdr state)))
    (else (updateHelper (cons (list (cdaar state) (cdadar state)) (cdr state)) lis))))
   
(define declareHelper
  (lambda (state lis)
    ((cons (list (cons (car lis) (caar state)) (cons (cadr lis) (caadr state)))))))
    
  
;helper method to check if a variable is declared
(define isDeclaredHelper
  (lambda (lis key)
    (cond
      ((null? lis) #f)
      ((eq? (car lis) key) #t)
      (else (isDelaredHelper (cdr lis) key)))))

  ;checks if a variable is declared
 (define isDeclared
   (lambda (state key)
     (cond
       ((null? state) #f)
       ((eq? (isDeclaredHelper (caar state) key) #t) #t)
       (else (isDeclared (cdr state) key)))))
       


;expression evaluators
(define addHandler
  (lambda (state lis)
    (+ (oEval state (car lis)) (oEval state (cadr lis)))))

(define subtractHandler
  (lambda (state lis)
    (cond
      ((null? (cdr lis)) (- 0 (oEval state (car lis))))
      (else (- (oEval state (car lis)) (oEval state (cadr lis)))))))

(define multiplyHandler
  (lambda (state lis)
    (* (oEval state (car lis)) (oEval state (cadr lis)))))

(define divideHandler
  (lambda (state lis)
    (quotient (oEval state (car lis)) (oEval state (cadr lis)))))

(define modHandler
  (lambda (state lis)
    (modulo (oEval state (car lis)) (oEval state (cadr lis)))))

(define equalHandler
  (lambda (state lis)
    (cond
      ((equal? (oEval state (car lis)) (oEval state (cadr lis))) #t)
      (else #f))))
(define invertBool
  (lambda (state val)
    (cond
      ((eq? (oEval state (car val)) #t) #f)
      (else #t))))

(define notEqualHandler
  (lambda (state lis)
    (cond
      ((equalHandler state lis) #f)
      (else #t))))

(define greaterHandler
  (lambda (state lis)
    (cond
      ((> (oEval state (car lis)) (oEval state (cadr lis))) #t)
      (else #f))))

(define lessHandler
  (lambda (state lis)
    (cond
      ((< (oEval state (car lis)) (oEval state (cadr lis))) #t)
      (else #f))))

(define lessEqualHandler
  (lambda (state lis)
    (cond
      ((or (lessHandler state lis) (equalHandler state lis)) #t)
      (else #f))))

(define greaterEqualHandler
  (lambda (state lis)
    (cond
      ((or (greaterHandler state lis) (equalHandler state lis)) #t)
      (else #f))))


(define andHandler
  (lambda (state lis)
    (and (oEval state (car lis)) (oEval state (cadr lis)))))

(define orHandler
  (lambda (state lis)
    (or (oEval state (car lis)) (oEval state (cadr lis)))))

;
;state mutators
;
;handles if operator
(define ifHandler
  (lambda (state lis)
    (cond
      ((oEval state (car lis)) (oMutate state (cadr lis)))
      ((not (null? (cddr lis))) (oMutate state (caddr lis)))
      (else state))))


(define assignHandler
  (lambda (state lis)
    (cond
      ((and (getStateNoCheckAssign state (car lis)) #f) (error "Variable cannot be assigned to before declaration")) ;condition never evaulates to true, only to raise error if not set 
      ((list? (cadr lis)) (updateState state (cons (car lis) (cons (oEval state (cadr lis)) '()))))
      (else (updateState state (cons (car lis) (cons (oEval state ( cadr lis)) '())))))))


(define declareHandler
  (lambda (state lis)
    (cond
      ((null? (cdr lis)) (updateState state (emptyVar (car lis))))
      (else (updateState state (cons (car lis) (cons (oEval state (cadr lis)) '())))))))

(define returnHandler
  (lambda (state lis)
    (updateState state (list 'return (oEval state (car lis))))))

(define whileHandler
  (lambda (state lis)
    (cond
      ((equal? (oEval state (car lis)) #t) (whileHandler (oMutate state (cadr lis)) lis))
      (else state))))


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
      ((not (null? (getState state 'return))) (getState state 'return))
      ((null? parsed) (getState state 'return))
      (else (sInterpreter (oMutate state (car parsed)) (cdr parsed) )))))

(define oMutate
  (lambda (state lis)
    ((getMutator (car lis)) state (cdr lis))))

(define oEval
  (lambda (state lis)
    (cond
      ((null? lis) lis)
      ((not (list? lis)) (getState state lis))
      ((null? (cddr lis)) ((getHandler (car lis)) state (list (oEval state (cadr lis)))))
      (else ((getHandler (car lis)) state (list (oEval state (cadr lis)) (oEval state (caddr lis))))))))

(define maskReturn
  (lambda (val)
    (cond
      ((eq? val #t) 'true)
      ((eq? val #f) 'false)
      (else val))))

(define interpret
  (lambda (fileName)
    (maskReturn (sInterpreter (createState) (parser fileName)))))


;test cases

;debug interpreter for hardcoded variables
(define debugInterpreter
  (lambda (value)
    (maskReturn (sInterpreter (createState) value))))
(define testIf '(if (> (* x x) y) (return (* x x))))

(define test '((var x) (= x 10) (var y (+ (* 3 x) 5)) (while (!= (% y x) 3)
  (= y (+ y 1))) (if (> x y) (return x) (if (> (* x x) y) (return (* x x)) (if (> (* x (+ x x)) y)
    (return (* x (+ x x))) (return (- y 1)))))))

(define whileTest '((var x) (= x 10) (var y (+ (* 3 x) 5)) (while (!= (% y x) 3)
  (= y (+ y 1)))))