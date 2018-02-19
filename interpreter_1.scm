;Issues: 1. getting a fuck ton of null lists before the correct return value.
;        2. when there are multiple return possibilities, we are at risk of
;           the last valid return call in our parser list overwriting the old once
;        ---> I tried to simply initialize a return variable to '() or done
;             and have the first statement of sInterpreter test if it value changes
;             but for whatever reason, scheme doesn't catch that the return of
;             (getState 'return) is equal to either '() or done. sooo we have to 
;             catch that some how or try to implement the previous method

load "simpleParser.scm"
;use pretty big
;Griffin Saiia, Chad Johnson
;PLC Project 1

;test
(define test '((var x) (= x 10) (var y (+ (* 3 x) 5)) (while (!= (% y x) 3)
  (= y (+ y 1))) (if (> x y) (return x) (if (> (* x x) y) (return (* x x)) (if (> (* x (+ x x)) y)
    (return (* x (+ x x))) (return (- y 1)))))))

;variable storage
(define stateList '(('return)('())))

(define emptyVar
  (lambda (var)
    (cons var '(()))))

;(void) abstraction - (set! x x) returns #<void> as output
;done allows me to check for that cleanly
(define done (list (void)))



;whenever adding or updating variables
;takes varPair - a list whose first element is the variable name, and whose second is the value
;returns current stateList
;test cases: (a 10), (a '()), (a 17)
(define updateState
  (lambda (varPair)
    ;checks if variable exists, if it does it changes the value for it - if not, it adds variable and value
    ;lis is the variable half of the stateList
    ;index keeps track of how deep it goes into the variable array in case of a value change on a predefined variable
    (define (updateList var value lis index)
      (cond
        ;if lis is null, then variable has not been defined, thus we just cons variable and value to stateList
        ((null? lis) (set! stateList (list (cons var (car stateList)) (cons value (cadr stateList)))))
        ;if var is equal to an active variable, the index is passed to changeValue
        ((eq? var (car lis)) (set! stateList (list (car stateList) (changeValue value (cadr stateList) index))))
        ;recursive call that increments index
        (else (updateList var value (cdr lis) (+ index 1)))))
    ;handles whenever a variable's value needs changed
    (define (changeValue value lis j)
      (cond
        ;checks for null just in case index goes out of bounds
        ((null? lis) (cadr stateList))
        ;checks index against 0
        ((eq? 0 j) (cons value (cdr lis)))
        ;recursive call that decrements index until at target value
        (else (cons (car lis) (changeValue value (cdr lis) (- j 1))))))
    ;logic for running helpers
    (cond
      ;if passed a null, returns stateList
      ;((null? varPair) stateList)
      ((null? varPair) '())
      ;this was a way to get updateState to accomplish two things, change stateList
      ;and return stateList - see 'done' definition for further info
      ;((equal? done varPair) stateList)
      ((equal? done varPair) '())
      ;recursive call feeding updateList's output into updateState
      (else (updateState (list (updateList (car varPair) (cadr varPair) (car stateList) 0)))))))


;takes variable
;returns value or '() if no value, returns done if the variable is undefined
(define getState
  (lambda (var)
    ;takes variable we're searching for, a list, and start index
    ;returns index of variable if it exists
    (define (checkList var lis index)
      (cond
        ;if lis is null, then variable has not been defined, thus invalid index is returned
        ((null? lis) -1)
        ;if equal, returns index
        ((eq? var (car lis)) index)
        ;iterates down list, increments index
        (else (checkList var (cdr lis) (+ index 1)))))
    ;takes list and target index
    ;returns value at given index
    (define (fetchValue lis index)
      (cond
        ;if passed -1, variable was never defined
        ((eq? index -1) done)
        ;once index gets down to 0, we have our value
        ((eq? index 0) (car lis))
        ;recursives down list decrementing index
        (else (fetchValue (cdr lis) (- index 1)))))
    ;logic for running helpers
    (cond
      ;if passed a null, returns null
      ((null? var) '())
      ((number? var) var)
      ;calls fetchValue on the value half of stateList, and the index returned by checkList
      (else (fetchValue (cadr stateList) (checkList var (car stateList) 0))))))


;handles if operator
(define ifHandler
  (lambda (lis)
    (cond
      ((oEval (car lis)) (oEval (cadr lis)))
      (else '()))))

(define addHandler
  (lambda (lis)
    (+ (oEval (car lis)) (oEval (cadr lis)))))

(define subtractHandler
  (lambda (lis)
    (- (oEval (car lis)) (oEval (cadr lis)))))

(define multiplyHandler
  (lambda (lis)
    (* (oEval (car lis)) (oEval (cadr lis)))))

(define divideHandle
  (lambda (lis)
    (/ (oEval (car lis)) (oEval (cadr lis)))))

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

(define assignHandler
  (lambda (lis)
    (cond
      ((equal? (getState (car lis)) done) error)
      ((list? (cadr lis)) (updateState (cons (car lis) (cons (oEval (cadr lis)) '()))))
      (else (updateState (cons (car lis) (cdr lis)))))))


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
      ((equal? (oEval (car lis)) #t) (begin
                                      (oEval (cadr lis))
                                      (whileHandler lis)))
      (else '()))))


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
      ((eq? operator 'return) returnHandler)
      ((eq? operator 'if) ifHandler)
      ((eq? operator 'var) declareHandler)
      ((eq? operator 'while) whileHandler)
      ((eq? operator '=) assignHandler)
      (else getState))))


(define sInterpreter
  (lambda (parsed)
    (cond
      ((null? parsed) (getState 'return))
      (else (cons (oEval (car parsed)) (sInterpreter (cdr parsed)))))))

(define oEval
  (lambda (lis)
    (cond
      ((null? lis) lis)
      ((not (list? lis)) (getState lis))
      ((list? (cadr lis)) ((getHandler (car lis)) (list (oEval (cadr lis)) (oEval (caddr lis)))))
      (else ((getHandler (car lis)) (cdr lis))))))
