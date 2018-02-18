load "simpleParser.scm"
;use pretty big
;Griffin Saiia
;PLC Project 1


;variable storage
(define stateList '(()()))

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
      ((null? varPair) stateList)
      ;this was a way to get updateState to accomplish two things, change stateList
      ;and return stateList - see 'done' definition for further info
      ((equal? done varPair) stateList)
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
      ;calls fetchValue on the value half of stateList, and the index returned by checkList
      (else (fetchValue (cadr stateList) (checkList var (car stateList) 0))))))

(define ifHandler
  (lambda (lis)
    (cond
      ((eval (car lis)) (eval (cadr lis)))
      (else '()))))

(define addHandler
  (lambda (lis)
    (+ (eval (car lis)) (eval (cadr lis)))))

(define multiplyHandler
  (lambda (lis)
    (* (eval (car lis)) (eval (cadr lis)))))

(define divideHandle
  (lambda (lis)
    (/ (eval (car lis)) (eval (cadr lis)))))

(define modHandler
  (lambda (lis)
    (modulo (eval (car lis)) (eval (cadr lis)))))

(define equalHandler
  (lambda (lis)
    (cond
      ((equal? (eval (car lis)) (eval (cadr lis))) #t)
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
      ((> (eval (car lis)) (eval (cadr lis))) #t)
      (else #f))))

(define lessHandler
  (lambda (lis)
    (cond
      ((< (eval (car lis)) (eval (cadr lis))) #t)
      (else #f))))

(define assignHandler
  (lambda (lis)
    (cond
      ((equal? (getState (car lis)) done) error)
      (else (updateState (cons (car lis) (cons (eval (cdr lis)) '())))))))
    

(define declareHandler
  (lambda (lis)
    (updateState (cons (car lis) (cons (eval (cdr lis)) '())))))

(define returnHandler
  (lambda (lis)
    (eval lis)))

(define whileHandler
  (lambda (lis)
    (cond
      ((equal? (eval (car lis)) #t) (begin
                                      (eval (cadr lis))
                                      (whileHandler lis)))
      (else '()))))

    
(define getHandler
  (lambda (operator)
    (cond
      ((eq? operator '+) addHandler)
      ((eq? operator 'if) ifHandler)
      (else '()))))

(define eval
  (lambda (element)
    (cond
      ((list? element) ( (getHandler (car element)) (cdr element)))

     
      
