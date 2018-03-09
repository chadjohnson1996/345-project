(load "simpleParser.scm")
;use pretty big
;Griffin Saiia, Chad Johnson
;PLC Project 1

;creates the state
(define createState
  (lambda ()
    '(((true false) (#t #f)))))

;builds chain of continuations to test
(define continuationFactory
  (lambda (prev key continuation)
    (lambda (key2)
      (cond
        ((eq? key2 key) continuation)
        (else (prev key2))))))

;methods for trimming frames off state after breaks etc
(define revertToOldLevel
  (lambda (new old)
    (truncateLevels new (- (getDepth new) (getDepth old)))))

(define getDepth
  (lambda (state)
    (cond
     ((null? state) 0)
     (else (+ 1 (getDepth (cdr state)))))))

(define truncateLevels
  (lambda (state levels)
    (cond
      ((eq? levels 0) state)
      (else (truncateLevels (cdr state) (- levels 1))))))
    
                                                        
(define createStateFrame
  (lambda ()
    '(()())))

(define addFrame
  (lambda (state)
    (cons (createStateFrame) state)))

(define emptyVar
  (lambda (var)
    (cons var '(()))))

;adds values to lowest state frame without checking. Used for return and try/catch/finally
(define addToFrameNoCheck
  (lambda (state key value)
    (cons (list (cons key (caar state)) (cons value (cadar state))) (cdr state))))
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
    (cond
      ((null? state) (error key))
     ((null? state) (error "Variable must be declared before reference"))
     ((null? (caar state)) (getStateHelper (cdr state) key))
     ((eq? key (caaar state)) (caadar state))
     (else (getStateHelper (cons (list (cdaar state) (cdadar state)) (cdr state)) key))))) 

(define updateState
  (lambda (state lis)
    (cond
      ((isDeclared state (car lis)) (updateHelper state lis))
       (else (declareHelper state lis)))))

(define updateHelper
  (lambda (state lis)
    (cond
    ((null? state) (error "Invalid state, never should be hit"))
    ((null? (caar state)) (cons (createStateFrame) (updateHelper (cdr state) lis)))
    ((eq? (car lis) (caaar state)) (cons (list (cons (car lis) (cdaar state)) (cons (cadr lis) (cdadar state))) (cdr state)))
    (else
     (let ((result (updateHelper (cons (list (cdaar state) (cdadar state)) (cdr state)) lis))) ;let used here to avoid tedious and complicated duplication of calls to updateHelper
              (cons (list (cons (caaar state) (caar result)) (cons (caadar state) (cadar result))) (cdr result))
              )))))
  
(define declareHelper
  (lambda (state lis)
    (cons (list (cons (car lis) (caar state)) (cons (cadr lis) (cadar state))) (cdr state))))
        
;helper method to check if a variable is declared
(define isDeclaredHelper
  (lambda (lis key)
    (cond
      ((null? lis) #f)
      ((eq? (car lis) key) #t)
      (else (isDeclaredHelper (cdr lis) key)))))

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

;enters a block


(define enterBlock
  (lambda (state lis continuations)
    (cdr (evalExpressionList (addFrame state) lis continuations))))

(define continueHandler
  (lambda (state lis continuations)
    ((continuations 'continue) state continuations)))
    

(define breakHandler
  (lambda (state lis continuations)
    ((continuations 'break) state)))

(define ifHandler
  (lambda (state lis continuations)
    (cond
      ((oEval state (car lis)) (oMutate state (cadr lis) continuations))
      ((not (null? (cddr lis))) (oMutate state (caddr lis) continuations))
      (else state))))


(define assignHandler
  (lambda (state lis continuations)
    (cond
      ((and (getStateNoCheckAssign state (car lis)) #f) (error "Variable cannot be assigned to before declaration")) ;condition never evaulates to true, only to raise error if not set 
      ((list? (cadr lis)) (updateState state (cons (car lis) (cons (oEval state (cadr lis)) '()))))
      (else (updateState state (cons (car lis) (cons (oEval state ( cadr lis)) '())))))))


(define declareHandler
  (lambda (state lis continuations)
    (cond
      ((null? (cdr lis)) (updateState state (emptyVar (car lis))))
      (else (updateState state (cons (car lis) (cons (oEval state (cadr lis)) '())))))))

(define returnHandler
  (lambda (state lis continuations)
    ((continuations 'return)(oEval state (car lis)))))

;preps break and continue for while invokation
(define prepWhile
  (lambda (state lis continuations)
    (call/cc (lambda (break)
               (whileHandler state lis (continuationFactory
                                        (continuationFactory continuations 'break
                                                             (lambda (v)(break (revertToOldLevel v state)))) 'continue
                                                                                                             (lambda (state2 continuations2)
                                                                                                                  (break (revertToOldLevel (whileHandler state2 lis continuations2) state)
                                                                                                                         ))))))))
(define whileHandler
  (lambda (state lis continuations)
    (cond
      ((equal? (oEval state (car lis)) #t) 
                                                      (whileHandler (oMutate state (cadr lis) continuations) lis continuations))
      (else state))))


(define throwHandler
  (lambda (state lis continuations)
    ((continuations 'catch) state (oEval state (car lis)))))

(define tryHandler
  (lambda (state lis continuations)
    (oMutate (call/cc
     (lambda (break)
       (evalExpressionList state (car lis) (addCatchHandler state (getCatchPortion lis) continuations break))))(caddr lis) continuations) ))

(define getCatchPortion
  (lambda (lis)
    (cond
      ((null? (cadr lis)) '())
      (else (cdadr lis)))))

(define addCatchHandler
  (lambda (state lis continuations break)
    (cond
      ((null? lis) continuations)
      (else (continuationFactory continuations 'catch (lambda (state2 thrown)
                                                (break (revertToOldLevel (evalExpressionList (updateState (addFrame (revertToOldLevel state2 state)) (list (caar lis) thrown)) (cadr lis) continuations) state))))))))
       
(define catchHandler
  (lambda (state lis continuations)
    ((continuations 'catch) state (car lis))))
    

(define finallyHandler
  (lambda (state lis continuations)
    (evalExpressionList state (car lis) continuations)))
       
(define getMutator
  (lambda (operator)
    (cond
      ((eq? operator 'return) returnHandler)
      ((eq? operator 'if) ifHandler)
      ((eq? operator 'var) declareHandler)
      ((eq? operator 'while) prepWhile)
      ((eq? operator '=) assignHandler)
      ((eq? operator 'begin) enterBlock)
      ((eq? operator 'throw) throwHandler)
      ((eq? operator 'try) tryHandler)
      ((eq? operator 'finally) finallyHandler)
      ((eq? operator 'catch) catchHandler)
      ((eq? operator 'continue) continueHandler)
      ((eq? operator 'break) breakHandler)
      (else (error operator)))))

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

(define callInterpreter
  (lambda (state parsed)
    (call/cc (lambda (break)
               (sInterpreter state parsed (bootstrapContinuations break))))))

(define bootstrapContinuations
  (lambda (break)
    (continuationFactory (lambda (v) (error "Invalid use of continuation")) 'return break)))

(define addReturn
  (lambda (state callback)
    (addToFrameNoCheck state 'return callback)))

;adds the catch handler to the state
(define addCatch
  (lambda (state callback)
    (addToFrameNoCheck state 'catch callback)))
    
(define sInterpreter
  (lambda (state parsed continuations)
    (cond
      ((null? parsed) (error "no return specified"))
      (else (sInterpreter (oMutate state (car parsed) continuations) (cdr parsed) continuations)))))

(define evalExpressionList
  (lambda (state lis continuations)
    (cond
      ((null? lis) state)
      (else (evalExpressionList (oMutate state (car lis) continuations) (cdr lis) continuations)))))

(define oMutate
  (lambda (state lis continuations)
    (cond
      ((null? lis) state)
      (else ((getMutator (car lis)) state (cdr lis) continuations)))))

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
    (maskReturn (callInterpreter (createState) (parser fileName)))))


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

(define emptyStateTest '((()())))
(define setStateTest '(((y) (7)) ((x) (5))))
(define test1State '(((x return true false) ((20) ((20)) #t #f))))