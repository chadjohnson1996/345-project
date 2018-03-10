(load "simpleParser.scm")
;use pretty big
;Chad Johnson, Griffin Saiia
;PLC Project 1 part 2

;hope we commented enough this time, main function to run interpreter is at bottom


;**************utility functions************


;creates the default state
(define createState
  (lambda ()
    '(((true false) (#t #f)))))

;creates an empty state frame                                                       
(define createStateFrame
  (lambda ()
    '(()())))

;adds an empty state frame to the state
(define addFrame
  (lambda (state)
    (cons (createStateFrame) state)))

;gets representation of empty variable
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
      ;if variable is null, the unassigned value, throw error, otherwise return it
      (else (getStateNoCheckAssign state key)))))

;gets state without checking assignment
(define getStateNoCheckAssign
  (lambda (state key)
    (cond
      ((number? key) key) ;if the key is a number just return it
      ((boolean? key) key) ;if the key is a boolean just return it
      (else (getStateHelper state key)))))

;helper method to get the state
(define getStateHelper
  (lambda (state key)
    (cond
     ((null? state) (error "Variable must be declared before reference"))
     ((null? (caar state)) (getStateHelper (cdr state) key))
     ((eq? key (caaar state)) (caadar state))
     (else (getStateHelper (cons (list (cdaar state) (cdadar state)) (cdr state)) key))))) 

;method called to update or declare a variable, calls the necesary helper according to the case
(define updateState
  (lambda (state lis)
    (cond
      ((isDeclared state (car lis)) (updateHelper state lis))
       (else (declareHelper state lis)))))

;updates an existing state, finds the appropriate frame
(define updateHelper
  (lambda (state lis)
    (cond
    ((null? state) (error "Invalid state, never should be hit")) ;more for us than for anything
    ((null? (caar state)) (cons (createStateFrame) (updateHelper (cdr state) lis)))
    ((eq? (car lis) (caaar state)) (cons (list (cons (car lis) (cdaar state)) (cons (cadr lis) (cdadar state))) (cdr state)))
    (else
     (let ((result (updateHelper (cons (list (cdaar state) (cdadar state)) (cdr state)) lis))) ;let used here to avoid tedious and complicated duplication of calls to updateHelper
              (cons (list (cons (caaar state) (caar result)) (cons (caadar state) (cadar result))) (cdr result))
              )))))

;declares a variable on current frame
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

 ;checks if a variable is declared on any layer
 (define isDeclared
   (lambda (state key)
     (cond
       ((null? state) #f)
       ((eq? (isDeclaredHelper (caar state) key) #t) #t)
       (else (isDeclared (cdr state) key)))))

 

;*********continuation preservation functions*************


 
;builds chain of continuations
;precedence goes to most recently added, so it functions with layering
(define continuationFactory
  (lambda (prev key continuation)
    (lambda (key2)
      (cond
        ((eq? key2 key) continuation)
        (else (prev key2))))))

;method for trimming frames off state after breaks, ensures the right states are there
(define revertToOldLevel
  (lambda (new old)
    (truncateLevels new (- (getDepth new) (getDepth old)))))

;gets the depth of a state
(define getDepth
  (lambda (state)
    (cond
     ((null? state) 0)
     (else (+ 1 (getDepth (cdr state)))))))

;truncates the specified numer of levels from the state
(define truncateLevels
  (lambda (state levels)
    (cond
      ((eq? levels 0) state)
      (else (truncateLevels (cdr state) (- levels 1))))))
       




;**********expression evaluator functions*********
;any operation that does not have side effects is below



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




;*************state mutator functions******************
;all operations with side effects below (including break/continue/throw/catch)




(define enterBlock
  (lambda (state lis continuations)
    (cdr (evalExpressionList (addFrame state) lis continuations)))) ;enters a block by adding a state frame and removing it when it is done

;uses 'continue to store appropriate jump point, see prepWhile
(define continueHandler
  (lambda (state lis continuations)
    ((continuations 'continue) state continuations)))
    ;gets the current continue handler and invokes it on the state
    

(define breakHandler
  (lambda (state lis continuations)
    ((continuations 'break) state)))
    ;gets the current break handler and invokes it on the state

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
     ;gets the return handler and invokes it on the state

;preps break and continue for while invokation
;then invokes while
(define prepWhile
  (lambda (state lis continuations)
    (call/cc (lambda (break)
               ;while handler is invoked on state and list but continuations are updated
               ;break continuation calls break and is passed the current state, pops any added state frames, and returns the stat
               ;without the added state frames
               ;the continue continuation is passed the state and current continuation list
               ;pops added frames off of state
               ;and calls while handler on state and passed continuations
               ;it passes the same continuations that are created here, but it is easier to pass them in to avoid generation issues
               (whileHandler state lis (continuationFactory
                                        (continuationFactory continuations 'break
                                                             (lambda (v)(break (revertToOldLevel v state)))) 'continue
                                                                                                             (lambda (state2 continuations2)
                                                                                                                  (break (revertToOldLevel (whileHandler state2 lis continuations2) state)
                                                                                                                         ))))))))
;nothing fancy here, all magic is above
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
    ;calls finally outside call/cc so no matter if the state is returned from regular block or exception block it is invoked
    (oMutate (call/cc
     (lambda (break)
       ;adds the catch block as a catch handler to the continuations and then invokes the try block
       (evalExpressionList state (car lis) (addCatchHandler state (getCatchPortion lis) continuations break))))(caddr lis) continuations) ))

;helper method to get catch portion of parsed "try" value
(define getCatchPortion
  (lambda (lis)
    (cond
      ((null? (cadr lis)) '()) ;if it is null return null
      (else (cdadr lis))))) ;else return catch block without the "catch" keyword

;adds a catch handler to the continuations and returns the continuations
(define addCatchHandler
  (lambda (state lis continuations break)
    (cond
      
      ((null? lis) continuations) ;if there is no catch block, return current continuations
      ;else add catch handler that pops added frames from state, adds thrown value to new layer on popped state
      ;invokes catch block on mutated state
      ;and then pops frames catch handler added and breaks with that state
      (else (continuationFactory continuations 'catch (lambda (state2 thrown)
                                                (break (revertToOldLevel (evalExpressionList (updateState (addFrame (revertToOldLevel state2 state)) (list (caar lis) thrown)) (cadr lis) continuations) state))))))))

;invokes finally expression list on state
(define finallyHandler
  (lambda (state lis continuations)
    (evalExpressionList state (car lis) continuations)))




;***********operation functions****************
;logistic functions that run the interpreter



;get function for mutators
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
      ((eq? operator 'continue) continueHandler)
      ((eq? operator 'break) breakHandler)
      (else (error operator)))))

;get function for expressions
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

;helper for sInterpreter that ensures that it carries appropriate continuations (calls bootstrapContinuations)
(define callInterpreter
  (lambda (state parsed)
    (call/cc (lambda (break)
               (sInterpreter state parsed (bootstrapContinuations break))))))

;helper that sets continuations
(define bootstrapContinuations
  (lambda (break)
    (continuationFactory (lambda (v) (error "Invalid use of continuation")) 'return break)))

;adds return handler to state - ensures return always bubbles up
(define addReturn
  (lambda (state callback)
    (addToFrameNoCheck state 'return callback)))

;adds the catch handler to the state
(define addCatch
  (lambda (state callback)
    (addToFrameNoCheck state 'catch callback)))

;umbrella function that interpreter runs inside
(define sInterpreter
  (lambda (state parsed continuations)
    (cond
      ((null? parsed) (error "no return specified"))
      (else (sInterpreter (oMutate state (car parsed) continuations) (cdr parsed) continuations)))))

;deals with a line of code at a time
(define evalExpressionList
  (lambda (state lis continuations)
    (cond
      ((null? lis) state)
      (else (evalExpressionList (oMutate state (car lis) continuations) (cdr lis) continuations)))))

;Mstate - gets appropriate mutator operation and calls it on contents
;mutator operator calls Mvalue function
(define oMutate
  (lambda (state lis continuations)
    (cond
      ((null? lis) state)
      (else ((getMutator (car lis)) state (cdr lis) continuations)))))

;Mvalue - gets appropriate expression operator and calls it on contents
(define oEval
  (lambda (state lis)
    (cond
      ((null? lis) lis)
      ((not (list? lis)) (getState state lis))
      ((null? (cddr lis)) ((getHandler (car lis)) state (list (oEval state (cadr lis)))))
      (else ((getHandler (car lis)) state (list (oEval state (cadr lis)) (oEval state (caddr lis))))))))

;masks boolean returns to return non-scheme terms
(define maskReturn
  (lambda (val)
    (cond
      ((eq? val #t) 'true)
      ((eq? val #f) 'false)
      (else val))))

;main - creates base state, calls parser on test file, starts interpreting process, masks return
(define interpret
  (lambda (fileName)
    (maskReturn (callInterpreter (createState) (parser fileName)))))









;************test cases*************

;debug interpreter for hardcoded variables
;tests used to build interpreter
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