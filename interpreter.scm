load "simpleParser.scm"
;use pretty big

(define ifHandler
  (lambda (lis)
    (cond
      ((eval (car lis)) (eval (cadr lis)))
      (else '()))))

;insert key in state with value if it doesn't exist, if it exists update it
(define updateState
  (lambda (key value)
    ))

;return value of key in state, some error condition if it is not 
(define getState
  (lambda (key)))

(define addHandler
  (lambda (lis)
    (+ (eval (car lis)) (eval (cadr lis)))))

(define eval
  (lambda (element)
    (cond
      ((list? element) ( (getHandler (car element)) (cdr element)))
    
(define getHandler
  (lambda (operator)
    (cond
      ((eq? operator '+) addHandler)
      ((eq? operator 'if) ifHandler)
      (else '()))))

     
      
