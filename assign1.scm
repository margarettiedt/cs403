(define (author)
	(println "AUTHOR: Margaret Tiedt jmtiedt@crimson.ua.edu")
	)

(define (exprTest # $expr target)
	(define result (catch (eval $expr #)))
	if (error? result)
        	(println $expr " is EXCEPTION: " (result'value)
             	" (it should be " target ")")
         	(println $expr " is " result
             	" (it should be " target ")")
            )

;******************************* TASK 1 *********************************

(define (my-if a b c)
	(if (true? a)
		b
		c
		)
	)

; We are checking for the case where a=0 and x=0. This is where my-if will deivate
; from the expected behavior. This is because for my-if, statement c will execute 
; whether or not a is true or not. For if, when a is false, statement c will not execute!

 (define (run1)
 	(define x 0)
 	(define a 0)
	(println "")
 	(println " -------------Test 1------------")
	(println "")
	(println "when a=0 and x=0... my-if does not behave like if (like expected)")
 	(println "if statement results: ")
	(inspect (if (= a 0) x (/ a x)))
	(println "my-if results: ")
	(inspect (my-if (= a 0) x (/ a x)))
	)

;(run1)

;******************************* TASK 2 *********************************
 

 ;******************************* TASK 2 *********************************
(define (zeno_cost d c f)
	(define daktylos (/ 1.0 9600.0))
	(define hemibool (/ 1.0 12.0))
	(define (flagger d c f first)
		(define cf (* c f))
		(cond
			((true? first)
				(cond
					((<= d daktylos) 7.0)
					((<= cf hemibool) hemibool)
					(else (flagger d c f #f))
					)
				)
			(else 
				(cond
					((<= d daktylos) (+ c 7.0))
					((<= cf hemibool) (+ c hemibool))
					(else (flagger (/ d 2.0) cf f #f))
					)
				) ;end else
			)
		) ; end flagger function
	(flagger d c f #t)
	) ; end zeno_cost

(define (run2)
	(inspect (zeno_cost 1 1 2))
	(inspect (zeno_cost 2 10 3))
	(inspect (zeno_cost 8 10 2))
	(inspect (zeno_cost 1 1 1))
	)

;(run2)

;------------------------------TASK 3---------------------------------

(define (mandelbot-iter thresh)
	(define (tester x y)
		(define ())))






;--------------------------------TASK 4-------------------------------

(define (average x y) 
	(/ (+ x y) 2.0))

(define (square x) (* x x))

(define (root3 x)

	(define (binaryAlg f firstGuess)
		(define (checkGuess guess)
			(let ((next (f guess)))
				(if (= guess next)
					next
					;(println "next: " next)
					(checkGuess next)
					) ; end if
				) ; end let
			) ; end checkGuess
		(checkGuess firstGuess))

	(define (average-damp f)
		(lambda (x) (average x (f x))))

	(binaryAlg (average-damp (lambda (y) (/ (+ (/ x (square y) )(* 2 y) ) 3 ) ) ) 1.0)
	)


(define (run4)
	(inspect (root3 2))
	(inspect (root3 5.05))
	(inspect (root3 8.00))
	(inspect (root3 100))
	)



;(run4)


;--------------------------------TASK 5-------------------------------

(define (crazyTriangle l r n)
 	
 	(define (helper ro c)
 		(cond
 			((== c 0) l)
 			((== ro c) r)
 			(else (+ (helper (- ro 1) (- c 1)) (helper (- ro 1) c)))
 			)
		
		)

 	(define (iterateRow ro)

 		(define diff (- n ro))
 		(printHelper diff)

 		(define (iterateCol c)
 			(cond
 				((<= c ro) (print (helper ro c) " ") (iterateCol (+ c 1)))
 				)
 			)

 		(cond
 			((< ro n) (iterateCol 0) (println) (iterateRow (+ ro 1)))
 			)	

 		)

 	(define (printHelper x)
 		(cond 
 			((>= x 0) (print " ") (printHelper (- x 1)) )
 			)
 		)	

 	(iterateRow 0)

	)


(define (run5)
	(inspect (crazyTriangle 1 1 6))
	(inspect (crazyTriangle 1 2 6))
 	)


(run5)


;--------------------------------TASK 6-------------------------------














;--------------------------------TASK 7---------------------
