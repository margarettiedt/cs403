(define (author)
	(println "AUTHOR: Margaret Tiedt jmtiedt@crimson.ua.edu")
	)

(define (exprTest # $expr target)
        (define result (catch (eval $expr #)))
        (if (error? result)
            (println $expr " is EXCEPTION: " (result'value)
                " (it should be " target ")")
            (println $expr " is " result
                " (it should be " target ")")
            )
        )

;------------------------------TASK 1---------------------------------

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
	;(println "when a=0 and x=0... my-if does not behave like if (like expected)")
 	;(println "if statement results: ")
	;(inspect (if (= a 0) x (/ a x)))
	;(println "my-if results: ")
	;(inspect (my-if (= a 0) x (/ a x)))
	)

;(run1)
 

;------------------------------TASK 2---------------------------------

(define (zeno_cost d c f)
	(define daktylos (/ 1.0 9600.0))
	(define hemibool (/ 1.0 12.0))

	(define (zHelp totalCost d c)
		(cond
			((<= d daktylos) (+ totalCost 7.0))
			((<= c hemibool) (+ totalCost hemibool))
			(else (zHelp (+ totalCost c) (/ d 2.0) (* c f)))
			)

		) ; end help

	(zHelp 0.0 d c)

	) ; end zeno_cost

(define (run2)
	(exprTest (zeno_cost 1 1 1) 21.0)
	(exprTest (zeno_cost 2 1 1) 22.0)
	(exprTest (zeno_cost 1 2 1) 35.0)
	(exprTest (zeno_cost 1 1 2) 16390.0)
	(exprTest (zeno_cost 2 2 1) 37.0)
	(exprTest (zeno_cost 2 1 2) 32774.0)
	(exprTest (zeno_cost 1 2 2) 32773.0)
	(exprTest (zeno_cost 2 2 2) 65541.0)
	(exprTest (zeno_cost 0 0 0) 7.0)
	(exprTest (zeno_cost 1 0 0) 0.0833333333)
	(exprTest (zeno_cost 1 1 0) 1.0833333333)
	)

;(run2)

;------------------------------TASK 3---------------------------------

(define (mandelbot-iter thresh)
	(define tester (mandelbot-iter j))
		(cond
			(if (= (tester x y) 0)
				(print "point (" x "," y ") is in the Mandelbrot set!\n")
				(print "point (" x "," y ") is NOT in the Mandelbrot set.\n")
				)
			)

		(define r ( + ( - (* r r) (*s s)) x ))
		(define s (+ (* 2 r s) y) )
		(if(> ( + (* r r) (*s s)) 4) "not in set")
		(else ())


		)

(define (run3)
	(exprTest ((mandelbrot-iter 100) 2 2) 1)
	(exprTest ((mandelbrot-iter 100) 1 1) 2)
	(exprTest ((mandelbrot-iter 100) -2 -2) 1)
	(exprTest ((mandelbrot-iter 100) -1 -1) 3)
	(exprTest ((mandelbrot-iter 100) .5 .5) 5)
	(exprTest ((mandelbrot-iter 100) -.5 -.5) 0)
	(exprTest ((mandelbrot-iter 10000) -.5 -.5) 0)
	(exprTest ((mandelbrot-iter 100) .4 .2) 31)
	(exprTest ((mandelbrot-iter 100) .2 .4) 0)
	(exprTest ((mandelbrot-iter 10000) .2 .4) 0)
	(exprTest ((mandelbrot-iter 100) .364 .36) 72)
	(exprTest ((mandelbrot-iter 100) .363 .36) 93)
	)








;--------------------------------TASK 4-------------------------------

(define (average x y) ; basic function to calculate average of two numbers
	(/ (+ x y) 2.0))

(define (square x) (* x x)) ; calculates the square of a number

(define (root3 x)

	(define (binaryAlg f firstGuess) ;binary algorithm for guessing cube root
		(define (checkGuess guess) ; checks to see if current guess will be equal to the next guess
			(let ((next (f guess)))
				(if (= guess next)
					next ; this means our estimate is good, return "next" as our final answer
					(checkGuess next) ; otherwise keep going
					) ; end if
				) ; end let
			) ; end checkGuess
		(checkGuess firstGuess))

	(define (average-damp f) ;average-damp function defined in book
		(lambda (x) (average x (f x))))

	(binaryAlg (average-damp (lambda (y) (/ (+ (/ x (square y) )(* 2 y) ) 3 ) ) ) 1.0)
	; The above statement uses Newton's Method for cube roots to estimate our answers! 
	)

; run function for root3
(define (run4)
	(inspect (root3 2))
	(inspect (root3 5.05))
	(inspect (root3 8.00))
	(inspect (root3 100))
	)

;(run4)


;--------------------------------TASK 5-------------------------------

;TO CLARIFY: variable r denotes the RIGHT element, variable ro denotes the ROW number!
(define (crazyTriangle l r n)
 	(define (crazyTriangleHelper ro c) ;function to help calculate values to display
 		(cond
 			((== c 0) l) ;when column is 0, print the left element
 			((== ro c) r) ;when row = column, print the right element
 			(else (+ (crazyTriangleHelper (- ro 1) (- c 1)) (crazyTriangleHelper (- ro 1) c)))
 			)
		) ;end crazyTriangleHelper

 	(define (iterateRow ro) ;function to recursively iterate through the rows
 		;(define diff (- n ro)) ; difference in depth and current row
 		(printHelper (-n ro)) 
 		(define (iterateCol c) ;function to recursively iterate through the columns
 			(cond
 				((<= c ro) (print (crazyTriangleHelper ro c) " ") (iterateCol (+ c 1)))
 				)
 			) ; end iterateCol

 		(cond
 			((< ro n) (iterateCol 0) (println) (iterateRow (+ ro 1)))
 			)	
 		) ;end iterateRow

 	(define (printHelper x) ; function that helps print spaces for "pretty printing" (centered around axis)
 		(cond 
 			((>= x 0) (print " ") (printHelper (- x 1)) )
 			)
 		) ;end printHelper

 	(iterateRow 0) ;initial call to start iteration
	) ;end crazyTriangle


(define (run5)
	(inspect (crazyTriangle 1 1 6))
	(inspect (crazyTriangle 1 2 6))
	(inspect (crazyTriangle 1 1 13))
 	) ;end run5


;(run5)


;--------------------------------TASK 6-------------------------------

(define (oppy oper1)
	(lambda (var1)
		(lambda (oper2)
			(lambda (var2)
				(lambda (var3) (oper1 var1 (oper2 var2 var3))
					)
				)
			)
		)

	)

(define (run6)
	(inspect (((((oppy +) 9) *) 7) 5))
	(exprTest (((((oppy +) 9) *) 7) 5) 0)
	(exprTest (((((oppy *) 3) +) 5) 4) 27)
	(exprTest (((((oppy *) 6) -) -7) 2) -54)
	(exprTest (((((oppy /) 9) +) 4) 7.0) 0.8181818182)
	(exprTest (((((oppy /) -2) -) 6) 4) -1)
	(exprTest (((((oppy +) -3) *) 7) 9) 60)
	(exprTest (((((oppy -) 3) /) 7.0) 9) 2.2222222222)
	(exprTest (((((oppy *) 9) *) 9) 9) 729)
	(exprTest (((((oppy *) 0) *) 0) 0) 0)
	(exprTest (((((oppy *) 1000) /) 1) 1000) 0)
	(exprTest (((((oppy *) 1000000) +) 1000) 1000) 2000000000)

	)

;(run6)


;--------------------------------TASK 7---------------------

(define (shank f i)
	(define (w)
		(cond
			((= i 0) (f i))
			(else 
				(define (sum currentIndex n total)
					(cond
						((> currentIndex n) total)
						(else (sum (+ currentIndex 1) n (+ total (f currentIndex)))) ; end else
						) ;end cond
					) ; end sum

				(define S (sum 0 (- i 1) 0)) 
				(define numerator (- (* (+ S (f i) (f (+ i 1))) S) (square (+ S (f i)))))
				(define denominator (+(- (+ S (f i) (f (+ i 1))) (* 2 (+ S (f i)))) S))

				(/ numerator denominator) ; return this statement!
				) ; end big else
			) ; end cond
		); end w

		(w) 

	) ; end shank

(define (run7)
	(inspect(shank (lambda (x) x) 5))
	(exprTest (shank square 0) 0) ; the 'problem' functions
	(exprTest (shank square 15.0) -618.06451613) ; the 'problem' functions
	(exprTest (shank square 1000) 503389) ; the 'problem' functions
	(exprTest (shank sqrt 10000) -1333433.5623)
	(exprTest (shank sqrt 15.0) -81.498536939)
	(exprTest (shank sqrt 0) 0.0)
	)

;(run7)



;---------------------------TASK 8--------------------------------

(define (egypt* x y)
	(define (egyptHelp a b c)
		(cond
			((> a b) (egyptHelp2 a b c 0))
			(else (egyptHelp (+ a a) b (+ c c)))
			)
		) ;end help1

	(define (egyptHelp2 a b c d)
		(cond
			((= b 0) d)
			;((<= a b) (egyptHelp2 (/ a 2) (- b a) (/ c 2) (+ d c)))
			((<= a b) (egyptHelp2 (halve a) (- b a) (halve c) (+ d c)))
			(else (egyptHelp2 (/ a 2) b (/ c 2) d))
			)
		) ;end help2
	(egyptHelp 1 x y)

	) ;end egypt*

(define (halve x)
	(define (counter c x)
		(cond
			((>= x 2) (counter (+ c 1) (- x 2)))
			(else c)
			)
		) ; end counter
	(counter 0 x)

	) ;end halve

(define (run8)
	(exprTest (egypt* 0 0) 0)
	(exprTest (egypt* 0 1) 0)
	(exprTest (egypt* 1 0) 0)
	(exprTest (egypt* 1 1) 1)
	(exprTest (egypt* 1 2) 2)
	(exprTest (egypt* 2 1) 2)
	(exprTest (egypt* 2 2) 4)
	(exprTest (egypt* 100 100) 10000)
	(exprTest (egypt* 16 16) 256)
	(exprTest (egypt* 17 13) 221)
	(exprTest (egypt* 303 0) 0)
	(exprTest (egypt* 0 303) 0)
	(exprTest (halve 56) 28)
	)

;(run8)


;-----------------------------TASK 9---------------------------------


(define (mystery n)
	(define (mysteryHelper n total)
		(cond
			((= n 0) (+ total 1))
			(else
				(cond
					((= 0 (% n 2)) (mysteryHelper (- n 1) (/ 1.0 (+ 2.0 total))))
					(else (mysteryHelper (- n 1) (/ 1.0 (+ 1.0 total))))
					)
				) 
			)
		;(println "total: " total)
		) ; end mysteryHelper
	(mysteryHelper n 0)
	) ; end mystery

(define (run9)
	(exprTest (mystery 2) 1.6666666667)
	(inspect (mystery 1))
	)

;(run9)


;-----------------------------TASK 10--------------------------------

(define (rama d x)
	(define (ramaHelper counter)
		(cond
			((= counter d) 1.0)
			(else 
				(* (sqrt(* (+ 1.0 (+ counter x)) (ramaHelper (+ counter 1) ) )))
				) ; end else
			)
		) ; end ramaHelper

	(if(= d 0) 0
		(ramaHelper 0)
		) ; end if
	) ; end rama (recursive)

; (define (irama d x)
; 	(define total 1)
; 	(if(= d 0) 0)
; 	(define (ramaIter dep)
; 		(cond
; 			((= 0 dep) total)
; 			(else 
; 				total = (* total (sqrt(* (+ 1 (+ (- dep 1) x)) (ramaHelper (- 1 dep) ) )))
; 				) ; end else
; 			)
; 		) ; end ramaHelper
; 	(ramaHelper d)

;	) ; end irama



(define (irama d x)
	
	(define (iramaIter prod count max)
		(cond 
			((= count max) 1 )
			(else 
				(println "inside new prod sqrt : " (+ 1 (+ (- d (+ count 1)) x)))
				(println "current prod at count " count " : " prod )
				(* (iramaIter (sqrt(+ 1 (+ (- d (+ count 1)) x))) (+ count 1) max) prod)
				) ;end else
			) ;end cond
		) ;end iramaIter


	(if(= d 0) 
		0
		(iramaIter 1.0 0 d)
		) ; end if

	; (cond
	; 	((= d 0) (define y 1))
	; 	(else   (define y (+ 1 (+ x (- d 1) ) ) ) (println "first value at d = " d " is " y )          
	; 		( sqrt (* y (irama (- d 1) x) ) ) ) ;end else
	; 	) ; end cond

	) ; end irama

(define (run10)
	(exprTest (irama 2 3) 2.99069756)
	(exprTest (rama 0 3) 0)
	(exprTest (rama 1 3) 2.0000000000)
	(exprTest (rama 0 0) 0)
	(exprTest (rama 0 1) 0)
	(exprTest (rama 1 0) 1.0)
	(exprTest (rama 1 1) 1.4142135624)
	(exprTest (rama 1 2) 1.7320508076)
	(exprTest (rama 2 2) 2.2360679775)
	(exprTest (rama 3 3) 3.2951592364)
	(exprTest (rama 7 4) 4.9198021247)
	(exprTest (rama 6 11) 11.497813243)
	(exprTest (rama 116 87) 88.000000000)
	(exprTest (rama 116 11793) 11794.000000)
	)

(run10)




