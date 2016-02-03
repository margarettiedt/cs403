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

;--------------------------------------------TASK 1-------------------------------------------

; We are checking for the case where y=0 and x=0. This is where my-if will deivate
; 		from the expected behavior. This is because for my-if, statement c will execute 
; 		where for 'if', statement c will not execute. 
; For the typical if statement, when a is true, statement b will be executed and statement
;		c will be ignored because it is the "else clause". C will only be executed when the
;		condition in a is false and b is ignored.


(define (my-if a b c) ;my-if given to us in task prompt
	(if (true? a)
		b
		c
		)
	)

 (define (run1)
 	(define x 0)
 	(define y 0)
	(exprTest(if (= y 0) x (/ y x)) 0)
	(exprTest(my-if (= y 0) x (/ y x)) "divide by 0 error")
	)

;(run1)
 

;-----------------------------------------TASK 2----------------------------------------------

; Computes a price of a ticket for Zeno Airlines using a recursive process
; d = distance of flight in stadia, c = cost of first half of trip in drachma, f= cost factor
; The function zeno_cost takes in a d, c, f and uses zHelp to keep track of the 
;		totalCost and update the remaining distance and new cost.

(define (zeno_cost d c f)
	(define daktylos (/ 1.0 9600.0)) ;given by definition in prompt
	(define hemibool (/ 1.0 12.0)) ;also given by definition in prompt

	(define (zHelp totalCost d c)
		(cond
			((<= d daktylos) (+ totalCost 7.0)) ; when the d gets this small, add 7 to cost and end recursion
			((<= c hemibool) (+ totalCost hemibool))  ; when cost gets this small, add a hemibool and end recursion
			(else (zHelp (+ totalCost c) (/ d 2.0) (* c f))) ; call zHelp again with updated variables
			)
		) ; end help

	(zHelp 0.0 d c) ; first call to zHelp, starts totalCost at 0

	) ; end zeno_cost

(define (run2) ;run function with test cases
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

;-----------------------------------------TASK 3-----------------------------------------------

(define (square x) (* x x)) ; basic function we will use to calculate the square of a number

; Mandelbrot-iter takes in a treshhold (tresh) and then returns another function. This second
;		function takes in a point (x,y) and determines if this point is within the Mandelbrot set
;		for the given threshold. We first test for divergence and return the current count if it 
;		diverges. Then we check to see if we have reached the threshold, and return 0 if we have.
;		If neither of these conditions are met, we continue out tests by going through another cycle.		

(define (mandelbrot-iter thresh)
	(lambda (x y) ; (x,y) is our point to test
		(define (tester count r s) ;tester function with count variable to keep track of current cycle
			(define rEquation (+ (- (square r) (square s) )x )) ;equation for r given in prompt
			(define sEquation  (+ (* 2 r s) y)) ;equation for s given in prompt

			(cond
				((> (+ (square rEquation) (square sEquation)) 4) (+ count 1)) ;condition for divergence given in prompt
				((= count thresh) 0) ;if we have reached treshold, return 0
				(else
					(tester (+ count 1) rEquation sEquation) ;continue testing
					) 
				) 
			) ;end tester

		(tester 0 0.0 0.0) ;inital call with count=0, r and s = 0.0
		) ;ends lambda function
	) ;end mandel-iter

(define (run3) ;run function with test cases
	(exprTest ((mandelbrot-iter 100) 2 2) 1)
	(exprTest ((mandelbrot-iter 100) 1 1) 2)
	(exprTest ((mandelbrot-iter 100) -2 -2) 1)
	(exprTest ((mandelbrot-iter 100) -.5 -.5) 0)
	(exprTest ((mandelbrot-iter 10000) -.5 -.5) 0)
	(exprTest ((mandelbrot-iter 100) .4 .2) 31)
	(exprTest ((mandelbrot-iter 10000) .2 .4) 0)
	(exprTest ((mandelbrot-iter 100) .364 .36) 72)
	) ;end run3

;(run3)


;--------------------------------------------TASK 4--------------------------------------------

; Root3 function calculates the cube root of 'x' using Newton's method for cube roots
;		that was defined in our textbook.

(define (average x y) ; basic function to calculate the average of two numbers
	(/ (+ x y) 2.0))

(define (root3 x)
	; my binaryAlg function is loosely based off of some code from our textbook 
	(define (binaryAlg f firstGuess) ;binary algorithm for guessing cube root
		(define (checkGuess guess) ; checks to see if current guess will be equal to the next guess
			(let ((next (f guess)))
				(if (= guess next)
					next ; this means our estimate is good, return "next" as our final answer
					(checkGuess next) ; otherwise keep going
					) ; end if
				) ; end let
			) ; end checkGuess
		(checkGuess firstGuess) ;initial call to checkGuess using our firstGuess
		) ;end binaryAlg

	(define (average-damp f) ;average-damp function defined in book
		(lambda (x) (average x (f x))) ;returns another function 
		) ;end average-damp

	(binaryAlg (average-damp (lambda (y) (/ (+ (/ x (square y) )(* 2 y) ) 3 ) ) ) 1.0)
	; The above statement uses Newton's Method for cube roots to estimate our answers! 
	) ;end root3

(define (run4) ; run function for root3 with test cases
	(exprTest (root3 2) 1.2599210499)
	(exprTest (root3 5.05) 1.7156569716)
	(exprTest (root3 8.00) 2.0000000000)
	(exprTest (root3 100) 4.6415888336)
	) ;end run4

;(run4)


;------------------------------------------TASK 5-----------------------------------------

; crazyTriangle : prints out 'n' levels of special version of Pascal's Triangle. 
; 'l' is the leftmost element that goes all the way down the left side of the triangle
; 'r' is the rightmost element that goes all the way down the right side of the triangle
; Elements are computed the same as the normal Pascal's Triangle, but just with the special
; 		right element stipulation! 
; TO CLARIFY: variable 'r' denotes the RIGHT element, variable 'ro' denotes the ROW number!


(define (crazyTriangle l r n)
 	(define (crazyTriangleHelper ro c) ;function to help calculate values to display
 		(cond
 			((= c 0) l) ;when column is 0, return the left element
 			((= ro c) r) ;when row = column, return the right element
 			; otherwise, return the sum of the upper left diagonal element and the element above the current index
 			(else (+ (crazyTriangleHelper (- ro 1) (- c 1)) (crazyTriangleHelper (- ro 1) c)))
 			)
		) ;end crazyTriangleHelper

 	(define (iterateRow ro) ;function to recursively iterate through the rows
 		(printHelper (- n ro 1)) ; helps print out spaces before a row
 		(define (iterateCol c) ;function to recursively iterate through the columns
 			(cond
 				((<= c ro) (print (crazyTriangleHelper ro c) " ") (iterateCol (+ c 1)) )
 				)
 			) ; end iterateCol
 		(cond
 			((< ro n) (iterateCol 0) (println) (iterateRow (+ ro 1)))
 			)	
 		) ;end iterateRow

 	(define (printHelper x) ; function that helps print spaces for "pretty printing" (centered around axis)
 		(cond 
 			((>= x 1) (print " ") (printHelper (- x 1)) )
 			)
 		) ;end printHelper

 	(iterateRow 0) ;initial call to start iteration
	) ;end crazyTriangle

(define (run5) 	; run function with test cases for crazyTriangle
	(inspect (crazyTriangle 1 1 6))
	(inspect (crazyTriangle 1 2 6))
	(inspect (crazyTriangle 1 1 13))
 	) ;end run5

;(run5)


;----------------------------------------TASK 6---------------------------------------

; The function oppy curries a mathematical expression of the form (? x (? y z))
;		where x,y,z are numbers and '?' are operators. 

(define (oppy operator1) ; takes in first operator
	(lambda (variable1) ; Function that takes in the first variable
		(lambda (operator2) ; Function that takes in the second operator
			(lambda (variable2) ; Function that takes in the second variable
				; Return another function that takes in the 3rd variable and then 
				; 		calculates the value of the expression.
				(lambda (variable3) (operator1 variable1 (operator2 variable2 variable3))
					)
				)
			)
		)
	) ;end oppy

(define (run6) ;run function with test cases 
	(exprTest (((((oppy +) 1) +) 1) 1) 3)
	(exprTest (((((oppy +) 9) *) 7) 5) 0)
	(exprTest (((((oppy *) 6) -) -7) 2) -54)
	(exprTest (((((oppy /) 9) +) 4) 7.0) 0.8181818182)
	(exprTest (((((oppy /) -2) -) 6) 4) -1)
	(exprTest (((((oppy +) -3) *) 7) 9) 60)
	(exprTest (((((oppy *) 9) *) 9) 9) 729)
	(exprTest (((((oppy *) 1000000) +) 1000) 1000) 2000000000)
	) ;end run6

;(run6)

;----------------------------------------TASK 7-------------------------------------

; The function w takes in another function 'f' and a value 'i'. I then use the helper
;		function to apply Shank's transformation using an iterative process. My process
;		is iterative because we are keeping a running total and updating it with each pass.

(define (w f i)
	(define (wHelper)
		(cond
			((= i 0) (f i)) ;base case! 
			(else 
				(define (sum currentIndex n total)
					(cond
						((> currentIndex n) total)
						(else (sum (+ currentIndex 1) n (+ total (f currentIndex)))) ; end else
						) 
					) ; end sum

				(define S (sum 0 (- i 1) 0)) ;recursive call to sum
				; I define these next two values for ease of reading the code instead of using one giant line of calculation!
				(define numerator (- (* (+ S (f i) (f (+ i 1))) S) (square (+ S (f i))))) ;top of fraction for Shank
				(define denominator (+(- (+ S (f i) (f (+ i 1))) (* 2 (+ S (f i)))) S)) ;bottom of fracition for Shank

				(/ numerator denominator) ; return this statement!
				) 
			) ; end cond statement
		); end wHelper

		(wHelper) ;initial call to wHelper

	) ; end shank

(define (run7) ;run function with test cases
	(inspect (w (lambda (x) x) 5))
	(exprTest (w square 0) 0) 
	(exprTest (w square 15.0) -618.06451613) 
	(exprTest (w square 1000) 503389) 
	(exprTest (w sqrt 10000) -1333433.5623)
	(exprTest (w sqrt 15.0) -81.498536939)
	(exprTest (w sqrt 0) 0.0)
	) ;end run7

;(run7)


;---------------------------------------TASK 8----------------------------------------

; Halve is a function that takes in a number and divdes it in half without using the division
; 		operator '/'. We do this by repeated subtraction, but my halve function is a little more
;		efficient. We keep track of a count, which will tell us how many times we have to subtract
;		2 from our value to get to zero. Once our number gets smaller than 2, we return the count,
;		which ends up being half of the initial value (integer wise).
; Egypt* is a function that takes in two numbers (x,y) and multiplies them without using
;		the the multiplication operator '*'. We use the process outlined in the prompt
;		for Task 8 to do this.

(define (halve x) 
	(define (counter c x) ;'c' is our count variable
		(cond
			((>= x 2) (counter (+ c 1) (- x 2)))
			(else c) ; when x isn't >= 2, we return our count 'c'
			)
		) ; end counter
	(counter 0 x) ;initial call to counter

	) ;end halve

(define (egypt* x y)
	(define (egyptHelp a b c) ;helps implement first part of procedure as outlined in prompt
		(cond
			((> a b) (egyptHelp2 a b c 0))
			(else (egyptHelp (+ a a) b (+ c c)))
			)
		) ;end egyptHelp

	(define (egyptHelp2 a b c d) ;helps implement second part of procedure as outlined in prompt
		(cond
			((= b 0) d) ;base case
			((<= a b) (egyptHelp2 (halve a) (- b a) (halve c) (+ d c)))
			(else (egyptHelp2 (halve a) b (halve c) d))
			)
		) ;end egyptHelp2

	(egyptHelp 1 x y) ;initial call to egyptHelp
	) ;end egypt*

(define (run8) ;run function with test cases
	(exprTest (egypt* 0 0) 0)
	(exprTest (egypt* 0 1) 0)
	(exprTest (egypt* 1 1) 1)
	(exprTest (egypt* 25 17) 425)
	(exprTest (egypt* 100 100) 10000)
	(exprTest (egypt* 16 16) 256)
	(exprTest (egypt* 17 13) 221)
	(exprTest (egypt* 303 0) 0)
	(exprTest (egypt* 0 303) 0)
	(exprTest (halve 0) 0)
	(exprTest (halve 56) 28)
	(exprTest (halve 1777) 888)
	(exprTest (halve 1000002) 500001)
	(exprTest (halve 7) 3)
	) ;end run8

;(run8)


;-----------------------------------------TASK 9----------------------------------------

; The function mystery takes in an integer 'n' which determines how many levels to go
;		into this nested fraction defined in the Task 9 prompt. 

(define (mystery n)
	(define (mysteryHelper n total) 
		(cond
			((= n 0) (+ total 1))
			(else
				(cond
					((= 0 (% n 2)) (mysteryHelper (- n 1) (/ 1.0 (+ 2.0 total)))) ;case where 'n' is even
					(else (mysteryHelper (- n 1) (/ 1.0 (+ 1.0 total)))) ;case where 'n' is odd
					)
				) 
			) ;end cond
		) ; end mysteryHelper
	(mysteryHelper n 0) ;first call to mysteryHelper, initializes total as 0
	) ; end mystery

(define (run9) ;run function with test cases
	(exprTest (mystery 2) 1.6666666667)
	(exprTest (mystery 50) 1.7320508076)
	(exprTest (mystery 1)  2.0000000000)
	) ;end run9

;(run9)


;----------------------------------------TASK 10----------------------------------------

;	Ramanujan implements a recursive process because it takes the value returned
;		by the next function call and transforms it. When the base case is met, 1 is returned
;		and we must go back and implement all of those transformations to functions calls, beginning
;		with the deepest recursion.
;	Iramanujan implements an iterative process because we have a variable called total
; 		that is updated with each pass and then returned when the base case is met. 

(define (ramanujan d x) ;recursive ramanujan
	(define (ramaHelper counter)
		(cond
			((= counter d) 1.0) ;base case 
			(else 
				(* (sqrt(* (+ 1.0 (+ counter x)) (ramaHelper (+ counter 1) ) )))
				) ; end else
			)
		) ; end ramaHelper

	(if(= d 0) ;special case
		0
		(ramaHelper 0) ;first call to ramaHelper, starts count at 0
		) ; end if
	) ; end rama (recursive)

(define (iramanujan d x) ;iterative ramanujan
	(define (iramanujanIter total count)
		(cond 
			((= count 0) total ) ;base case
			(else 
				(iramanujanIter (sqrt ( * total (+ 1 (+ (+ x (- count 1) ) ))) ) (- count 1)) 
				) ;end else
			) ;end cond
		) ;end iramanujanIter

	(if(= d 0) ;special case
		0
		(iramanujanIter 1.0 d) ;first call to iramanujanIter, starts total at 1 to avoid multiplying by 0!
		) ; end if

	) ; end iramanujan (iterative)

(define (run10) ;run function with test cases
	(exprTest (iramanujan 2 3) 2.9906975624)
	(exprTest (iramanujan 0 3) 0)
	(exprTest (iramanujan 1 3) 2.0000000000)
	(exprTest (iramanujan 0 0) 0)
	(exprTest (iramanujan 0 1) 0)
	(exprTest (iramanujan 3 4) 4.463341015276)
	(exprTest (iramanujan 1 0) 1.0)
	(exprTest (iramanujan 1 1) 1.4142135624)
	(exprTest (iramanujan 1 2) 1.7320508076)
	(exprTest (iramanujan 116 87) 88.989004539)
	(exprTest (iramanujan 116 11793) 11794.99991)
	(exprTest (ramanujan 1 3) 2.0000000000)
	(exprTest (ramanujan 0 0) 0)
	(exprTest (ramanujan 0 1) 0)
	(exprTest (ramanujan 3 4) 4.463341015276)
	(exprTest (ramanujan 1 0) 1.0)
	(exprTest (ramanujan 1 1) 1.4142135624)
	(exprTest (ramanujan 1 2) 1.7320508076)
	(exprTest (ramanujan 116 87) 88.989004539)
	(exprTest (ramanujan 116 11793) 11794.999915)
	)

;(run10)


(println "assignment 1 loaded!") ;final line of code!! 