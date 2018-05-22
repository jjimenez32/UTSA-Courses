;;; +=
;;;	Parameters:			
;;;		x	- Variable to be incremeted
;;;		i 	- Amount to be incremented by	
;;;	Purpose:
;;;		Incremets x by i
(defmacro += (x i)
	`(setf ,x( + ,x ,i))
)
;;; ITERATE
;;;	parameters:
;;;		i 	- Control Variable which is used to count from begin to end 
;;;		begin 	- Beginning count
;;;		end	- Ending count
;;;		inc	- Amount to be incremented by
;;;		bodies 	- Body expression of what this function will iterate
;;;	purpose:
;;;		Iterates expressions in a fixed amount(which is given from begin to end)
;;;
(defmacro ITERATE ( i begin end inc &rest bodies)
	(let ((endValue (gensym)) (incValue(gensym)) )
		`( do ( ( ,incValue (eval ,inc) ) ( ,i(eval ,begin) (+ ,i ,incValue) ) ( ,endValue (eval ,end) )	
		      )
			( ( > ,i ,endValue) T )
			,@bodies
	         )
	)
)	

(setf x 1)
(+= x 5)

(iterate i 1 5 1
	(print (list `one i))
)

(setf n 5)
(iterate i 1 n 1
	(print (list `two i n))
	(+= i 1)
)

(setf n 5)
(iterate i 1 n 1 
		(print (list 'three i n))
		(+= n 1)
)

(setf n 5)
(setf inc 2)
(iterate i 1 n inc
	(print (list `three i n inc))
	(+= inc 1)
)
