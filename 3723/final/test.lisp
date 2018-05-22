(DEFUN F (L A B)
    (COND ( (NULL L ) NIL )
          ( (ATOM L) (LIST L) )
          ( (NULL (CAR L )) (F (CDR L) A B) )
          ( (NOT (ATOM (CAR L)))
                (APPEND 
                    (F (CAR L) A B)
                    (F (CDR L) A B)
                 ) )
          ( (EQL (CAR L) A)
               (CONS B (F (CDR L) A B)) )
          ( T (CONS (CAR L) (F (CDR L) A B)) )
    )
)

(defmacro plus (A B)
       ( let ((X 39) (Y 54))
       `(print (+ ,A ,B) )
       )
)
(defmacro ITERATE ( i begin end inc &rest bodies)
	(let ((endValue (gensym)) (incValue(gensym)) )
		`( do ( ( ,incValue (eval ,inc) ) ( ,i(eval ,begin) (+ ,i ,incValue) ) ( ,endValue (eval ,end) )	
		      )
			( ( > ,i ,endValue) T )
			,@bodies
	         )
	)
)	
(defmacro += (A B)
        `(setf ,A (+ ,A ,B))
)

;;;(print (F '(X (Y X Z) X) 'X 'W))
;;(print (F '((X (X) () Z) (X)) 'X 'W))

( plus 5 3)


(setf n 5)
(iterate i 1 n 2
        (print (list 'three i n))
        (+= n 2)
)

(setf A 30)


(setf fruits '(apple orange banana))
(do ((count 0) (fruitVar fruits) )
    ((null fruitVar)(print  count))
    ( setf count( 1+ count))
    ( print (list count (car fruitVar)))
    ( setf fruitVar (cdr fruitVar))
)

(do ((x 20) (y 40))
    ((eql x 0) (print Y))
    (setf x (1- x))
    (print x)
)


