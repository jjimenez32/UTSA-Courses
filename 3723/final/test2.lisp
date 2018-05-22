(print (cons '(a b c d) '(a b d)))
(print (append '(a b c d) '(a b d)))


(defmacro += ( a b)
       `(setf ,a (+ ,a ,b))
)


(defun test(L)
      (COND ((NULL L) NIL)
            ( (ATOM L) (LIST L))
            ( t (test (cdr l)) (print))
)
)	
(setf L '(a b c d))

(print (test 'a))


(print (list L))

(print (list 'a 'v))


(defmacro testmacro (a )
        (let ((x (gensym)))
       `(do ((,x (eval ,a))(count ,a) (fruitVar '(apple orange banana)))
            ((eql count 10) count)
	    (setf count (1+ count))
            (print (list count (car fruitVar)))
            (print ,x)
            (setf fruitVar (cdr fruitVar))
        )
       )
	
)

(defmacro ITERATE ( i begin end inc &rest bodies)
	(let ((endValue (gensym)) (incValue(gensym)) )
		`( do ( ( ,incValue (eval ,inc) ) ( ,i(eval ,begin)))
			( ( > ,i ,endValue) T )
                        (+ ,i ,incValue)  
                        ( ,endValue (eval ,end)) 

			,@bodies
                 )
	    )
)



(print (iterate i 1 5 1
        (print (list 'three i 5))
        (+= i 1)
))


(defmacro shit (a b) 
            `(do ((a 30) (b 40))
               ( ( eql a 0) nil )
               (setf (1- a))
               (print "got here")
             )
          )

(setf x 20)
(setf y 30)
(print (shit (x y
           (print "hello"))
       )
)
