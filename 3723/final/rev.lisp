(defun reverseAll(L)
       (cond ((null l) nil)
        ((not (atom (car L)))
             (append (reverseall (cdr L))(list(reverseall (car L)))))
        ( T (append (reverseall (cdr l)) (list (car l))))
        
       )
)


;(defun removenil (l)
;       (cond ((null l) nil)
;            ((eql (car l) nil)
;                  ( cond (( not ( atom (car l)))
;                          ( append (removenil(car l) (removenil( cdr l))))
;                          ( T ( removenil (cdr l))) )
;                  )
;            ( T (cons (car l) (removenil (cdr l)))) )
;       )
;)
            
(setf ln '(nil 2 1 3 nil))
(setf l '(r (a c) e c a ((r))))
(print (reverseall l))




(defmacro Iterate (begin end i &rest bodies)
                 (let ((x (gensym)) (y (gensym)))
                     ` (do ((,x (eval ,begin)) (,y (eval ,end)) ( i (eval ,i)))
                          (( > begin end) T)
                          ,@bodies
                      )
                 )
)



(setf begin 1)
(setf end 30)
(setf i 1)

(iterate begin end i
         (setf i(1+ i))
         (setf begin (1+ begin))
         (print "start here") 
)
     
