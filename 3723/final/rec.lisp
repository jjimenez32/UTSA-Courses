(defun changeSign (l)
       ( cond ((null l ) nil)
              ( (not (atom (car l))) 
                     (append (changeSign (car L)) (changeSign (cdr l))))
              ((eql (car l) '+)  
                     (cons '- (changeSign( cdr l))))
              ( T   (cons (car l) (changeSign( cdr l)) ))
       )
              
) 
	
(print (list 'a 'b))
(print (append '(a) '(b)))
(setf lis '(1 ((+)) 3 3 2 (+) 3))
(print (changeSign lis))
