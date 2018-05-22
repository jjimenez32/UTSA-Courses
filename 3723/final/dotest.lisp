(defmacro rand (a b &rest bodies)
          (let ((x (gensym)))
          `(do (,x (eval ,a))
              ((eql ,x 0) ,b)
              (setf ,x(1- ,a))
              (print ,x)
              ,@bodies
          )
        )
)

(rand 20 30
     (print "hello there")
)
