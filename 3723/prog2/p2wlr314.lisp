;;; ReverseTop
;;;		Parameters:
;;;				L - list of items
;;;		Purpose:
;;;			Returns a reversed list of the high-level entries.
(defun ReverseTop (L)
	(cond 	( (NULL L) NIL )
		((append (ReverseTop (cdr L)) (list (car L))))
	)
)
;;; ReverseAll
;;;		Parameters:
;;;				L - list of items
;;;		Purpose:
;;;			Returns a reversed list at all levels.
(defun ReverseAll (L)
	(cond 	( (NULL L) NIL )
		( (NOT (ATOM (CAR L))) (append (ReverseAll (cdr L)) (list (ReverseAll (car L)))) )
		( T (append(ReverseAll (cdr L)) (list (car L))))
	)
)
;;; RemoveNILTop
;;;		Parameters:
;;;				L - list of items
;;;		Purpose:
;;;			Removes NIL's at the top level from the list passed in.
(defun RemoveNILTop (L)
	(cond ( (NULL L) NIL)
	      ( (equal (car L) NIL) (RemoveNILTop (cdr L)))
	      ( T  (cons (car L) (RemoveNILTop (cdr l))))
	 )
)	
;;; RemoveNILMost
;;;		Parameters:
;;;				L - list of items
;;;		Purpose:
;;;			Removes NIL's at any level from the list passed in.
(defun RemoveNILMost (L)
	(cond 	( (NULL L) NIL )	      	
		( ( NULL ( NOT (car L))) 
		( cons 	(cond 	( ( NOT(ATOM (car L)) ) (RemoveNILMost( car L)) )
				( T	(car L)) )
		( RemoveNILMost (cdr L))) )
		( T 	(RemoveNILMost (cdr L)) )	
	)	
)
;;; palindrome
;;;		Parameters:
;;;				L - list of items
;;;		Purpose:
;;;			Returns T if the list passed in is a palindrome; otherwise, it returns NIL.
(defun palindrome (L)
	(cond 	( ( NULL L) NIL) )
		( equal L (ReverseTop L) )
)
;;; MEMALL
;;;		Parameters:
;;;			atm - atom to check if in list
;;;			L - list of items
;;;		Purpose:
;;;			Sees if an atom is anywhere in a list
(defun MEMALL (atm L)
	(cond 	( (NULL L ) NIL )
		( (ATOM L) (EQL atm L)) 
		( (NULL (CAR L))) 
		( T	(OR (MEMALL atm (CAR L) )
			    (MEMALL atm (CDR L) ) ) )
	)
)
;;; RemoveNILALL
;;;		Parameters:
;;;			L - list of items
;;;		Purpose:
;;;			Removes any resulting NIL (except the single outermost)
;;;		Note:
;;;			Uses the function MEMALL to check if a NIL exists in a list.
(defun RemoveNILALL (L)
	(cond 	( (NULL L) NIL )
		( (MEMALL NIL L) (RemoveNILALL (removeNILMost L)) )
		( (if 	(NULL 	(cond 	( (NULL L) NIL) 		
					( (AND (ATOM L) (NOT (NULL L))) )
					( T	(OR (removeNILALL (car L)) (removeNILALL (cdr L))) )
				)
			)
		T NIL)NIL )(L)
	)
)	

(reverseTop '(X Y Z))
(reverseTop '(X (Y Z (A)) (W)))

(reverseAll '(X Y Z))
(reverseAll '(X (Y Z (A)) (W)))

(removeNILTop '(NIL X NIL NIL Y  NIL Z))
(removeNILTop '(X NIL Y NIL Z NIL))
(removeNILTop '(NIL (X NIL Y) (NIL NIL)))

(removeNILMost '(NIL X NIL NIL Y  NIL Z))
(removeNILMost '(X NIL (Y NIL Z) NIL))
(removeNILMost '(NIL (NIL) (X NIL Y) (NIL NIL) Z))
(removeNILMost '(NIL ( (((((NIL) NIL)))))))

(palindrome '(R A C E C A R))
(palindrome '(W A S I T A C A R O R A C A T I S A W))
(palindrome '(N I X O N))

(RemoveNILALL '(NIL (NIL ( X NIL Y) (NIL NIL) Z)))
(RemoveNILALL '(NIL ( (((((NIL)NIL)))))))
(RemoveNILALL '(NIL (X (NIL) Y)((NIL)) ))
(RemoveNILALL '(NIL ((X((((((((((NIL))))))))) Y) Z) W) (((NIL))) ))
