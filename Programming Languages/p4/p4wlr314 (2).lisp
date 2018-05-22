;;; useful functions and test cases for LISP natural language parsing program

;;; Set up the parse-obj and word-dict
(setf parse-obj (MAKE-HASH-TABLE))
(setf word-dict (MAKE-HASH-TABLE))

;;; processSentence 
;;;    Parameters:
;;;       sentence - a list of words making a sentence
;;;    Purpose:
;;;       - Sets the parse-obj to contain the sentence
;;;       - Sets the cursor position to zero.
;;;       - Uses resetPartsOfSpeech to reset the values of each part of speech.
;;;       - Prints the sentence
;;;       - Invokes checkSentence to check for valid syntax and prints
;;;         the result.
;;;       - Prints the value for each part of speech
;;;    Notes:
;;;       - Sentences will not contain punctuation since
;;;             -- commas have a meaning in Common LISP associated with backquote
;;;             -- periods are used for dotted pairs
;;;       -  For commas, we will use the symbol COMMA  

(defun processSentence(sentence)
    (PROG (result)
        ;;; Save the sentence in parse-obj.
        (putp 'sentence parse-obj sentence) 
        ;;; Set the cursor position to 0.
        (putp 'cursor parse-obj 0)
 
        ;;; reset the parts of speech to NIL
        (resetPartsOfSpeech parse-obj 'subject 'verb 'prep 'directObject 'indirectObject)
        (resetPartsOfSpeech parse-obj 'subConjunction 'subSubject 'subVerb 'SubPrep 'SubDirectObject 'SubIndirectObject)
        (format T "~% ******************************************************************")
        (format T "~%  ~a" sentence)
        (setf result (checkSentence parse-Obj))
        (format T "~%    checkSentence returned ~a" result)
        (format T "~%    subject= ~a" (getp 'subject parse-obj))
        (format T "~%    verb= ~a" (getp 'verb parse-obj))
        (format T "~%    directObject= ~a" (getp 'directObject parse-obj))
        (format T "~%    prep= ~a" (getp 'prep parse-obj))
        (format T "~%    indirectObject= ~a" (getp 'indirectObject parse-obj))
        (if (not (eql doing_extra 'EC3)) (return result))
        
        (format T "~%    subConjunction= ~a" (getp 'subConjunction parse-obj))
        (format T "~%    subSubject= ~a" (getp 'subSubject parse-obj))
        (format T "~%    subVerb= ~a" (getp 'subVerb parse-obj))
        (format T "~%    SubDirectObject= ~a" (getp 'SubDirectObject parse-obj))
        (format T "~%    SubPrep= ~a" (getp 'SubPrep parse-obj))
        (format T "~%    SubIndirectObject= ~a" (getp 'SubIndirectObject parse-obj))
        (return result) ) )
 
;;; putp 
;;;    Parameters:
;;;       symbol   - symbol to be given the property
;;;       ht       - hash table to store the symbol and its property value
;;;       value    - the property value
;;;    Purpose:
;;;       stores the property value for the symbol in the specified hash table
;;;    Notes:
;;;       If the symbol isn't an ATOM, putp breaks execution with an ERROR.
;;;    Example Usage:
;;;       (putp 'prep parse (list prep))
;;;       (putp 'mickey word-dict (list 'noun))

(defun putp (symbol ht value)
    (if (ATOM symbol)
        (setf (gethash symbol ht) value)
        (ERROR "~s is not a valid symbol for putp" symbol)
    )
)
    
;;; getp 
;;;    Parameters:
;;;       symbol   - symbol about which we want its property value
;;;       ht       - hash table which stores the symbol and its property value
;;;    Purpose:
;;;       returns the property value for the symbol in the specified hash table
(defun getp (symbol ht)
     (gethash symbol ht) )
     
;;; getCursor 
;;;    Parameters:
;;;       parse - the parse object containing a sentence, cursor position, and
;;;               value for each part of speech
;;;    Purpose:
;;;       returns the current cursor position (relative to zero)
(defun getCursor (parse)
    (getp 'cursor parse) )

;;; setCursor 
;;;    Parameters:
;;;       parse - the parse object containing a sentence, cursor position, and
;;;               value for each part of speech
;;;       cursorPosition - new cursor position
;;;    Purpose:
;;;       Sets the value of the cursor position (relative to zero) in the
;;;       parse object
;;;    Notes:
;;;       If the cursorPosition isn't a numeric, setCursor breaks execution 
;;;       with an ERROR.
(defun setCursor (parse cursorPosition)
    (if (numberp cursorPosition)
        (putp 'cursor parse cursorPosition)
        (ERROR "~s is not a numeric argument for setCursor" cursorPosition)
    )
)
;;; getToken
;;;    Parameters:
;;;       parse - the parse object containing a sentence, cursor position, and
;;;               value for each part of speech
;;;    Purpose:
;;;       returns the next token from the sentence.  If there are no more
;;;       tokens, it returns NIL.
;;;    Notes:
;;;       This modifies the cursor position after getting the current token
;;;       at the old position.
(defun getToken (parse)
    (prog (temp)
        (setf temp (nth (getp  'cursor parse) (getp  'sentence parse))) 
        (setCursor parse (1+ (getp 'cursor parse))) 
        (return temp) ) )
;;; set_isa
;;;			parameters:		
;;;				speechPart 		-hashtable key
;;;				words		-hashtable value
;;;			purpose:
;;;				Assigns value to key specified by speechPart.
;;;				Returns nil if words is empty.
(defun set_isa (speechPart words)
	(cond ( (null words) NIL )
	      ( T	(putp (car words) word-dict speechPart)
			(set_isa speechPart (cdr words))
	      )
	)
)
;;; isa
;;;			parameters:
;;;				word			-the word to search for in key
;;;				speechPart		-hashtable key
;;;			Purppose:
;;;				Returns true if word is found in speechPart.
;;;				Returns nil otherwise.
(defun isa (word speechPart)
	( setf temp (getp word word-dict) )
	( eql temp speechPart)
)
;;; resetPartsOfSpeech
;;;			Parameters:
;;;				parse			-the parse object containing a sentence, cursor position, and
;;;               				  value for each part of speech.Hashtable
;;;				others			-what to have the hashtable operate on
;;;			Purpose:
;;;				Sets variables from others to nil for the hashtable
(defun resetPartsOfSpeech (parse &rest others)
	(dolist (item others)
		(putp item parse NIL)
	)
)
;;; checkNP
;;;			Parameter:
;;;				parse			- the parse object containing a sentence, cursor position, and
;;;               				  value for each part of speech.Hashtable
;;;				speechPart		--hashtable key
;;;   		Purpose:
;;;				determines where putp will store the property value in the hash table.
(defun checkNP (parse speechPart)
	(prog ()
		(setf artc (getToken parse))
		(if (isa artc 'article)
			(setf noun (getToken parse))
			(setf noun artc)
		)
		(if (not (isa noun 'noun))(return NIL))
		(if (isa artc 'noun) ( putp speechPart parse (list noun) ))
		(if (isa artc 'article) ( putp speechPart parse (list artc noun) ))
		(return T)
	)
)
;;; checkSentence
;;;			Parameters:
;;;				parse			-the parse object containing a sentence, cursor position, and
;;;               				  value for each part of speech. Hashtable
;;;			Purpose:
;;;				Checks if sentence is valid and returns True or nil otherwise.
;;;				Sets the speech parts for the sentence.
(defun checkSentence (parse)
	(prog ()
		(checkNP parse 'subject)
		(setf verb (getToken parse))
		(if (not (isa verb 'verb))
			(return NIL)
			(putp 'verb parse (list verb))
		)
		(setf saveCursor (getCursor parse))
		(if (not (checkNP parse 'directObject)) ( setCursor parse saveCursor) )
		(setf prep (getToken parse))
		(if (isa prep 'prep) ( putp 'prep parse (list prep)) )	
		(if (and (isa prep 'prep) ( not (checkNP parse 'indirectObject)) )(return NIL))
		(return T)
	)
)


     
;;; Use set_isa to set the part of speech for each of the words.
(set_isa 'article '(a an the))
(set_isa 'noun '(mickey ball dog home))
(set_isa 'verb '(ran throw throws threw hit shot)) 
(set_isa 'prep '(at on under above to of from))

;;; uncomment these lines of code to turn on tracing.  Please turn off the tracing 
;;; for what you hand in.
;; (trace checkSentence)
;; (trace getToken)
;; (trace checkNP)
;; (trace checkVerb)
 
;;; trace any additional functions that you introduced within checkSentence

;;; running the check sentence
 (setf doing_extra NIL)
 (processSentence '(mickey throws a ball to the dog ))  

 (processSentence '(the dog ran home))  
 
 (processSentence '(mickey throws at the dog ))  
  
 (processSentence '(mickey throws a ball to the))  
