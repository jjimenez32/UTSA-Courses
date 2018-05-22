;;;useful functions and test cases for LISP natural language parsing program

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
;;;       (putp 'prep parse-obj (list prep))

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
        (setf temp (nth (getp  'cursor parse) (getp  'sentence parse))) ;; nth auto returns NIL when it hits the end of the sentence.
        (setCursor parse (1+ (getp 'cursor parse))) 
        (return temp) ) )


;;; set_isa
;;;    Parameters:
;;;       partOfSpeech - the key in the hashtable
;;;       wordlist     - the value in the hashtable
;;;    Purpose:
;;;       Assigns the value to the specified key.
;;;       If the wordlist is empty then it returns NIL.
;;;    Notes:
;;;       The value is a list.

;;     name     key          value
(defun set_isa (partOfSpeech wordlist)
   ;; if the car of wordlist is nil then stop
   (cond ( (null wordlist) NIL )
         ;; else   'a             .         'article        The rest of the elements in the wordlist
         ( T 
           (putp (car wordlist) word-dict partOfSpeech)
           ;(cons (getp (car wordlist) word-dict) (set_isa partOfSpeech (cdr wordlist)) )
           (set_isa partOfSpeech (cdr wordlist))
         )
   )
   ;; (cons (getp (car wordlist) word-dict)) ** This is for the extra credit.
)

;;; Use set_isa to set the part of speech for each of the words.
(set_isa 'article '(a an the))
(set_isa 'noun '(mickey ball dog home))
(set_isa 'verb '(ran throw throws threw hit shot)) 
(set_isa 'prep '(at on under above to of from))
(set_isa 'adjective '(large majestic red))


;;; Helper function to see if a list contains a specified value
(defun MEMSET (atm L)
   (cond ( (NULL L) NIL )
         ( (EQL atm (CAR L)) T )
         ( T        (MEMSET atm (CDR L)) )
   )
)

;;; isa
;;;    Parameters:
;;;       word         - specific element to search for in a key
;;;       partOfSpeech - the key in the hash to operate on
;;;    Purpose:
;;;       Returns true if the word the user searches for is in the specified
;;;       partOfSpeech. Otherwise it returns NIL
;;;    Notes:
;;;       First has to determine if the partOfSpeech is part of the hash table

(defun isa (word partOfSpeech)
   ;; Set a variable equal to the key
   (setf temp (getp word word-dict))
   ;(print temp)
   ;(print word)   
   ;(print partOfSpeech)
   ;; Check if value is in the key
   ;(memset word temp)
   (EQL temp partOfSpeech)
)

;; debug (PLEASE REMOVE)
;;(isa 'mickey 'noun)
;;(isa 'red 'adjective)


;;; resetPartsOfSpeech
;;;    Parameters:
;;;       parse - The hash table to perform the operations on
;;;       others - Variable number of items in the hash table to operate on
;;;    Purpose:
;;;       Sets the variables specified in others to NIL for the specified hash table
;;;    Notes:
;;;       dolist works for iterating through each argument. Use putp to assign hash table variables.

;;     name                HT    variables
(defun resetPartsOfSpeech (parse &rest others)
   (dolist (item others)
      (putp item parse NIL)
   )
)


;;; checkNP
;;;    Parameters:
;;;       parse - The hash table to perform the operations on
;;;       partOfSentence - subject, direct object, indirect object, etc...
;;;    Purpose:
;;;       Parses the parts of a sentence for grammer and store the appropriate part in the hash table
;;;       Takes into account that there can exist an adjective between an article and a noun.
;;;		 An adjective can also come before a noun without an article preceeding it.
;;;       If the sentence does not contain a noun, then the sentence is no longer valid.
;;;    Notes:
;;;       All of the parsed items are placed in a list.

(defun checkNP (parse partOfSentence)
   ;; Check partOfSpeech for article
   
   (prog ()
      ;; if article
      (setf article (getToken parse)) ; first word is article (supposedly)

      (if (isa article 'article)
          ;; is article so next word is adjective
          (setf adjective (getToken parse))
          ;; not an article so this word has to be a noun
          (setf adjective article)
      )

		(if (isa adjective 'adjective)
			 ;; The current word is an adjective so the following word is a noun (supposedly)
			 (setf noun (getToken parse))
			 ;; Not an adjective so this word has to be a noun
			 (setf noun adjective)
		)

      (if (not(isa noun 'noun))
          ;; is not a noun so it can't be a noun phrase
          (return NIL)
      )

		;; Check if there exist an article and a adjective.
		(if (AND (isa article 'article) (isa adjective 'adjective))
			 (putp partOfSentence parse (list article adjective noun))
		)

		;; Check if there exist an article with no adjective in the sentence
      (if (AND (isa article 'article) (NOT(isa adjective 'adjective)))
          (putp partOfSentence parse (list article noun))
      )

		;; Check if there exist only an adjective.
		(if (AND (NOT(isa article 'article)) (isa adjective 'adjective))
			 (putp partOfSentence parse (list adjective noun))
		)

		;; No article or adjective so place noun in the table
      (if (isa article 'noun)
          (putp partOfSentence parse (list noun))
      )

      (return T)
   )
)

;;; checkSentence
;;;    Parameters:
;;;       parse - The hash table to perform the operations on
;;;    Purpose:
;;;       Returns T if valid sentence, otherwise it returns NIL.
;;;       Sets the other parts of speech for the sentence.
;;;    Notes:
;;;       Has a definite return as well as side effects.

;;                    HT w/ sentence inside
(defun checkSentence (parse)
   (prog ()
		;; Required, set the subject
      (checkNP parse 'subject)

		;; Required, check for verb
      (setf verb (getToken parse))
      (if (not (isa verb 'verb))
          (return NIL)
          (putp 'verb parse (list verb))
      )

		;; Optional, set the directObject
		(setf saveCursor (getCursor parse))
		(if (not (checkNP parse 'directObject) )
			 (setCursor parse saveCursor)
		)

		;; Optional, check the preposition
		(setf prep (getToken parse))
		(if (isa prep 'prep)
          (putp 'prep parse (list prep))
		)

		;; Optional, set the indirectObject
		(if (AND (isa prep 'prep) (NOT(checkNP parse 'indirectObject)))
			(return NIL)
		)

		(return T)
   ) 
)

(set_isa 'adjective  '(rancid large  mashed brilliant Halloween hughs his clever new mangy ))
 (set_isa 'noun  '(maynard bowl cheese pet costume arrow hugh doctor hand he larry students they hair cat))
 (set_isa 'noun  '(pluto goofy potatoes gravy clever))
 (set_isa 'verb  '(fed ate made shot removed pet was taught lost has are))
 (set_isa 'subConjunction '(while after))

;;; uncomment these lines of code to turn on tracing.  Please turn off the tracing 
;;; for what you hand in.
; (trace checkSentence)
; (trace getToken)
; (trace checkNP)
 
;;; trace any additional functions that you introduced within checkSentence

;;; running the check sentence
(setf doing_extra 'EC1)

(processSentence ' (mickey ate a large bowl of rancid cheese ))
(processSentence ' (mickey has a new pet))
(processSentence ' (mickey pet a mangy cat))
(processSentence ' (mickey pet his pet)) 
