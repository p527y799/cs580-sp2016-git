; Author: Nathan Swartz
; Student ID: p527y799
; Assignment: #2
; Main Function: pschologist
; Example: 
;	   Enter an example here
;


; (string-tokenize <string>)
; string->symbol
; symbol->string
; (string->append "The" " " "tree" " " "is" " " "big.")
; (map square '(2 7 1 3)) => (4 49 1 9))
; (map symbol->string '(the long day)) => ("the" "long" "day")
; (map string-symbol ...)

;(define my-map
;  (lambda (func lst)
;    (if (null? lst)
;      '()
;      ...

; conversation data
(define rules '((() 		(Please answer the question.)
				(Please give a response.)
				(I sorry, what is your response.))
		((* hi *)	(Welcome) 
				(Glad to see you)
				(Hi, what is you name?))))

; welcome messages
(define msgGreeting '((It is nice to meet you)
		     (Hello)))

; exiting messages
(define msgGoodbye '((Good bye)
		     (Have a nice day)
		     (See you later)))
		     
(define msgResponse
  '(
    (()
      (Please enter your response.))
    ((* you don't know *)
      (That is probably true.)
      (Please tell me about %2.))
    ((* hello *)
      (Nice to meet you.)
      (What is your name?))
    ((* computer science *)
      (Have you taken the necessary math courses?)
      (When are you planning to graduate?)
      (Are you more interested in software or hardware?))
    ((* engineering *)
      (What are your plans for your senior design project?)
      (What about engineering interests you?)
      (What type of engineering work would you like to do?))
    ((* computers *)
      (Is that based on your own interest or because you feel required to?)
      (What interests you about computers?))
    ((* i don’t know *)
      (Do you think there’s some way to find out?))
    ((* if *)
      (Do you think its likely that %2?)
      (Do you wish that %2?)
      (What do you think about %2?))
    ((* can you help *)
      (I may have some ideas if you can give me more details about your question.)
      (I will try to think of some ideas.))
    ((* i want *)
      (Do you think %2 will help?)
      (How will that help you?))
    ((*)
      (Sorry I did not understand you.)
      (Could you explain?)
      (Could you clarify that?))))

; ***************************
; *** pschologist program ***
; ***************************
(define (pschologist)

  ; lets have a conversation
  (define (repeat)
    (let ((input (read-list)))
      (cond
	((and (not (null? input)) (equal? (car input) 'exit))
	  (begin
	    (newline)
	    (display (msgPick msgGoodbye))
	    (newline)))
	(else
	  (begin
	    (displayln (respond-to-input input))
	    (repeat))))))
  
  ; initialization
  (set! *random-state* (random-state-from-platform))
  
  ; welcome screen
  (display "\nWelcome, I am the psychologist.\n")
  (display "Type exit to end the program.\n\n")
  (displayln (msgPick msgGreeting))
  (let ((input (read-list)))
    (if (null? input)
      (display "Come back when you are ready to talk.\n")
      (if (equal? (car input) 'exit)
	(begin
	  (newline)
	  (display (msgPick msgGoodbye))
	  (newline))
	(begin
	  (displayln (respond-to-input input))
	  (repeat))))))

; return user's string as a list of lowercased symbols stripped of any punctuation
(define (read-list)
  (let ((lst (string-tokenize (remove-punctuation (string-downcase (readline))))))
    (map string->symbol lst)))
    
    
    
    ; find the matching rule
    ; replace any %2 with the content from the results of the get-match
    
; search response-rules for response
(define (respond-to-input input)

  ; rules must 
  (define (search-for-rule rule)
    (cond
      ;((null? rule) #f)
      
      ; match, return the rule
      ((match (car (car rule)) input) (car rule)) ;(msgPick (cdr (car rule))))
     
      (else (search-for-rule (cdr rule)))))
  
  (let ((rule (search-for-rule msgResponse)))
    rule
    ;(replace (msgPick (cdr rule)) '%2 (car (car (cdr (match (car rule) input)))))
  )
)
    
; display message and display a prompt for the user
(define (displayln x)
  (display x)
  (display "\n-> "))

; pick a random list element
(define (msgPick lst)	
  (list-ref lst (random (length lst))))
;; (string-join (map symbol->string 
  
(define (merge-lists lst)
  (cond
    ((or (null? lst) (null? (car lst)))
      '())
    ((list? (car lst))
      (append (merge-lists (car lst)) (merge-lists (cdr lst))))
    (else
      (append (list (car lst)) (merge-lists (cdr lst))))))
    
; remove certain characters
(define (remove-character rem lst)
    (cond
      ((null? lst)
	'())
      ((list? (car lst))
	(append (list (remove-character rem (car lst))) (remove-character rem (cdr lst))))
      ((eqv? rem (car lst))
	(remove-character rem (cdr lst)))
      (else 
	(append (list (car lst)) (remove-character rem (cdr lst))))))

; strip punctuation from a symbol list
(define (remove-punctuation lst)
  (define (remove lst str)
    (if (null? lst)
      str
      (remove (cdr lst) (remove-character (car lst) str))))
  (unless (null? lst)
    (string-join (map list->string (remove '(#\, #\. #\? #\; #\: #\" #\!) (map string->list (map symbol->string lst)))))))
    
; adjust the pronouns based on person-orientation 1st->3rd && 3rd->1st
(define (invert-pronoun lst)
  (define pronoun-table '((i . you) (me . you) (you . me) (my . your) (your . my)))
  (define (get-next word pronouns)
    (cond 
      ((null? pronouns) word)
      ((equal? word (car (car pronouns))) (cdr (car pronouns)))
      (else (get-next word (cdr pronouns)))))
  
  (if (null? lst)  
    '()
    (append (list (get-next (car lst) pronoun-table)) (invert-pronoun (cdr lst)))))
  
; replace only on the top-level
(define (replace lst old new)
  (cond
    ((null? lst) '())
    ((equal? (car lst) old) (cons new (replace (cdr lst) old new)))
    (else (cons (car lst) (replace (cdr lst) old new)))))

(define (match pattern fact)
  (define (search pattern fact)
    (cond
      ((and (null? pattern) (null? fact)) #t)
      ((equal? pattern '(*)) fact)
      ((or (null? pattern) (null? fact)) #f)
      ((or (equal? (car pattern) '?) (equal? (car pattern) (car fact)))
	(search (cdr pattern) (cdr fact)))
      ((equal? (car pattern) '*)
	(or (and (search (cdr pattern) fact) (list '() (search (cdr pattern) fact)))
	    (and (search (cdr pattern) (cdr fact)) (list (car fact) (search (cdr pattern) (cdr fact))))
	    (and (search pattern (cdr fact)) (append (list (car fact)) (search pattern (cdr fact))) )))
      (else #f)))
 
;  (unless (or (null? pattern) (null? fact))
    (let ((result (search pattern fact)))
      (cond
	((or (null? result) (equal? result #f)) #f)
	((equal? result #t) #t)
	((equal? (car result) '()) result)
	(else (reverse (list (car (reverse result)) (reverse (cdr (reverse result)))))))));)
    
























#!
; check if fact contains something
(define (contain pattern fact)
  (match (append '(*) pattern '(*)) fact))
  
; checks for a match of the pattern found in fact
(define (match pattern fact)
  (cond
    ((and (null? pattern) (null? fact)) #t)
    ((equal? pattern '(*)) #t)
    ((or (null? pattern) (null? fact)) #f)  
    ((or (equal? (car pattern) (car fact)) (equal? (car pattern) '?))
      (match (cdr pattern) (cdr fact)))
    ((equal? (car pattern) '*)
      (or 
	(match (cdr pattern) fact)   
	(match (cdr pattern) (cdr fact))  
	(match pattern (cdr fact))))
    (else #f)))    
    

; returns the *s value separated by : or #f if there is no match
(define (get-match pattern fact)
  (cond
    ((and (null? pattern) (null? fact)) '())
    ((equal? pattern '(*)) fact)
    ((or (null? pattern) (null? fact)) #f)
    ((or (equal? (car pattern) '?) (equal? (car pattern) (car fact)))
      (get-match (cdr pattern) (cdr fact)))
    ((equal? (car pattern) '*)
      (or (get-match (cdr pattern) fact)
	  (and (get-match (cdr pattern) (cdr fact)) (merge-lists (list (list (car fact) ':) (get-match (cdr pattern) (cdr fact)))))
	  (and (get-match pattern (cdr fact)) (merge-lists (list (car fact) (get-match pattern (cdr fact)))))))
    (else #f)))
    

; converts get-match results into a list of symbol lists
(define (get-matches pattern fact)
  
  ; split string into two strings at the first instance of elem  
  (define (split-string elem str)
    
    (define (find-index elem str)
      (define (find index)
	(cond
	  ((equal? index (string-length str)) -1)		;; not found
	  ((char=? (string-ref str index) elem) index)	;; found
	  (else (find (+ index 1)))))
      (find 0))
  
    (let ((index (find-index elem str)))
      (if (= index -1)
	str
	(list (substring str 0 (- index 1)) (substring str (+ index 2) (string-length str))))))
  
  ; convert a list of strings into a list of symbols
  (define (string-to-symbol-list lst)
      (cond 
	((null? lst) '())
	(else (cons (map string->symbol (string-tokenize (car lst))) (string-to-symbol-list (cdr lst))))))
  
  (string-to-symbol-list (split-string #\: (string-join (map symbol->string(get-match pattern fact)))))
)

!#










  