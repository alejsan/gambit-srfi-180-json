; an implementation of srfi 180: json
;; https://srfi.schemers.org/srfi-180/

(define-record-type <json-error>
  (make-json-error reason irritants port-or-generator)
  json-error?
  (reason json-error-reason)
  (irritants json-error-irritants)
  (port-or-generator json-error-port-or-generator))

(define (json-error reason . irritants) (raise (make-json-error reason irritants #f)))

(define (json-null? obj) (eq? obj 'null))

(define json-nesting-depth-limit
  (make-parameter +inf.0))

(define json-number-of-character-limit
  (make-parameter +inf.0))

(define (char-hexdigit? ch)
  (let ((n (char->integer ch)))
    (or (and (>= n 48) (<= n 57))
	(and (>= n 65) (<= n 70))
	(and (>= n 97) (<= n 102)))))

(define (json-char-whitespace? ch)
  (memv ch '(#\space #\tab #\newline #\return)))

(define (json-char-delimiter? ch)
  (or (memv ch '(#\[ #\{ #\] #\} #\, #\" #\:))
      (json-char-whitespace? ch)))

(define (json-string-char-must-be-escaped? ch)
  (let ((n (char->integer ch)))
    (>= #x001F n #x000)))

(define (digit0-9? ch)
  (<= 48 (char->integer ch) 57))

(define (digit1-9? ch)
  (<= 49 (char->integer ch) 57))

(define (high-surrogate? n)
  (<= #xD800 n #xDBFF))

(define (low-surrogate? n)
  (<= #xDC00 n #xDFFF))

(define (surrogate? n)
  (or (high-surrogate? n)
      (low-surrogate? n)))

(define (surrogate-pair->code-point high low)
  (+ (* #x400 (- high #xd800))
     (- low #xdc00)
     #x10000))

(define (code-point->surrogate-pair n)
  ;; n must be greater than #xFFFF
  (values
   ;; high
   (+ #xd800 (floor-quotient (- n #x10000) #x400))
   ;; low
   (+ #xdc00 (modulo (- n #x10000) #x400))))


;; Given a textual input port or a character generator return 2 thunks peek and
;; get which are analogous to peek-char and read-char
(define (wrap-port-or-generator port-or-generator)
  (if (port? port-or-generator)
      
      (values (lambda ()
		(with-exception-catcher
		    (lambda (e)
		      (cond ((and (os-exception? e)
				  ;; TODO the exact code may be OS dependant!
				  (= -507496832 (os-exception-code e)))
			     (json-error "Invalid UTF-8 encoding" e))
			    (else (raise e))))
		  (lambda () (peek-char port-or-generator))))
	      (lambda ()
		(with-exception-catcher
		    (lambda (e)
		      (cond ((and (os-exception? e)
				  ;; TODO the exact code may be OS dependant!
				  (= -507496832 (os-exception-code e)))
			     (json-error "Invalid UTF-8 encoding" e))
			    (else (raise e))))
		  (lambda () (read-char port-or-generator)))))

      ;; TODO if i'm going to use unsafe ## procedures then I need to add checks
      ;; that user provided generator always produces either a character or eof
      
      (let ((peeked #f))
	(values (lambda () (or peeked
			       (let ((x (port-or-generator)))
				 (set! peeked x)
				 x)))
		(lambda () (if peeked
			       (let ((x peeked))
				 (set! peeked #f)
				 x)
			       (port-or-generator)))))))

  ;; Input is position on first non-whitespace character or eof
(define (skip-json-whitespace peek get)
  (let loop ((x (peek)))
    (when (and (char? x) (json-char-whitespace? x))
      (get)
      (loop (peek)))))

;; read characters from the input accumulating them into a string until
;; encountering a delimiter (or eof). Returns the string. Does not consume the
;; delimiter.
(define (read-until-delimiter peek get)
  (with-output-to-string
    (lambda ()
      (let loop ((x (peek)))
	(unless (or (eof-object? x) (json-char-delimiter? x))
	  (write-char (get))
	  (loop (peek)))))))

(define (read-json-literal peek get)
  (case (string->symbol (read-until-delimiter peek get))
    ((false) #f)
    ((true) #t)
    ((null) 'null)
    (else (json-error "invalid json syntax, unrecognized literal"))))

(define (read-json-number peek get)

  (define (write-ascii-digit ch)
    (if (digit0-9? ch)
	(write-char ch)
	(json-error "Invalid number")))

  (define (read-int in)
    (cond ((eof-object? in) #t)
	  ((digit0-9? in)
	   (write-char (get))
	   (read-int (peek)))
	  ((char=? in #\.)
	   (read-frac-start))
	  ((memq in '(#\e #\E))
	   (read-exp-start))
	  ((json-char-delimiter? in) #t)
	  (else (json-error "Invalid number"))))

  (define (read-frac-start)
    ;; write the '.'
    (write-char (get))
    ;; there must be at least 1 digit in the fractional
    (write-ascii-digit (get)) 
    (read-frac (peek)))

  (define (read-frac in)
    (cond ((eof-object? in) #t)
	  ((digit0-9? in)
	   (write-char (get))
	   (read-frac (peek)))
	  ((memq in '(#\e #\E))
	   (read-exp-start))
	  ((json-char-delimiter? in) #t)
	  (else (json-error "Invalid number"))))

  (define (read-exp-start)
    ;; write the 'e' or 'E'
    (write-char (get))
    ;; maybe write the sign
    (when (memq (peek) '(#\- #\+)) (write-char (get)))
    ;; must be at least 1 digit in the exponent
    (write-ascii-digit (get))
    (read-exp (peek)))

  (define (read-exp in)
    (cond ((eof-object? in) #t)
	  ((digit0-9? in)
	   (write-char (get))
	   (read-exp (peek)))
	  ((json-char-delimiter? in) #t)
	  (else (json-error "Invalid number"))))
  
  (string->number
   (with-output-to-string
     (lambda ()
       (when (char=? (peek) #\-) (write-char (get)))
       (let ((in (peek)))
	 (cond ((char=? in #\0)
		(write-char (get))
		(let ((next (peek)))
		  (cond ((eof-object? next) #t)
			((char=? next #\.)
			 (read-frac-start))
			((memq next '(#\e #\E))
			 (read-exp-start))
			((json-char-delimiter? next) #t)
			(else (json-error "Invalid number")))))
	       ((digit1-9? in)
		(write-char (get))
		(read-int (peek)))
	       (else (json-error "Invalid number"))))))))

;; this should be called immediately after the '\u' escape in a json
;; string. Read 4 hex digits and return their numeric value
(define (read-unicode-escape peek get)

  (define (hexdigit-value ch)
    (let ((n (char->integer ch)))
      (cond ((<= 48 n 57)  ; 0-9
	     (- n 48))
	    ((<= 65 n 70)  ; A-F
	     (- n 55))
	    ((<= 97 n 102) ; a-f
	     (- n 87))
	    (else
	     (json-error "Invalid unicode escape")))))
  
  (let loop ((in (peek))
	     (acc 0)
	     (count 0))
    (if (= count 4)
	acc
	(if (eof-object? in)
	    (json-error "Unexpected 'eof'")
	    (let ((n (hexdigit-value (get))))
	      (loop (peek)
		    (bitwise-ior (arithmetic-shift acc 4) n)
		    (+ count 1)))))))

(define (read-json-string peek get)
  (get) ;; get initial "
  (with-output-to-string
    (lambda ()
      (let loop ((in (get)))
	(cond ((eof-object? in)
	       (json-error "invalid json syntax, missing terminating \""))
	      ((eq? in #\\)
	       (let ((x (get)))
		 (if (eof-object? x)
		     (json-error "invalid json syntax, missing terminating \"")
		     (case x
		       ((#\" #\\ #\/) (write-char x))
		       ((#\b) (write-char #\backspace))
		       ((#\f) (write-char #\x000c))
		       ((#\n) (write-char #\newline))
		       ((#\r) (write-char #\return))
		       ((#\t) (write-char #\tab))
		       ((#\u)
			(let ((n (read-unicode-escape peek get)))
			  (cond ((high-surrogate? n)
				 (unless (eq? (get) #\\) (json-error "unmatched high surrogate"))
				 (unless (eq? (get) #\u) (json-error "unmatched high surrogate"))
				 (let ((maybe-low (read-unicode-escape peek get)))
				   (if (low-surrogate? maybe-low)
				       (write-char (integer->char (surrogate-pair->code-point n maybe-low)))
				       (json-error "unmatched high surrogate"))))
				((low-surrogate? n)
				 (json-error "unmatched low surrogate"))
				(else
				 (write-char (integer->char n))))))
		       (else
			(json-error "Invalid escape sequence" x))))
		 (loop (get))))
	      ((eq? in #\") #t)
	      ((json-string-char-must-be-escaped? in)
	       (json-error "Unescaped control character"))
	      (else
	       (write-char in)
	       (loop (get))))))))

(define (make-json-token-generator peek get)
  (lambda ()
    (let ((in (begin (skip-json-whitespace peek get) (peek))))
      (if (eof-object? in)
	  in
	  (case in
	    ((#\[) (get) 'array-start)
	    ((#\]) (get) 'array-end)
	    ((#\{) (get) 'object-start)
	    ((#\}) (get) 'object-end)
	    ((#\:) (get) 'name-seperator)
	    ((#\,) (get) 'value-seperator)
	    ((#\") (read-json-string peek get))
	    ((#\t #\f #\n) (read-json-literal peek get))
	    ((#\-) (read-json-number peek get))
	    (else
	     (if (digit0-9? in)
		 (read-json-number peek get)
		 (json-error "Unexpected character" in))))))))

;; any token that is not ':' ',' ']' or '}'
(define (value-token? tk)
  (and (not (eof-object? tk))
       (or (not (symbol? tk))
	   (memq tk '(null array-start object-start)))))

(define (token->string tk)
  (cond ((symbol? tk)
	 (case tk
	   ((array-start) "[")
	   ((array-end)   "]")
	   ((object-start) "{")
	   ((object-end)   "}")
	   ((name-seperator) ":")
	   ((value-seperator) ",")
	   ((null) "null")
	   (else (error "token->string unknown token" tk))))
	((string? tk) tk)
	((number? tk) (number->string tk))
	((eq? tk #t) "true")
	((eq? tk #f) "false")
	((eof-object? tk) "eof")
	(else (error "token->string unknown token" tk))))

(define (json-unexpected-token-error tk)
  (json-error (string-append "Unexpected '" (token->string tk) "'")))

(define (make-json-generator tgen)

  #;
  (define (tgen)
    (let ((tk (tgen*)))
      (display "TOKEN: " ##stdout-port) (write tk ##stdout-port)
      ;; (display " " ##stdout-port)
      ;; (write *context* ##stdout-port)
      (newline ##stdout-port)
      tk))
  
  (define depth 0)

  (define (depth-inc! n)
    ;; TODO check the depth limit
    (set! depth (+ depth n)))

  (define *context* '(init eof))

  (define (current-context) (car *context*))

  (define (context-pop!)
    (let ((x (car *context*)))
      (set! *context* (cdr *context*))
      x))

  (define (context-push! sym)
    (set! *context* (cons sym *context*)))

  (define (maybe-enter! tk)
    (case tk
      ((array-start) (context-push! 'array-start) (depth-inc! 1))
      ((object-start) (context-push! 'object-start) (depth-inc! 1))))

  (lambda ()

    #;
    (begin (newline ##stdout-port)
	   (display "CONTEXT: " ##stdout-port)
	   (write *context* ##stdout-port)
	   (newline ##stdout-port))
    
    (case (current-context)
      
      ((eof)
       (eof-object))
      
      ((init)
       (context-pop!)
       (let ((tk (tgen)))
	 (cond ((eof-object? tk) tk)
	       ((value-token? tk)
		(maybe-enter! tk)
		tk)
	       (else
		(json-error
		 (string-append "invalid json syntax, unexpected '" (token->string tk) "'"))))))
      
      ((array-start)
       (context-pop!)
       (let ((tk (tgen)))
	 (cond ((value-token? tk)
		(context-push! 'array-body)
		(maybe-enter! tk)
		tk)
	       ((eq? tk 'array-end)
		tk)
	       (else (json-error
		      (string-append "invalid json syntax, unexpected '" (token->string tk) "'"))))))
      
      ((array-body)
       (let ((tk (tgen)))
	 (case tk
	   ((value-seperator)
	    (let ((tk (tgen)))
	      (cond ((value-token? tk)
		     (maybe-enter! tk)
		     tk)
		    (else (json-error
			   (string-append "invalid json syntax, unexpected '" (token->string tk) "'"))))))
	   ((array-end)
	    (context-pop!)
	    (depth-inc! -1)
	    tk)
	   ((object-end)
	    (json-error "invalid json syntax, unexpected '}'"))
	   (else
	    (json-error "invalid json syntax, missing ',' between array elements")))))

      ((object-start)
       (context-pop!)
       (let ((tk (tgen)))
	 (cond ((string? tk)
		(context-push! 'object-member-value)
		tk)
	       ((eq? tk 'object-end)
		(depth-inc! -1)
		tk)
	       (else (json-error "invalid json syntax, expected object member name")))))

      ((object-member-value)
       (context-pop!)
       (case (tgen)
	 ((name-seperator)
	  (let ((tk (tgen)))
	    (cond ((value-token? tk)
		   (context-push! 'object-body)
		   (maybe-enter! tk)
		   tk)
		  (else (json-error
			 (string-append "invalid json syntax, unexpected '" (token->string tk) "'"))))))
	 (else
	  (json-error "invalid json syntax, missing ':'"))))

      ((object-body)
       (let ((tk (tgen)))
	 (case tk
	   ((value-seperator)
	    (let ((tk (tgen)))
	      (cond ((string? tk)
		     (context-pop!)
		     (context-push! 'object-member-value)
		     tk)
		    (else
		     (json-error "invalid json syntax, expected object member name")))))
	   ((object-end)
	    (context-pop!)
	    (depth-inc! -1)
	    tk)
	   (else
	    (if (value-token? tk)
		(json-error "invalid json syntax, missing ',' between object members")
		(json-unexpected-token-error tk)))))))))


(define (json-generator #!optional (port-or-generator (current-input-port)))
  (receive (peek get) (wrap-port-or-generator port-or-generator)
    (make-json-generator
     (make-json-token-generator peek get))))

(define (json-fold proc
		   array-start
		   array-end
		   object-start
		   object-end
		   seed
		   #!optional
		   (port-or-generator (current-input-port)))
  (let ((gen (json-generator port-or-generator)))
    (let loop ((acc seed))
      (let ((x (gen)))
;	(write x)
;	(newline)
	(cond ((eof-object? x) acc)
	      ((eq? x 'array-start)
	       (let ((x (loop (array-start acc))))
		 (loop (proc x acc))))
	      ((eq? x 'array-end)
	       (array-end acc))
	      ((eq? x 'object-start)
	       (let ((x (loop (object-start acc))))
		 (loop (proc x acc))))
	      ((eq? x 'object-end)
	       (object-end acc))
	      (else
	       (loop (proc x acc))))))))


;; This definition of json-read is taken from the srfi document

(define (json-read #!optional (port-or-generator (current-input-port)))

  (define %root '(root))

  (define (array-start seed)
    ;; array will be read as a list, then converted into a vector in
    ;; array-end.
    '())

  (define (array-end items)
    (list->vector (reverse items)))

  (define (object-start seed)
    ;; object will be read as a property list, then converted into an
    ;; alist in object-end.
    '())

  (define (plist->alist plist)
    ;; PLIST is a list of an even number of items.  Otherwise,
    ;; json-generator would have raised a json-error.
    (let loop ((plist plist)
               (out '()))
      (if (null? plist)
          out
          (loop (cddr plist) (cons (cons (string->symbol (cadr plist)) (car plist)) out)))))

  (define object-end plist->alist)

  (define (proc obj seed)
    ;; proc is called when a JSON value or structure was completely
    ;; read.  The parse result is passed as OBJ.  In the case where
    ;; what is parsed is a simple JSON value, OBJ is simply
    ;; the token that is read.  It can be 'null, a number or a string.
    ;; In the case where what is parsed is a JSON structure, OBJ is
    ;; what is returned by OBJECT-END or ARRAY-END.
    (if (eq? seed %root)
        ;; This is toplevel.  A complete JSON value or structure was
        ;; read, so return it.
        obj
        ;; This is not toplevel, hence json-fold is called recursively
        ;; to parse an array or object.  Both ARRAY-START and
        ;; OBJECT-START return an empty list as a seed to serve as an
        ;; accumulator.  Both OBJECT-END and ARRAY-END expect a list
        ;; as argument.
        (cons obj seed)))

  (let ((out (json-fold proc
                        array-start
                        array-end
                        object-start
                        object-end
                        %root
                        port-or-generator)))
    ;; if out is the root object, then the port or generator is empty.
    (if (eq? out %root)
        (eof-object)
        out)))

