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

;;; ======================================================================
;;; Reading related utilities

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
    (or (= n #x005c)            ; \
	(= n #x0022)            ; "
	(>= #x001F n #x000))))  ; control characters

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

;;; ======================================================================
;;; Reading

  ;; Input is positioned on first non-whitespace character or eof
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

(define (read-json-scalar peek get)
  (let ((x (peek)))
    (case x
      ((#\") (read-json-string peek get))
      ((#\t #\f #\n) (read-json-literal peek get))
      ((#\-) (read-json-number peek get))
      (else
       (cond ((eof-object? x)
	      (json-error "Unexpected 'eof'"))
	     ((digit0-9? x)
	      (read-json-number peek get))
	     (else
	      (json-error (string-append "Unexpected character '" (string x) "'"))))))))

(define (make-json-generator peek get)

  (define depth 0)

  (define stack '())

  (define (stack-pop!)
    (let ((x (car stack)))
      (set! depth (- depth 1))
      (set! stack (cdr stack))
      (set! require-comma? #t)
      x))

  (define (stack-push! x)
    ;; TODO check depth limit
    (set! depth (+ depth 1))
    (set! stack (cons x stack))
    (set! require-comma? #f))

  (define generator-first-use? #t)
  
  (define require-comma? #f)

  (define require-colon? #f)

  (define (read-json-scalar-or-enter-non-scalar)
    (case (peek)
      ((#\[)
       (get)
       (stack-push! 'array)
       'array-start)
      ((#\{)
       (get)
       (stack-push! 'object)
       'object-start)
      (else (read-json-scalar peek get))))

  (lambda ()
    (cond
     
     (generator-first-use?
      (set! generator-first-use? #f)
      (skip-json-whitespace peek get)
      (if (eof-object? (peek))
	  (eof-object)
	  (read-json-scalar-or-enter-non-scalar)))
     
     ((null? stack)
      (eof-object))
     
     ((eq? 'array (car stack))
      (skip-json-whitespace peek get)
      (let ((x (peek)))
	(cond ((eof-object? x)
	       (json-error "Unexpected 'eof'"))
	      ((eq? x #\])
	       (get)
	       (stack-pop!)
	       'array-end)
	      (else
	       (if require-comma?
		   (if (eq? x #\,)
		       (begin (get) (skip-json-whitespace peek get))
		       (json-error "Missing ',' between array elements"))
		   (set! require-comma? #t))
	       (read-json-scalar-or-enter-non-scalar)))))

     ;; stack-top = object
     (else
      (skip-json-whitespace peek get)
      (let ((x (peek)))
	(cond ((eof-object? x)
	       (json-error "Unexpected 'eof'"))
	      (require-colon?
	       (if (eq? x #\:)
		   (begin (get)
			  (set! require-colon? #f)
			  (skip-json-whitespace peek get)
			  (read-json-scalar-or-enter-non-scalar))
		   (json-error "Unmatched object key, expected ':'")))
	      ((eq? x #\})
	       (get)
	       (stack-pop!)
	       'object-end)
	      (else
	       (let ((x (if require-comma?
			    (if (eq? x #\,)
				(begin (get)
				       (skip-json-whitespace peek get)
				       (peek))
				(json-error "Missing ',' between object members"))
			    (begin (set! require-comma? #t)
				   x))))
		 (cond ((eq? x #\")
			(begin (set! require-colon? #t)
			       (read-json-string peek get)))
		       ((eof-object? x)
			(json-error "Unexpected 'eof'"))
		       (else
			(json-error "Invalid object member name, expected string")))))))))))

(define (json-generator #!optional (port-or-generator (current-input-port)))
  (receive (peek get) (wrap-port-or-generator port-or-generator)
    (make-json-generator peek get)))

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

;;; ======================================================================
;;; Writing related utilities

(define (output-port->accumulator port)
  (lambda (ch) (write-char ch port)))

(define (accumulate-substring str acc start end)
  (let loop ((i start))
    (unless (>= i end)
      (acc (string-ref str i))
      (loop (+ i 1)))))

(define-syntax dotimes
  (syntax-rules ()
    ((_ n expr)
     (let loop ((count n))
       (when (> count 0)
	 expr
	 (loop (- count 1)))))))

(define (scheme-number-is-valid-json? num)
  (or (exact-integer? num)
      (and (inexact? num) (rational? num))))

(define (scheme-alist-is-valid-json? alist)
  ;; must check both null? and pair? in case of improper list
  (or (null? alist)
      (and (pair? alist)
	   (pair? (car alist))
	   (symbol? (caar alist))
	   (scheme-is-valid-json? (cdar alist))
	   (scheme-alist-is-valid-json? (cdr alist)))))

(define (scheme-vector-is-valid-json? vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (or (= i len)
	  (and (scheme-is-valid-json? (vector-ref vec i))
	       (loop (+ i 1)))))))

(define (scheme-is-valid-json? x)
  (or (string? x)
      (and (number? x) (scheme-number-is-valid-json? x))
      (and (vector? x) (scheme-vector-is-valid-json? x))
      (memq x '(#t #f null))
      (scheme-alist-is-valid-json? x)))

;;; ======================================================================
;;; writing

(define (write-json-string str acc)
  (acc #\")
  (string-for-each (lambda (ch)
		     (case ch
		       ((#\" #\\)     (acc #\\) (acc ch))
		       ((#\backspace) (acc #\\) (acc #\b))
		       ((#\x000c)     (acc #\\) (acc #\f))  ; form feed
		       ((#\newline)   (acc #\\) (acc #\n))
		       ((#\return)    (acc #\\) (acc #\r))
		       ((#\tab)       (acc #\\) (acc #\t))
		       (else
			(if (<= (char->integer ch) #x001F)
			    (let ((hexstring (number->string (char->integer ch) 16)))
			      (acc #\\)
			      (acc #\u)
			      (dotimes (- 4 (string-length hexstring))
				(acc #\0))
			      (string-for-each acc hexstring))
			    (acc ch)))))
		   str)
  (acc #\"))

(define (write-json-number num acc)
  (cond ((exact-integer? num)
	 (string-for-each acc (number->string num)))
	((and (inexact? num) (rational? num))
	 (let* ((str (number->string num))
		(len (string-length str))
		(i (if (eq? #\- (string-ref str 0))
		       (begin (acc #\-) 1)
		       0)))
	   ;; if number starts with '.' add leading 0
	   (when (char=? #\. (string-ref str i)) (acc #\0))
	   (accumulate-substring str acc i len)
	   ;; if number ends with  '.' add trailing 0
	   (when (char=? #\. (string-ref str (- len 1))) (acc #\0))))
	(else
	 (json-error (string-append "Scheme number is not valid json: " (number->string num))))))

;; alist is assumed to be valid json
(define (unsafe-write-json-object alist acc)
  (if (null? alist)
      (begin (acc #\{) (acc #\}))
      (begin (acc #\{)
	     (let loop ((alist alist))
	       (write-json-string (symbol->string (caar alist)) acc)
	       (acc #\:)
	       (unsafe-write-json-scheme (cdar alist) acc)
	       (if (pair? (cdr alist))
		   (begin (acc #\,)
			  (loop (cdr alist)))
		   (acc #\}))))))

;; vec is assumed to be valid json
(define (unsafe-write-json-vector vec acc)
  (let ((len (vector-length vec)))
    (if (zero? len)
	(begin (acc #\[) (acc #\]))
	(begin (acc #\[)
	       (let loop ((i 0))
		 (unsafe-write-json-scheme (vector-ref vec i) acc)
		 (if (= i (- len 1))
		     (acc #\])
		     (begin (acc #\,)
			    (loop (+ i 1)))))))))

;; x is assumed to be valid json
(define (unsafe-write-json-scheme x acc)
  (cond ((number? x)   (write-json-number x acc))
	((string? x)   (write-json-string x acc))
	((vector? x)   (unsafe-write-json-vector x acc))
	((eq? x #t)    (acc #\t) (acc #\r) (acc #\u) (acc #\e))
	((eq? x #f)    (acc #\f) (acc #\a) (acc #\l) (acc #\s) (acc #\e))
	((eq? x 'null) (acc #\n) (acc #\u) (acc #\l) (acc #\l))
	(else          (unsafe-write-json-object x acc))))

(define (make-json-accumulator acc)

  (define stack '())

  ;; structure-state is #f or one of the symbols head | body | object-member-value

  ;; head means we have just begun an object or array (do NOT emit a comma)

  ;; body means we have emitted at least one element of an array or key/value
  ;; pair of an object (do emit comma)

  ;; object-member-value means we are expecting the value portion of an object
  ;; key/value pair (emit colon, do NOT emit comma)
  
  (define structure-state #f)
  
  (define (stack-pop!)
    (let ((head (car stack))
	  (tail (cdr stack)))
      (set! structure-state (if (pair? tail) 'body #f))
      (set! stack tail)
      head))

  (define (stack-push! x)
    (set! structure-state 'head)
    (set! stack (cons x stack)))

  ;; Write a scheme object x into the accumulator. Does not handle writing
  ;; preceding comma or colon, does not handle array-end or object-end
  (define (write-json-scheme x acc)
    (cond ((number? x) (write-json-number x acc))
	  ((string? x) (write-json-string x acc))
	  ((eq? x #t) (acc #\t) (acc #\r) (acc #\u) (acc #\e))
	  ((eq? x #f) (acc #\f) (acc #\a) (acc #\l) (acc #\s) (acc #\e))
	  (else
	   (case x
	     ((null) (acc #\n) (acc #\u) (acc #\l) (acc #\l))
	     ((array-start)
	      (stack-push! 'array)
	      (acc #\[))
	     ((object-start)
	      (stack-push! 'object)
	      (acc #\{))
	     ((array-end)
	      (json-error "Write error, unexpected array-end"))
	     ((object-end)
	      (json-error "Write error, unexpected object-end"))
	     (else
	      (json-error "Write error, unexpected value passed to accumulator" x))))))

  (lambda (x)
    (if (null? stack)
	(write-json-scheme x acc)
	(case (car stack)
	  
	  ((object)
	   (case structure-state
	     ((head body)
	      (cond ((string? x)
		     (when (eq? structure-state 'body) (acc #\,))
		     (set! structure-state 'object-member-value)
		     (write-json-string x acc))
		    ((eq? x 'object-end)
		     (stack-pop!)
		     (acc #\}))
		    (else
		     (json-error "Write error, accumulator expected either a \
                                  string beginning a new key/value pair or object-end"
				 x))))
	     (else ;object-member-value
	      (acc #\:)
	      (set! structure-state 'body)
	      (write-json-scheme x acc))))

	  (else ;array
	   (cond ((eq? x 'array-end)
		  (stack-pop!)
		  (acc #\]))
		 ((eq? structure-state 'head)
		  (set! structure-state 'body)
		  (write-json-scheme x acc))
		 (else
		  (acc #\,)
		  (write-json-scheme x acc))))))))

(define (json-accumulator #!optional (port-or-accumulator (current-output-port)))
  (make-json-accumulator (if (port? port-or-accumulator)
			     (output-port->accumulator port-or-accumulator)
			     port-or-accumulator)))

(define (json-write obj #!optional (port-or-accumulator (current-output-port)))
  (if (scheme-is-valid-json? obj)
      (unsafe-write-json-scheme obj (if (port? port-or-accumulator)
					(output-port->accumulator port-or-accumulator)
					port-or-accumulator) )
      (json-error "json-write: scheme object is not valid json" obj)))

