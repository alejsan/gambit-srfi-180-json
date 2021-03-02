; an implementation of srfi 180: json
;; https://srfi.schemers.org/srfi-180/

(declare (standard-bindings)
	 (block)
	 (not safe))

(define-record-type <json-error>
  (make-json-error reason irritants source-or-sink)
  json-error?
  (reason json-error-reason)
  (irritants json-error-irritants)
  (source-or-sink json-error-source-or-sink))

(define (json-error reason . irritants)
  (raise (make-json-error reason irritants (json-current-source-or-sink))))

(define (json-null? obj) (eq? obj 'null))

(define json-nesting-depth-limit
  (make-parameter +inf.0
		  (lambda (n)
		    (if (number? n) n (error "json-nesting-depth-limit not a number" n)))))

(define json-number-of-character-limit
  (make-parameter +inf.0
		  (lambda (n)
		    (if (number? n) n (error "json-number-of-character-limit not a number" n)))))

;; Unexported parameter used when raising a json-error. Should be parameterized
;; to the supplied port-or-generator when reading OR the port-or-accumulator
;; when writing
(define json-current-source-or-sink
  (make-parameter #f))

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
(define (wrap-port-or-generator port-or-generator character-limit)

  (define count 0)

  (define (count-inc!)
    (when (= count character-limit)
      (json-error "Read error, character limit reached"))
    (set! count (+ count 1)))
  
  (cond ((and (textual-port? port-or-generator)
	      (input-port? port-or-generator))
	 (values (lambda () (peek-char port-or-generator))
		 (lambda () (count-inc!) (read-char port-or-generator))))
	
	;; TODO if i'm going to use unsafe ## procedures then I need to add checks
	;; that user provided generator always produces either a character or eof
	((procedure? port-or-generator)
	 (let ((peeked #f))
	   (values (lambda ()
		     (or peeked
			 (let ((x (begin (count-inc!) (port-or-generator))))
			   (set! peeked x)
			   x)))
		   (lambda ()
		     (if peeked
			 (let ((x peeked))
			   (set! peeked #f)
			   x)
			 (begin (count-inc!) (port-or-generator)))))))
	(else
	 (error "Expected textual input-port or generator" port-or-generator))))

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
    (else (json-error "Read error, unrecognized literal"))))

(define (read-json-number peek get)

  (define (write-ascii-digit ch)
    (if (digit0-9? ch)
	(write-char ch)
	(json-error "Read error, invalid number")))

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
	  (else (json-error "Read error, invalid number"))))

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
	  (else (json-error "Read error, invalid number"))))

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
	  (else (json-error "Read error, invalid number"))))
  
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
			(else (json-error "Read error, invalid number")))))
	       ((digit1-9? in)
		(write-char (get))
		(read-int (peek)))
	       (else (json-error "Read error, invalid number"))))))))

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
	     (json-error "Read error, invalid unicode escape in string")))))
  
  (let loop ((in (peek))
	     (acc 0)
	     (count 0))
    (if (= count 4)
	acc
	(if (eof-object? in)
	    (json-error "Read error, unexpected eof")
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
	       (json-error "Read error, string missing terminating \""))
	      ((eq? in #\\)
	       (let ((x (get)))
		 (if (eof-object? x)
		     (json-error "Read error, string missing terminating \"")
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
				 (unless (eq? (get) #\\)
				   (json-error "Read error, unmatched high surrogate in string"))
				 (unless (eq? (get) #\u)
				   (json-error "Read error, unmatched high surrogate in string"))
				 (let ((maybe-low (read-unicode-escape peek get)))
				   (if (low-surrogate? maybe-low)
				       (write-char (integer->char (surrogate-pair->code-point n maybe-low)))
				       (json-error "Read error, unmatched high surrogate in string"))))
				((low-surrogate? n)
				 (json-error "Read error, unmatched low surrogate in string"))
				(else
				 (write-char (integer->char n))))))
		       (else
			(json-error "Read error, invalid escape sequence in string" (string #\\ x)))))
		 (loop (get))))
	      ((eq? in #\") #t)
	      ((json-string-char-must-be-escaped? in)
	       (json-error "Read error, unescaped control character in string" in))
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
	      (json-error "Read error, unexpected eof"))
	     ((digit0-9? x)
	      (read-json-number peek get))
	     (else
	      (json-error "Read error, unexpected character" x)))))))

(define (read-json-array depth peek get)
  (get) ; get initial '['
  (skip-json-whitespace peek get)
  (if (eq? (peek) #\])
      (begin (get) #())
      (let loop ((acc '()))
	(let ((value (read-json-value depth peek get)))
	  (skip-json-whitespace peek get)
	  (let ((x (peek)))
	    (cond ((eof-object? x)
		   (json-error "Read error, unexpected eof"))
		  ((eq? x #\,)
		   (get)
		   (skip-json-whitespace peek get)
		   (loop (cons value acc)))
		  ((eq? x #\])
		   (get)
		   (list->vector (reverse (cons value acc))))
		  (else
		   (json-error "Read error, missing ',' between array elements"))))))))

(define (read-json-object depth peek get)

  (define (read-key)
    (let ((x (peek)))
      (cond ((eq? x #\") (read-json-string peek get))
	    ((eof-object? x) (json-error "Read error, unexpected eof"))
	    (else (json-error "Read error, object member key is not a string")))))
  
  (define (read-colon-and-value key)
    (skip-json-whitespace peek get)
    (let ((x (get)))
      (cond ((eq? x #\:)
	     (skip-json-whitespace peek get)
	     (read-json-value depth peek get))
	    ((eof-object? x)
	     (json-error "Read error, unexpected eof"))
	    (else
	     (json-error "Read error, missing ':' after object key" key)))))
  
  (get) ; get initial '{'
  (skip-json-whitespace peek get)
  (if (eq? (peek) #\})
      (begin (get) '())
      (let loop ((acc '()))
	(let* ((key (read-key))
	       (value (read-colon-and-value key)))
	  (skip-json-whitespace peek get)
	  (let ((x (peek)))
	    (cond ((eof-object? x)
		   (json-error "Read error, unexpected eof"))
		  ((eq? x #\,)
		   (get)
		   (skip-json-whitespace peek get)
		   (loop (cons (cons (string->symbol key) value) acc)))
		  ((eq? x #\})
		   (get)
		   (reverse (cons (cons (string->symbol key) value) acc)))
		  (else
		   (json-error "Read error, missing ',' between object members"))))))))

(define (read-json-value depth peek get)
  (let ((x (peek)))
    (case x
      ((#\[)
       (when (= (+ depth 1) (json-nesting-depth-limit))
	 (json-error "Read error, nesting depth limit reached"))
       (read-json-array (+ depth 1) peek get))
      ((#\{)
       (when (= (+ depth 1) (json-nesting-depth-limit))
	 (json-error "Read error, nesting depth limit reached"))
       (read-json-object (+ depth 1) peek get))
      (else
       (read-json-scalar peek get)))))

(define (make-json-generator peek get port-or-generator depth-limit)

  (define depth 0)

  (define stack '())

  (define (stack-pop!)
    (let ((x (car stack)))
      (set! depth (- depth 1))
      (set! stack (cdr stack))
      (set! require-comma? #t)
      x))

  (define (stack-push! x)
    (set! depth (+ depth 1))
    (when (= depth depth-limit)
      (json-error "Read error, nesting depth limit reached"))
    (set! stack (cons x stack))
    (set! require-comma? #f))
  
  (define require-comma? #f)

  (define (read-json-scalar-or-enter-non-scalar)
    (case (peek)
      ((#\[)
       (get)
       (stack-push! handle-array)
       'array-start)
      ((#\{)
       (get)
       (stack-push! handle-object)
       'object-start)
      (else (read-json-scalar peek get))))

  (define (start)
    (set! stack (cdr stack))
    (skip-json-whitespace peek get)
    (if (eof-object? (peek))
	(eof-object)
	(read-json-scalar-or-enter-non-scalar)))

  (define (handle-array)
    (skip-json-whitespace peek get)
    (let ((x (peek)))
      (cond ((eof-object? x)
	     (json-error "Read error, unexpected eof"))
	    ((eq? x #\])
	     (get)
	     (stack-pop!)
	     'array-end)
	    (else
	     (if require-comma?
		 (if (eq? x #\,)
		     (begin (get) (skip-json-whitespace peek get))
		     (json-error "Read error, missing ',' between array elements"))
		 (set! require-comma? #t))
	     (read-json-scalar-or-enter-non-scalar)))))

  (define (handle-object)
    (skip-json-whitespace peek get)
    (let ((x (peek)))
      (cond ((eof-object? x)
	     (json-error "Read error, unexpected eof"))
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
			      (json-error "Read error, missing ',' between object members"))
			  (begin (set! require-comma? #t)
				 x))))
	       (cond ((eq? x #\")
		      (begin (set! stack (cons handle-object-value stack))
			     (read-json-string peek get)))
		     ((eof-object? x)
		      (json-error "Read error, unexpected eof"))
		     (else
		      (json-error "Read error, object member key is not a string"))))))))

  (define (handle-object-value)
    (skip-json-whitespace peek get)
    (let ((x (peek)))
      (cond ((eof-object? x)
	     (json-error "Read error, unexpected eof"))
	    ((eq? x #\:)
	     (get)
	     (set! stack (cdr stack))
	     (skip-json-whitespace peek get)
	     (read-json-scalar-or-enter-non-scalar))
	    (else
	     (json-error "Read error, missing ':' after object key")))))

  (set! stack (list start eof-object))
  
  (lambda ()
    (parameterize ((json-current-source-or-sink port-or-generator))
      (with-exception-catcher
	  (lambda (e)
	    (cond ((and (os-exception? e)
			(memq port-or-generator (os-exception-arguments e))
			;; TODO the exact code may be OS dependant!
			(= -507496832 (os-exception-code e)))
		   (json-error "Read error, invalid UTF-8 encoding" e))
		  (else (raise e))))
	(lambda ()
	  ((car stack)))))))

(define (json-generator #!optional (port-or-generator (current-input-port)))
  (receive (peek get) (wrap-port-or-generator port-or-generator (json-number-of-character-limit))
    (make-json-generator peek get port-or-generator (json-nesting-depth-limit))))

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

(define (json-read #!optional (port-or-generator (current-input-port)))
  (receive (peek get) (wrap-port-or-generator port-or-generator (json-number-of-character-limit))
    (parameterize ((json-current-source-or-sink port-or-generator))
      (with-exception-catcher
	  (lambda (e)
	    (cond ((and (os-exception? e)
			(memq port-or-generator (os-exception-arguments e))
			;; TODO the exact code may be OS dependant!
			(= -507496832 (os-exception-code e)))
		   (json-error "Read error, invalid UTF-8 encoding" e))
		  (else (raise e))))
	(lambda ()
	  (skip-json-whitespace peek get)
	  (if (eof-object? (peek))
	      (eof-object)
	      (read-json-value 0 peek get)))))))


;;; ======================================================================
;;; Writing related utilities

(define (wrap-port-or-accumulator port-or-accumulator)
  (cond ((and (textual-port? port-or-accumulator)
	      (output-port? port-or-accumulator))
	 (lambda (ch) (write-char ch port-or-accumulator)))
	((procedure? port-or-accumulator)
	 port-or-accumulator)
	(else
	 (error "Expected textual output-port or accumulator" port-or-accumulator))))

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
	 (json-error "Write error, scheme number is not valid json" num))))

;; alist is assumed to be valid json
(define (unsafe-write-json-object alist acc)
  (if (null? alist)
      (begin (acc #\{) (acc #\}))
      (begin (acc #\{)
	     (let loop ((alist alist))
	       (write-json-string (symbol->string (caar alist)) acc)
	       (acc #\:)
	       (unsafe-write-json-scheme (cdar alist) acc)
	       (when (pair? (cdr alist))
		 (acc #\,)
		 (loop (cdr alist))))
	     (acc #\}))))

;; vec is assumed to be valid json
(define (unsafe-write-json-vector vec acc)
  (let ((len (vector-length vec)))
    (if (zero? len)
	(begin (acc #\[) (acc #\]))
	(begin (acc #\[)
	       (unsafe-write-json-scheme (vector-ref vec 0) acc)
	       (let loop ((i 1))
		 (unless (= i len)
		   (acc #\,)
		   (unsafe-write-json-scheme (vector-ref vec i) acc)
		   (loop (+ i 1))))
	       (acc #\])))))

;; x is assumed to be valid json
(define (unsafe-write-json-scheme x acc)
  (cond ((number? x)   (write-json-number x acc))
	((string? x)   (write-json-string x acc))
	((vector? x)   (unsafe-write-json-vector x acc))
	((eq? x #t)    (acc #\t) (acc #\r) (acc #\u) (acc #\e))
	((eq? x #f)    (acc #\f) (acc #\a) (acc #\l) (acc #\s) (acc #\e))
	((eq? x 'null) (acc #\n) (acc #\u) (acc #\l) (acc #\l))
	(else          (unsafe-write-json-object x acc))))

(define (make-json-accumulator port-or-accumulator acc)

  (define stack '())

  (define write-comma? #f)
  
  (define (stack-pop!)
    (let ((x (car stack)))
      (set! stack (cdr stack))
      x))

  (define (stack-push! x)
    (set! write-comma? #f)
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
	      (stack-push! handle-array)
	      (acc #\[))
	     ((object-start)
	      (stack-push! handle-object)
	      (acc #\{))
	     ((array-end)
	      (json-error "Write error, unexpected array-end"))
	     ((object-end)
	      (json-error "Write error, unexpected object-end"))
	     (else
	      (json-error "Write error, unexpected value passed to accumulator" x))))))

  (define (start x)
    (write-json-scheme x acc))

  (define (handle-array x)
    (if (eq? x 'array-end)
	(begin (stack-pop!)
	       (acc #\]))
	(begin (if write-comma? (acc #\,) (set! write-comma? #t))
	       (write-json-scheme x acc))))

  (define (handle-object x)
    (cond ((string? x)
	   (if write-comma? (acc #\,) (set! write-comma? #t))
	   (set! stack (cons handle-object-value stack))
	   (write-json-string x acc))
	  ((eq? x 'object-end)
	   (stack-pop!)
	   (acc #\}))
	  (else
	   (json-error "Write error, accumulator expected either a \
                        string beginning a new key/value pair or object-end"
		       x))))

  (define (handle-object-value x)
    (stack-pop!)
    (acc #\:)
    (write-json-scheme x acc))

  (set! stack (list start))
  
  (lambda (x)
    (parameterize ((json-current-source-or-sink port-or-accumulator))
      ((car stack) x))))

(define (json-accumulator #!optional (port-or-accumulator (current-output-port)))
  (make-json-accumulator port-or-accumulator (wrap-port-or-accumulator port-or-accumulator)))

(define (json-write obj #!optional (port-or-accumulator (current-output-port)))
  (if (scheme-is-valid-json? obj)
      (parameterize ((json-current-source-or-sink port-or-accumulator))
	(unsafe-write-json-scheme obj (wrap-port-or-accumulator port-or-accumulator)))
      (json-error "Write error, scheme object is not valid json" obj)))

(define (json-pretty-print obj
			   #!optional
			   (port-or-accumulator (current-output-port))
			   (indent-string "  "))

  (define acc (wrap-port-or-accumulator port-or-accumulator))

  (define (indent)
    (dotimes depth
      (accumulate-substring indent-string acc 0 (string-length indent-string))))

  (define depth 0)
  (define previous #f)

  (define (handle-string ch)
    (if (and (eq? ch #\")
	     (not (eq? previous #\\)))
	(begin (set! previous #f)
	       (set! k main))
	(set! previous ch))
    (acc ch))

  (define (handle-structure-start ch)
    (if (memq ch '(#\] #\}))
	(begin (set! k main)
	       (acc ch))
	(begin (set! depth (+ depth 1))
	       (acc #\newline)
	       (indent)
	       (set! k main)
	       (main ch))))

  (define (main ch)
    (case ch
      ((#\[ #\{)
       (set! k handle-structure-start)
       (acc ch))
      ((#\] #\})
       (set! depth (- depth 1))
       (acc #\newline)
       (indent)
       (acc ch))
      ((#\:)
       (acc ch)
       (acc #\space))
      ((#\,)
       (acc ch)
       (acc #\newline)
       (indent))
      ((#\")
       (set! k handle-string)
       (acc ch))
      (else
       (acc ch))))

  (define k main)
  
  (if (scheme-is-valid-json? obj)
      (if (string? indent-string)
	  (parameterize ((json-current-source-or-sink port-or-accumulator))
	    (unsafe-write-json-scheme obj (lambda (ch) (k ch)))
	    (acc #\newline))
	  (error "Expected string" indent-string))
      (json-error "Write error, scheme object is not valid json" obj)))

