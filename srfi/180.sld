(define-library (srfi 180)
  (export json-error?
	  json-error-reason
	  json-error-irritants
	  json-error-source-or-sink
	  json-null?
	  json-nesting-depth-limit
	  json-number-of-character-limit
	  json-generator
	  json-fold
	  json-read
	  json-accumulator
	  json-write)
  (import (gambit))
  (include "180.scm"))
