(define-library (json)
  (export json-error?
	  json-error-reason
	  json-null?
	  json-nesting-depth-limit
	  json-number-of-character-limit
	  json-generator
	  json-fold
	  json-read
	  )
  (import (gambit))
  (include "json.scm"))
