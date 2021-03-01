(import (gambit)
	(_test)
	(srfi 180))

(define-syntax current-filename
  (lambda (src) (##source-path src)))

(define test-data-dir
  (string-append (path-directory (current-filename)) "/test_data/test_parsing"))

(define (string->json-generator str)
  (json-generator (open-input-string str)))

(define (file->json-generator f)
  (json-generator (open-input-file (list path: f char-encoding: 'UTF-8))))

(define (json-read-file f)
  (with-input-from-file (list path: f char-encoding: 'UTF-8) json-read))

(define (json-read-string str)
  (with-input-from-string str json-read))

(define (json-write-into-string x)
  (with-output-to-string (lambda () (json-write x))))

(define (generator-for-each proc gen)
  (let loop ((x (gen)))
    (unless (eof-object? x)
      (proc x)
      (loop (gen)))))

(define (list->generator lis)
  (lambda ()
    (if (null? lis)
	(eof-object)
	(let ((x (car lis)))
	  (set! lis (cdr lis))
	  x))))

(define (generator->list gen)
  (let loop ((x (gen))
	     (acc '()))
    (if (eof-object? x)
	(reverse acc)
	(loop (gen) (cons x acc)))))

(define (json-tokenize-file f)
  (generator->list (file->json-generator f)))

(define (json-tokenize-string str)
  (generator->list (string->json-generator str)))

;; The current implementation of json-write does not use json-accumulator so
;; they must be tested seperately

(define (json-accumulate-into-string json-gen)
  (call-with-output-string
   (lambda (output-string)
     (generator-for-each (json-accumulator output-string) json-gen))))

(define (json-accumulate-file-into-string file)
  (json-accumulate-into-string (file->json-generator file)))

(define (json-accumulate-string-into-string string)
  (json-accumulate-into-string (string->json-generator string)))

(define-syntax test-json
  (syntax-rules ()
    ((_ expected file)
     (test-assert
	 (let* ((test-file (string-append test-data-dir "/" file))
		(x (json-read-file test-file))
		(expected* expected))
	   (and (equal? x expected*)
		(equal? expected* (json-read-string (json-write-into-string x)))
		(equal? expected* (json-read-string (json-accumulate-file-into-string test-file)))))))))

(define-syntax test-json-error
  (syntax-rules ()
    ((_ file)
     (begin
       (test-error json-error? (json-read-file (string-append test-data-dir "/" file)))
       (test-error json-error? (json-tokenize-file (string-append test-data-dir "/" file)))))))

(define-syntax test-json-accumulator-error
  (syntax-rules ()
    ((_ tokens)
     (test-error json-error? (json-accumulate-into-string (list->generator tokens))))))

;;; ======================================================================
;;; implementation dependant tests

;; note that several of these tests deal with very large or small numbers so the
;; exact behavior may depend on how your gambit was compiled (bignum support?)

;; note that all of these tests assume the file is opened with the char-encoding
;; set to UTF-8. Some of the files for which we expect a json-error object could
;; be parsed if that files specific encoding was used instead.



;; when running this test gambit attempts to construct a very large bignum and
;; eats all my memory

;; (test-json #(+inf.0) "i_number_huge_exp.json")

;; gambit doesn't represent these numbers accurately

;; (test-json #(0.) "i_number_double_huge_neg_exp.json")
;; (test-json #(-inf.0) "i_number_neg_int_huge_exp.json")
;; (test-json #(+inf.0) "i_number_pos_double_huge_exp.json")
;; (test-json #(-inf.0) "i_number_real_neg_overflow.json")
;; (test-json #(+inf.0) "i_number_real_pos_overflow.json")
;; (test-json #(0.) "i_number_real_underflow.json")


(test-json #(-123123123123123123123123123123) "i_number_too_big_neg_int.json")
(test-json #(100000000000000000000) "i_number_too_big_pos_int.json")
(test-json
 #(-237462374673276894279832749832423479823246327846)
 "i_number_very_big_negative_int.json")

(test-json-error "i_object_key_lone_2nd_surrogate.json")
(test-json-error "i_string_1st_surrogate_but_2nd_missing.json")
(test-json-error "i_string_1st_valid_surrogate_2nd_invalid.json")
(test-json-error "i_string_UTF-16LE_with_BOM.json")

(test-json-error "i_string_UTF-8_invalid_sequence.json")
(test-json #("���") "i_string_UTF8_surrogate_U+D800.json")
(test-json-error "i_string_incomplete_surrogate_and_escape_valid.json")
(test-json-error "i_string_incomplete_surrogate_pair.json")
(test-json-error "i_string_incomplete_surrogates_escape_valid.json")
(test-json-error "i_string_invalid_lonely_surrogate.json")
(test-json-error "i_string_invalid_surrogate.json")
(test-json-error "i_string_invalid_utf-8.json")
(test-json-error "i_string_inverted_surrogates_U+1D11E.json")
(test-json-error "i_string_iso_latin_1.json")
(test-json-error "i_string_lone_second_surrogate.json")
(test-json-error "i_string_lone_utf8_continuation_byte.json")
(test-json-error "i_string_not_in_unicode_range.json")
(test-json-error "i_string_overlong_sequence_2_bytes.json")
(test-json-error "i_string_overlong_sequence_6_bytes.json")
(test-json-error "i_string_overlong_sequence_6_bytes_null.json")
(test-json-error "i_string_truncated-utf-8.json")
(test-json-error "i_string_utf16BE_no_BOM.json")
(test-json-error "i_string_utf16LE_no_BOM.json")
(test-json
 #(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  "i_structure_500_nested_arrays.json")
(test-json-error "i_structure_UTF-8_BOM_empty_object.json")

;;; ======================================================================
;;; tests with invalid json, a strictly conformant parser should reject these

(test-json-error "n_array_1_true_without_comma.json")
(test-json-error "n_array_a_invalid_utf8.json")
(test-json-error "n_array_colon_instead_of_comma.json")
(test-json-error "n_array_comma_and_number.json")
(test-json-error "n_array_double_comma.json")
(test-json-error "n_array_double_extra_comma.json")
(test-json-error "n_array_extra_comma.json")
(test-json-error "n_array_incomplete.json")
(test-json-error "n_array_incomplete_invalid_value.json")
(test-json-error "n_array_inner_array_no_comma.json")
(test-json-error "n_array_invalid_utf8.json")
(test-json-error "n_array_items_separated_by_semicolon.json")
(test-json-error "n_array_just_comma.json")
(test-json-error "n_array_just_minus.json")
(test-json-error "n_array_missing_value.json")
(test-json-error "n_array_newlines_unclosed.json")
(test-json-error "n_array_number_and_comma.json")
(test-json-error "n_array_number_and_several_commas.json")
(test-json-error "n_array_spaces_vertical_tab_formfeed.json")
(test-json-error "n_array_star_inside.json")
(test-json-error "n_array_unclosed.json")
(test-json-error "n_array_unclosed_trailing_comma.json")
(test-json-error "n_array_unclosed_with_new_lines.json")
(test-json-error "n_array_unclosed_with_object_inside.json")
(test-json-error "n_incomplete_false.json")
(test-json-error "n_incomplete_null.json")
(test-json-error "n_incomplete_true.json")
(test-json-error "n_multidigit_number_then_00.json")
(test-json-error "n_number_++.json")
(test-json-error "n_number_+1.json")
(test-json-error "n_number_+Inf.json")
(test-json-error "n_number_-01.json")
(test-json-error "n_number_-1.0..json")
(test-json-error "n_number_-2..json")
(test-json-error "n_number_-NaN.json")
(test-json-error "n_number_.-1.json")
(test-json-error "n_number_.2e-3.json")
(test-json-error "n_number_0.1.2.json")
(test-json-error "n_number_0.3e+.json")
(test-json-error "n_number_0.3e.json")
(test-json-error "n_number_0.e1.json")
(test-json-error "n_number_0_capital_E+.json")
(test-json-error "n_number_0_capital_E.json")
(test-json-error "n_number_0e+.json")
(test-json-error "n_number_0e.json")
(test-json-error "n_number_1.0e+.json")
(test-json-error "n_number_1.0e-.json")
(test-json-error "n_number_1.0e.json")
(test-json-error "n_number_1_000.json")
(test-json-error "n_number_1eE2.json")
(test-json-error "n_number_2.e+3.json")
(test-json-error "n_number_2.e-3.json")
(test-json-error "n_number_2.e3.json")
(test-json-error "n_number_9.e+.json")
(test-json-error "n_number_Inf.json")
(test-json-error "n_number_NaN.json")
(test-json-error "n_number_U+FF11_fullwidth_digit_one.json")
(test-json-error "n_number_expression.json")
(test-json-error "n_number_hex_1_digit.json")
(test-json-error "n_number_hex_2_digits.json")
(test-json-error "n_number_infinity.json")
(test-json-error "n_number_invalid+-.json")
(test-json-error "n_number_invalid-negative-real.json")
(test-json-error "n_number_invalid-utf-8-in-bigger-int.json")
(test-json-error "n_number_invalid-utf-8-in-exponent.json")
(test-json-error "n_number_invalid-utf-8-in-int.json")
(test-json-error "n_number_minus_infinity.json")
(test-json-error "n_number_minus_sign_with_trailing_garbage.json")
(test-json-error "n_number_minus_space_1.json")
(test-json-error "n_number_neg_int_starting_with_zero.json")
(test-json-error "n_number_neg_real_without_int_part.json")
(test-json-error "n_number_neg_with_garbage_at_end.json")
(test-json-error "n_number_real_garbage_after_e.json")
(test-json-error "n_number_real_with_invalid_utf8_after_e.json")
(test-json-error "n_number_real_without_fractional_part.json")
(test-json-error "n_number_starting_with_dot.json")
(test-json-error "n_number_with_alpha.json")
(test-json-error "n_number_with_alpha_char.json")
(test-json-error "n_number_with_leading_zero.json")
(test-json-error "n_object_bad_value.json")
(test-json-error "n_object_bracket_key.json")
(test-json-error "n_object_comma_instead_of_colon.json")
(test-json-error "n_object_double_colon.json")
(test-json-error "n_object_emoji.json")
(test-json-error "n_object_garbage_at_end.json")
(test-json-error "n_object_key_with_single_quotes.json")
(test-json-error
 "n_object_lone_continuation_byte_in_key_and_trailing_comma.json")
(test-json-error "n_object_missing_colon.json")
(test-json-error "n_object_missing_key.json")
(test-json-error "n_object_missing_semicolon.json")
(test-json-error "n_object_missing_value.json")
(test-json-error "n_object_no-colon.json")
(test-json-error "n_object_non_string_key.json")
(test-json-error "n_object_non_string_key_but_huge_number_instead.json")
(test-json-error "n_object_repeated_null_null.json")
(test-json-error "n_object_several_trailing_commas.json")
(test-json-error "n_object_single_quote.json")
(test-json-error "n_object_two_commas_in_a_row.json")
(test-json-error "n_object_unquoted_key.json")
(test-json-error "n_object_unterminated-value.json")
(test-json-error "n_object_with_single_string.json")
(test-json-error "n_string_1_surrogate_then_escape.json")
(test-json-error "n_string_1_surrogate_then_escape_u.json")
(test-json-error "n_string_1_surrogate_then_escape_u1.json")
(test-json-error "n_string_1_surrogate_then_escape_u1x.json")
(test-json-error "n_string_accentuated_char_no_quotes.json")
(test-json-error "n_string_backslash_00.json")
(test-json-error "n_string_escape_x.json")
(test-json-error "n_string_escaped_backslash_bad.json")
(test-json-error "n_string_escaped_ctrl_char_tab.json")
(test-json-error "n_string_escaped_emoji.json")
(test-json-error "n_string_incomplete_escape.json")
(test-json-error "n_string_incomplete_escaped_character.json")
(test-json-error "n_string_incomplete_surrogate.json")
(test-json-error "n_string_incomplete_surrogate_escape_invalid.json")
(test-json-error "n_string_invalid-utf-8-in-escape.json")
(test-json-error "n_string_invalid_backslash_esc.json")
(test-json-error "n_string_invalid_unicode_escape.json")
(test-json-error "n_string_invalid_utf8_after_escape.json")
(test-json-error "n_string_leading_uescaped_thinspace.json")
(test-json-error "n_string_no_quotes_with_bad_escape.json")
(test-json-error "n_string_single_doublequote.json")
(test-json-error "n_string_single_quote.json")
(test-json-error "n_string_single_string_no_double_quotes.json")
(test-json-error "n_string_start_escape_unclosed.json")
(test-json-error "n_string_unescaped_ctrl_char.json")
(test-json-error "n_string_unescaped_newline.json")
(test-json-error "n_string_unescaped_tab.json")
(test-json-error "n_string_unicode_CapitalU.json")
(test-json-error "n_structure_100000_opening_arrays.json")
(test-json-error "n_structure_U+2060_word_joined.json")
(test-json-error "n_structure_UTF8_BOM_no_data.json")
(test-json-error "n_structure_angle_bracket_..json")
(test-json-error "n_structure_angle_bracket_null.json")
(test-json-error "n_structure_array_with_unclosed_string.json")
(test-json-error "n_structure_ascii-unicode-identifier.json")
(test-json-error "n_structure_capitalized_True.json")
(test-json-error "n_structure_comma_instead_of_closing_brace.json")
(test-json-error "n_structure_end_array.json")
(test-json-error "n_structure_incomplete_UTF8_BOM.json")
(test-json-error "n_structure_lone-invalid-utf-8.json")
(test-json-error "n_structure_lone-open-bracket.json")
(test-json-error "n_structure_null-byte-outside-string.json")
(test-json-error "n_structure_number_with_trailing_garbage.json")
(test-json-error "n_structure_object_unclosed_no_value.json")
(test-json-error "n_structure_object_with_comment.json")
(test-json-error "n_structure_open_array_apostrophe.json")
(test-json-error "n_structure_open_array_comma.json")
(test-json-error "n_structure_open_array_object.json")
(test-json-error "n_structure_open_array_open_object.json")
(test-json-error "n_structure_open_array_open_string.json")
(test-json-error "n_structure_open_array_string.json")
(test-json-error "n_structure_open_object.json")
(test-json-error "n_structure_open_object_close_array.json")
(test-json-error "n_structure_open_object_comma.json")
(test-json-error "n_structure_open_object_open_array.json")
(test-json-error "n_structure_open_object_open_string.json")
(test-json-error "n_structure_open_object_string_with_apostrophes.json")
(test-json-error "n_structure_open_open.json")
(test-json-error "n_structure_single_eacute.json")
(test-json-error "n_structure_single_star.json")
(test-json-error "n_structure_uescaped_LF_before_string.json")
(test-json-error "n_structure_unclosed_array.json")
(test-json-error "n_structure_unclosed_array_partial_null.json")
(test-json-error "n_structure_unclosed_array_unfinished_false.json")
(test-json-error "n_structure_unclosed_array_unfinished_true.json")
(test-json-error "n_structure_unclosed_object.json")
(test-json-error "n_structure_unicode-identifier.json")
(test-json-error "n_structure_whitespace_U+2060_word_joiner.json")
(test-json-error "n_structure_whitespace_formfeed.json")


;; read eof from empty file 
(test-equal (eof-object)
	    (json-read-file (string-append test-data-dir "/" "n_structure_no_data.json")))

;; won't fix
;; (test-json-error "n_array_comma_after_close.json")
;; (test-json-error "n_array_extra_close.json")
;; (test-json-error "n_object_trailing_comma.json")
;; (test-json-error "n_object_trailing_comment.json")
;; (test-json-error "n_object_trailing_comment_open.json")
;; (test-json-error "n_object_trailing_comment_slash_open.json")
;; (test-json-error "n_object_trailing_comment_slash_open_incomplete.json")
;; (test-json-error "n_object_with_trailing_garbage.json")
;; (test-json-error "n_single_space.json")
;; (test-json-error "n_string_with_trailing_garbage.json")
;; (test-json-error "n_structure_array_trailing_garbage.json")
;; (test-json-error "n_structure_array_with_extra_array_close.json")
;; (test-json-error "n_structure_close_unopened_array.json")
;; (test-json-error "n_structure_double_array.json")
;; (test-json-error "n_structure_object_followed_by_closing_object.json")
;; (test-json-error "n_structure_object_with_trailing_garbage.json")
;; (test-json-error "n_structure_trailing_#.json")

;; ======================================================================
;; tests with valid json

(test-json #(#()) "y_array_arraysWithSpaces.json")
(test-json #("") "y_array_empty-string.json")
(test-json #() "y_array_empty.json")
(test-json #("a") "y_array_ending_with_newline.json")
(test-json #(#f) "y_array_false.json")
(test-json #(null 1 "1" ()) "y_array_heterogeneous.json")
(test-json #(null) "y_array_null.json")
(test-json #(1) "y_array_with_1_and_newline.json")
(test-json #(1) "y_array_with_leading_space.json")
(test-json #(1 null null null 2) "y_array_with_several_null.json")
(test-json #(2) "y_array_with_trailing_space.json")
(test-json #(1.23e67) "y_number.json")
(test-json #(0e+1) "y_number_0e+1.json")
(test-json #(0e1) "y_number_0e1.json")
(test-json #(4) "y_number_after_space.json")
(test-json #(-1e-78) "y_number_double_close_to_zero.json")
(test-json #(200.) "y_number_int_with_exp.json")
(test-json #(0) "y_number_minus_zero.json")
(test-json #(-123) "y_number_negative_int.json")
(test-json #(-1) "y_number_negative_one.json")
(test-json #(0) "y_number_negative_zero.json")
(test-json #(1e22) "y_number_real_capital_e.json")
(test-json #(.01) "y_number_real_capital_e_neg_exp.json")
(test-json #(100.) "y_number_real_capital_e_pos_exp.json")
(test-json #(1.23e47) "y_number_real_exponent.json")
(test-json #(1.23456e80) "y_number_real_fraction_exponent.json")
(test-json #(.01) "y_number_real_neg_exp.json")
(test-json #(100.) "y_number_real_pos_exponent.json")
(test-json #(123) "y_number_simple_int.json")
(test-json #(123.456789) "y_number_simple_real.json")
(test-json '((asd . "sdf") (dfg . "fgh")) "y_object.json")
(test-json '((asd . "sdf")) "y_object_basic.json")
(test-json '((a . "b") (a . "c")) "y_object_duplicated_key.json")
(test-json '((a . "b") (a . "b")) "y_object_duplicated_key_and_value.json")
(test-json '() "y_object_empty.json")
(test-json '((|| . 0)) "y_object_empty_key.json")
(test-json '((|foo\x0;bar| . 42)) "y_object_escaped_null_in_key.json")
(test-json '((min . -1e28) (max . 1e28)) "y_object_extreme_numbers.json")
(test-json
 '((x . #(((id . "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))))
   (id . "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
 "y_object_long_strings.json")
(test-json '((a . #())) "y_object_simple.json")
(test-json '((title . "Полтора Землекопа")) "y_object_string_unicode.json")
(test-json '((a . "b")) "y_object_with_newlines.json")
(test-json #("`Īካ") "y_string_1_2_3_bytes_UTF-8_sequences.json")
(test-json #("\x10437;") "y_string_accepted_surrogate_pair.json")
(test-json #("\x1f639;\x1f48d;") "y_string_accepted_surrogate_pairs.json")
(test-json #("\"\\/\b\f\n\r\t") "y_string_allowed_escapes.json")
(test-json #("\\u0000") "y_string_backslash_and_u_escaped_zero.json")
(test-json #("\"") "y_string_backslash_doublequotes.json")
(test-json #("a/*b*/c/*d//e") "y_string_comments.json")
(test-json #("\\a") "y_string_double_escape_a.json")
(test-json #("\\n") "y_string_double_escape_n.json")
(test-json #("\x12;") "y_string_escaped_control_character.json")
(test-json #("￿") "y_string_escaped_noncharacter.json")
(test-json #("asd") "y_string_in_array.json")
(test-json #("asd") "y_string_in_array_with_leading_space.json")
(test-json #("\x10ffff;") "y_string_last_surrogates_1_and_2.json")
(test-json #("new line") "y_string_nbsp_uescaped.json")
(test-json #("􏿿") "y_string_nonCharacterInUTF-8_U+10FFFF.json")
(test-json #("￿") "y_string_nonCharacterInUTF-8_U+FFFF.json")
(test-json #("\x0;") "y_string_null_escape.json")
(test-json #(",") "y_string_one-byte-utf-8.json")
(test-json #("π") "y_string_pi.json")
(test-json #("𛿿") "y_string_reservedCharacterInUTF-8_U+1BFFF.json")
(test-json #("asd ") "y_string_simple_ascii.json")
(test-json " " "y_string_space.json")
(test-json #("\x1d11e;") "y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF.json")
(test-json #("ࠡ") "y_string_three-byte-utf-8.json")
(test-json #("ģ") "y_string_two-byte-utf-8.json")
(test-json #(" ") "y_string_u+2028_line_sep.json")
(test-json #(" ") "y_string_u+2029_par_sep.json")
(test-json #("aクリス") "y_string_uEscape.json")
(test-json #("new\nline") "y_string_uescaped_newline.json")
(test-json #("\x7f;") "y_string_unescaped_char_delete.json")
(test-json #("ꙭ") "y_string_unicode.json")
(test-json #("\\") "y_string_unicodeEscapedBackslash.json")
(test-json #("\x2342;\x3234;\x2342;") "y_string_unicode_2.json")
(test-json #("\x10fffe;") "y_string_unicode_U+10FFFE_nonchar.json")
(test-json #("\x1fffe;") "y_string_unicode_U+1FFFE_nonchar.json")
(test-json #("​") "y_string_unicode_U+200B_ZERO_WIDTH_SPACE.json")
(test-json #("⁤") "y_string_unicode_U+2064_invisible_plus.json")
(test-json #("\xfdd0;") "y_string_unicode_U+FDD0_nonchar.json")
(test-json #("￾") "y_string_unicode_U+FFFE_nonchar.json")
(test-json #("\"") "y_string_unicode_escaped_double_quote.json")
(test-json #("€𝄞") "y_string_utf8.json")
(test-json #("a\x7f;a") "y_string_with_del_character.json")
(test-json #f "y_structure_lonely_false.json")
(test-json 42 "y_structure_lonely_int.json")
(test-json -.1 "y_structure_lonely_negative_real.json")
(test-json 'null "y_structure_lonely_null.json")
(test-json "asd" "y_structure_lonely_string.json")
(test-json #t "y_structure_lonely_true.json")
(test-json "" "y_structure_string_empty.json")
(test-json #("a") "y_structure_trailing_newline.json")
(test-json #(#t) "y_structure_true_in_array.json")
(test-json #() "y_structure_whitespace_array.json")

;;; ======================================================================
;;; Tests for json-write and json-accumulator on bad input

;; unmatched object-end
(test-json-accumulator-error '(object-end))

;; unmatched array-end
(test-json-accumulator-error '(array-end))
(test-json-accumulator-error '(object-start array-end))

;; numbers must be exact-integers or inexact rationals
(test-json-accumulator-error '(+inf.0))
(test-json-accumulator-error '(+nan.0))
(test-json-accumulator-error '(3/5))
(test-json-accumulator-error '(0+1i))

(test-error json-error? (json-write-into-string +inf.0))
(test-error json-error? (json-write-into-string +nan.0))
(test-error json-error? (json-write-into-string 3/5))
(test-error json-error? (json-write-into-string 0+1i))

;; symbols other than null
(test-json-accumulator-error '(foo))
(test-json-accumulator-error '(nul))

(test-error json-error? (json-write-into-string 'foo))
(test-error json-error? (json-write-into-string 'nul))

;; some objects that are not valid json
(test-json-accumulator-error '(array-start #u8(12 32 12) array-end))
(test-json-accumulator-error (list 'array-start (lambda () 1) 'array-end))

(test-error json-error? (json-write-into-string (vector #u8(12 32 12))))
(test-error json-error? (json-write-into-string (vector (lambda () 1))))

;; json-accumulator requires keys be strings for consistency with json-generator
(test-json-accumulator-error '(object-start foo 12 object-end))
(test-json-accumulator-error '(object-start #t 12 object-end))

;; json-write requires keys are symbols
(test-error json-error? (json-write-into-string '(("invalid" . 12) (valid . 2))))
(test-error json-error? (json-write-into-string '((#t . 42))))

;; not an alist
(test-error json-error? (json-write-into-string '(this is not valid)))

;; improper lists
(test-error json-error? (json-write-into-string '(#t . #f)))
(test-error json-error? (json-write-into-string #(((foo . 2) (bar . "bar") . #f))))

;;; ======================================================================
;;; Test checking of nesting-depth-limit and character-limit

(test-error json-error?
	    (parameterize ((json-number-of-character-limit 1))
	      (json-read-string "true")))

(test-error json-error?
	    (parameterize ((json-number-of-character-limit 1))
	      (json-tokenize-string "true")))

(test-equal #t
	    (parameterize ((json-nesting-depth-limit 1))
	      (json-read-string "true")))

(test-equal '(#t)
	    (parameterize ((json-nesting-depth-limit 1))
	      (json-tokenize-string "true")))

(test-error json-error?
	    (parameterize ((json-nesting-depth-limit 1))
	      (json-read-string "[true]")))

(test-error json-error?
	    (parameterize ((json-nesting-depth-limit 1))
	      (json-tokenize-string "[true]")))

