#lang racket
(require plai/test-harness)
(require rackunit)
(provide (struct-out exn:311)
         error/311
         check-exn/311)

;;
;; exn-311.rkt - Mechanisms for signaling and unit testing student-signaled
;;  exceptions
;;


;; check that an expression signals a student-
;; Adapted from plai/test-harness

;; We only catch exceptions of this type.  error/311 throws such exceptions.
(define-struct (exn:311 exn:fail:user) () #:transparent)

;; directly analogous to error, but raises exn:311 instead of exn:fail
(define (error/311 . args)
  (with-handlers
      [(exn:fail? (λ (exn) 
                    (raise
                     (make-exn:311 (exn-message exn)
                                   (exn-continuation-marks exn)))))]
    (apply error args)))

;; Helper for check-exn/311
(define ((make-exn-pred exn-substring) e)
  (and (exn:311? e)
       (string-contains? (exn-message e) exn-substring)))

;; analogous to PLAI's test/exn: only succeeds if error/311 was used to signal
;; the exception, and the substring matches.
(define-syntax check-exn/311
  (λ (stx)
    (syntax-case stx ()
      [(_ expr exn-substring)
       (syntax/loc stx
         (check-exn/311 expr exn-substring #f))]
      [(_ expr exn-substring message)
       ;; syntax/loc ensures that rackunit points to the use site of this macro,
       ;; not the call to check-exn in its implementation:
       ;; from https://stackoverflow.com/a/32235222/9100       
       (syntax/loc stx
         (check-exn (make-exn-pred exn-substring)
                    (λ () expr)
                    message))])))
       
;; Passing example:
;; (check-exn/311 (error/311 "doh!") "" "this is fine")

;; Failing examples:
;; (check-exn/311 (car '()) "" "this is NOT fine")
;; (check-exn/311 (error/311 'my-function "doh!") "yay!")