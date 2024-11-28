#lang racket

(provide (all-defined-out))

(define depth (box 0))
(define max-depth (box 0))

(define-syntax tramp-define
  (syntax-rules ()
    [(_ (f x* ...) e)
     (define (f x* ...)
       (dynamic-wind 
         (λ ()
           (begin (set-box! depth (add1  (unbox depth)))
                  (when (> (unbox depth) (unbox max-depth))
                    (set-box! max-depth (unbox depth)))))
         (λ () e)
         (λ () (set-box! depth (sub1 (unbox depth))))))]))

;; -> Void
;; Effect: reports the maximum depth
(define (report-max-depth)
  (printf "max-depth: ~a\n" (unbox max-depth)))

;; ( -> X) -> X
;; reset trampoline depth counters and run (th)
;; Effect: reports maximum recurison depth among tramp-define'd functions
(define (with-tramp th)
  (begin
    (set-box! depth 0)
    (set-box! max-depth (unbox depth))
    (let ([v (th)])
      (report-max-depth)
      v)))

