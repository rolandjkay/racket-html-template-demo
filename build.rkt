#lang racket/base

(require html-parsing)
(require sxml)
(require "noji-components.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page component
;;;;;;;;;;;;;;;;;;;;;;

;(define (page props . children)
;  `(html (@ (lang "en"))
;         (noji-head ,props)
;         ,@children))
                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation
;;;;;;;;;;;;;;;;;;;;;;

;(define (expand-sxml sxml)
;  (display sxml)
;  (parameterize ([current-namespace (make-base-empty-namespace)])
;    (namespace-require "html-tags.rkt")
;    (namespace-set-variable-value! 'noji-page noji-page)
;    (namespace-require 'racket/base)  ; XXX What do we need from this to make function application work?
;    (eval (eval sxml))))

(let [(landing-page (call-with-input-file "./data/index.html"
                      (λ (in) (html->xexp in)))
                    )]
  ;(srl:sxml->html (pre-post-order landing-page italicizer)))
  (expand-sxml landing-page))


;;;;;
;
(require macro-debugger/expand)
(require (for-syntax racket))

(define foo #'(html (my-head) (my-body)))

(define-syntax (my-head stx)
  `(head
    (meta (@ (charset "utf-8"))
      ,@(for/list [(x '("foo", "bar"))]
          `(lnk (@ (href ,x)))))))


(syntax->datum
 (expand-only foo
              (list '#'my-head #'my-body #'navbar)))
