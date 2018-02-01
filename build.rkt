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

(require macro-debugger/expand)
(require (for-syntax racket))

(define foo #'(html (my-head (@ (title "Greater than zero"))) (my-body)))

;(define-syntax (my-head stx)
;  `(head
;    (meta (@ (charset "utf-8"))
;      ,@(for/list [(x '("foo", "bar"))]
;          `(lnk (@ (href ,x)))))))

;(define-for-syntax (get-opt-attrib props name)
;  "foobar")

(define-for-syntax (get-opt-attrib attribs name)
  (display attribs)
  (let [(matches (filter (lambda (kv) (begin (equal? (car kv) name)))
                         (cdr attribs)))]
    (if (null? matches)
        null
        (cadar matches))))

(define-for-syntax (my-head* props . children)
  (display children)
  (let [(title-text (get-opt-attrib props "title"))
        (fonts( get-opt-attrib props "fonts"))
        (css (get-opt-attrib props "css"))]
    `(head
       (meta (@ (charset "utf-8")))
       (meta (@ (name "viewport") (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
       ,(if (eq? fonts null)
             '(*COMMENT* "fonts attribute not specified")
             '(link (@ (href (string-append "https://fonts.googleapis.com/css?family=" fonts)) (rel "stylesheet"))))
       ,@(for/list [(c css)]
           '(lnk (@ (rel "stylesheet") (type "text/css") (href "css/nijo.css"))))
       (*COMMENT* "Bootstrap CSS")
       (link (@ (rel "stylesheet") (href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css") (integrity "sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm") (crossorigin "anonymous")))
       ,(if (eq? title-text null)
            `(*COMMENT* "title attribute not specified")
            `(title ,title-text)))))

(define-syntax (my-body stx)
  #'(body (@ foo "bar"))) 

(define-syntax (my-head stx)
  (syntax-case stx ()
    [(_ (@ props ...) child ...)
     (with-syntax ([replacement (datum->syntax stx
                                               (my-head*
                                                 (syntax->datum #'(props ...))
                                                 (syntax->datum #'(child ...)) ))])
       #'replacement)]))

   ;  (my-head* ('@ props ...) (my-body) child ... ) ]))



(syntax->datum
 (expand-only foo
              (list #'my-head #'my-body #'navbar)))





;(define (expand-sxml sxml)
;  (display sxml)
;  (parameterize ([current-namespace (make-base-empty-namespace)])
;    (namespace-require "html-tags.rkt")
;    (namespace-set-variable-value! 'noji-page noji-page)
;    (namespace-require 'racket/base)  ; XXX What do we need from this to make function application work?
;    (eval (eval sxml))))

;(let [(landing-page (call-with-input-file "./data/index.html"
;                      (λ (in) (html->xexp in)))
;                    )]
  ;(srl:sxml->html (pre-post-order landing-page italicizer)))
;  (expand-sxml landing-page))
