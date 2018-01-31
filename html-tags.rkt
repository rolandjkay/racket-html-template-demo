#lang racket

(provide *TOP* *DECL* @ html head link meta a div img button span li ul nav title *COMMENT* body h1 p figure figcaption hr h1 h2 h3 h4 em)
(provide doctype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML keywords
;;;;;;;;;;;;;;;;;;;;;

(define doctype 'doctype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML attributes
;;;;;;;;;;;;;;;;;;;;;

;; Make an sttributes list resolve to itself, without evaluating
;; its arguments
(define-syntax @
    (syntax-rules ()
      [(@ args ...) `(@ ,(quote args)...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML tags
;;;;;;;;;;;;;;;;;;;;;

;; In the functions below, children might be a list of elements or
;; a list of a list of elements. In the latter case, we flatten
;; by one level.
(define (get-children children)
  (cond
    [(eq? children '())            '()]       ; No arguments
    [(equal? children '(()))       '()]       ; One arguments, which is the empty list
    [(not (pair? (car children)))  children]  ; Doesn't have a CAAR
    [(symbol? (caar children))     children]
    [#t                            (car children)])) 


;; If we use for/lisp etc. to generate children for an element then they will end up 
;; in their own list; e.g.
;;
;;   (ul ((li "A") (li "B") (li "C")))
;;
;; This needs to be unpacked to
;;
;;   (ul (li "A") (li "B") (li "C"))
;;
;; We can tell because there will be an extra level of nexting.
;;
;; This function will unpack
;;
;;  (((li "A") (li "B") (li "C"))) to ((li "A") (li "B") (li "C"))
;; but leave  ((li "A") (li "B") (li "C")) unchanged.
(define (unpack-params params)
  (if (and (list? params) (= (length params) 1) (list? (car params)) (= (length (car params)) 1) (list? (caar params))) (first params) params))

(define (has-attribs? params)
  (cond
    [(equal? params '())         #f]
    [(equal? params '(()))       #f]
    [(not (pair? params))        #f]  ; Doesn't have CAR (e.g. "foo")
    [(not (pair? (car params)))  #f]  ; Doesn't have a CAAR (e.g ("foo")
    [#t                          (equal? '@ (caar params))]))
    

(define (splice-params params)
  (let [(params_ (unpack-params params))]
    (cond
      [(has-attribs? params_) (cons (first params_) (unpack-params (rest params_)))]
      [#t                     params_])))

(define (html . params)
  `(html ,@(splice-params params)));

(define (head . children)
  `(head ,@(splice-params children)));

(define (link . children)
  `(link ,@(splice-params children)));

(define (meta . children)
  `(meta ,@(splice-params children)));

(define (a . children)
  `(a ,@(splice-params children)));

(define (div . children)
  `(div ,@(splice-params children)));

(define (img . children)
  `(img ,@(splice-params children)));

(define (button . children)
  `(button ,@(splice-params children)));

(define (span . children)
  `(span ,@(splice-params children)));

(define (li . children)
  `(li ,@(splice-params children)))

(define (ul . children)
  `(ul ,@(splice-params children)))

(define (nav . children)
  `(nav ,@(splice-params children)))

(define (title . children)
  `(title ,@(splice-params children)))

(define (*COMMENT* . children)
  `(*COMMENT* ,@(splice-params children)))

(define (*TOP* . children)
  `(*TOP* ,@(splice-params children)))

(define (*DECL* . children)
  `(*DECL* ,@(splice-params children)))

(define (body . children)
  `(body ,@(splice-params children)))

(define (h1 . children)
  `(h1 ,@(splice-params children)))

(define (h2 . children)
  `(h2 ,@(splice-params children)))

(define (h3 . children)
  `(h3 ,@(splice-params children)))

(define (h4 . children)
  `(h4 ,@(splice-params children)))

(define (p . children)
  `(p ,@(splice-params children)))

(define (figure . children)
  `(figure ,@(splice-params children)))

(define (figcaption . children)
  `(figcaption ,@(splice-params children)))

(define (hr . children)
  `(hr ,@(splice-params children)))

(define (em . children)
  `(em ,@(splice-params children)))