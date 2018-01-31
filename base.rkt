#lang racket
(require "html-tags.rkt")

(provide image separator get-attrib get-opt-attrib get-child-by-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lang
;;;;;;;;;;;;;;;;;;;;;

(define (get-attrib attribs name)
  (cadar (filter (lambda (kv) (begin (equal? (car kv) name))) (cdr attribs))))

(define (get-opt-attrib attribs name)
  (let [(matches (filter (lambda (kv) (begin (equal? (car kv) name)))
                         (cdr attribs)))]
    (if (null? matches)
        null
        (cadar matches))))

(define (get-child-by-name name children)
  (car (filter (lambda (x) (eq? (car x) name)) children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Image
;;;;;;;;;;;;;;;;;;;;;

(define (image attribs)
  (figure
    (img `(@ (width "100%") (src ,(get-attrib attribs 'src))))
    (figcaption
     (get-attrib attribs 'caption))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Separator
;;;;;;;;;;;;;;;;;;;;;

(define (separator) (hr))
