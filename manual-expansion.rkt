#lang racket

(define (get-opt-prop props name)
  (let [(matches (filter (lambda (kv) (begin (equal? (car kv) name)))
                         props))]
    (if (null? matches)
        null
        (cadar matches))))

(define (my-head props . children)
  (display props)
  (let [(title-text (get-opt-prop props 'title))
        (fonts( get-opt-prop props 'fonts))
        (css (get-opt-prop props 'css))]
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


(define (my-body props . children)
  '(body (@ foo "bar"))) 

;; We need to recursively evalulate expanding non-primative nodes only
;;
;;
; An expansion of a DOM is    (html  (nijo-navbar) (nijo-article))
; (nijo-navbar (nijo-artice)
; There might be meta-data in the article that is needed by the navbar.
; In other words, characteristics of the children can float up to the parents,
; but never in the other direction. So, we must expand the current node
; before the chilrend.
;
; So, the expansion of the node if the exansion of the root, given the current
; children whose children are the expansions of the children.
  

(define (expansion-of components node)
  (if (string? node)      ; Pass text nodes through unchanged
      node                ; 
      (splice-children
       (expand-node components node)
       (for/list [(child (node-children node))]
         (expansion-of components child)))))

(define (expand-node components node)
  (match-let ([(list tag props children) (decompose-node node)])
    (let [(component (hash-ref components tag '()))]
      (if (null? component)
          (if (null? props) `(,tag) `(,tag (@ ,props)))            ;; Return a childless version of the same node.                       
          (expansion-of components (component props children)))))) ;; Call the component to generate the markup.
  

;; Splice the given chilren into the node
;; - 'node' should have no children, but if it does, we will just append
;;    the additional ones.
(define (splice-children node children)
  (append node children))

;; Get the tag of a node
(define (node-tag node)
  (if (> (length node) 1)
      (first node)
      null))

;; Decompose a node into (tag, props, children)
(define (decompose-node node)
  (match node
    [(list tag (list '@ props ...) children ...) `(,tag ,props ,children)] ;; A node with props
    [(list tag children ...) `(,tag () ,children)]))                       ;; A node with no props

(define (node-tags node)
  (first (decompose-node node)))

(define (node-props node)
  (second (decompose-node node)))

(define (node-children node)
  (third (decompose-node node)))
   
(expansion-of
  (hash 'my-head my-head 'my-body my-body)
 '(html (my-head (@ (title "Greater than zero"))) (my-body)))

(display "FFF")
(expansion-of
  (hash 'my-head my-head 'my-body my-body 'foo (λ (props . children) '(my-head)))
 '(html (foo (my-body))))