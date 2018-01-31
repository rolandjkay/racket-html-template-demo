;; Module defining instances of the specific components used by the Noji site.
;; E.g. the navbar with links to the pages on the site.
;;
#lang racket/base
(require "base.rkt")
(require "navbar.rkt")
(require "html-tags.rkt")


(provide noji-page noji-head)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Noji head
;;;;;;;;;;;;;;;;;;;;;

(define (noji-head props . children)
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
           (link (@ (rel "stylesheet") (type "text/css") (href "css/nijo.css"))))
       (*COMMENT* "Bootstrap CSS")
       (link (@ (rel "stylesheet") (href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css") (integrity "sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm") (crossorigin "anonymous")))
       ,(if (eq? title-text null)
            `(*COMMENT* "title attribute not specified")
            `(title ,title-text)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Noji page
;;;;;;;;;;;;;;;;;;;;;

(define (noji-page props . children)
  `(html (@ (lang "en"))
     ,(noji-head props)
     ,@children))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Noji nav bar
;;;;;;;;;;;;;;;;;;;;;

(define (noji-navbar active)
  (navbar '("Home" "Articles" "Bio") active))