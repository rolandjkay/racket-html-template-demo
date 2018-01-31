#lang racket
(require "html-tags.rkt")

(provide navbar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation bar
;;;;;;;;;;;;;;;;;;;;;

;; The logo and brand name
;;
(define (navbar-brand)
  (a `(@ (class "navbar-brand") (href "#")) 
      (div `(@ (class "nav-logo")) 
           (img `(@ (src "assets/lambda 1.png"))) 
            "Nijo"
           ) 
      ))

;; The hamburger icon
;;
(define (navbar-hamburger)
  (button `(@ (type "button") (role "button") (class "navbar-toggler lines-button x2") (data-toggle "collapse") (data-target "#navbarNav") (aria-controls "navbarNav") (aria-expanded "false") (aria-label "Toggle navigation")) 
    (span `(@ (class "lines")))))
                  
;; An (inactive) link in the nav bar
;;
(define (navbar-inactive-item target label)
  (li `(@ (class "nav-item")) 
      (a `(@ (class "nav-link") (href ,target))
         label)))

;; An (active) link in the nav bar
;;
(define (navbar-active-item target label)
  (li `(@ (class "nav-item active")) 
      (a `(@ (class "nav-link") (href ,target))
         label (span '(@ (class "sr-only")) "(current)"))))

(define (navbar-item target label active?)
  ((if active? navbar-active-item navbar-inactive-item) target label))

(define (navbar items active-item)
  (nav '(@ (class "navbar navbar-expand-md fixed-top navbar-light bg-light"))
    (navbar-brand)
    (navbar-hamburger)
    (div '(@ (class "collapse navbar-collapse justify-content-end") (id "navbarNav"))
      (ul '(@ (class "navbar-nav"))
        (for/list ((item items))
          (navbar-item "#" item (equal? active-item item)))))))
