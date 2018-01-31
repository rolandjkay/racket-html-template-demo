#lang racket
(require html-parsing)
(require sxml)
(require "html-tags.rkt")
(require "navbar.rkt")
(require "base.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Noji head
;;;;;;;;;;;;;;;;;;;;;

(define (noji-head title-text)
  (head
    (meta `(@ (charset "utf-8")))
    (meta `(@ (name "viewport") (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
    (link `(@ (href "https://fonts.googleapis.com/css?family=Muli:regular,light,bold,extrabold|VT323") (rel "stylesheet")))
    (link `(@ (rel "stylesheet") (type "text/css") (href "css/nijo.css")))
    (link `(@ (rel "stylesheet") (type "text/css") (href "css/pony.css")))
    (*COMMENT* "Bootstrap CSS")
    (link `(@ (rel "stylesheet") (href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css") (integrity "sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm") (crossorigin "anonymous"))
    (title title-text))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Noji page
;;;;;;;;;;;;;;;;;;;;;

(define (page title-text . children)
  `(html (@ (lang "en"))
     ,(noji-head title-text)
     ,@children))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Noji nav bar
;;;;;;;;;;;;;;;;;;;;;

(define (noji-navbar active)
  (navbar '("Home" "Articles" "Bio") active))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Article meta data
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (meta-data-title title)
  (h1 title))


(define (do-meta-data . children)
  (let [(title (second (get-child-by-name 'title children)))
        (author (second (get-child-by-name 'author children)))
        (date (second (get-child-by-name 'date children)))]
    (div '(@ (class "entry-header"))
         (div '(@ (class "entry-avatar"))
              (img '(@ (src "assets/headshot.png"))))
         (h1 title)
         (p '(@ (class "entry-meta"))
            (span '(@ (class "entry-author")) author)
            " · "
            (span '(@ (class "entry-date")) date)))))

; A macro which quotes the arguments before passing to do-meta-data
;
(define-syntax meta-data
    (syntax-rules ()
      [(meta-data args ...) (do-meta-data (quote args)...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Article page
;;;;;;;;;;;;;;;;;;;;;

(define (article-page article)
  ; XXX Should be able to make this work.
  ; XXX Need to lazily evaluate the markup. It should stay as data until it is
  ;     transformed to SXML in one pass. That can be done by just passing to
  ;     eval; '@' would likely need to be a function that returns itself.
  ;(cadr (get-child-by-name 'title (get-child-by-name 'meta-data article))) 
  (page "Stop getting lost in a mass of callbacks!" 
    (body '(@ (class "article"))
          (noji-navbar "Articles")
          article)))

(define (article . children)
  (div '(@ (class "container content"))
       children))

(define (async-article)
  (article
   (meta-data
    (title "Stop getting lost in a mass of callbacks!")
    (author "Roland Kay")
    (date "21st January 2018"))
  
   (p "I was recently involved in a project in which we needed to respond to events on a database system.
      The system was event driven, which meant that we weren’t allowed to make blocking calls; every
      function had to return immediately and call a callback when it had completed.")

   (p "To avoid blocking, we kept having to write code like this:")

   (image '(@ (src "assets/articles/async/figure-1.png") (caption "Figure 1: Non-blocking code with callbacks")))

   (p "Wow! That looks complicated. What is going on here? Well, all this code does is…")

   (ul
    (li "Get the user’s profile so we know what region they are in")
    (li "Get the pricing parameters for the user’s region")
    (li "Calculate the price of a phone call")
    (li "Write the price to the database"))

   (p "That’s perfectly straightforward. Why can’t we just write that,
     without all these nested function calls?")

   (p "There’s another problem as well. What should happen if, for some
      reason, we can’t load the pricing parameters? We should probably
      stop and return a failure code from the write_price() function.
      How on earth do we do that? write_price() has already returned 
      long before fetch_pricing_parameters() is even called!")

    (p "We were left wishing that somebody would invent a language in
        which we could just write this:")

    (image '(@ (src "assets/articles/async/figure-2.png")
           (caption "Figure 2: Wouldn’t it be nice to be able to write this?")))

    (p "That’s so much nicer. The sequence of operations is clear and error
       handling is easy. We’re not allowed to write like this, because the
       database is asynchronous. But, I don’t care about that. That’s just an
       implementation detail of the database. I want our code to be clear and
       simple, so that we can get it right the first time and my guys won’t be
       struggling to understand it and fix obscure bugs for the rest of the
       project.")

    (separator)

    (h3 "Functional programming to the rescue")

    (p "Of course, we weren’t the first to meet this problem. Computer scientists
       have been looking at this for years and have come up with something called
       the " (em "Monad") " to help us. There are lots of articles on the web
       which explain what monads are. The trouble is that it is quite an abstract
       concept and so I wasn’t at all clear how it could help us. The good news is
       that we didn’t have to write our own Monad, we found one that somebody else
       had developed and used that.")

    (h3 "Monads")
    
    (p "If we were going to solve our problem from scratch, we would probably try
       to implement some kind of task queue. Then, we could push tasks onto the
       queue to 1) get the user profile and 2) the pricing parameters and 3) write
       the call cost to the database and leave the system to execute the queue for
       us.")

    (p "It turns out that monads are a clever way of encapsulating the logic for
       scheduling the execution of our sequence of tasks and keeping this logic
       separate from the logic of the problem that the tasks are being used to
       solve. They’re a bit like our imaginary task queue but much more powerful
       and, crucially, we don’t have to worry about implementing and maintaining
       the infrastructure to make it work.")

    (h3 "The Promise monad")

    (p "To solve our problem, we used Javascript’s Promise monad. This allowed us
       to write the code like this:")

    (image '(@ (src "assets/articles/async/figure-3.png")
           (caption "Figure 3: Using the Promise monad")))

    (p "This is not quite as simple as the code in Fig. 2, but it’s close. The code
       now reflects the simple sequential structure of the operations and error
       handling is simple once more. We can chain as many operations as we like just
       by adding “then” blocks and “catch” blocks to handle any errors.")

    (p "In fact, what we are doing here is building a Promise object that will live
       in the event loop of our application. Each stage will be executed as and when
       the previous stage completes. Any code that we need to handle completion or
       errors can simply be attached by chaining a “then” or a “catch” block.")

    (h3 "What does this have to do with functional programming?")

    (p "Figure 3 may look like Javascript, but it isn’t really. It’s really a
       functional language embedded in Javascript. Remember that in a purely
       functional language, all variables are immutable — there is no mutating “state”
       — and each function is a real mathematical function that returns a result based
       only on its inputs and not on anything else.")

    (p "Notice that each “then” block is really a function that maps an input to
       an output.")

    (image '(@ (src "assets/articles/async/figure-4.png")
           (caption "Figure 4: f maps a username ‘u’ to a profile ‘p’; g maps ‘p’
                     to pricing parameters ‘r’; h maps ‘r’ to a price ‘e’.")))

    (p "Now, these aren’t, strictly speaking, mathematical functions because, if we
       change the user’s profile or the pricing parameters in the database, then
       their outputs will change. But, it’s close enough for us.")

    (p "Because this is Javascript, rather than a real functional language, such as
       Haskell, we could declare some global, mutable objects and change them in our
       “then” blocks. But, that would be going against the spirit of the monad. It
       would be a really bad idea to do this because the timing of the execution of
       each block is uncertain, and so it would be very difficult to predict the state
       of these global objects at any point in time and understand how the different
       parts of the application would interact.")

    (p "One final point to note is that the monad has defined a nicer way to compose 
       functions for us. Rather than having to write the result of applying ‘h’ to the
       result of ‘g’ to the result of ‘f’ as")

    (image '(@ (src "assets/articles/async/figure-5.png")
               (caption "Figure 5: Nesting, from a mathematical perspective")))

    (p "as we did in Fig. 1., the monad gives us a “function composition operator”,
       called “.then”, that allows us to define the promise as")

    (image '(@ (src "assets/articles/async/figure-6.png")
               (caption "Figure 6: A Promise as function composition")))

    (p "So now, we can build complicated functions from simple ones by simply chaining
       them together, without all of the nesting that was so confusing.")

    (separator)

    (p "Functional programming gave us an simple and elegant solution to our problem of
       complicated and hard to maintain asynchronous code. Once we’d discovered monads,
       we found others that could help us. For example, RxJava has the Observable monad
       that helped us manage asynchronicity in our Android app; Android UI is another
       event-loop driven system.")

    (p "There are many great articles available on monads and functional programming.
       I’ve listed a selection in the bibliography below. They can often seem a bit
       technical and abstract, and so it took us a little time to realize that this was
       the solution to our problem. Hopefully, now that we’ve shown you a practical
       example, you’ll get to the solution much sooner and be free of callback hell!")
        
    ;; If you enjoyed this story, please click the clap button and share to help others 
    ;; find it! Feel free to leave a comment below.

    (h2 "Bibliography")

    (p
     (a '(@ (href "https://dzone.com/articles/functor-and-monad-examples-in-plain-java"))
     "Functional Programming in Pure Java: Functor and Monad Examples")
     " — A fantastic and detailed article that goes into lots of practical uses of
     monads.")

    (p
     (a '(@ (href "https://www.youtube.com/watch?v=ZhuHCtR3xq8"))
        "Brian Beckman: Don’t fear the Monad")
     " — A great video which explains the theoretical basis for monads.")

    (p
     (a '(@ (href "https://en.wikipedia.org/wiki/Function_composition"))
        "Function composition")
     " — Wikipedia article on function composition.")

    (p
     (a '(@ (href "http://learnyouahaskell.com/a-fistful-of-monads"))
        "A Fistful of Monads")
     " — A discussion of various monads that come with Haskell. If
     you’re not familiar with Haskell, and you have the time, it’s
     well worth reading the whole book.")

    (p
     (a '(@ (href "https://mttkay.github.io/blog/2014/01/25/your-app-as-a-function/"))
        "Monads: Your App as a Function, Part 1; ")
     (a '(@ (href "https://mttkay.github.io/blog/2014/01/25/monads-your-app-as-a-function-part-2/"))
        "Monads: Your App as a Function, Part 2")
     " — Another detailed practical discussion of monads in Java. ")
    
   ))
   

(call-with-output-file "foobar.html" #:exists 'replace
  (lambda (out)
    (fprintf out
     (srl:sxml->xml
      (article-page (async-article))))))
  

