(module defs racket

  (require (for-syntax racket/base bystroTeX/slides_for-syntax racket/syntax))
  (require racket scribble/core scribble/base scribble/html-properties)
  (require bystroTeX/common bystroTeX/slides)

  ;; Here the basic syntax can be adjusted:
  (provide bystro-def-formula)
  (define-syntax (bystro-def-formula stx)
    (bystro-formula-syntax 
     #:autoalign-formula-prefix "f"
     #:manual-formula-prefix    "f"
     #:display-math-prefix      "equation"
     #:size-change-notation     "fsize"
     #:size-increase-notation   "fsize+"
     #:size-restore-notation    "fsize="
     #:max-size-increase        8
     #:max-size-decrease        5
     #:max-vert-adjust          8
     stx))

  ;; Here we define new functions:
  (provide label)
  (define (label s) (elemtag s (number-for-formula s)))  
  (provide ref)
  (define (ref s) (elemref s (ref-formula s)))
  (provide red)
  (define (red . x) (apply clr (cons "red" x)))
  (provide green)
  (define (green . x) (apply clr (cons "green" x)))
  (provide greenbox-style)
  (define (greenbox-style more) 
    (bystro-elemstyle (string-append "border-style:solid;border-color:#00aa00;" more)))
  (provide redbox-style)
  (define (redbox-style more)   
    (bystro-elemstyle (string-append "border-style:solid;border-color:#aa0000;" more)))
  (provide greenbox)
  (define (greenbox more . x) (elem #:style (greenbox-style more) x))
  (provide redbox)
  (define (redbox   more . x) (elem #:style (redbox-style more) x))
  (provide greenbox-wide)
  (define (greenbox-wide more . x) (nested #:style (greenbox-style more) x))
  (provide redbox-wide)
  (define (redbox-wide   more . x) (nested #:style (redbox-style   more) x))
  (provide hrule)
  (define (hrule) (element (make-style #f (list (alt-tag "hr"))) ""))
  (provide leftbar)
  (define (leftbar . x) 
    (para 
     #:style (bystro-elemstyle 
              "border-left-style:solid;border-color:green;padding-left:12px;") 
     x))
  (require racket/dict)
  (init-counter exercise)
  (init-counter theorem)
  (init-counter definition)
  (provide ex-num ex-ref th-num th-ref defn-num defn-ref)
  (define (ex-num label)
    (elemtag label (exercise-next label)))
  (define (ex-ref label)
    (elemref label (list "Exercise "  (exercise-number label))))
  (define (th-num label)
    (elemtag label (theorem-next label)))
  (define (th-ref label)
    (elemref label (list "Theorem "  (theorem-number label))))
  (define (defn-num label)
    (elemtag label (definition-next label)))
  (define (defn-ref label)
    (elemref label (list "Definition "  (definition-number label))))
  (provide comment)
  (define-syntax (comment stx)
    (syntax-case stx ()
      [(_ x ...)
       (with-syntax ([bc (format-id stx "bystro-conf")])
         #'(let ([a (bystro-bg 240 240 255)]
                 [s1 (set-bystro-formula-size! bc (- (bystro-formula-size bc) 3))]
                 [b (nested 
                     #:style (style "comment" 
                               (list (make-attributes '((style . "background-color:rgb(240,240,255);")))))
                     x ...)]
                 [s2 (set-bystro-formula-size! bc (+ (bystro-formula-size bc) 3))]
                 [c (bystro-bg 255 255 255)])
             b))]))
  (provide short-intro)
  (define-syntax (short-intro stx)
    (syntax-case stx ()
      [(_ x ...)
       #'(let ([a (bystro-bg 240 255 240)]
               [b (nested 
                   #:style (style "greenbox" 
                             (list (make-attributes '((style . "background-color:rgb(240,255,240);")))))
                   x ...)]
               [c (bystro-bg 255 255 255)])
           b)]))
  (provide literature)
  (define literature
    (hash 
     "Schwarz:1992nx"    (hyperlink "http://arxiv.org/abs/hep-th/9205088" "GeomBV")
     "Schwarz:1992gs"    (hyperlink "http://arxiv.org/abs/hep-th/9210115" "SemiClassical")
     "Khudaverdian:1999" (hyperlink "http://arxiv.org/abs/math/9909117" "Khudaverdian")
     "Berkovits:2014ama" (hyperlink "http://arxiv.org/abs/1403.2429" "BerkovitsChandia")
     "Berkovits:2010zz"  (hyperlink "http://arxiv.org/abs/1004.5140" "BerkovitsMazzucato")
     "Witten:2012bh"     (hyperlink "http://arxiv.org/abs/1209.5461" "PerturbativeSuperstringTheoryRevisited")
     "Sen:2014pia"       (hyperlink "http://arxiv.org/abs/1408.0571" "OffShellAmplitudes")
     ))
  (provide cite)
  (define (cite x)
    (let [(xs (string-split x ","))
          (h literature)
          ]
      (make-element 
       (style "citation" '()) 
       (add-between (map (λ (u) (hash-ref h u)) xs) ","))))
              

  )
