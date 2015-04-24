#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require (for-syntax "defs_for-syntax.rkt" (planet amkhlv/bystroTeX/slides_for-syntax)))
@(require "defs.rkt" (planet amkhlv/bystroTeX/common) (planet amkhlv/bystroTeX/slides))
@(require (only-in (planet jaymccarthy/sqlite) close))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf   
   (bystro (bystro-connect-to-server #f "127.0.0.1" 29049 "svg")
           "formulas_formulas.sqlite"  ; name for the database
           "formulas" ; directory where to store .png files of formulas
           25  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@(set-bystro-extension! bystro-conf "svg")
@; This controls the single page mode:
@(define singlepage-mode #t)
@; ---------------------------------------------------------------------------------------------------
@(begin ;do not change anything here:
   (define-syntax (syntax-setter x) (defines-syntax-for-formulas x))                
   (syntax-setter defineshiftedformula)
   (defineshiftedformula "formula-enormula-humongula!"))
@; ---------------------------------------------------------------------------------------------------


@(bystro-inject-style "misc.css" "no-margin.css")

@title[#:style '(no-toc no-sidebar)]{Formulas}

@table-of-contents[]
@bystro-ribbon[]

@fsize+[8]

@f{\cal X}


@f{\mbox{Fun}(\widehat{M})}
@bystro-bg[255 255 200]
@f{\mbox{Fun}(\widehat{M})}
@bystro-bg[255 255 255]

@f{{\cal X}_{\rm OS} \otimes}
@f{{\cal X}_{\rm OS} \otimes\;\; \mbox{Fun}(\widehat{M})}

@align[r.l.n @list[
@f{\Big[\;}
@f{{\partial\over\partial\tau^+} + 
J_{0+}^{[mn]}t^0_{[mn]} + J_{3+}^{\alpha}{\bf D}^L_{\alpha} + J_{2+}^m {\bf A}^L_m + J_{1+}^{\hat{\alpha}}{\bf W}^L_{\hat{\alpha}}\;,}
""
]@list[
""
@f{{\partial\over\partial\tau^-} + 
J_{0-}^{[mn]}t^0_{[mn]} + J_{1-}^{\hat{\alpha}}{\bf D}^R_{\hat{\alpha}} + J_{2-}^m {\bf A}^R_m + J_{3-}^{\alpha}{\bf W}^R_{\alpha}\;\Big]\;=0}
@label{TautologicalLaxAdSOurNotations}
]
]

@align[r.l.n @list[
@f{}@f{\{{\bf D}(s_L+s_R)\;,\;{\bf D}(s_L+s_R)\} = }""
]@list[
@f{=\;}@f{(s_L\Gamma^m s_L){\bf A}^L_m + (s_R\Gamma^m s_R){\bf A}^R_m \;+}""
]@list[
@f{}@f{\;+
   R^{LL}_{\alpha\beta}s_L^{\alpha}s_L^{\beta} +
   R^{RR}_{\dot{\alpha}\dot{\beta}}s_R^{\dot{\alpha}}s_R^{\dot{\beta}} +
   R^{LR}_{\alpha\dot{\beta}} s_L^{\alpha} s_R^{\dot{\beta}}}
@label{SpecialD}
]]

@equation{
\begin{array}{l}
\mbox{a point of }\widehat{M} \mbox{ is a pair}\cr
\left(x_0,(E^L_1,\ldots,E^L_{16}, \;E^R_1,\ldots,E^R_{16})\right)\cr
\mbox{where }x_0\in M \mbox{ and } \cr
E^{L}_{\alpha}\in {\cal S}_L(x_0)\;,\;E^{R}_{\hat{\alpha}}\in {\cal S}_R(x_0)
\end{array}
}

@equation{
\begin{array}{l}
\mbox{a fiber of } \widehat{M}\to M \mbox{ over a point } x_0\in M \cr
\mbox{consists of all the tuples } \cr
E^{L}_{\alpha}\in {\cal S}_L(x_0)\;,\;E^{R}_{\hat{\alpha}}\in {\cal S}_R(x_0)\cr
\mbox{belonging to {\bf the same orbit} of }\cr 
\hat{H}_L\times \hat{H}_R\mbox{ acting as follows:}\cr
E^{L}_{\alpha}\mapsto (h_L)^{\alpha'}_{\alpha}E^{L}_{\alpha'} \;,\;
E^{R}_{\hat{\alpha}}\mapsto (h_R)^{\hat{\alpha}'}_{\hat{\alpha}}E^{R}_{\hat{\alpha}'}
\end{array}
}

@bystro-ribbon[]

@; ---------------------------------------------------------------------------------------------------
@(bystro-close-connection bystro-conf)
@close[formula-database]

 
  
