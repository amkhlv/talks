#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require (for-syntax (planet amkhlv/bystroTeX/slides_for-syntax)))
@(require "defs.rkt" (planet amkhlv/bystroTeX/common) (planet amkhlv/bystroTeX/slides))
@(require (only-in db/base disconnect))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf   
   (bystro (bystro-connect-to-server #f "127.0.0.1" 29049 "svg")
           "talk_Perimeter_formulas.sqlite"  ; name for the database
           "talk_Perimeter" ; directory where to store .png files of formulas
           25  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@(set-bystro-extension! bystro-conf "svg")
@; This controls the single page mode:
@(define singlepage-mode #f)
@; ---------------------------------------------------------------------------------------------------


@(bystro-def-formula "formula-enormula-humongula!")

@; ---------------------------------------------------------------------------------------------------


@(bystro-inject-style "misc.css" "no-margin.css")

@use-LaTeX-preamble{\newcommand{\gbl}{[\![\;}\newcommand{\gbr}{\;]\!]}}

@(define-syntax highlight
   (syntax-rules () 
     [(highlight a) (elem (bystro-bg 255 255 200) a (bystro-bg 255 255 255))]))
@(define-syntax highlight-green
   (syntax-rules () 
     [(highlight a) (elem (bystro-bg 200 255 200) a (bystro-bg 255 255 255))]))
@(define-syntax highlight-pink
   (syntax-rules () 
     [(highlight a) (elem (bystro-bg 255 220 220) a (bystro-bg 255 255 255))]))
@(define-syntax blue-ink
   (syntax-rules () 
     [(highlight a) (elem (bystro-fg 0 0 220) a (bystro-fg 0 0 0))]))

@title[#:style '(no-toc no-sidebar)]{Type IIB string sigma-model}

@(table 
  (make-style #f `(,(table-cells `( ,(build-list 3 (lambda (n) (make-style #f '(top))))))))
  `((,(table-of-contents)
     ,(para (hspace 3))
     ,(tabular 
       `(("Andrei Mikhailov")
         ("IFT UNESP, São Paulo")
         (,(image "pngs/from-the-window.png"))
         (,(hrule))
         ("on leave from ITEP, Moscow")
         )))))
@linebreak[]
@hyperlink["https://github.com/amkhlv/talks"]{source code}

@slide["Plan of the talk" #:tag "Plan" #:showtitle #t]{
@itemlist[
@item{In the first part of I will talk about the role of the @f{b}-ghost in the classical SUGRA}
@item{In the second part I will discuss some new point of view on the relation between 
integrated and unintegrated vertices, which makes some use of the SYM quadratic algebra}
]
}

@slide["Sigma-models" #:tag "Introduction" #:showtitle #t]{
We will start with a general question.

Consider the sigma-model:
@equation{
S = \int \sqrt{h} h^{\alpha\beta} A_{MN}(Z)\partial_{\alpha}Z^M \partial_{\beta}Z^N + \int \sqrt{h} R\Phi
}
(notice that it does not have @f{bc} ghosts! Just a sigma-model)

@spn[attn]{Question:} which properties should such a sigma-model have to define a string theory 
background?
@itemlist[
@item{nilpotent symmetry @f{Q}}
@item{a nilpotent @bystro-fg[255 0 0]@f{b}@spn[attn]{-ghost}@bystro-fg[0 0 0] such that @f{\{Q,b\}=T}}
]
@hrule[]
}
@after-pause{
It seems that the second property is automatically satisfied locally on the target space.
Locally we can always introduce the coordinates such that @f{Q={\partial\over\partial\theta^1}} and put
@equation[#:label "WrongBGhost"]{
b_{++} = \theta^1 T_{++}
}
It seems that it is even nilpotent when the central charge is zero. However, Eq. (@ref{WrongBGhost}) is a
@spn[attn]{wrong answer}. 
@hrule[]
}
@after-pause{
(And the list of requirements is probably incomplete)

Because @f{\theta^1} is @bold{not globally well-defined}.

It seems that being a well-defined string theory is a @bold{global} property of the target space, 
there is no such thing as a locally well-defined string theory. 

It is essential to have submanifold where @f{Q} has @bold{zeroes}, and the structure near this
submanifold should be somehow a part of the axioms. 

In the pure spinor formalism, part of the target space is considered ``ghosts'' and treated separately
from the matter part. 
}

@slide["Pure spinor action" #:tag "PureSpinors" #:showtitle #t]{
@image["graphics/ps-action.svg"]
@linebreak[]
Notice the singularity at @f{\lambda_L=0} or @f{\lambda_R=0}
@hrule[]
}

@slide["Holomorphicity" #:tag "Holomorphicity" #:showtitle #t]{
I also want @f{Q=Q_L + Q_R} where @f{Q_L} and @f{Q_R} are separately
nilpotent symmetries, and moreover @f{Q_L} is holomorphic and @f{Q_R}
antiholomorphic.

And I want the action of algebraic tori: @f{{\bf C}_{\times}^L} and @f{{\bf C}_{\times}^R}
(holomorphic and antiholomorphic). I want their orbits to go through the @seclink["SingularLocus"]{singular locus},
by scaling the bosonic ghosts to zero. The idea is that the bosonic ghosts are ``infinitesimal''. 
Everything infinitesimal can be scaled. (But they take values in the cone, not in a linear space.)

The BRST operators should be charged under @f{{\bf C}_{\times}^L} and @f{{\bf C}_{\times}^R}:
@tbl[@list[
 @list[
  "" @f{{\bf C}_{\times}^L} @f{{\bf C}_{\times}^R}
]@list[
  @f{Q_L} @f{+1} @f{0}
]@list[
  @f{Q_R} @f{0} @f{-1}
]
]]

}

@slide["Singular locus" #:tag "SingularLocus" #:showtitle  #t]{
Near @f{\lambda_L=0} we want to have the BRST transformation of the form:
@align[r.l @list[
@f{Q_L\theta_L =\;} @f{\lambda_L}
]@list[
@f{Q_L\lambda_L =\;} @f{0}
]@list[
@f{Q_L(\!\mbox{\tt\small other fields}) =\;} @f{o(\lambda_L)}
]
]
This means that we should have  @bold{action of the first order}, and remember
I also want @f{Q_L} holomorphic:
@equation{
\int (w^L_+\partial_-\lambda_L + p^L_+\partial_-\theta_L) + \ldots
}
@div[comment]{
If we had action like @f{\partial_+\theta\partial_-\theta + \partial_+\lambda\partial_-\lambda} then
we would have something like @f{Q\theta = \lambda,\quad Q\lambda= -\theta} which is not what we want!
It is essential to have the action of the first order. In practice, the leading term comes out like
@f{\varepsilon^2\partial_+x\partial_-x + \varepsilon \partial_+\theta\partial_-\theta}; to make
it of the order @f{\varepsilon^2} we rewrite @f{\varepsilon^2 p_+\partial_-\theta_L + \varepsilon^2 p_-\partial_+\theta_R + \varepsilon^3p_+p_-}
}
The other fields are of the higher order:
@align[r.l @list[
@f{Q_Lx^m =\;} @f{{\bf q}^m(\theta_L,\lambda_L)}
]]
with some @bold{quadratic form} @f{{\bf q}^m}. For @f{Q_L^2=0} we need:
@equation{{\bf q}^m(\lambda_L,\lambda_L) = 0}
In the pure spinor formalism we put @f{{\bf q}^m=\Gamma^m} (the Gamma-matrices)
}

@slide["Perturbation theory" #:tag "PerturbationTheory" #:showtitle #t]{
So, in the vicinity of any point, the theory should look like:

@image["graphics/perturbation-theory.svg"]

For this to work, we need 
@align[l.n @list[
@f{QU \simeq dW} @label{QUOnShell}
]@list[
@f{QW = dV} ""
]]
where @f{\simeq} means ``equal on-shell''. Now:
@fsize+[5]
@bystro-bg[255 255 200]
@equation{
V\in H^2(Q)
}
@bystro-bg[255 255 255]
@fsize=[]
This @f{V} called @spn[attn]{unintegrated vertex operator}
}
@remove-slide[]

@after-pause{
This program faces @spn[attn]{some subtleties}

@table[
@(make-style #f `(,(table-cells `( ,(build-list 3 (lambda (n) (make-style #f '(top))))))))
@list[@list[@nested[@image["graphics/locally-flat.svg"]]
@nested[@hspace[2]]
@nested{
We approximate the SUGRA fields by expanding them as polynomials in @f{\delta x = x - x^{(0)}}.
The @bold{order} of this perturbation theory is measured by the @bold{degree of the polynomial}.

This is slightly @bold{unusual}, because usually physicists consider exponential solutions
@f{e^{ikx}}, rather than polynomial. 

Polynomial sector has some funny features.
}
]]]

}

@slide["Linear dilaton" #:tag "LinearDilaton" #:showtitle #t]{
First of all, the ``most leading'' deformation actually has @f{U=0}, @italic{i.e.}
@f{S[\epsilon]=S_{\rm flat}}. 
The @bold{action is not deformed}. 

What does get deformed is the @bold{BRST transformation}. The deformation of the BRST charge is:

@align[r.c.c.l.n @list[
@f{\delta q = } 
@f{\varepsilon A^m\Gamma_m^{\alpha\beta}\Big(
   (\theta_R\Gamma_m\lambda_R)
   \Gamma^m_{\alpha\gamma}\theta_R^{\gamma} 
\; S^R_{\beta}}
@f{-}
@f{
   (\theta_L\Gamma_m\lambda_L)
   \Gamma^m_{\alpha\gamma}\theta_L^{\gamma}
\; S^L_{\beta} \Big)}
""
]]
where  @f{S^L} and @f{S^R} are the left and right SUSY transformations.
The corresponding unintegrated vertex is:
@equation{
V = 
((\lambda_L\Gamma^m\theta_L)\Gamma_m\theta_L)^{\alpha} \;
\Gamma^m_{\alpha\beta} \;
((\lambda_L\Gamma^m\theta_L)\Gamma_m\theta_L)^{\beta} - (L\leftrightarrow R)
}
@div[comment]{
What happens is @f{dV = QW} where @f{W} is a conserved current. 
Flat space superstring has a @spn[attn]{ghost number 1 conserved charge} anticommuting with @f{Q}
(and different from @f{Q}).
We are deforming the BRST transformation @bold{without deforming the action}.
}
@hrule[]
}

@after-pause{
Notice that @f{(\theta_L\Gamma_m\lambda_L)\Gamma^m\theta_L} is the ghost number one vertex
corresponding to left SUSY transformation. Generally speaking:
@itemlist[
@item{ghost number one vertices correspond to global symmetries}
]
For example, @f{(\lambda_L\Gamma_m\theta_L) - (\lambda_R\Gamma_m\theta_R)} corresponds to
the Poincare translation.
}

@remove-slide[]

@after-pause{
@hrule[]
But what happens if we deform @f{Q} like this:
@align[r.c.c.l.l.n @list[
@f{\delta Q = } 
@f{\varepsilon A^m\Gamma_m^{\alpha\beta}\Big(
   (\theta_R\Gamma_m\epsilon\lambda_R)
   \Gamma^m_{\alpha\gamma}\theta_R^{\gamma} 
\; S^R_{\beta}}
@elem{@bystro-fg[255 0 0]@f{\Large +}@bystro-fg[0 0 0]}
@f{
   (\theta_L\Gamma_m\epsilon\lambda_L)
   \Gamma^m_{\alpha\gamma}\theta_L^{\gamma}
\; S^L_{\beta} \Big)}
"?"
@label{AsymmetricDilaton}
]]
(different sign) This actually satisfies all the axioms we introduced until now. But this deformation
does not correspond to anything in SUGRA. It is @spn[attn]{unphysical}.

}

@slide["Ghost number three" #:tag "GhostNumberThree" #:showtitle #t]{
Even more alarmingly, there is some nontrivial @bold{cohomology at the ghost number three}.

The cohomology at the ghost number three is isomorphic (up to some zero mode subtleties) to the
cohomology at the ghost number two (@italic{i.e.} physical states). So, for each physical state
@f{\phi} there is (besides the corresponding  ghost number 2 vertex @f{V_2[\phi]}) also some
ghost number 3 vertex @f{V_3[\phi]}.

It can be obtained by multiplying the ghost number one vertex with the ghost number
two vertex:
@equation{
V_1[T\,]V_2[\phi] = V_3[T.\phi] \mbox{ \tt\small mod } Q(smth)
}

Cohomology at the ghost number 3 is alarming because it could potentially @spn[attn]{obstruct}
our linearized solutions. 

}

@slide["Potential obstacle" #:tag "PotentialObstacle" #:showtitle #t]{
Suppose @f{\phi} is a solution of the linearized SUGRA and @f{V_2[\phi]} the corresponding
ghost number 2 vertex. We have:
@equation{
Q_{\rm flat\;space}V_2[\phi]=0
}
Now let us deform the worldsheet action with the integrated vertex corresponding to another
linearized solution @f{\tilde{\phi}}. This will deform also the BRST operator:
@equation{
Q_{\rm flat\;space}\mapsto Q = Q_{\rm flat\;space} + \Delta_{\tilde{\phi}}Q
}
With the new BRST operator:
@align[r.c.l @list[
"" ""
@f{(Q_{\rm flat\;space} + \Delta_{\tilde{\phi}}Q)\;V_2[\phi]\neq 0}
]@list[
@v+[5 "because"] @hspace[1]
@f{(\Delta_{\tilde{\phi}}Q)\;V_2[\phi]\neq 0}
]
]
However it is true that @f{Q((\Delta_{\tilde{\phi}}Q)\;V_2[\phi]) = 0}. The question
is whether @f{(\Delta_{\tilde{\phi}}Q)\;V_2[\phi]} is BRST exact or not?

This was a bit sloppy, the actual question is, if we deform the worldsheet action
@equation{
S \mapsto S + \varepsilon\int U[\phi] + \tilde{\varepsilon}\int U[\tilde{\phi}] 
}
with nilpotent parameters @f{\varepsilon} and @f{\tilde{\varepsilon}}, if exists the operator @f{X} such that:
@equation{
S \mapsto S + \varepsilon\int U[\phi] + \tilde{\varepsilon}\int U[\tilde{\phi}] + \varepsilon\tilde{\varepsilon} \int X
}
is a good sigma-model. The obstacle is some @bold{ghost number three} cohomology class linear in @f{\phi\otimes\tilde{\phi}}
and symmetric in @f{\phi\leftrightarrow \tilde{\phi}}. 

For more details see @hyperlink["http://arxiv.org/abs/1005.0049"]{my paper on beta-deformations}

If the ghost number three cohomology were zero, we would be sure that there is no obstruction.
But it is actually nonzero. How do we know that there is no obstruction?
}

@slide["B ghost to the resque" #:tag "BGhost" #:showtitle #t]{
@centered{
@nested{@spn[redbox]{@larger{the @f+0+3{b}-ghost: @bystro-bg[255 255 200]@f+0+3{\{Q,b\}=T}@bystro-bg[255 255 255]}}}
}
It has the components @f{b=b_{zz}} and @f{\bar{b}=b_{\bar{z}\bar{z}}}. 

To address our ``anomalies'' at the level of SUGRA, it is enough to consider
the zero modes @f{b_0} and @f{\bar{b}_0}:
@equation{
(b_0 - \bar{b}_0){\cal O}(z_0)  = 
\oint \Big(dz(z-z_0)b_{zz} - d\bar{z}(\bar{z}-\bar{z}_0)b_{\bar{z}\bar{z}}\Big){\cal O}(z_0)
}
This @bold{does not} act as a derivative:
@align[r.l.n @list[
@f{(b_0 - \bar{b}_0)({\cal O}_1 {\cal O}_2) \;\;= \;\;
((b_0 - \bar{b}_0){\cal O}_1){\cal O}_2 \;+\; (-)^{|{\cal O}_1|}{\cal O}_1((b_0 - \bar{b}_0){\cal O}_2)\;+\;}
@elem{@bystro-bg[255 255 200]@f{(-)^{|{\cal O}_1|}\gbl {\cal O}_1, {\cal O}_2 \gbr}@bystro-bg[255 255 255]}
@label{GerstenhaberBracket}
]]
The last term defines the @hyperlink["http://arxiv.org/abs/hep-th/9211072"]{Gerstenhaber structure} @f{\gbl {\cal O}_1, {\cal O}_2 \gbr}.

}

@slide["Removal of nonphysical states" #:tag "RemoveNonphysical" #:showtitle #t]{
We @elemref["AsymmetricDilaton"]{have seen} that there are some @bold{unphysical deformations}.
One way to look at them is to consider the so-called  @bold{beta deformation} which is adding to the
action the wedge product of two global symmetry currents:
@align[r.c.l.n @list[
@f{S\mapsto S + \varepsilon \int B^{ab}j_a\wedge j_b\qquad} 
@smaller{where }
@f+0-2{\;B^{ab}=-(-)^{|a||b|}B^{ba}=\mbox{const}}
@label{BetaDeformation}
]]
The corresponding unintegrated vertex is @f{B^{ab}\;V_1[T_a]\;V_1[T_b]}.
This is @bold{nonphysical} when @f{B^{ab}f_{ab}^c \neq 0} where @f{f_{ab}^c} is the structure constants of
the global symmetry group (super-Poincare). Notice that:
@equation{
(b_0 - \bar{b}_0)(B^{ab}\;V_1[T_a]\;V_1[T_b]) = B^{ab}V_1[T_a,T_b] = B^{ab}f_{ab}^c T_c
}
Therefore, we must @spn[attn]{postulate} that: 
@bystro-bg[255 255 200]
@fsize+[5]
@equation{(b_0 - \bar{b}_0)V_2 =0}
@fsize=[]
@bystro-bg[255 255 255]
@hrule[]
}

@after-pause{
But what happens if we, still, deform the action with (@ref{BetaDeformation}) with @f{B^{ab}f_{ab}^c \neq 0}?
The worldsheet action will be perfectly consistent @bold{as a classical action}, and enjoying a nice BRST symmetry.
At the classical level, nothing is wrong. But: 
@itemlist[
@item{the @bold{quantum theory} will be @spn[attn]{anomalous at the level of one loop}}
]
(anomaly in the holomorphicity of the @f{Q_L} and antiholomorphicity of @f{Q_R})
}

@after-pause{
@itemlist[
@item{Still, why SUGRA equations are not satisfied when @f{B^{ab}f_{ab}^c \neq 0}?}
]

Because the equation for the @spn[attn]{dilaton} becomes inconsistent.
}

@slide[@elem{Obstacle in terms of the @blue-ink[@f+0+3{b}]-ghost} #:tag "ObstacleViaB" #:showtitle #t]{
@table-of-contents[]

Remember @seclink["PotentialObstacle"]{our obstacle} is a bilinear map:
@equation{
V_2[\phi]\otimes V_2[\tilde{\phi}] \mapsto V_3[?]
}
@section{Conjecture about the obstacle}
@equation[#:label "ObstacleViaGB"]{V_3 = \gbl V_2,V_2 \gbr}
where @f{\gbl,\gbr} is defined in (@ref{GerstenhaberBracket})
@div[comment]{This means that when we deform @f{S\mapsto S+\varepsilon U_2} the obstacle at the second
order in @f{\varepsilon} will be controlled by @f{\gbl V_2,V_2\gbr}}

@section{Conjecture about the relation between @f{H^3(Q)} and @f{H^2(Q)}}
@equation{
(b_0  - \bar{b}_0)\;:\; H^3(Q) \to H^2(Q)
}
is an @spn[attn]{isomorphism}

@section{Consistency of the action}
From these conjectures follows that the actual obstacle vanishes. Indeed, @f{b_0 - \bar{b}_0}
is a @bold{derivation} of @f{\gbl,\gbr}. (But not of the product!) Therefore, when @f{V_2} is physical
(@italic{i.e.} @f{(b_0 - \bar{b}_0)V_2  = Q(\ldots)}) we automatically have @f{(b_0-\bar{b}_0)\gbl V_2,V_2\gbr = 0}.
But the kernel of @f{b_0 - \bar{b}_0} on the ghost number three cohomology is trivial. Therefore the obstacle
@bold{actually vanishes}.

@section{Physical and nonphysical are mutually obstructed}
@equation{
(b_0 - \bar{b}_0)\gbl V_2^{\rm phys}, \;V_2^{\rm nonphys} \gbr = \gbl V_2^{\rm phys} , V_1[T\,] \gbr = T.V_2^{\rm phys}
}
where @f{T} is the symmetry corresponding to the nonphysicality of @f{V_2^{\rm nonphys}}.
@tabular[@list[
@list[@elem{This explains how it could be that the equation for the dilaton is consistent for almost all deformations, 
but becomes inconsistent on nonphysical. (``How come the obstacles to solving for @f{\Phi} form a finite-dimensional space?'') 
We now see that nonphysical deformations form a @bold{separate branch}.}
@image{graphics/nonphysical_small.svg}]]]

}

@slide[@elem{Role of @blue-ink[@f+0+3{b}]-ghost in classical SUGRA} #:tag "RoleOfBGhost" #:showtitle #t]{
We conclude that the @f{b}-ghost, more precisely @f{b_0 -\bar{b}_0}, is important in classical SUGRA:
@itemlist[
@item{it removes the nonphysical states}
@item{it explains the consistency of the SUGRA constraints at the nonlinear level}
]
However, the @f{b}-ghost is nonpolynomial. It would be fair to say that it is not sufficiently 
well understood.

@itemlist[
@item{an elementary argument for the consistency of the nonlinear theory might shed some light on the @f{b}-ghost}
]
(However, I do not know such argument)

}

@slide["Part II" #:tag "Part-2" #:showtitle #t]{
Now I will talk about some details of the Type II superspace. 

The main question is: 
@itemlist[
@item{are quadratic algebras useful in the context of SUGRA? (As they were in SYM)}
]
As a possible application I will give some interpretation of the relation between integrated
and unintegrated vertex operators. 
@equation{
U = A_{\alpha} \partial \theta^{\alpha} + A_m\Pi^m + W^{\alpha}d_{\alpha} + F_{mn} N^{mn}
}

We will start by discussing the SUGRA data, @italic{i.e.} which structures should
be defined on the space-time?
}

@slide["The bundle of pure spinors" #:tag "PureSpinorBundle" #:showtitle #t]{
The @seclink["PureSpinors"]{worldsheet theory} has spinor ghosts @f{\lambda_L} and @f{\lambda_R}.
Because, @seclink["SingularLocus"]{as we explained}, they are in some sense infinitesimal objects,
we should tell them to live in some nontrivial bundle over space-time. The transition functions of that bundle
should respect the  pure spinor constraints 
@f{\lambda_L\Gamma^m\lambda_L = \lambda_R\Gamma^m\lambda_R = 0}. This means that the
transition functions should belong to:
@equation{
\hat{H} = \hat{H}_L\times \hat{H}_R = \mbox{Spin}_L(1,9)\times {\bf C}_{\times}^L \times \mbox{Spin}_R(1,9)\times {\bf C}_{\times}^R
}

@table[
@(make-style #f `(,(table-cells `( ,(build-list 2 (lambda (n) (make-style #f '(top))))))))
@list[@list[
@nested{
It is useful to consider the corresponding @spn[attn]{principal bundle}, which we will call @f{\widehat{M}}.

It is constructed in the following way.

The description of the SUGRA data starts with specifying a distribution
@highlight[@f{{\cal S}_L\oplus {\cal S}_R\subset TM}]. Invariantly, this is defined
as @f{{\rm im}(Q_*\;:\;TM\to TM)} at the singular locus @f{\lambda=0}.

Moreover, we must have specified, for every point @f{x\in M}, 
@highlight[@spn[amkhighlight]{an @f{\hat{H}_L\times \hat{H}_R}-orbit of}]
a 32-ple @f{(E_1^L,\ldots,E_{16}^L,\;E_1^R,\ldots,E_{16}^R)}
in @f{{\cal S}_L(x)\oplus {\cal S}_R(x)}. This orbit
is the fiber of @f{\hat{M}} over @f{x\in M}
}
@nested{@image[#:scale 2.0]{graphics/tetrad.svg}}
]]]

We will formulate the sigma-model so that the target space is @f{\widehat{M}}, but there
is a gauge symmetry projecting @f{\widehat{M}\to M}. 

@div[comment]{
This is how the sigma-model for @f{AdS_5\times S^5} is constructed. The target space is 
@f{PSU(2,2|4)}, but with the gauge symmetry @f{SO(1,4)\times SO(5)}.
}

}

@slide["Target Space" #:tag "TargetSpace" #:showtitle #t]{
Equivalently: let us consider a fixed representation @f{S_L\oplus S_R} of @f{\hat{H}_L\times \hat{H}_R}
and consider the @bold{space of maps}
@f{{\cal D}:\; S_L\oplus S_R \to {\cal S}_L(x)\oplus {\cal S}_R(x)}

In terms of the previous slide: @f{{\cal D}(s_L\oplus s_R)= s_L^{\alpha}E^L_{\alpha} + s_R^{\hat{\alpha}}E^R_{\hat{\alpha}}}

The @bold{fiber} of @f{\hat{M}\to M} is some @bold{orbit} of the following action of @f{\hat{H}_L\times \hat{H}_R}:
@equation{{\cal D}\mapsto {\cal D}\circ g}
(there is no preferred point on the orbit)
@hrule[]
}

@after-pause{
The @bold{target space} of our sigma-model is a cone in the associated vector bundle:
@bystro-bg[255 255 200]
@equation{
\mbox{\tt\small target space } = \widehat{M}\times_{\hat{H}_L\times\hat{H}_R}(C_L\times C_R)
}
@bystro-bg[255 255 255]
What can we say about the action of @f{Q} on this target space? We postulate
the action on the matter fields:
@equation{
\left.Q\right|_M = \lambda_L^{\alpha}E^L_{\alpha} + \lambda_R^{\hat{\alpha}}E^R_{\hat{\alpha}}
}
@div[comment]{This is @bold{without loss of generality}; we can just say that this is the definition
of the pure spinor coordinates @f{\lambda_L^{\alpha}} and @f{\lambda_R^{\hat{\alpha}}}}
@itemlist[
@item{And how does @f{Q} act on @f{\lambda_L} and @f{\lambda_R}?}
]
}

@slide[@elem{Lift of @f{Q}} #:tag "LiftOfQ" #:showtitle #t]{
@image{graphics/lift-of-Q.svg}
}

@slide["SUGRA data" #:tag "SUGRA-Data" #:showtitle #t]{
Now we define @f{{\bf D}_{\alpha}^L} and @f{{\bf D}_{\hat{\alpha}}^R} --- collections of vector fields on @f{\widehat{M}}
by this formula: 
@equation{
\widehat{Q} = \lambda_L^{\alpha}{\bf D}^L_{\alpha} + \lambda_R^{\hat{\alpha}}{\bf D}^R_{\hat{\alpha}}
}
(there is an imbiguity called ``shift transformations'' --- see @seclink["Shifts"]{next slide}).

We can now continue describing the SUGRA data. We got:
@align[r.l.n @list[
@f{{\bf D}\;:}
@f{\; S_L\oplus S_R \to \mbox{Vect}(\widehat{M})}
""
]@list[
@f{{\bf D}(s_L + s_R) \;=}
@f{\; s_L^{\alpha}{\bf D}^L_{\alpha} + s_R^{\hat{\alpha}}{\bf D}^R_{\hat{\alpha}}}
@label{DOfSLPlusSR}
]
]
satisfying the following properties:
@itemlist[
@item{@f{\bf D} commutes with the action of @f{\hat{H}}}
@item{@f{\bf D} is ``fixed modulo @f{\mbox{Vect}\widehat{M}/M}'' in the following sense: for any
   point @f{(x,{\cal D})\in M} let @f{\pi(x)} be the natural projection  @f{T_{(x,{\cal D})}\widehat{M}\to T_xM}, then 
@equation{
   \pi(x)\Big(({\bf D}(s_L+s_R))(x,{\cal D})\Big) = {\cal D}(s_L+s_R)
}
(in other words, only the vertical component of @f{\bf D} is non-obvious; the 
projection to @f{TM} is tautological)}
@item{SUGRA constraints:
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
where:
@itemlist[
@item{@f{{\bf A}_m^L} and @f{{\bf A}_m^R} are some sections of @f{{\cal T}\widehat{M}} and}
@item{ @f{R_{\alpha\beta}^{LL}}, @f{R_{\dot{\alpha}\dot{\beta}}^{RR}} and @f{R_{\alpha\dot{\beta}}^{LR}} 
       some sections of @f{{\cal T}\widehat{M}/M} (@italic{i.e.} vertical vector fields); 
       they are essentially ``curvatures''}
]}
]
Notice that satisfying the SUGRA constraints @bold{does} depend on the vertical component of @f{{\bf D}}.
}

@slide["Shift transformations" #:tag "Shifts" #:showtitle #t]{
Exist @f{\omega_{\alpha}^{\rm L} \in T\widehat{M}/M} such that 
@highlight-pink[@f{\lambda_L^{\alpha}\omega^{\rm L}_{\alpha}}] @f{\in \mbox{St}(\lambda_L)\subset \hat{\bf h}_L}; in 
other words:
@align[r.c.l @list[
@highlight-pink[@f{\lambda_L^{\alpha}(\omega^{\rm L}_{\alpha})^{\beta}_{\gamma}}]
@highlight-green[@f{\lambda_L^{\gamma}}] 
@f{= 0}
]]
Explicitly, they are given by the following formula 
(@italic{cf.}  Eq. (61) of  @hyperlink["http://arxiv.org/abs/hep-th/0112160"]{Berkovits and Howe}):
@equation{
   (\omega_{\alpha}^L)^{\beta}_{\gamma} = (\Gamma^n\Gamma^m)^{\beta}_{\gamma} \Gamma^m_{\alpha\delta}h_L^{\delta n}
}
with an arbitrary parameter @f{h_L^{\delta n} = h_L^{\delta n}(Z\,)}.
Observe that:
@centered{@nested[
@spn[greenbox]{The modification @f{{\bf D}_{\alpha}^L \mapsto {\bf D}_{\alpha}^L + \omega^L_{\alpha}} does not change @f{Q}}
]}
Similarly, we can modify @f{{\bf D}_{\hat{\alpha}}^R} by adding to it some @f{\omega_{\hat{\alpha}}^R} defined in a similar way;
we stress that @f{\omega^L} takes values in @f{\hat{\bf h}_L} and @f{\omega_R} takes values in @f{\hat{\bf h}_R}.
Obviously, these @bold{``shift gauge transformations''} depend on two parameters: @f{h_L^{\alpha n}} and 
@f{h_R^{\hat{\alpha } n}}. In terms of Eq. (@ref{DOfSLPlusSR}):
@equation{
   {\bf D}_{\rm new}(s_L + s_R) = {\bf D}(s_L + s_R) + s_L^{\alpha} \omega_{\alpha}^L
   + s_R^{\hat{\alpha}}\omega_{\hat{\alpha}}^R
}

}

@slide[@elem{@blue-ink[@f+0+3{AdS_5\times S^5}]: SUGRA data} #:tag "AdS5xS5" #:showtitle #t]{

The main ingredient is the symmetry group @f{PSU(2,2|4)} and its Lie superalgebra @f{{\bf psu}(2,2|4)},
generated by @f{\{t^0_{[mn]},t^3_{\alpha},t^1_{\hat{\alpha}},t^2_{m}\}} (in the flat space limit, 
@f{t^2_m} will become Poincare translations, @f{t^3_{\alpha}} left SUSYs and @f{t^1_{\hat{\alpha}}} right SUSYs).

The principal bundle @bystro-bg[255 255 200]@f{\widehat{M}=PSU(2,2|4)}@bystro-bg[255 255 255] is used 
(instead of @f{M}) as the target space. It is parametrized by the group element @f{g\in PSU(2,2|4)}:
@equation{g = \exp(\omega)\exp(x+\theta_L+\theta_R)}
where @f{\omega\in{\bf h}}. The action is built from currents:
@equation{J_{\pm} = - \partial_{\pm}g g^{-1}}

The map 
@f{{\bf D}\;:\;S_L \oplus S_R \to {\cal S}_L\oplus {\cal S}_R} is given by the left shifts:
@bystro-bg[255 255 200]
@equation{{\bf D}(s_L,s_R)g = (s_L^{\alpha}t^3_{\alpha} + s_R^{\hat{\alpha}}t^1_{\hat{\alpha}})g}
@bystro-bg[255 255 255]
It is straightforward to verify the SUGRA constraints (@ref{SpecialD}):
@equation{
\{{\bf D}(s_L+s_R),{\bf D}(s_L+s_R)\} \;=\; 
(s_L \Gamma^m s_L) t^2_m + (s_R \Gamma^m s_R) t^2_m  + s_L^{\alpha}s_R^{\hat{\alpha}} f_{\alpha\hat{\alpha}}{}^{[mn]} t^0_{[mn]}
}
@div[comment]{this is the simplest way to see that @f{AdS_5\times S^5} is a solution of the Type IIB SUGRA}

}

@slide[@elem{Commutators of @blue-ink[@f+0+3{{\bf D}_{\alpha}}]} #:tag "Commutators" #:showtitle #t]{
@image["graphics/commutators.svg"]
}

@slide[@elem{Basis in the tangent space} #:tag "Basis" #:showtitle #t]{
@image["graphics/basis.svg"]
}

@slide[@elem{Superfield @blue-ink[@f+0+3{C^{\alpha}_{\beta\gamma}}]} #:tag "SuperfieldC" #:showtitle #t]{
As a first example, let us decompose in this basis @f{\{\; {\bf D}^L_{\beta} \;,\; {\bf W}^{\alpha}_L \;\}}.

The coefficient of @f{{\bf W}^{\gamma}} defines an interesting SUGRA field @f{C^{\alpha}_{\beta\gamma}}:
@equation[#:label "DefCAlphaBetaGamma"]{
   \{\; {\bf D}^L_{\beta} \;,\; {\bf W}^{\alpha}_L \;\}\;=\;
   C^{\alpha}_{\beta}{}_{\gamma}\;{\bf W}^{\gamma}_L  \;
   \mbox{\tt\small mod } \langle {\bf D}^L_{\alpha},{\bf A}^L_m\rangle +T\widehat{M}/M
}
@hrule[]
}

@after-pause{
The action, copied from @hyperlink["http://arxiv.org/abs/hep-th/0112160"]{the paper of N.Berkovits and P.Howe}, is:
@(fsize+(- 3))
@align[r.c.l.n @list[
@f{S = }
@f{\frac{1}{2\pi\alpha'} \int d^2 z \Big(} 
@f{{1\over 2} \big(G_{MN}(Z) 
+ B_{MN}(Z)\big) \partial Z^M \overline\partial Z^N 
+ E^\alpha_M (Z) d_\alpha \overline\partial Z^M + }
""
]@list[
""
""
@f{+ E^{\hat\alpha}_M (Z) \tilde{d}_{\hat\alpha} \partial Z^M 
+ \Omega_{M\alpha}{}^\beta(Z) \lambda^\alpha  w^L_\beta \overline\partial Z^M 
+ \hat\Omega_{M\hat\alpha}{}^{\hat\beta}(Z) \tilde\lambda^{\hat\alpha} 
w^R_{\hat\beta} \partial Z^M \; +}
""
]@list[
""
""
@f{+ P^{\alpha\hat\beta}(Z) d_\alpha \tilde d_{\hat\beta} 
+ C^{\beta\hat\gamma}_{\alpha}(Z) \lambda^{\alpha}  w^L_{\beta} \tilde d_{\hat\gamma} 
+ \hat C^{\hat\beta\gamma}_{\hat\alpha}(Z) \tilde\lambda^{\hat\alpha} 
w^R_{\hat\beta} d_{\gamma} \; +} 
""
]@list[
""
""
@f{+ S^{\beta\hat\delta}_{\alpha\hat\gamma}(Z) 
\lambda^{\alpha}  w^L_{\beta} \tilde \lambda^{\hat\gamma} w^R_{\hat\delta} 
+ {1\over 2} \alpha' \Phi(Z)r \; +}
""
]@list[
""
""
@f{ + w^L_{\alpha+}\partial_-\lambda_L^{\alpha} + 
w^R_{\hat{\alpha}-}\partial_+\lambda_R^{\hat{\alpha}}
\quad\Big)}
@label{ActionOfBerkovitsAndHowe}
]
]
@fsize=[]
In @hyperlink["http://arxiv.org/abs/1503.01005"]{my paper} it was shown that:
@equation[#:label "SigmaVsAlg"]{
   C^{\alpha}_{\beta}{}^{\hat{\gamma}} P^{-1}_{\hat{\gamma}\gamma} = -C^{\alpha}_{\beta\gamma} \mbox{ \tt\small mod shift }
}
where the fields on the left hand side are from Eq. (@ref{ActionOfBerkovitsAndHowe}).

}

@slide["Ramond-Ramond bispinor" #:tag "RamondRamond" #:showtitle #t]{
We constructed the basis of the tangent space out of the commutators of the @bold{left} @f{{\bf D}^L_{\alpha}}.
We can similarly use the @bold{right} @f{{\bf D}^R_{\hat{\alpha}}}. It is useful to compare the two bases.

@bold{Conjecture}:

@equation{
{\bf D}^R_{\hat{\alpha}} = (P^{-1})_{\hat{\alpha}\alpha}{\bf W}_L^{\alpha} \mbox{ \tt\small mod } 
\langle {\bf A}^L_m,{\bf D}^L_{\alpha}\rangle + T\widehat{M}/M
}
This is the best definition of the Ramond-Ramond bispinor.
}

@slide["Worldsheet velocities" #:tag "WorldsheetVelocities" #:showtitle #t]{
@image{graphics/tautological-Lax.svg}
}

@remove-slide[]

@after-pause{
In particular, we have a @bold{natural basis} @highlight[@f{{\bf D}^L_{\alpha},\; {\bf A}^L_m,\; {\bf W}^{\alpha}_L}]. 

Let us decompose @f{\partial_+ \widehat{Z}^{\widehat{M}}} using this basis:

@align[r.l.n @list[
@f{{\partial Z^{\widehat{M}}(\tau^+,\tau^-)\over\partial\tau^+} \;=}
@f{\;
   \widetilde{J}_{0+}^{{\rm L}[mn]} a^{\widehat{M}}(t^{{\rm L}0}_{[mn]}) 
   + \widetilde{J}_{0+}^{{\rm R}[mn]} a^{\widehat{M}}(t^{{\rm R}0}_{[mn]}) \;+
   }
""
]@list[
@f{}
@f{\; 
   + \widetilde{J}_{+}^{\alpha} a^{\widehat{M}}({\bf D}^L_{\alpha}) 
   + \widetilde{\Pi}_+^m a^{\widehat{M}}({\bf A}^L_m) 
   + \widetilde{\psi}_{\alpha+}a^{\widehat{M}}({\bf W}_L^{\alpha})}
@label{DZDtauPlus}
]]
Here @f{a^{\widehat{M}}(\mbox{\tt\small vector})} denotes (@bold{for now...}) the @f{\widehat{M}}-component.
On the right hand side, the first line contains the @f{T\widehat{M}/M}-component, and the second line
the non-vertical components.

Again, it makes sense to ask what is @f{\widetilde{\psi}_{\alpha+}}. I have shown
@hyperlink["http://arxiv.org/abs/1503.01005"]{my paper} (in a rather roundabout manner) that
@f{\widetilde{\psi}_{\alpha+}} is the matter part of the worldsheet field @f{d_{\alpha+}}
from the action (@ref{ActionOfBerkovitsAndHowe}), which is related to the charge density of 
the BRST transformation @f{Q_L}:
@equation{
q_L = \int \lambda_L^{\alpha}d_{\alpha+}
}
}

@slide["Maurer-Cartan equations" #:tag "MaurerCartan" #:showtitle #t]{
Then @f{\left[{\partial\over\partial\tau^+}\;,{\partial\over\partial\tau^-}\right]Z^{\widehat{M}} = 0} leads to the tautological zero curvature equation:
@align[r.l @list[
@f{}@f{{\partial\over\partial\tau^+} \left(
      \widetilde{J}^{L{\rm M}}_{0-} + \widetilde{J}^{R{\rm M}}_{0-} +
      \widetilde{J}_{-}^{\hat{\alpha}} {\bf D}^R_{\hat{\alpha}} + 
      \widetilde{\Pi}_-^m {\bf A}^R_m + 
      \widetilde{\psi}_{\hat{\alpha}-}{\bf W}_R^{\hat{\alpha}}
   \right)\;-}
]@list[
@f{-\;}@f{{\partial\over\partial\tau^-} \left(
      \widetilde{J}^{L{\rm M}}_{0+} + \widetilde{J}^{R{\rm M}}_{0+} +
      \widetilde{J}_{+}^{\alpha} {\bf D}^L_{\alpha} + 
      \widetilde{\Pi}_+^m {\bf A}^L_m + 
      \widetilde{\psi}_{\alpha+}{\bf W}_L^{\alpha}
   \right)\;+}
]@list[
@f{+\;}@f{\Big[
         \widetilde{J}^{L{\rm M}}_{0+} + \widetilde{J}^{R{\rm M}}_{0+} +
         \widetilde{J}_{+}^{\alpha} {\bf D}^L_{\alpha} + 
         \widetilde{\Pi}_+^m {\bf A}^L_m + 
         \widetilde{\psi}_{\alpha+}{\bf W}_L^{\alpha}
         \;,}
]@list[
@f{}@f{\;\;
         \widetilde{J}^{L{\rm M}}_{0-} + \widetilde{J}^{R{\rm M}}_{0-} +
         \widetilde{J}_{-}^{\hat{\alpha}} {\bf D}^R_{\hat{\alpha}} + 
         \widetilde{\Pi}_-^m {\bf A}^R_m + 
         \widetilde{\psi}_{\hat{\alpha}-}{\bf W}_R^{\hat{\alpha}}
      \Big]\;=0}
]]
In this formula @f{\partial\over\partial\tau^+} in the first line and @f{\partial\over\partial\tau^-} in the second line only act on
the currents @f{\widetilde{J},\widetilde{\Pi},\widetilde{\psi}} and do not act on @f{{\bf D}, {\bf A}, {\bf W}}. The commutator is 
the commutator of the vector fields, @italic{e.g.}:
@equation{
   \left[
      \widetilde{\Pi}_+^m {\bf A}^L_m \;,\;
      \widetilde{\Pi}_-^n {\bf A}^R_n 
   \right]\; =\;
   \widetilde{\Pi}_+^m \widetilde{\Pi}_-^n 
   \left[
      {\bf A}^L_m \;,\;{\bf A}^R_n 
   \right]
}

}

@slide[@elem{Lax pair for @f{AdS_5\times S^5}} #:tag "AdS5xS5-MC" #:showtitle #t]{
Let us look at how it works for @f{AdS_5\times S^5}. 
@align[r.l.n @list[
@f{\Big[}
@f{{\partial\over\partial\tau^+} + 
J_{0+}^{[mn]}t^0_{[mn]} + J_{3+}^{\alpha}t^3_{\alpha} + J_{2+}^m t^2_m + J_{1+}^{\hat{\alpha}}t^1_{\hat{\alpha}}\;,}
""
]@list[
""
@f{{\partial\over\partial\tau^-} + 
J_{0-}^{[mn]}t^0_{[mn]} + J_{1-}^{\hat{\alpha}}t^1_{\hat{\alpha}} + J_{2-}^m t^2_m + J_{3-}^{\alpha}t^3_{\alpha}\;\Big]\;=0}
@label{TautologicalLaxAdS}
]
]
where @f{J=-dgg^{-1}}. This is a @bold{tautology}. 
@hrule[]
}

@after-pause{
In our notations:
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
@div[comment]{slight mismatch: we should have @f{{\bf W}_L^{\alpha}} instead of @f{{\bf W}^L_{\hat{\alpha}}}, and a similar mismatch
for @f{{\bf W}^R}; in fact, one can raise and lower indices with @f{P^{\alpha\hat{\alpha}}}}
}

@remove-slide[]

@after-pause{
However, there is a nontrivial generalization, leading to the @spn[attn]{integrability}:

@align[r.l.n @list[
@f{\Big[}
@f{{\partial\over\partial\tau^+} + 
J_{0+}^{[mn]}t^0_{[mn]} + J_{3+}^{\alpha}{t^3_{\alpha}\over z} + J_{2+}^m {t^2_m \over z^2} + 
J_{1+}^{\hat{\alpha}}{t^1_{\hat{\alpha}}\over z^3} +  N^{[mn]}_+\left({t_{[mn]}^0\over z^4} - 1\right) \;\;,}
""
]@list[
""
@f{{\partial\over\partial\tau^-} + 
J_{0-}^{[mn]}t^0_{[mn]} + J_{1-}^{\hat{\alpha}}(zt^1_{\hat{\alpha}}) + J_{2-}^m (z^2 t^2_m) + 
J_{3-}^{\alpha}(z^3t^3_{\alpha}) + N^{[mn]}_-(z^4t_{[mn]}^0 - t_{[mn]}^0)
\;\Big]\;=0}
@label{LaxAdS}
]
]
with @f{N^{[mn]}_+ = (w_+\Gamma_{mn}\lambda)}. Notice that we obtained the nontrivial Lax pair from
the tautological one by replacing @f{{\bf psu}(2,2|4)} with some @bold{twisted loop algebra}.

@bold{What can we say from our point of view?} We seem to need:
@align[r.l.n @list[
@f{{\bf D}^L_{\alpha} = t^3_{\alpha} \;\mapsto\;}
@highlight[@f{z^{-1}t^3_{\alpha}}]
""
]@list[
@f{{\bf A}^L_m = t^2_m \;\mapsto\;}
@highlight[@f{z^{-2}t^2_m}]
""
]@list[
@f{{\bf W}^L_{\hat{\alpha}} = t^1_{\hat{\alpha}}\;\mapsto\;}
@highlight[@f{z^{-3}t^1_{\hat{\alpha}}}]
""
]@list[
@f{{\bf D}^R_{\hat{\alpha}} = t^1_{\hat{\alpha}}\;\mapsto\;}
@highlight[@f{zt^1_{\hat{\alpha}}}]
""
]@list[
@f{{\bf A}^R_m = t^2_m \;\mapsto\;}
@highlight[@f{z^2t^2_m}]
""
]@list[
@f{{\bf W}^R_{\alpha} = t^3_{\alpha} \;\mapsto\;}
@highlight[@f{z^3t^3_{\alpha}}]
""
]
]
what could this mean?

Idea: let us further extend this algebra to something like a sum of two SYM algebras.

}

@slide[@elem{Yang-Mills-like algebra} #:tag "YangMillsAlgebra" #:showtitle #t]{
@image{graphics/Yang-Mills-like-algebra.svg}
}

@slide[@elem{Gluing together two Yang-Mills algebras} #:tag "Gluing" #:showtitle #t]{
@image{graphics/left-right.svg}
}

@slide[@elem{Gluing together two Yang-Mills algebras} #:tag "Summary-Ltot" #:showtitle #t]{
@image{graphics/Ltot.svg}
@linebreak[]
We will call this algebra @f+0+4{\cal L}.
}

@slide[@elem{Consistency} #:tag "PBW-1" #:showtitle #t]{
@image{graphics/PBW_1.svg}
}

@slide[@elem{Consistency} #:tag "PBW-2" #:showtitle #t]{
@image{graphics/PBW_2.svg}
}

@slide[@elem{Koszul property} #:tag "Koszulity" #:showtitle #t]{
Some quadratic algebras have special property: Koszulity.

If we introduce nonhomogeneity in the @italic{Koszul} quadratic algebra, it is
enough to verify the Jacobi identity for @bold{three elementary letters}.
If it is satisfied, then the nonhomogeneous algebra will be automatically consistent,
in the sense of the PBW property:

@equation{
{\bf gr} {\cal L} = q{\cal L}
}
where @f{q{\cal L}} is the corresponding homogeneous quadratic algebra.

In other words, the leading term is well-defined.

@hrule[]
}

@after-pause{
In our case, the Koszul property is a special  (nontrivial) property of the pure spinor constraint:
@equation{
\lambda_L\Gamma_m\lambda_L = \lambda_R\Gamma_m\lambda_R = 0
}
@hrule[]
}

@after-pause{
It turns out that besides the consistency of @f{\cal L}, the Koszul property also implies some 
relation between the cohomology of the pure spinor BRST operator and the cohomology of @f{\cal L}.
This relation allows to understand the relation between integrated and unintegrated vertex operators
@spn[attn]{without using the @f{b}-ghost}.
}

@slide[@elem{BRST cohomology for @f{AdS_5\times S^5}} #:tag "BRST-cohomology" #:showtitle #t]{
BRST operator acts on the functions @f{f(g,\lambda_L,\lambda_R)} which are invariant under @f{{\bf g}_{\bar{0}}}:
@equation[#:label "BRST"]{
Q_{\rm BRST} f(g,\lambda_L,\lambda_R) = \left.{d\over dt}\right|_{t=0}
f\left(e^{t(\lambda_L^{\alpha}t^3_{\alpha} + \lambda_R^{\hat{\alpha}}t^1_{\hat{\alpha}})} g,\;\lambda_L,\;\lambda_R\right)
}

@hrule[]
}

@after-pause{
The Koszul property of pure spinors implies that the cohomology of the complex (@ref{BRST}) can be 
computed as the relative Lie algebra cohomology of @f{{\cal L}}:
@equation{
H^n(Q_{\rm BRST}) = H^n({\cal L},{\bf g}_{\bar{0}}\;; C^{\infty}(\widehat{M}))
}
where @f{\widehat{M} = PSU(2,2|4)}.

}

@slide["Algebroid" #:tag "Algebroid" #:showtitle #t]{
@image{graphics/algebroid.svg}
}

@slide["Zero curvature" #:tag "ZeroCurvature" #:showtitle #t]{
Let us return to @seclink["Gluing"]{the question of constructing the Lax pair}

@table-of-contents[]

@section{Case of @f{AdS_5\times S^5}}
Doing literally this does not work:
@align[r.l.c.n @list[
@f{\Big[\;}
@f{{\partial\over\partial\tau^+} + 
J_{0+}^{[mn]}t^0_{[mn]} + J_{3+}^{\alpha}{\bf D}^L_{\alpha} + J_{2+}^m {\bf A}^L_m + J_{1+}^{\hat{\alpha}}{\bf W}^L_{\hat{\alpha}}\;,}
""
""
]@list[
""
@f{{\partial\over\partial\tau^-} + 
J_{0-}^{[mn]}t^0_{[mn]} + 
J_{1-}^{\hat{\alpha}}{\bf D}^R_{\hat{\alpha}} + J_{2-}^m {\bf A}^R_m + J_{3-}^{\alpha}{\bf W}^R_{\alpha}\;\Big]\;=0\quad}
@bold{(wrong!)}
""
]
]
The correct formula is:
@align[r.l.l.n @list[
@f{\Big[\;}
@f{{\partial\over\partial\tau^+} + 
J_{0+}^{[mn]}t^0_{[mn]} + 
J_{3+}^{\alpha}{\bf D}^L_{\alpha} + J_{2+}^m {\bf A}^L_m + J_{1+}^{\hat{\alpha}}{\bf W}^L_{\hat{\alpha}} +
\left(\{{\bf D}^L_{\alpha}\;,\;{\bf W}_L^{\beta}\} - f^{\beta[mn]}_{\alpha} t^0_{[mn]}\right)w^L_{\beta+}\lambda_L^{\alpha}
}
@f{,}
""
]@list[
""
@f{{\partial\over\partial\tau^-} + 
J_{0-}^{[mn]}t^0_{[mn]} + 
J_{1-}^{\hat{\alpha}}{\bf D}^R_{\hat{\alpha}} + J_{2-}^m {\bf A}^R_m + J_{3-}^{\alpha}{\bf W}^R_{\alpha} +
\left(\{{\bf D}^R_{\hat{\alpha}}\;,\;{\bf W}_R^{\hat{\beta}}\} - f^{\hat{\beta}[mn]}_{\hat{\alpha}} t^0_{[mn]}\right)w^R_{\hat{\beta}+}\lambda_R^{\hat{\alpha}}
\;\;}
@f{\Big]\;=0\quad}
@label{GeneralizedLaxAdS}
]
]


@section{General case}
(Partly @spn[attn]{conjectured}, partly proven) Use the @bold{anchor} map
@equation{
a\;:\;{\cal A} \to \mbox{Vect}(\widehat{M})
}
and its lifts @f{\kappa_L} and @f{\kappa_R} such that 
@equation{
a\kappa_L = a\kappa_R = {\bf id}\;:\; \mbox{Vect}(\widehat{M})\to \mbox{Vect}(\widehat{M})
}

@align[r.l.n @list[
@f{\Big[\;}
@f{{\partial\over\partial\tau^+} + 
J_{0+}^{[mn]}t^0_{[mn]} + 
J_{3+}^{\alpha}{\bf D}^L_{\alpha} + J_{2+}^m {\bf A}^L_m + J_{1+}^{\hat{\alpha}}{\bf W}^L_{\hat{\alpha}} +
(1-\kappa_L a)\{{\bf D}^L_{\alpha}\;,\;{\bf W}_L^{\beta}\}w^L_{\beta+}\lambda_L^{\alpha}
}
""
]@list[
""
@f{{\partial\over\partial\tau^-} + 
J_{0-}^{[mn]}t^0_{[mn]} + 
J_{1-}^{\hat{\alpha}}{\bf D}^R_{\hat{\alpha}} + J_{2-}^m {\bf A}^R_m + J_{3-}^{\alpha}{\bf W}^R_{\alpha} +
(1-\kappa_R a)\{{\bf D}^R_{\hat{\alpha}}\;,\;{\bf W}_R^{\hat{\beta}}\} w^R_{\hat{\beta}-}\lambda_R^{\hat{\alpha}}
\;\;\Big]\;=0\quad}
@label{GeneralizedLax}
]
]
What we actually checked is another identity:
@equation{
[{\bf Q_L} , {\bf L}_+] = 0
}
}

@slide[@elem{BRST transformations of the Lax operators} #:tag "BRST-of-Lax" #:showtitle #t]{

Notice that BRST transformations are symmetries, and therefore both @f{Q_L} and @f{Q_R} commute with 
@f{\partial\over\partial\tau^+} and @f{\partial\over\partial\tau^-}. This leads to the tautological identity:

@image{graphics/tautological_QL.svg}

And more generally:
@equation{
\left[\;Q + \lambda_L^{\alpha}{\bf D}_{\alpha}  + \lambda_R^{\hat{\alpha}}{\bf D}^R_{\hat{\alpha}}\;,\;
{\partial\over\partial\tau^{\pm}} + \ldots \;\right]
}
As with zero curvature, this tautological identity can be made nontrivial by promoting @f{{\bf D}^L_{\alpha}}
and @f{{\bf D}^R_{\hat{\alpha}}} from generators of @f{{\bf psu}(2,2|4)} to generators of @f{\cal L}.
We should also modify the Lax operator as in Eq. (@ref{GeneralizedLaxAdS}).

}

@slide[@elem{Integrated vertex: case of @f{AdS_5\times S^5}} #:tag "IntegratedVertexAdS" #:showtitle #t]{
The zero curvature equation allows to give a @bold{unified description of integrated and unintegrated vertices}.
@align[l.n @list[
@f{J - \lambda = J_+ d\tau^+ + J_- d\tau^- + \lambda_L^{\alpha}{\bf D}^L_{\alpha} + \lambda_R^{\hat{\alpha}}{\bf D}^R_{\hat{\alpha}}}
""
]]
We have:
@equation|{
(d+Q_{\rm BRST})\;(J-\lambda) = 
- {1\over 2}\left\{\;(J-\lambda)\;,\;(J-\lambda)\;\right\}\quad \mbox{mod } {\bf g}_{\bar{0}}
}|
(only modulo @f{{\bf g}_{\bar{0}}} because @f{\{\lambda,\lambda\}\in {\bf g}_{\bar{0}}}) This means that: 
@centered[
@spn[greenbox]{
the operation of the substitution of @f{J-\lambda} intertwines @f{d+Q_{\rm BRST}} with @f{Q_{\rm Lie}}}
]
Given the cocycle @f{\psi\in H^2_{\rm Lie}\left({\cal L}, {\bf g}_{\bar{0}};\;\mbox{Fun}(\widehat{M})\right)} the following 
expression:
@equation{\psi(J-\lambda)(g)}
gives a cohomology class of @f{d+Q_{\rm BRST}} which is the sum of integrated and unintegrated vertex operator, and an
intermediate expression. 

In other words, we expressed integrated and unintegrated operators as different components of the same cohomology class.


} 

@;(setq amkhlv/snapshot-dir "graphics")

@; ---------------------------------------------------------------------------------------------------
@(bystro-close-connection bystro-conf)
@disconnect[formula-database]

 
  
