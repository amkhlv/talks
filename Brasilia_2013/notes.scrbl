#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require (for-syntax "defs_for-syntax.rkt" (planet amkhlv/bystroTeX/slides_for-syntax)))
@(require "defs.rkt" (planet amkhlv/bystroTeX/common) (planet amkhlv/bystroTeX/slides))
@(require (only-in (planet jaymccarthy/sqlite) close))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf 
   (bystro (find-executable-path "amkhlv-java-formula.sh")
           "notes_formulas.sqlite"  ; name for the database
           "notes" ; directory where to store .png files of formulas
           25  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@; This controls the single page mode:
@(define singlepage-mode #t)
@; ---------------------------------------------------------------------------------------------------
@(begin ;do not change anything here:
   (define (start-formula-database)
     (configure-bystroTeX-using bystro-conf)
     (bystro-initialize-formula-collection bystro-conf))
   (define formula-database (start-formula-database))
   (unless (bystro-formula-processor bystro-conf)
     (error "*** could not find executable for formula processing ***"))
   (define-syntax (syntax-setter x) (defines-syntax-for-formulas x))                
   (syntax-setter defineshiftedformula)
   (defineshiftedformula "formula-enormula-humongula!")
   (bystro-titlepage-init #:singlepage-mode singlepage-mode))
@; ---------------------------------------------------------------------------------------------------
@; AND HOPEFULLY SOME CONTENT:

@(define amkhlv/css-dir (string->path "./"))
@elem[#:style (make-style #f (list (make-css-addition (build-path amkhlv/css-dir "no-margin.css"))))]{}
@elem[#:style (make-style #f (list (make-css-addition (build-path amkhlv/css-dir "misc.css"))))]{}

@title{Notes on my talk in Brasilia}
@table-of-contents[]
@section{Pure spinor superstring}

@subsection{Fundamental principle: Existence of the BRST operator}
There are different approaches to string theory, which all start from slightly different (although similar) postulates.

@spn[attn]{Pure spinor string} postulates (among other things) that the classical backgrounds of the Type IIB SUGRA
are in one-to-one correspondence with the classical 2d sigma-models satisfying certain additional requirements.

The requirements are:
@itemlist[#:style 'ordered
@item{The @f{\sigma}-model should be classically conformally invariant}
@item{The @f{\sigma}-model should be classically invariant under a fermionic symmetry @f{Q} which is nilpotent: @f{Q^2 = 0};
it is usually called ``BRST symmetry''. Moreover:
@itemlist[#:style 'ordered
@item{The @f{\sigma}-model should be classically invariant under two @f{U(1)} symmetries called ``left and right ghost number''}
@item{The BRST operator splits into two parts: @f{Q = Q_L + Q_R} where @f{Q_L} has left ghost number +1 and right ghost
number 0, and @f{Q_R} has left ghost number 0 and right ghost number +1. The @f{Q_L} and @f{Q_R} should be separately nilpotent.}
]}]

@subsection{Unintegrated vertices}
@bold{Definition @th-num{SpaceOfOperators}:} ``@spn[attn]{Operators}'' are reasonably nice expressions built on
the sigma-model fields and their derivatives, modulo the @f{\sigma}-model classical equations of motion.

The sigma-model fields are typically matter fields @f{x,\theta} and ghosts @f{\lambda_L,\lambda_R,w_+,w_-}. 
The ghosts @f{\lambda_L,\lambda_R} satisfy the @spn[attn]{pure spinor constraints}:
@equation[#:label "PureSpinorConstraints"]{
\lambda_L^{\alpha}\,\Gamma_{\alpha\beta}\,\lambda_L^{\beta} \; = \;
\lambda_R^{\dot{\alpha}}\,\Gamma_{\dot{\alpha}\dot{\beta}}\,\lambda_R^{\dot{\beta}} = 0
}

@bold{Definition @th-num{ConformalDimension}:} 
The @spn[attn]{conformal dimension} of an operator is defined as the total number of derivatives, plus the number of @f{w_{\pm}}.

The BRST operator @f{Q}, being a symmetry of the theory, acts on such expressions. 

@bold{Definition @th-num{Ghost number}:}
The @spn[attn]{ghost number} of an operator is the total number of @f{\lambda}'s minus the total number of @f{w's}.

@bold{Definition @th-num{UnintegratedVertices}:}
@spn[attn]{Unintegrated vertex} is an operator @f{V(x,\theta,\lambda)} of the conformal dimension zero annihilated by @f{Q}.
The @spn[attn]{gauge transformation} of the vertex operator is:
@equation[#:label "GaugeTransformationOfUnintegratedVertices"]{ V \mapsto V + Qu }

@div[redbox]{Unintegrated operators form the cohomology of the BRST operator @f{Q} at the ghost number two.
This is, essentially, the Hilbert space of states in the worldsheet sigma-model}

It is a fundamental principle of the string theory, that every state corresponds to some deformation of
the worldsheet theory. This leads to the concept of @bold{integrated vertex}, which we will now describe.

@subsection{Integrated vertices}
@bold{Definition @th-num{IntegratedVertex}:} @spn[attn]{Integrated vertices are deformations} of the string 
worldsheet theory, preserving the existence of the BRST structure:
@equation[#:label "DeformationOfTheAction"]{
S \mapsto S + \int \;d\tau^+ d\tau^- \; U
}
--- this @f{U} is the integrated vertex.

As we already said, there is the correspondence between physical states of the worldsheet theory and its deformations. 
This means that @bold{for every unintegrated vertex there exists a corresponding integrated vertex}. The 
correspondence is described by the @spn[attn]{descent procedure}, which goes as follows. Given some unintegrated
vertex @f{V}, do the following:

@align[r.l.n @list[
@v+[3 @f{dV \;=\;}] @f{QW} @label{DefW}
]@list[
@v+[3 @f{dW \;=\;}] @f{QU} @label{DefU}
]]

@bold{Theorem @th-num{Descent}}: @spn[attn]{Exist} operators @f{W} and @f{U} defined in (@ref{DefW}) and (@ref{DefU})
exist.

@div[redbox]{The main result in this talk is the proof of this theorem for the superstring in @f{AdS_5\times S^5}.}

@subsection{Plan of the talk}
In order to prove the descent procedure, we will need two ingredients:

@itemlist[#:style 'ordered
@item{Relation between the cohomology of @f{Q} and the @bold{Lie algebra cohomology}.

@smaller{Lie algebra cohomology is the cohomology of the operator which is well-known to physicists:}
@equation{Q_{\rm Lie} = c^a t_a - {1\over 2} f^a_{bc}\;c^bc^c{\partial\over\partial c^a}}
@smaller{This is what is usually called ``BRST operator'' @italic{e.g.} in Yang-Mills theory. But mathematicians
call such things ``Lie algebra cohomology''. It turns out that the BRST cohomology can be cast in this form.}
}
@item{Some observation about the @bold{integrable structure} of the worldsheet theory}
]


@section{Simplest example of the pure spinor formalism}
Here I will give the simplest nontrivial example of the pure spinor formalism. 

Consider the following
@spn[attn]{pure spinor BRST operator} acting on functions of @f{x,\theta,\lambda}
as follows:
@equation[#:label "SimplestBRSTComplex"]{
Qf(x,\theta,\lambda) \; = \; \lambda^{\alpha}\left(
{\partial\over\partial\theta^{\alpha}} + \theta^{\beta}\Gamma^m_{\beta\alpha}\; {\partial\over\partial x^m}
\right)\;f(x,\theta,\lambda)
}
We assume that @f{f} are polynomials in @f{\lambda}.
The @spn[attn]{simplest example of the pure spinor formalism} is given by the following

@bold{Theorem @th-num{CohomologyOfSimplestBRSTComplex}:} The cohomology is nonzero only at the ghost number 1,
and counts the solutions of SUSY Maxwell equations.


@section{Algebras formed by covariant derivatives}

We will start by relating the cohomology of (@ref{SimplestBRSTComplex}) with some Lie algebra cohomology.

@itemlist[@item{
Pure spinor cohomology is related to the Lie algebra cohomology of some infinite-dimensional Lie superalgebra
}]

So, we have to first @bold{define} that infinite-dimensional Lie superalgebra. The definition is motivated
by the super-Yang-Mills theory, and the resulting algebra is called SYM. 

Let us proceed to its definition.

@subsection{The SYM algebra}
Classical equations of motion of the ten-dimensional super-Yang-Mills algebra are @bold{encoded in the constraints}:
@equation[#:label "QuadraticConstraint"]{\Gamma_{[m_1\ldots m_5]}^{\alpha\beta}\;\;\;\;\;\;\nabla_{\alpha}\nabla_{\beta} = 0}
As a consequence of these constraints, there exist operators @f{\nabla_m} such that:
@equation[#:label "SYMConstraints"]{
\{\nabla_{\alpha},\nabla_{\beta}\} = \Gamma_{\alpha\beta}{}^m \nabla_m
}
The actual definition of the operators @f{\nabla_{\alpha}} and @f{\nabla_m} in the super-Yang-Mill theory is:
@align[r.l.n @list[
@f{\nabla_{\alpha} \;=\;} 
@f{{\partial\over\partial\theta^{\alpha}} + \theta^{\beta}\Gamma^m_{\beta\alpha}\;{\partial\over\partial x^m} + A_{\alpha}(x,\theta)}
@label{InternalStructureOfNablaAlpha}
]@list[
@f{\nabla_m \;=\;} @f{{\partial\over\partial x^m} + A_m(x,\theta)} @label{InternalStructureOfNablaM}
]]
@(fsize+ (- 3)) @div[comment]{
I am sorry for the abuse of notations; notice that @f{A_{\alpha}} and @f{A_m} are completely different fields; 
but in my notations the only difference between them is greek subindex @f{\alpha} @italic{vs.} latin subindex @f{m}.
}@(fsize=)


@div[redbox]{
Now let us forget about the ``internal structure'' of @f{\nabla_{\alpha}} and @f{\nabla_m} given by (@ref{InternalStructureOfNablaAlpha})
and (@ref{InternalStructureOfNablaM}) and think of Eq. (@ref{QuadraticConstraint}) as defining 
relations in an @(begin (bystro-fg 255 0 0) "")
@spn[attn]{abstract algebra formed by the letters} @f{\nabla_{\alpha}}. @(begin (bystro-fg 0 0 0) "")
}

This is an infinite-dimensional algebra. Moreover, this is actually a @bold{quadratic algebra}, because the constraint
(@ref{QuadraticConstraint}) is quadratic in generators.

@subsubsection{Relation between the SYM algebra and the susy algebra}
The Lie superalgebra @f{\rm susy} is formed by the operators @f{S_{\alpha}} and @f{T_m} with the following commutation relations:
@align[r.l.n @list[
@f{\{S_{\alpha},S_{\beta}\} \;=\;} @f{\Gamma_{\alpha\beta}^m\; T_m} @label{AnticommutatorOfSAlpha}
]@list[
@f{\{S_{\alpha},T_m\} \;=\;} @f{0} ""
]]
The algebra @f{\rm susy} (unlike SYM algebra) is finite-dimensional. 
There is a homomorphism from the SYM algebra to  @f{\rm susy}:
@align[r.l.n @list[
@f{\nabla_{\alpha} \;\to\;} @f{S_{\alpha}} @label{HomomorphismFromSYMToSUSY}
]]
The kernel of this homomorphism is an ideal in SYM, which we will call @f{I_{\rm susy}}. We have:
@equation[#:label "RelationBetweenSYMAndSUSY"]{
{\rm susy} = {\rm SYM}/I_{\rm susy}
}

@subsubsection{Cohomology of SYM algebra and pure spinor cohomology}
Now we want to relate the cohomology of the operator (@ref{SimplestBRSTComplex}) to the cohomology of the Lie superalgebra SYM.

It is convenient to start with a generalization of (@ref{SimplestBRSTComplex}). Remember that in (@ref{SimplestBRSTComplex})
the BRST operator is:
@equation[#:label "SimpleQ"]{
Q \; = \; \lambda^{\alpha}\left(
{\partial\over\partial\theta^{\alpha}} + \theta^{\beta}\Gamma^m_{\beta\alpha}\; {\partial\over\partial x^m}
\right)}
We observe that the components of pure spinor @f{\lambda^{\alpha}} get contracted with the differential operators
@f-3{{\partial\over\partial\theta^{\alpha}} + \theta^{\beta}\Gamma^m_{\beta\alpha}\; {\partial\over\partial x^m}}
which @bold{form a representation} of the algebra @f{\rm susy}  (@ref{AnticommutatorOfSAlpha}).

Let us therefore consider @bold{any} representation @f{V\;} of  @f{\rm susy}. 
To every such representation corresponds a pure spinor BRST complex, which is constructed as follows. 

Let @f{{\cal P\,}^n} be the space of polynomials of the pure spinor variables @f{\lambda^{\alpha}}
of degree @f{n}. It is assumed that @f-3{\lambda^{\alpha}\Gamma_{\alpha\beta}^m\;\lambda^{\beta}= 0}; in other words
@f{{\cal P\,}^n} is the commutative algebra of polynomials of the degree @f{n} of the variables @f{\lambda^{\alpha}} modulo the
ideal of polynomials divisible by the expression @f-3{\lambda^{\alpha}\Gamma^m_{\alpha\beta}\;\lambda^{\beta}}.

@equation[#:label "PureSpinorBRSTComplexWithV"]{
\ldots\longrightarrow {\cal P\,}^n\otimes V \xrightarrow{Q} {\cal P\,}^{n+1}\otimes V \longrightarrow \ldots
}
where the BRST operator acts as follows (compare to (@ref{SimpleQ})):
@equation[#:label "BRSTOperatorOfSYM"]{Q = \lambda^{\alpha} S_{\alpha}}

@div[redbox]{The cohomology of this complex is related to the Lie algebra cohomology of the SYM algebra.}
In order to explain how, we have to notice that @f{V\;}, being a representation of @f{\rm susy}, is also a representation of @f{SYM}
(because of the homomorphism (@ref{HomomorphismFromSYMToSUSY})). Then we have:
@(fsize+ 10)
@equation[#:label "BRSTForSYMvsLie"]{H^n(Q,V\;) = H^n({\rm SYM}, V\;)}
@(fsize=)
This is a very nontrivial theorem. Our goal is to establish a similar theorem in the SUGRA context.
But before, let us consider some consequences. First of all, an important special case is when @f{V\;} is 
the dual space of the universal enveloping algebra of the @f{\rm susy} algebra:
@equation{V = (U{\rm susy})'}
This is the representation in the space of  Taylor series of @bold{functions on the super-space-time}. 
In this case the cohomology of the BRST complex computes the @bold{solutions of the Maxwell equations}.

@subsection{What we are going to do}
We want some analogue of (@ref{BRSTForSYMvsLie}) for SUGRA. Let us first understand the LHS.

The LHS should give the @spn[attn]{space of vertex operators} in a given background.

Equivalently: this is the same as the space of solutions of the linearized SUGRA equations. 
(Linearized around a given background solution.)

We will only discuss the case when the background solution is @f{AdS_5\times S^5} (or flat space as a limit of AdS).
(Generalization of these methods to arbitrary backgrounds is work in progress.)

@subsection{Superconformal algebra}
@subsubsection{General definition and generators}
It is important to remember that the classical SUGRA background @f{AdS_5\times S^5} has a large algebra of 
symmetries, the @bold{superconformal algebra} a.k.a. @f{{\bf psu}(2,2|4)}. We will denote it @f{{\bf g}}:
@equation{{\bf g} = {\bf psu}(2,2|4)}
It has the subalgebra @f{{\bf g}_{\bar{0}}\subset {\bf g}} which consists of those generators which preserve a fixed point:
@equation{{\bf g}_{\bar{0}} = so(1,4)\oplus so(5)}
Indeed, @f{AdS_5\times S^5} is a coset space:
@equation{{AdS_5\times S^5} = G_{\bar{0}}\backslash G}
Let us use a calligraphic index to parametrize the generators of @f{{\bf g}_{\bar{0}}}:
@equation{[t^0_{\cal A}\;,\;t^0_{\cal B}] = f_{\cal AB}{}^{\cal C} t^0_{\cal C}}
The rest of the generators of @f{{\bf psu}(2,2|4)} are denoted:
@equation{t_{\hat{\alpha}}^1\;,\; t_m^2 \;,\; t_{\alpha}^3}
The superindex @f{0,1,2,3} corresponds to some @f{{\bf Z}_4}-grading which we do not need now, we just have
to remember that the generators @f{t^0_{\cal A}} generate @f{{\bf g}_{\bar{0}}}, 
and @f{t_{\hat{\alpha}}^1\;,\; t_m^2 \;,\; t_{\alpha}^3} generate the rest of @f{{\bf g}}.

@subsubsection{Structure of commutation relations}
The commutation relations are parametrized by the @bold{structure constants}. 

For our present purpose, we have to know the following facts:
@itemlist[#:style 'ordered
@item{First of all, @f{{\bf g}_{\bar{0}}} acts by the adjoint representation:
@align[r.l.n @list[
@f{[t^0_{\cal A}\;,\,t^1_{\hat{\alpha}}\,] \;=\;} @v-[8 @f{f_{{\cal A}\hat{\alpha}}{}^{\hat{\beta}} t^1_{\hat{\beta}}}] ""
]@list[
@f{[t^0_{\cal A}\;,\,t^2_m\,] \;=\;} @f{f_{{\cal A}m}{}^n t^2_n} ""
]@list[
@f{[t^0_{\cal A}\;,\,t^3_{\alpha}\,] \;=\;} @v-[8 @f{f_{{\cal A}\alpha}{}^{\beta} t^1_{\beta}}] ""
]]}
@item{We need to know that the commutator of @f{t^3_{\alpha}} and @f{t^1_{\hat{\beta}}} falls into @f{{\bf g}_{\bar{0}}}:
@equation{\{t^3_{\alpha}\;,\,t^1_{\dot{\beta}}\} = f_{\alpha\dot{\beta}}{}^{\cal A} t^0_{\cal A}}
}
@item{The commutator  @f{\{t_{\alpha}^3\;,\,t_{\beta}^3\}} is proportional to @f{t^2_m}, and moreover there is a basis
where the structure constants are proportional to the gamma-matrices:
@align[r.l.n @list[
@f{\{t^3_{\alpha}\;,\,t^3_{\beta}\} \;=\; } @f{\Gamma^m_{\alpha\beta}\; t^2_m} ""
]@list[
@f{\{t^1_{\hat{\alpha}}\;,\,t^1_{\hat{\beta}}\} \;=\; } @f{\Gamma^m_{\hat{\alpha}\hat{\beta}}\; t^2_m} ""
]]
}
]
We see that @bold{the superconformal algebra is somewhat similar to the @f{\rm susy} algebra (@ref{AnticommutatorOfSAlpha})},
or maybe we should say ``to a sum of two copies of it''.

@subsection{Algebra @f{{\cal L}_{\rm tot}} and its relation to vertex operators}
We will now extend the superconformal algebra to some infinite-dimensional algebra, just like @f{\rm susy} was extended
to SYM.

@subsubsection{Definition}
We will take @bold{two copies} @f{{\cal L}_L} and @f{{\cal L}_R} of the SYM algebra (@ref{QuadraticConstraint}), add a copy 
of @f{{\bf g}_{\bar{0}}}, and @bold{glue them all together}:
@align[r.l.n @list[
@f{\{\nabla^L_{\alpha},\nabla^R_{\dot{\beta}}\} =\;}@f{f_{\alpha\dot{\beta}}{}^{\cal A}t^0_{\cal A}\;\;\;}@label{NablaLNablaR}
]@list[
@f{[t^0_{\cal A}\;,\;\nabla^L_{\alpha}] = \;}@f{f_{{\cal A}\alpha}{}^{\beta}\nabla^L_{\beta}}@label{T0NablaL}
]@list[
@f{[t^0_{\cal A}\;,\;\nabla^R_{\dot{\alpha}}] = \;}@f{f_{{\cal A}\dot{\alpha}}{}^{\dot{\beta}}\nabla^R_{\dot{\beta}}}@label{T0NablaR}
]@list[
@f{\Gamma_{m_1\ldots m_5}^{\alpha\beta} \;\;\;\;\; \{\nabla^L_{\alpha}\;,\;\nabla^L_{\beta}\} = \;}@v+[6 @f{0}]@label{NablaLNablaL}
]@list[
@f{\Gamma_{m_1\ldots m_5}^{\dot{\alpha}\dot{\beta}} \;\;\;\;\;
\{\nabla^R_{\dot{\alpha}}\;,\;\nabla^R_{\dot{\beta}}\} = \;}@v+[6 @f{0}]@label{NablaRNablaR}
]]
In other words:
@equation[#:label "Ltot"]{
   {\cal L}_{\rm tot} = {\cal L}_L + {\cal L}_R + {\bf g}_{\bar{0}}
}

@subsubsection{BRST cohomology and relative cohomology}
The analogue of (@ref{BRSTForSYMvsLie}) is:
@equation[#:label "BRSTVsRelative"]{
H^n(Q,V\,) = H^n({\cal L}_{\rm tot}\;,\;{\bf g}_{\bar{0}}\;,\;V\,)
}
It involves @bold{relative Lie algebra cohomology}. The cochain complex consists of
@f{{\bf g}_{\bar{0}}}-invariant linear functions:
@equation{
C^n = \mbox{Hom}_{{\bf g}_{\bar{0}}}\left( 
\Lambda^n\left( {{\cal L}_{\rm tot}\over {\bf g}_{\bar{0}}} \right)\;,\;V
\right)
}

@subsubsection{Algebraic interpretation of SUGRA operators}
There is a relation analogous to (@ref{RelationBetweenSYMAndSUSY}), namely exists an ideal @f{I\subset {\cal L}_{\rm tot}}
such that:
@equation{
{\bf psu}(2,2|4) = {\cal L}_{\rm tot}/I
}
This has an interesting consequence in the case when @f{V = (U{\bf g})'}:
@equation{
H^n\left({\cal L}_{\rm tot}\;,\;{\bf g}_{\bar{0}}\;,\;\,(U{\bf g})'\right) \;=\; H^n(I)
}
In particular, the linear space dual to @f{H^2(I)}:
@align[r.l.n @list[
@v+[7 @f{H_2(I) \;=\;} ]
@f{\left\{ \sum_i x_i\wedge y_i\;|\; \sum_i [x_i,y_i] = 0 \right\}}
@label{H2}
]@list[
"" @elem{modulo @f{\sum_i \left([x_i,y_i]\wedge z_i + \mbox{cycl}(x,y,z)\right)}} ""
]]
describes @bold{gauge invariant local operators}. The analysis of (@ref{H2}) is nontrivial, the only
operator in AdS which I can identify is the @bold{dilaton} @f{C^{\alpha\dot{\alpha}} \nabla^L_{\alpha}\wedge\nabla^R_{\dot{\beta}}}
where @f{C^{\alpha\dot{\alpha}}} is some @f{{\bf g}_{\bar{0}}}-invariant tensor. In the @bold{flat space limit}, the analysis simplifies:
@align[r.l.n @list[
@f{[\nabla_k^L,\nabla_l^L]\wedge [\nabla_m^R,\nabla_n^R]
+ [\nabla_m^L\;,\nabla_n^L]\wedge [\nabla_k^R,\nabla_l^R]
\;}@f{= {\cal R}_{klmn}}""
]@list[
@f{(\nabla_{[k}^L-\nabla_{[k}^R)\wedge [\nabla^L_l,\nabla^L_{m]}\;] 
\;}@f{= H_{klm} =\partial_{[k}B_{lm]}}""
]@list[
@v+[7 @f{A_m^{\pm}\;}]@f{= A_m^L \pm A_m^R}@label{DefApm}
]]
We get the following equations of motion:
@align[r.l @list[
@v+[7 @f{g^{lm}{\cal R}_{klmn} \;}]@f{= {3\over 4} \nabla_{(k}A^-_{n)}}
]@list[
@v+[7 @f{0 \;}]@f{= \nabla_{[k}A^-_{n]}}
]@list[
@v+[7 @f{\nabla^k H_{klm} \;}]@f{= \nabla_{[l} A^+_{m]}}
]@list[
@v+[7 @f{0 \;}]@f{= \nabla_{(l}A^+_{m)}}
]]
The gradient of the dilaton corresponds to @f{A^-_n}, while @f{A_n^+} @bold{does not have a 
clear interpretation} in the Type IIB supergravity. 

@div[greenbox]{This rizes a question: do we correctly understand the low momentum sector of the Type IIB SUGRA?
The pure spinor formalism suggests a different low momentum spectrum from what we all know from the textbooks.
Is it possible that the textbooks miss some discrete degrees of freedom?}

But we will now turn to a different subject. Let us look at what we can learn from the
@spn[attn]{classical integrability} of the  worldsheet theory.

@section{Generalized Lax operator}
The algebra @f{{\cal L}_{\rm tot}} was essentially guessed. But it turns out that it has a natural interpretation
in the string worldsheet theory.

@subsection{Usual definition of the Lax operator}
For 
the pure spinor superstring in @f{AdS_5\times S^5} the Lax pair was constructed by 
@hyperlink["http://arxiv.org/abs/hep-th/0307018"]{B.C.Vallilo}:
@align[r.l.n @list[
@f{L_+  =\;}@f{{\partial\over\partial\tau^+} 
+ \left( J^{{\cal A}}_{0+} - N^{{\cal A}}_+ \right)t^0_{{\cal A}} \;\;\;+}""
]@list[
@f{}@f{\phantom{\partial\over\partial\tau^+} + {1\over z} J^{\alpha}_{3+}\;\;t^3_{\alpha} 
+ {1\over z^2} J^m_{2+}\;t^2_m + {1\over z^3} J^{\dot{\alpha}}_{1+}\; t^1_{\dot{\alpha}}
+ {1\over z^4} N^{{\cal A}}_+t^0_{{\cal A}}\;\;\;}@label{OrigLPlus}
]@list[
@f{L_- = \;}@f{{\partial\over\partial\tau^-} 
+ \left( J^{{\cal A}}_{0-} - N^{{\cal A}}_- \right)t^0_{{\cal A}} \;\;\; +}""
]@list[
@f{}@f{\phantom{\partial\over\partial\tau^-}
+ zJ^{\dot{\alpha}}_{1-}\;\;t^1_{\dot{\alpha}} + z^2 J^m_{2-}\;t^2_m 
+ z^3J^{\alpha}_{3-}\;\;t^3_{\alpha} + z^4N^{{\cal A}}_- t^0_{{\cal A}}\;\;\;}@label{OrigLMinus}
]]
where @f-2{t^0_{{\cal A}}\;\;\;, t^1_{\dot{\alpha}}\;, t^2_m\;, t^3_{\alpha}} are generators of @f{{\bf g} = {\bf psl}(4|4)}:
@equation[#:label "SuperCommutationRelations"]{
 [t^{\bar{a}}_A \;,  t^{\bar{b}}_B\; ] = f_{AB}{}^C t^{\bar{a} + \bar{b}\mbox{ \tiny mod } 4}_C
}
@f{J} are @bold{currents}: 
@equation[#:label "Jvsg"]{
   J_{\bar{k}} = -(dg g^{-1})_{\bar{k}}
}
and @f{z} is a complex number which is called @bold{spectral parameter}.

It follows from the equations of motion that @f{[L_+,L_-] = 0}. This observation
can be taken as a starting point of the classical integrability theory. 

@larger{We will now discuss @bold{generalizations} of this construction.}

@subsection{Lax operator and the loop algebra}
One can interpret @f-3{{1\over z}t_{\alpha}^3\;}, @f-3{{1\over z^2} t^2_m\;}, @f-3{{1\over z^3} t^1_{\dot{\alpha}}\;}, 
@f{\ldots} as generators of the 
twisted loop superalgebra @f{L{\bf g} = L{\bf psl}(4|4)}. 

@div[comment]{The word ``twisted'' means that the power of the spectral parameter mod 4 should 
correlate with the @f{{\bf Z}_4}-grading of the generators}

This observation allows us to rewrite (@ref{OrigLPlus}) 
and (@ref{OrigLMinus}) as follows:
@align[r.l.n @list[
@f{L_+  =\;}@f{{\partial\over\partial\tau^+} 
+ \left( J^{\cal A}_{0+} - N^{\cal A}_+ \right)T^0_{\cal A}\;\;\;+}""
]@list[
@f{}@f{\phantom{\partial\over\partial\tau^+} +  J^{\alpha}_{3+}\;\;T^{-1}_{\alpha} 
+  J^m_{2+}\;T^{-2}_m +  J^{\dot{\alpha}}_{1+}\; T^{-3}_{\dot{\alpha}}
+  N^{\cal A}_+T^{-4}_{\cal A}\;\;}@label{LoopLPlus}
]@list[
@f{L_- = \;}@f{{\partial\over\partial\tau^-} 
+ \left( J^{\cal A}_{0-} - N^{\cal A}_- \right)T^0_{\cal A} \;\;\;+}""
]@list[
@f{}@f{\phantom{\partial\over\partial\tau^-}
+ J^{\dot{\alpha}}_{1-}\;T^1_{\dot{\alpha}} + J^m_{2-}\;T^2_m 
+ J^{\alpha}_{3-}\;T^3_{\alpha} + N^{\cal A}_- T^4_{\cal A}\;\;\;}@label{LoopLMinus}
]]
where @f{T_{\alpha}^{-1}} replaces @f{ z^{-1}t^3_{\alpha}} @italic{etc.}; operators @f{T^{n}_A} are generators of the
twisted loop superalgebra. Withe these new notations, the spectral parameter 
is not present in @f{L_{\pm}}. Instead of entering explicitly in @f{L_{\pm}}, it now 
@bold{parametrizes a representation} of the generators @f{T_A^n}.

@subsection{Further generalization}


The basic relations (@ref{NablaLNablaL}) imply:
@align[r.l.n @list[
@f{\{\nabla_{\alpha}^L, \nabla_{\beta}^L\} = \;}@f{f_{\alpha\beta}{}^m A_m^L} @label{DefAm}
]@list[
@f{[\nabla_{\alpha}^L, A_m^L\,] = \;}@f{f_{\alpha m}{}^{\dot{\beta}} W_{L\dot{\beta}}}@label{DefW}
]]
and similar equations for the commutators of @f{\nabla^R_{\dot{\alpha}}}.
@(fsize+ (- 3))
@div[comment]{
Eq. (@ref{DefAm}) we already discussed; it encodes the SUGRA constraints and at the same time @bold{defines} @f{A_m^L}.
Eq. (@ref{DefW})  is a @italic{theorem-definition}: the @italic{theorem} says that the left hand side is proportional
to @f{f_{\alpha m}{}^{\dot{\beta}}}, and the @italic{definition} is of @f{W_{L\dot{\beta}}}
}
@(fsize=)

It turns out, that there is the following generalization of the Lax pair:
@align[r.l.n @list[
@f{L_+  = \;}@f{\left(
   {\partial\over \partial \tau^+} 
   + J_{0+}^{\cal A}\; t^0_{\cal A} \;\;\;
\right) 
+ J_{3+}^{\alpha}\; \nabla^L_{\alpha} + J_{2+}^m\; A^L_m + J_{1+}^{\dot{\alpha}} \; W^L_{\dot{\alpha}} +}""
]@list[
@f{}@f{+ \; \lambda_L^{\alpha} w_{L+}^{\dot{\beta}}\;\, \left(
   \{  \nabla^L_{\alpha} \,,\, W^L_{\dot{\beta}}\}
   - f_{\alpha\dot{\beta}}{}^{\cal A} t^0_{\cal A}\;\;\;\,\right)}@label{LPlus}
]@list[
@f{L_- = \;}@f{\left(
   {\partial\over\partial\tau^-}
   + J_{0-}^{\cal A}\; t^0_{\cal A}\;\;\;
\right) 
+ J_{1-}^{\dot{\alpha}}\; \nabla^R_{\dot{\alpha}} 
+ J_{2-}^m \,A^R_m + J_{3-}^{\alpha}\; W^R_{\alpha} +}""
]@list[
@f{}@f{+ \; \lambda_R^{\dot{\alpha}} w_{R-}^{\beta}\;\,\left(
   \{ \nabla^R_{\dot{\alpha}} \;,\; W^R_{\beta} \}
   - f_{\dot{\alpha}\beta}{}^{\cal A} t^0_{\cal A}\;\;\;\,
\right)}@label{LMinus}
]]
The generalized Lax operator has the following properties:
@itemlist[
@item{
@bold{The @f{{\bf g}_{\bar{0}}} gauge covariance.}
For @f{\xi_0(\tau^+,\tau^-) \in {\bf g}_{\bar{0}}}, consider the transformation
@f{\delta_{\xi_{\bar{0}}}} acting on the currents (@ref{Jvsg}) and ghosts @f{w_{\pm}}, @f{\lambda_L,\lambda_R} as follows:
@align[r.l @list[
@f{\delta_{\xi_{\bar{0}}} g =\;}@f{\xi_{\bar{0}} g}
]@list[
@f{\delta_{\xi_{\bar{0}}} \lambda_{L|R} =\;}@f{[\xi_{\bar{0}},\;\lambda_{L|R}]}
]@list[
@f{\delta_{\xi_{\bar{0}}} w_{\pm} =\;}@f{[\xi_{\bar{0}},\;w_{\pm}]}
]]
Then @f{L_{\pm}} transforms in a covariant way: 
@equation{
   \delta_{\xi_0}L_{\pm} = [\xi_0,\;L_{\pm}]
}
}
@item{@bold{Zero curvature}: @equation[#:label "ZeroCurvatureForGeneralizedLax"]{[L_+,L_-]=0}}
@item{@bold{Covariance under the BRST transformations.} When we act on the currents (@ref{Jvsg}) 
and ghosts @f{w_{\pm}}, @f{\lambda_L,\lambda_R} by the BRST transformation:
@align[r.l @list[
@v+[6 @f{Q_{\rm BRST}\; g \;=\;}] @f{(\lambda_L^{\alpha}t^3_{\alpha} + \lambda_R^{\hat{\alpha}}t^1_{\hat{\alpha}})g}
]@list[
@f{Q_{\rm BRST}\; w_{1+} \;=\;}@f{J_{1+}}
]@list[
@f{Q_{\rm BRST}\; w_{3-} \;=\;}@f{J_{3-}}
]@list[
@f{Q_{\rm BRST}\; \lambda_{L|R} \;=\;}@v+[2 @f{0}]
]]
then @f{L_{\pm}} transforms in a covariant way: 
@equation[#:label "BRSTCovariance"]{
   Q_{BRST}L_{\pm} \;=\; 
\left[
   L_{\pm}\;,\;\left(
      \lambda_L^{\alpha}\nabla^L_{\alpha} + 
      \lambda_R^{\dot{\alpha}} \nabla^R_{\dot{\alpha}}
   \right)
\right]
}
}]


@section{Integrated vertex}
Finally, we are now ready to discuss our construction of the integrated vertex.

Suppose that we are given an unintegrated vertex, and we want to construct the corresponding integrated vertex.

Unintegrated vertices correspond to the elements of  @f{H^2({\cal L}_{\rm tot},{\bf g}_{\bar{0}}, \; (U{\bf g})')}.
The 2-cocycle
representing such an element is a bilinear function of two elements of @f{{\cal L}_{\rm tot}}, 
which we will denote @f{\xi} and @f{\eta}. We have to remember that the 2-cocycle takes
values in @f{(U{\bf g})'} which is identified with the space of Taylor series at the
unit of the group manifold. Let @f{g\in PSU(2,2|4)} denote the group element.

Therefore, the element @f{\psi\in H^2({\cal L}_{\rm tot},{\bf g}_{\bar{0}}, \; (U{\bf g})')} is a function
of two elements of @f{{\cal L}_{\rm tot}}, which we will call @f{\xi} and @f{\eta}, taking values in the functions of @f{g}:
@equation[#:label "PsiCocycle"]{
   \psi(\xi,\eta)(g)
}
Remember that our generalized Lax operators (@ref{LPlus}), (@ref{LMinus}) have the form:
@equation{
L_{\pm} = {\partial\over\partial \tau^{\pm}} + {\cal J}_{\pm}
}
where @f{{\cal J}_{\pm}} are operators of the conformal dimension 1 taking values in @f{{\cal L}_{\rm tot}}.

@bold{Theorem @th-num{IntegratedFromCocycle}:} The integrated vertex operator is obtained from (@ref{PsiCocycle})
by replacing @f{\xi} and @f{\eta} with @f{{\cal J}_{\pm}}:

@equation{
U = \psi({\cal J}_+,{\cal J}_-)(g)
}
@bold{Proof.} We have to verify the descent procedure (@ref{DefU}), (@ref{DefW}).
The proof is a variation of the famous differential geometry formula @f{{\cal L}_v = \iota_v d + d \iota_v}.

Consider  @f{{\cal J} = {\cal J}_+ d\tau^+ + {\cal J}_- d\tau^-}. This is a @f{{\cal L}_{\rm tot}}-valued differential
1-form on the worldsheet. Substitution of @f{{\cal J}} into @f{\psi} intertwines the @f{Q_{\rm Lie}} with the de Rham 
differential @f{d}. (The verification of this statement uses the fact  that @f{{\cal J}_{\pm}} satisfies 
the zero-curvature equations (@ref{ZeroCurvatureForGeneralizedLax}).)

It is useful to introduce the free Grassmann parameters @f{\epsilon} and @f{\epsilon'}. Let us formally introduce the operator
@f{I_{\epsilon}} as follows:
@equation{
I_{\epsilon} = \epsilon(\lambda_L^{\alpha} \nabla^L_{\alpha} + \lambda_R^{\hat{\alpha}}\nabla^R_{\hat{\alpha}})
{\partial\over\partial {\cal J}}
}
This operator simply replaces @f{{\cal J}} 
with @f-3{\epsilon(\lambda_L^{\alpha} \nabla^L_{\alpha} + \lambda_R^{\hat{\alpha}}\nabla^R_{\hat{\alpha}})}.
Eq.  (@ref{BRSTCovariance}) implies that the BRST differential acts as follows:
@equation{
\epsilon Q = dI_{\epsilon} + I_{\epsilon}d
}
But @f{d\psi = 0}, therefore:
@align[r.c.l @list[
@v-[2 @f{\epsilon Q\psi = dw\;\;}] " where " @v-[2 @f{\;\;w = I_{\epsilon}\psi}]
]]
This was the first step (@ref{DefU}) of the descent procedure.

To make the second step (@ref{DefW}), 
we have to remember that @f{\psi} is a @bold{relative} cocycle, which means that it always vanishes whenever
one  of the ghosts falls into @f{{\bf g}_{\bar{0}}}. This implies:
@equation{
[dI_{\epsilon'} + I_{\epsilon'}d\;\;,\;\; I_{\epsilon}]\;\psi = 0
}
and further:
@align[r.c.l @list[
@v-[8 @f{\epsilon'Q\;w = {1\over 2}dv\;\;}] " where " @v-[4 @f{\;\;v = I_{\epsilon'}I_{\epsilon}\psi}]]]
Notice that the unintegrated vertex @f{v} is obtained from the cocycle @f{\psi} by replacing the ghost
fields with @f{\lambda_L + \lambda_R}.

@section{Instead of conclusion}
In my opinion, it is a drawback of the string theory that it is too geometric:

@centered{
@image[#:scale 0.5]{graphics/genusExpansion.png}
}
Theory being geometric indicates that the classical limit has somehow been already taken.
I think that the @bold{quantum} theory should use @bold{algebraic} description, instead of geometric. 

Here we attempt to interpret some constructions of the pure spinor formalism in terms of 
abstract algebra. We observe that at least some aspects of the formalism get simplified,
notably the relation between integrated and unintegrated vertices. At this point, however,
this is not entirely convincing, at least for the following reasons: 
@itemlist[
@item{Our results are only valid for the simplest nondegenerate background, @f{AdS_5\times S^5}. 
The correct description should, of course, be background-independent.}
@item{We only studied SUGRA, we did not attempt to say anything about the massive states}
]
We think that this circle of ideas should be further developed.



@; ---------------------------------------------------------------------------------------------------

@close[formula-database]
