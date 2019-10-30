#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require "defs.rkt" bystroTeX/common bystroTeX/slides (for-syntax bystroTeX/slides_for-syntax))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(bystro-set-css-dir (build-path (find-system-path 'home-dir) "a" "git" "amkhlv" "profiles" "writeup"))
@(define bystro-conf   
   (bystro (bystro-connect-to-server (build-path (find-system-path 'home-dir) ".config" "amkhlv" "latex2svg.xml"))
           "short/formulas.sqlite"  ; name for the database
           "short" ; directory where to store image files of formulas
           25  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@(define singlepage-mode #f)
@(bystro-def-formula "formula-enormula-humongula!")

@(define norm-bg (bystro-formula-bg-color bystro-conf))
@(define high-bg '(250 200 200))

@title[#:style '(no-toc no-sidebar)]{Symmetries, renormgroup, deformations, AdS/CFT}

Andrei Mikhailov

Talk on @hyperlink["https://www.ictp-saifr.org/workshop-on-quantum-symmetries/"]{Workshop on Quantum Symmetries}

October 16-18, 2019,
ICTP-SAIFR, São Paulo, Brazil

Based on @hyperlink["http://arxiv.org/abs/arXiv:1906.12042"]{http://arxiv.org/abs/arXiv:1906.12042}

@hyperlink["https://github.com/amkhlv/talks/blob/master/IFT-quantum-symmetries_2019/short.scrbl"](source code)

@bystro-toc[]

@page["A geometrical picture of renormgroup" #:tag "GeometricalAbstraction" #:showtitle #t]


@subpage[1 @elem{Tangent space} #:tag "sec:TangentSpace"]

Let @f{M} be a smooth manifold. Fix a point @f{p\in M}, and consider the tangent space at this point:

@div[
     greenbox
     ]{
       @f{T_pM} is the space of equivalence classes of trajectories @f{p(t)}
       passing through @f{p} at the time zero, @italic{i.e.} @f{p(0)=p}.
       The equivalence relation is that @f{p_1\equiv p_2} when their difference is @f{o(t)}
       }

Let us denote this space of trajectories @f{\Omega_p(M)}. By the definition of @f{T_pM}, we have a map:

@e{
   \Omega_p (M) \stackrel{\pi}{\longrightarrow} T_pM
   }


@subpage[1 @elem{Lifting tangent vectors to trajectories} #:tag "sec:Lifting"]

Suppose that @f{M} comes equipped with an action of a Lie algebra @f{\bf g}, preserving a point @f{p\in M}.

Then @f{g} also acts in @f{T_pM}

Let us try to construct a map

@(align
  r.l.n
  @`(""
     @,f{F\;:\;T_pM\longrightarrow \Omega_p(M)}
     "")
  @`("" @,v-[8 @elem[#:style 'no-break]{such that}] "")
  @`("" @,v-[8 @f{\pi\circ F \;=\;\bf 1}] "")
  )

@spn[attn]{respecting the action of @f{\bf g}}

@bold{Does such a map exist?}

@page["Structure of the obstacle" #:tag "Obstacle" #:showtitle #t]

@bold{Some} map @f{F\;:\;T_pM\longrightarrow \Omega_p(M)} always exists, the question is, can
we pick a @f{\bf g}-invariant one?

Let us choose @bold{some} @f{F}. Any map @f{T_pM\longrightarrow \Omega_p(M)} can be thought of as
a family of maps:

@e{
   F_{\epsilon}\;:\;T_pM\rightarrow M
   }

parametrized by @f{\epsilon\in {\bf R}} satisfying:

@(align
  r.l.n
  @`(@,f{}
        @,f{F_{\epsilon }(0) = p}
        @,label{MustPassThroughP})
  @`(@,f{}
        @,f{F_{\epsilon}(\kappa w) = F_{\kappa\epsilon}(w)}
        @,label{Homogeneity})
  @`(@,f{}
        @,f{\left .{d\over  d\epsilon }\right |_{\epsilon  = 0} F_{\epsilon } = {\bf  1}\; :\; T_pM \rightarrow  T_pM}
        @,label{InverseToProjector})
)

For each element @f{\xi \in  {\bf  g}} there is a corresponding vector field @f{v\langle \xi \rangle} on @f{M}. Let us 
consider @f{F_{\epsilon  *}^{-1}v\langle \xi \rangle}. It is a vector field on @f{T_pM}:
@e[#:label "ExpansionOfVectorField"]{
   F_{\epsilon  *}^{-1} v\langle \xi \rangle  =
   v_0\langle \xi \rangle  + \epsilon v_1\langle \xi \rangle  + \epsilon^2 v_2\langle \xi \rangle  + \ldots
   }
Because of Eq. (@ref{Homogeneity}), the scaling degree of @f{v_i} correlates with the power of @f{\epsilon}.
In particular, @f{v_0} is a @bold{linear vector field} on the linear space @f{T_pM}. This is the
action of @f{\bf g} on @f{T_pM}. (The @f{v_1} is a quadratic vector field:
                                      @f{v_1 = u^i_{jk}x^jx^k{\partial\over\partial x^k}},
                                      and @f{v_2} is qubic, @italic{etc.}.)

@(centered
  (div
   greenbox
   @`{Can we choose @,f{F_{\epsilon}} so that @,f{v_1 = v_2 = \ldots = 0}?}
   ))
@hrule[]

We would like to reformulate this question as a cohomological problem.

For two elements @f{\xi} and @f{\eta} of @f{\bf  g}, we have:
@e{
   \left[\; F_*^{-1} v\langle \xi \rangle \; ,\; F_*^{-1} v\langle \eta \rangle \; \right ]  
   \; =\;  F_*^{-1} v\langle[\xi ,\eta ] \rangle
   }

Let us introduce the ``ghosts'' @f{c = c^At_A \in  \Pi  {\bf  g}} and the ``BRST operator'':

@(align
  r.l.n
  @`(@,f{Q\;\in\;}
        @,f{\mbox{Vect}(\Pi {\bf g} \times T_pM)}
        "")
  @`(@,f{Q \;=\;}
        @,f|{{1\over  2}c^Ac^Bf_{AB}{}^C {\partial \over \partial  c^C} + v_0\langle  c\rangle}|
        @,label{BRSTOperator}
        )
  @`("" @,f{Q^2=0} "")
  )

This defines the differential in the Lia algebra
cohomology complex  of @f{\bf  g} with values in the space of vector fields on @f{T_pM} having zero
of at least second order at the point @f{p}. (The action of the second term, @f{v_0\langle  c\rangle},
is by the commutator of vector fields.)

Let us define @f{\Psi} as the sum of nonlinear terms in @f{F_*^{-1}v\langle  c\rangle} (@italic{ cf.} Eq. (@ref{ExpansionOfVectorField})):
@e[#:label "DefPsi"]{
                     \Psi  = F_*^{-1}v\langle  c\rangle  - v_0\langle  c\rangle  =
                     \epsilon v_1\langle  c\rangle  + \epsilon^2 v_2\langle  c\rangle  +\ldots
   }
It  satisfies the Maurer-Cartan equation:
@e[#:label "MaurerCartan"]{
   Q\Psi   + {1\over  2}[\Psi ,\Psi ] = 0
   }

Ambiguity in the choice of @f{F_{\epsilon}\;:\;T_pM\rightarrow M} translates to @bold{gauge transformations}
of @f{\Psi}:

@e[#:label "InfinitesimalGaugeTransformations"]{
   \delta _{\Phi }\Psi  = Q\Phi  + [\Psi ,\Phi ]
   }

@(centered
  (div
   greenbox
   @`{Obstacle to the existence of @,f{\bf g}-invariant @,f{F_{\epsilon}} is a solution of the MC
               Eq. (@,ref{MaurerCartan}) modulo gauge transformations defined in Eq. (@,ref{InfinitesimalGaugeTransformations})}
   ))

In particular, if the first non-removable term is @f{v_n}, then the corresponding obstacle is:
@e{
   H^1\Big({\bf g}\;,\; \mbox{Hom}(S^{n+1}T_0M, T_0M)\Big)
   }

@page["Relation to tree diagramms" #:tag "TreeDiagramms" #:showtitle #t]

@bystro-local-toc[]

@subpage[1 @elem{Perturbative solutions of classical field equations} #:tag "sec:PerturbativeSolutions"]

Let us take @f{M} to be the space of perturbative solutions @f{\phi} of nonlinear equations of the form:
@e[#:label "NonlinearEquation"]{
   L\phi  = f(\phi )
   }
where @f{L} is some linear differential operator, and @f{f(\phi )} is a nonlinear function 
describing the interaction. We assume that @f{f} is a polynomial starting with 
quadratic or higher order terms.

The point @f{p\in  M} will be the zero solution @f{p=0}.
Then @f{T_0M} can be identified with the space of solutions of 
the linearized equation:
@e[#:label "LinearizedEqM"]{
   L\phi  = 0
   }
Tree level perturbation theory can be thought of as a 1-parameter map
@e[#:label
   "FeynmanOneParameter"
   ]{
   F_{\epsilon }\; :\;  T_0M \rightarrow  M
   }
@e{
   F_{\epsilon}[\phi _0] = \epsilon\phi _0\; + L^{-1} f(F_{\epsilon}[\phi _0])
   }
where @f{L^{-1}} satisfies:
@e[#:label "LInverse"]{
   LL^{-1} = {\bf  1}
   }
The definitions of the operator @f{L^{-1}}  has an ambiguity
(because one can add a solution of the free equation).
Suppose that we made @bold{some choice} of @f{L^{-1}}.

@subpage[1 @elem{Amputation of the last leg} #:tag "sec:Amputation"]

Let us define @f{\Psi} as follows:
@(align
  r.l.n
  @`(@,f{\Psi \; \in \;}
   @,f{\mbox{Hom} \left (\Pi  {\bf  g}\; ,\;  \mbox{Vect} (T_0M)\right )}
   "")
@`(@,f{\Psi \langle  c\rangle[\phi _0]  \; =\;}
   @,f{{1\over \epsilon}[Q,L^{-1}]f(F_{\epsilon}[\phi _0])}
   @,label{PsiViaFeynmanDiagramms})
@`(""
   @,v-[5 @elem{where @f{Q = c^aT_a} -- symmetry generators @f{T_a} contracted with Grassmann ``ghosts'' @f{c^a}}]
   "")
)

@page["Renormalization of QFT deformations" #:tag "DeformationsOfAction" #:showtitle #t]

Consider a quantum field theory with an action @f{S} invariant under some Lie algebra of symmetries @f{\bf  g}.

Let us study the infinitesimal deformations of the action:
@e[#:label "Deformations"]{
   \delta  S \; =\;  \epsilon \int  d^dx \sum _I f_I(x) U_I(x)
   }
where:
@(itemlist
  @item{@f{\epsilon} is an @bold{infinitesimal} parameter}
  @item{@f{f_I(x)} are some space-time-dependent coupling constants}
  @item{@f{\{ U_I\}} is some set of local operators closed under @f{\bf  g}}
  )
Then the expressions on the RHS of Eq. (@ref{Deformations}) form a linear representations of @f{\bf  g};
call it @f{T_0}:

@(centered
  (div
   greenbox
   @`{@,f{T_0} is the linear space of infinitesimal deformations (@,ref{Deformations})}
  ))

@hrule[]

We can study the deformations (@ref{Deformations}) at @bold{finite} @f{\epsilon}. This requires taking care
of the divergencies. Suppose that the space of operators  @f{\{ U_I\}} is big enough to include all
the needed counterterms. After regularization and adding counterterms, Eq. (@ref{Deformations})
is well-defined in the quantum theory. Therefore we have a map:

@e{
   F_{\epsilon }\; :\; T_0 \longrightarrow  \left[\mbox{\tt \small space of finite deformations } \right ]
   }

Then:

@(centered
  (div
   greenbox
   @`{there is a natural action of @,f{\bf g} on the (non-linear) space of deformations}
  ))

@comment{
     This is a subtle point. 
     We need to regularize multiple integrals @f{\int  U\cdots \int  U}
     (to avoid collisions) and then subtract counterterms
     (which are also of the form @f{\int  U\cdots  \int  U}, but with less integrals).
     Then, @f{\bf  g} just acts on each @f{U}
     (@f{U} is a local operator in the undeformed theory, @f{\bf  g} acts on them).
  }

@hrule[]

Because we had some freedom in the choice of counterterms, the map @f{F} does
not necessarily commute with the action of @f{\bf  g}. Maybe we can choose counterterms
with some care, so the resulting @f{F} @bold{ does} commute with @f{\bf  g}?
Generally speaking we can not, there are @spn[attn]{obstacles}. 

@page["SUGRA on AdS" #:tag "AdS" #:showtitle #t]

Consider Type IIB SUGRA in @f{AdS_{1,4}\times  S^5} and @f{N=4} supersymmetric Yang-Mills on the boundary.
We can proceed in two ways, which are equivalent because of the AdS/CFT duality: 

@subpage[1 @elem{1. Renormgroup flow on the boundary} #:tag "sec:RenormgroupOnBoundary"]



@subpage[1 @elem{2. Classical solutions of SUGRA in the bulk} #:tag "sec:ClassicaSUGRASolutionsInTheBulk"]

        We fix some map
        from the space of solutions of the linearized SUGRA equations to the space of
        nonlinear solutions. Then we study the deviation of this map from being
        @f{\bf  g}-invariant.

@bold{Here we will discuss the second approach.}


@page["Beta-deformation" #:tag "BetaDeformation" #:showtitle #t]

We will now consider the case of  so-called beta-deformation.

@bold{Linearized beta-deformations} transform in the following representation:
@e[#:label "DefCalH"]{
   {\cal  H} = {({\bf  g}\wedge  {\bf  g})_0\over  {\bf  g}}
   }
where the subindex @f{0} means zero internal commutator;
@f{x\wedge  y\in  ({\bf  g}\wedge  {\bf  g})_0}
has @f{[x,y]=0}.

The nonlinear solutions were studied in:

@div[greenbox]{
         O.Aharony, B.Kol, and S.Yankielowicz,
         @italic{On exactly marginal deformations of @f{N = 4} SYM and type IIB supergravity
                    on @f{AdS_5 \times S^5}}
         @hyperlink["https://arxiv.org/abs/hep-th/0205090"]{https://arxiv.org/abs/hep-th/0205090}
       }

It was shown that the renormalization
of beta-deformation is again a beta-deformation, and the anomalous
dimension is an expression cubic in the deformation parameter. 

In @seclink["Obstacle"]{our language} this corresponds to the obstacle starting at @f{v_2}.
One would thing that the relevant cohomology group is:

@e{
   H^1\left ({\bf  g}_{\rm  ev}, \mbox{Hom} ({\cal  H}^{\otimes  3} , {\cal  H})\right )
   }
But this cohomology group is zero, because @f{\mbox{Hom} \left ({\cal  H}^{\otimes  3} , {\cal  H})\right )}
is finite-dimensional.

What actually happens is:
@e[#:label "BosonicCocycle"]{
   [v_2] = \phi _{\rm  bos} \in  H^1\left ({\bf  g}_{\rm  ev}, \mbox{Hom} ({\cal  H}^{\otimes  3} , \widehat{\cal  H} )\right )
   }
where @f{\widehat{\cal  H}} is some infinite-dimensional extension of @f{\cal  H}.

We will now explain this extension.


@page[@elem{AdS notations} #:tag "sec:AdSNotations" #:showtitle #t]

@subpage[1 @elem{Embedding formalism} #:tag "sec:EmbeddingFormalism"]

We here consider massless Laplace equation in @f{AdS_D}.
We realize @f{AdS_D} as a hyperboloid in @f{{\bf  R}^{2,D-1}} parametrized by coordinates @f{Z,\overline{Z} ,X^1,\ldots ,X^{D-1}}.
The equation of the hyperboloid is: 
@e[#:label "Hyperboloid"]{
   |Z|^2 - \vec{X} ^2 =  1
}
The Laplace operator in @f{{\bf  R}^{2,D-1}} can be written as 
@(align
  r.l.n
  @`(@,f{}
   @,f{2{\partial \over \partial  Z}{\partial \over \partial \overline{Z} }
        - {\partial \over \partial \vec{X} }{\partial \over \partial \vec{X} }
        \; =\; 
        (\Delta  + D - 1)\Delta  + {\bf  L}
        }
   "")
)
where  @f{\Delta \in \mbox{Vect}({\bf  R}^{2,D-1})} is the Euler vector field (rescaling of @f{X} and @f{Z}) and @f{{\bf  L}}  is the Laplace operator on @f{AdS_D}.
Therefore, on @bold{harmonic functions} @f{{\bf  R}^{2,D-1}\rightarrow {\bf C}}:
@e{
   {\bf  L} = - \Delta (\Delta  + D - 1)
}
@(centered
@div[greenbox]{Eigenfunctions
               of the Laplace operator on the hyperboloid can be obtained as restrictions of harmonic
               functions on @f{{\bf  R}^{2,D-1}}}
)
                              
Our space-time is not just @f{AdS_D}, but @f{AdS_D\times  S^D}. The formulas for Laplace operator on the sphere
are completely analogous. To distinguish between AdS and sphere, we use indices @f{\rm  A} and @f{\rm  S}:
@f{{\bf  L}_{\rm  A}}, @f{\Delta _{\rm  A}}, @f{{\bf  L}_{\rm  S}}, @f{\Delta _{\rm  S}}. The total Laplace operator
on @f{AdS_5\times  S^5} is:
@e{
   \square  = {\bf  L}_A - {\bf  L}_S
   }
Therefore, for the scalar function to be harmonic in @f{AdS_D\times  S^D}:
@e{
   \Delta _{\rm  A}(\Delta _{\rm  A} + D - 1) \; =\; \Delta _{\rm  S}(\Delta _{\rm  S} + D - 1)
   }
This means:
@e{
   \mbox{\tt \small  either }  \Delta _{\rm  A} = \Delta _{\rm  S} \quad  \mbox{\tt \small  or }  \Delta _{\rm  A} = - \Delta _{\rm  S} - (D-1)
   }


@subpage[1 @elem{Massless scalar in @f{AdS_D\times  S^D}} #:tag "sec:MasslessScalar"]

Let us parametrize @f{S^D} by a unit vector @f{{\bf  N}\in  {\bf  R}^{D+1}}.
Suppose that the @f{S^D} dependence is a harmonic polynomial @f{Y({\bf  N})}
of order @f{\Delta _{\rm  S}}. The full solution is:
@e{
   \phi(Z,\overline{Z} ,\vec{X} ) Y({\bf  N})
   }
where @f{\phi} is a harmonic function of order @f{\Delta_S} in 
@f{{\bf  R}^{2,D-1}} parametrized by coordinates @f{Z,\overline{Z} ,X^1,\ldots ,X^{D-1}}.

Those solutions where @f{\phi} is a @bold{polynomial} form a finite-dimensional representation.

When we allow @f{\phi} to be a @bold{rational function} with denominator powers of @f{\overline{Z}Z},
they form an infinite-dimensional representation. That infinite-dimensional representation
is the @f{\widehat{\cal H}} of Eq. (@ref{BosonicCocycle}).




@page[@elem{Lifting of @f{\Psi} to superalgebra} #:tag "sec:ObstacleIsQuadratic" #:showtitle #t]

Let @f{Q_{\rm  ev}} be the part of @f{Q}  involving only the ghosts
of @spn[attn]{ even} generators of @f{{\bf  g}} (essentially, all the ghosts of odd generators all put to zero).
The @f{\phi _{\rm  bos}} of Eq. (@ref{BosonicCocycle}) is annihilated by @f{Q_{\rm  ev}}.
What happens if we act on @f{\phi _{\rm  bos}} with the full @f{Q}, including the terms containing odd indices?
Can we extend @f{\phi _{\rm  bos}} to a cocycle @f{\phi} of @f{{\bf  g}}?

To answer this question, let us look at the spectral sequence
corresponding to  @f{{\bf  g}_{\rm  ev}\subset  {\bf  g}}.
At the first page we have (in these formulas we put @f{V = \mbox{Hom} ({\cal  H}^{\otimes  3} , \widehat{\cal  H})}):
@(align
  r.l.n
  @`(@,f{E_1^{0,1} \;  = \;}
   @,f{H^1({\bf  g}_{\rm  ev}; V)}
   "")
@`(@,f{E_1^{1,0} \; =\;}
   @,f{H^0({\bf  g}_{\rm  ev}; \mbox{Hom} ({\bf  g}_{\rm  odd}, V)) \; =\; 
    \mbox{Hom} _{{\bf  g}_{\rm  ev}}({\bf  g}_{\rm  odd}, V)}
   "")
)
Our @f{\phi _{\rm  bos}} belongs to @f{E_1^{0,1}}. The first obstacle lives in
@e{
   E_1^{1,1}= H^1({\bf  g}_{\rm  ev}; \mbox{Hom} ({\bf  g}_{\rm  odd}, V))
   }
We actually know that the SUGRA solution exist. Therefore this obstacle automatically vanishes.
But there is another obstacle, which arizes when we go to the second page. It lives in:
@(align
  r.l.n
  @`(@,f{E_2^{2,0} \; =\;}
   @,f{H^2({\bf  g},{\bf  g}_{\rm  ev};V)
    = H^2\left ({\bf  g},{\bf  g}_{\rm  ev};\mbox{Hom} ({\cal  H}^{\otimes  3} , \widehat{\cal  H} )\right )\; =}
   "")
@`(@,f{\; =\;}
   @,f{H^2\left ({\bf  g},{\bf  g}_{\rm  ev};\mbox{Hom} ({\cal  H}^{\otimes  3} , {\cal  H})\right )}
   "")
)
We used the fact that relative
cochains are @f{{\bf  g}_{\rm  ev}}-invariant, therefore the cocycles automatically fall into the finite-dimensional
@f{{\cal  H}\subset \widehat{\cal  H}}. In fact, this obstacle does not have to be zero, because
there is something that can cancel it. Remember that @f{\Psi} is generally speaking not annihilated
by @f{Q}, but rather satisfies Eq. (@ref{MaurerCartan}). And, in fact, there is a nontrivial cocycle:
@e[#:label "CocycleInS2HH"]{
   \psi  \in  H^1({\bf  g}, \mbox{Hom} (S^2{\cal  H}, {\cal  H}))
   }
We conjecture
that the supersymmetric extension @f{\phi} of @f{\phi _{\rm  bos}} indeed exists, but instead of
satisfying @f{Q\phi =0} satisfies:
@e{
   Q\phi  = [\psi ,\psi ]
   }
  This conjecture should be verified by explicit computations, which we leave for future work.
  It may happen that the obstacle which would take values in the cohomology group of Eq. (@ref{CocycleInS2HH})
  actually vanishes for some reason.

We will now describe @f{\psi} of Eq. (@ref{CocycleInS2HH}). 

@page[@elem{Cocycle in @f{\mbox{Hom} (S^2{\cal  H}, {\cal  H})}} #:tag "CocycleInS2HtoH" #:showtitle #t]

@subpage[1 @elem{Step 1: construct an element of @f{H^1\left ({\bf  g}, \mbox{Hom} ({\bf  g}, {\cal  H})\right )}} #:tag "CocycleStep1"]

Consider an element of @f{H^1\left ({\bf  g}, \mbox{Hom} ({\bf  g}, {\cal  H})\right )} corresponding
to the extension:
@e{
   0 \longrightarrow  {\cal  H}
   \longrightarrow  {{\bf  g}\wedge  {\bf  g}\over  {\bf  g}}
   \longrightarrow  \widehat{\bf  g} \longrightarrow  0
   }
(Remember that @f{\cal  H} is defined in Eq. (@ref{DefCalH}).)

@subpage[1 @elem{Step 2: compose it with an intertwiner @f{S^2{\cal  H} \rightarrow  \widehat{\bf  g}}} #:tag "CocycleStep2"]

There exists a @f{\bf  g}-invariant map
@e[#:label "FromS2HToG"]{
   S^2{\cal  H} \rightarrow  \widehat{\bf  g}
   }
Composing it with the element of @f{H^1\left ({\bf  g}, \mbox{Hom} ({\bf  g}, {\cal  H})\right )} we
get a class in @f{H^1({\bf  g}, \mbox{Hom} (S^2{\cal  H}, {\cal  H}))}.

We will now describe this intertwiner.

@subpage[1 @elem{Construction of the intertwiner of Eq. (@ref{FromS2HToG})} #:tag "sec:IntertwinerFromS2HToG"]

@subpage[2 @elem{Intertwiner: algebraic preliminaries} #:tag "sec:IntertwinerPreliminaries"]

Suppose that we have an associative algebra @f{A}. For any @f{x_1\otimes \cdots \otimes  x_{k} \in  A^{\otimes  n}} consider their product:
@e{
   \mu (x_1\otimes \cdots \otimes  x_{k}) = x_1 \cdots  x_{k}\in  A
   }
In particular, take @f{A = \mbox{Mat} (m|n)} — the algebra of supermatrices. Let us view the exterior product @f{\Lambda ^k A = A\wedge \cdots \wedge  A} as a subspace in @f{A^{\otimes  k}}.

For any element @f{x_1\otimes \cdots \otimes  x_{2k} \in  \Lambda ^{2k} A} we define:
@e{
   \langle  x_1\wedge \cdots \wedge  x_{2k}\rangle  = \mu (x_1\wedge \cdots \wedge  x_{2k})
   }
It defines a map:
@e{
   \Lambda ^{2k}{\bf  pl}(m|n) \rightarrow  {\bf  sl}(m|n)
   }



@subpage[2 @elem{Description of the intertwiner} #:tag "Intertwiner"]

For @f{B_1} and @f{B_2} belonging to @f{\cal  H}, we define:
@(align
  r.l.n
  @`(@,f{f\; :\;}
   @,f{S^2{\cal  H} \rightarrow  {\bf  g}}
   @,label{Intertwiner})
@`(@,f{f(B_1\wedge  B_2) \; =\;}
   @,f{\langle  B_1\wedge  B_2\rangle  }
   "")
)


@; ---------------------------------------------------------------------------------------------------
@(bystro-close-connection bystro-conf)
@disconnect[formula-database]

  
