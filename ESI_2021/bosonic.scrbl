#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require "defs.rkt" bystroTeX/common bystroTeX/slides (for-syntax bystroTeX/slides_for-syntax))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(bystro-set-css-dir (build-path (find-system-path 'home-dir) "a" "git" "amkhlv" "profiles" "talk"))
@(define bystro-conf   
   (bystro (bystro-connect-to-server (build-path (find-system-path 'home-dir) ".config" "amkhlv" "latex2svg.xml"))
           "bosonic/formulas.sqlite"  ; name for the database
           "bosonic" ; directory where to store image files of formulas
           25  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@(define singlepage-mode #f)
@(bystro-def-formula "formula-enormula-humongula!")


@title[#:style '(no-toc no-sidebar)]{Bosonic String}

@bystro-source[]

@bystro-ribbon[]

@table-of-contents[]

@bystro-ribbon[]

@page["Introduction to BV" #:tag "IntroductionBV" #:showtitle #t]

@bystro-local-toc[]

@subpage[1 "Lightning intro to BV" #:tag "sec:IntroBV"]

@subpage[1 "Integration in LAG" #:tag "sec:IntegrationInLAG"]

String theory requires one more element:
@(itemlist
  @item{integration over families of Lagrangian submanifolds}
  )

@subpage[2 "BV BRST" #:tag "sec:BVBRST"]

Let me explain this in the context of BV-BRST. Let @f{H} be the gauge group with
gauge Lie algebra @f{\bf h}. Remember that the space of fields
is @f{\Pi  {\bf h} \times X}, where @f{\Pi \bf h} is parametrized by @f{c}
and @f{X} by @f{\phi}. The Main Action has the form:
@e{
   S_{\rm BV}(\phi,\phi^{\star}) = S_{\rm cl}(\phi) + c^A v_A^i(\phi)\phi^{\star}_i +
   {1\over 2} c^A c^Bf_{AB}^C c^{\star}_C
   }
@comment{
         In fact the bosonic string belongs to this class.
         }
For quantization we need to restrict @f{S_{\rm BV}} to a Lagrangian submanifold,
and then take path integral over @f{\phi}.
How do we choose a Lagrangian submanifold? A very naive choice is @f{\phi^{\star}=0},
then the restriction would be just @f{S_{\rm cl}} --- degenerate, can not quantize.

@subpage[2 "Constraint and conormal bundle" #:tag "sec:ConormalBundle"]

As the next attempt, let us try the following construction. Let @f{X} denote the space
of fields @f{\phi}. Consider some subspace @f{Y\subset X} defined by some equations
(``constraints''). We want @f{Y} to be transverse to the the gauge orbits.
We choose:
@e[#:label "rotated-lag"]{
L(Y) \;=\; \Pi(TY)^{\perp}\times \mbox{[$c$-ghosts]}\;=\; \Pi(TY)^{\perp}\times \Pi {\bf h}
}
This is a Lagrangian submanifold. 

@subpage[2 "Degeneracy on conormal bundle" #:tag "sec:DegeneracyOnConormalBundle"]

Let us  study the quadratic terms in the expansion of @f{S_{\rm BV}|_{L(Y)}}.
Suppose that the extremum of @f{S_{\rm cl}(\phi)} is at @f{\phi = 0}.
We assume that the vector fields @f{v^i_A} do not vanish at @f{\phi = 0}:
@e{
   v^i_A(0) = T^i_A
   }
Then:
@e[#:label "RestrictionToConormal"]{
                                    \left(\left. S_{\rm BV} \right|_{L(Y\,)}\right)_{quadratic}
                                    \;=\;
                                    \left(\left.S_{\rm cl}(\phi)\;\right|_{\phi\in Y}\right)_{quadratic} \;+\; T_A^ic^A\phi^{\star}_i\;|_{\phi^{\star}\in \Pi(T\,Y\,)^{\perp}}
}

@subpage[3 @elem{@f{\phi\phi} terms} #:tag "sec:PhiPhiTerms"]

The quadratic terms for @f{\phi} come from @f{S_{\rm cl}(\phi)}.
We assume that all the degeneracy of @f{S_{\rm cl}} is due to gauge symmetry.
It is usually not difficult to choose @f{Y} transverse to gauge orbits, so let us assume that.
Then the quadratic term in @f{\left.S_{\rm cl}(\phi)\;\right|_{\phi\in Y}}
is a nondegenerate quadratic form.

@subpage[3 @elem{@f{c\phi^{\star}} terms} #:tag "sec:CPhiStarTerms"]

The quadratic term for @f{c} and @f{\phi^{\star}} comes from the second term:
@f{T_A^ic^A\phi^{\star}_i\;|_{\phi^{\star}\in \Pi(T\,Y\,)^{\perp}}}.
(It is perhaps useful to think of this term as a pairing
    @f{\left\langle \phi^{\star},T\langle c\rangle \right\rangle}.)
We assume that @f{T:\;{\bf h}\rightarrow T_0X} is an injection.
Then  @f{\left\langle \phi^{\star},T\langle c\rangle \right\rangle}
is degenerate in @f{\phi^{\star}}, in the following sense: exist @f{\phi^{\star}_{(0)}} such that:
@e[#:label "Kernel"]{
   \forall c:\;\left\langle \phi^{\star}_{(0)}, T\langle c\rangle\right\rangle = 0
   }
This degeneracy means that the path integral is not satisfactory.
If @f{\bf h} is bosonic, it would be zero. If gauge symmetries form a superalgebra, then
it is ill-defined.

@subpage[3 @elem{Interpretation of zero modes} #:tag "sec:InterpretationOfZeroModes"]
           
Those @f{\phi_{(0)}^{\star}} which satisfy Eq. (@ref{Kernel})
are precisely such elements of @f{\Pi T^*X} which vanish both on @f{TY}
@bold{and} on the tangent space to the orbits of @f{\bf h}.
This space has the following geometrical interpretation.
Let us consider gauge transformations of @f{Y\subset X}:
@e{
   \left\{ hY | h\in H \right\}
   }
Let us restrict to @f{h} belonging to a small neighborhood of the unit in @f{H}.
Let us consider their union:
@e{
   \bigcup\limits_{h\in H} hY 
   }
Remember that we assume that the orbits of @f{H} are transverse to @f{Y}.
This implies that @f{h_1Y} and @f{h_2Y} do not intersect for @f{h_1} and @f{h_2} close enough
to the unit of @f{H}. The union @f{\bigcup\limits_{h\in H} hY} seems to be actually well defined. 
But let us ask the question: does @f{\bigcup\limits_{h\in H} hY} coincide with the
whole @f{X} in the vicinity of @f{Y\subset X}?
In fact, the existence of nonzero solutions to Eq. (@ref{Kernel}) is equivalent
to @f{\bigcup\limits_{h\in H} hY} being strictly a subset of @f{X}. If the space
of solutions is finite-dimensional, then the codimension @f{\bigcup\limits_{h\in H} hY}
in @f{X} is equal to the number of linearly independent solutions of Eq. (@ref{Kernel}).
In fact, the space of solutions to Eq. (@ref{Kernel}) is (as a linear space) @bold{dual to}
the fiber of the normal bundle at the point @f{0\in X}:
@e{
   N_0 \bigcup\limits_{h\in H} hY
   }
where @f{\bigcup\limits_{h\in H} hY} is considered a submanifold of @f{X}.

This means that our gauge slice @f{Y\subset X}, even after being translated by
gauge transformations, @bold{does not cover the whole @f{X}}.
But we do want to integrate over the @bold{whole} @f{X} !
At first sight, it seems that the  problem can be easily solved.
Simply admit the wrong choice of @f{Y}: our chosen @f{Y\subset X} was too small!
Let us extend it, pick a larger @f{Y}!

But it turns out that there are examples (bosonic string amonth them) when we cannot
actually enlarge @f{Y}. The problem is, that we have to stick to particular class of constraints,
which basically respects the requirement of @bold{locality}. After we restrict to
the Lagrangian submanifold, we should have a local QFT.

@subpage[2 "Need to integrate in LAG" #:tag "sec:NeedToIntegrate"]

The solution is to @bold{integrate over families of gauge conditions @f{Y\subset X}}.
Moreover, there is canonical integration measure (more precisely, a PDF) on any families
of Lagrangan submanifolds, not only on families of conormal bundles.
If gauge fermions @f{\Psi_1,\ldots,\Psi_n} define @f{n} tangent vectors to @f{\rm LAG}
at the point @f{L\in {\rm LAG}} then the measure is:
@e{
   \int_L \Psi_1\cdots \Psi_n \; \exp(S_{\rm BV})
   }
How does this formula work in the case of family of conormal bundles?
We combine @f{n}-forms into a PDF:
@(align
  r.l.n
  `(@,f{\Omega \;=\;}
       @,f{\int_L \exp\left(S_{\rm BV}|_L + \alpha|_L\right)}
       "")
  `(@,elem{where }
        @,f{\alpha = \phi^{\star}_i d\phi^i}
        "")
  )
In this formula, @f{\alpha = \phi^{\star}_i d\phi^i} should be understood as follows.
Given an infinitesimal variation of @f{Y\subset X} we pick any @f{\dot{\phi}} representing
this variation, and compute @f{\langle \alpha, \dot{\phi}\rangle}. Remember that @f{\phi^{\star}}
is restricted to belong to the conormal fiber @f{\Pi (TY)^{\perp}};
therefore @f{\langle \alpha, \dot{\phi}\rangle} does not depend on the choice of
@f{\dot{\phi}}.
(We can add to @f{\dot{\phi}} any vector tangent to @f{Y\subset X} and
    @f{\langle \alpha, \dot{\phi}\rangle} will not change.)

Moreover, notice that the equations of motion of the action of Eq. (@ref{RestrictionToConormal})
project @f{\phi^{\star}} from @f{\Pi (TY)^{\perp}} down to @f{\Pi (TY + T{\cal O})^{\perp}}.
This means that the resulting PDF is @f{\bf h}-base.
                                                                             
@subpage[1 "Note on equivariant BV" #:tag "sec:EquivariantBV"]

The BV phase space is:
@e{
   M = \Pi T^* \left({\Pi TH\times X\over H}\right)
   }
Here we quotient by the action of @f{H} where @f{H} acts on @f{\Pi TH} @bold{from the right}.
The @f{Q_{\rm BRST}} comes from the canonical nilpotent vector field on @f{\Pi TH}.
We want @f{Q_{\rm BRST}} to preserve the volume on @f{\Pi TH\times X\over H}. This is some
condition on the trace of the structure constant and the @f{div}'s of generators.

But the @bold{left} action of @f{H} on @f{\Pi TH} remains. It is generated by an exact Hamiltonian:
@e{
   H\langle \xi\rangle = \Delta (\xi^{\alpha} c^{\star}_{\alpha})
   }
This means that:
@div[greenbox]{Gauge symmetries act on the BV phase space}
An equivariant form is given by:
@e{
   \int_L \exp\left(S_{\rm BV} + \Psi + {\bf t}^{\alpha} c^{\star}_{\alpha}\right)
   }
Notice that @f{c^{\star}} vanishes on Lagrangian submanifolds which are obtained
by the conormal bundle construction. 

@page["Bosonic string worldsheet  theory" #:tag "IntroductionString" #:showtitle #t]

@bystro-local-toc[]

@subpage[1 "BV phase space and Main Equation" #:tag "sec:BosonicBV"]

Bosonic string worldsheet theory is 2D gravity coupled to 2D matter.

The word ``matter'' means target space coordinates @f{X^0,\ldots, X^{D-1}}
(@italic{i.e.} there are @f{D} matter fields).

The word ``gravity'' could mean two different things:
@(itemlist
  @item{Non-critical: dynamical metric with Liouville action}
  @item{Critical (@f{D=26}): dynamical complex structure}
  )
In any case, there is gauge symmetry: @f{\mbox{Diff}(\Sigma)}.

We therefore apply the Faddeev-Popov procedure.
In the BV language, we add ghost fields @f{c^{\alpha}(z,\bar{z})} corresponding
to vector fields on the worldsheet with fermionic statistics. 


Let us concentrate on @bold{critical} string. The BV Main action is:
@e{
   S_{\rm BV} =
   \int_{\Sigma}
   \left(
         dx^a \wedge I.dx^a
         +
         ({\cal L}_c x) x^{\star} + ({\cal L}_c I) I^{\star} + {1\over 2} ({\cal L}_c c) c^{\star}
         \right)
   }
It satisfies the classical Main Equation:
@e{
   \{S_{\rm BV}, S_{\rm BV}\} = 0
   }
In this case @f{X} is parameterized by @f{(x,I,c)}.


@subpage[1 "Lagrangian submanifold" #:tag "sec:BosonicLagrangianSubmanifold"]

We pick some fixed complex structure @f{I^{(0)}} and introduce the following constraint:
@e{
   I = I^{(0)}
   }
This equation defines @f{Y\subset X}.
In genus @f{g\geq 2} this is a good gauge condition,
in the sense that the orbits of @f{\bf h} are transverse to @f{Y}. 
Our Lagrangian submanifold is @f{\Pi (TY)^{\perp}}.
In this case @f{Y} is parameterized by:
@e{
   \phi = x \mbox{ \tt and } c
   }
The conormal fiber is parametrized by:
@e{
   \phi^{\star} = I^{\star}
   }


@subpage[1 "Expansion of Main Action in the vicinity of Lagrangian submanifold" #:tag "sec:Expansion"]

@short-intro{This is joint work in progress
                  with my students Eggon Viana and Vinícius Bernardes da Silva.}

We will now work in a vicinity of a point in @f{\Sigma}. We may then introduce coordinates
@f{(z,\bar{z})} so that:
@(align
  r.l.n
  `(@,f{I^{(0)}\;:\;} @,f{T\Sigma \rightarrow T\Sigma} "")
  `(@,f{I^{(0)}\;=\;}
       @,f{
           \begin{pmatrix}
           i & 0 \\
           0 & - i
           \end{pmatrix}
           }
       "")
  )
At each point on @f{\Sigma} a complex structure is a  matrix satisfying @f{I^2 = -{\bf 1}}.
It can be parametrized by @f{m\in {\bf CP}^1}:
@(align
  r.l.n
  `(@,f{I\;=\;}
       @,f{
           \begin{pmatrix}
           I^z_z & I^z_{\bar{z}} \\ I^{\bar{z}}_z & I^{\bar{z}}_{\bar{z}}
           \end{pmatrix}
           =
           \begin{pmatrix}
           i \sqrt{ 1 + m \bar m } & m \\
           \bar m & - i \sqrt{ 1 + m \bar m }
           \end{pmatrix}
           }
       "")
  )
Let @f{\cal I} denote the manifold (@f{\simeq {\bf CP}^1}) of such matrices.
Then @f{\Pi T^* {\cal I}} is parametrized by @f{I} and @f{I^{\star}}. 
The odd symplectic form is:
@e{
   \omega = \mbox{tr}(dI^{\star}\wedge dI)
   }
with the following gauge symmetry of @f{I^{\star}}:
@e{
   \delta_{\eta} I^{\star} = \eta I + I \eta
   }
Let us gauge fix @f{I^{\star}} to:
@e{
   I^{\star} = \left( \begin{array}{cc} 0 & b \cr \bar{b} & 0 \end{array} \right)
   }
This is called ``@f{b}-ghost''.
It is defined so that @f{m} and @f{b} are Darboux coordinates of the BV symplectic form:
@e{
   \omega = dm\wedge d\bar{b} + d\bar{m}\wedge db
   }
The  expansion of the BV Main Action reads (renaming @f{m} as @f{b^{\star}}):
@(align
  r.l.n
  `(@,f{S_{\rm BV} \;=\;}
       @,f{\int_{\Sigma} d^2 z \left( \sqrt{ 1 + b^{\star} \bar b^{\star} } \partial x \bar \partial x + \frac{i}{2} b^{\star} ( \partial x )^2 - \frac{i}{2} \bar b^{\star} ( \bar \partial x )^2 \right)\;+}
       "")
  `(""
    @,f{+\;
        \int_{\Sigma} \bigg( \mathcal{L}_c x x^\star + \frac{1}{2} \mathcal{L}_c c c^\star \bigg)
        +}
    "")
  `(""
    @,f{+ \int_{\Sigma}
          \left( ( c \partial  + \bar c \bar \partial + \partial c - \bar \partial \bar c ) b^{\star} - 2i \bar \partial c \sqrt{ 1 + b^{\star} \bar b^{\star} } \right) b}
    "")
  `(""
    @,f{+\;\int_{\Sigma}
           \left(
                 ( c \partial + \bar c \bar \partial + \bar \partial \bar c - \partial c )
                 \bar b^{\star}
                 + 2i \partial \bar c \sqrt{ 1 + b^{\star} \bar b^{\star} }
                 \right) \bar b
                    }
    "")
  )

@subpage[2 "Zero order term (action)" #:tag "sec:ZeroOrderTerm"]

The restriction to @f{L} is:
@e{
   S = \int \left(
                  \partial  x \wedge \overline{\partial} x
                  +
                  b\overline{\partial}c
                  +
                  \overline{b}\partial\overline{c}
                  \right)
   }
Notice that @f{b} and @f{\bar{b}} only enter linearly.

@subpage[2 "First order term (BRST operator)" #:tag "sec:FirstOrderTerm"]

The BRST operator is read from the term linear in antifields. In particular:
@(align
  r.l.n
  `(""
    @,f{Qb = T}
    "")
  `(""
    @,f{Q\overline{b} = \overline{T}}
    "")
  `(@,elem{where }
         @,f{T = (\partial x)^2 + \ldots}
         "")
  `(""
    @,f{\overline{T} = (\overline{\partial}x)^2 + \ldots}
    "")
  )

@subpage[2 "Second order term (a bracket)" #:tag "sec:SecondOrderTerm"]

@(align
  r.l.n
  `(""
    @,f{\pi =~ \int d^2 z \left( \partial x \bar \partial x + 2i ( b \bar \partial c - \bar b \partial \bar c ) \right) \frac{ \delta }{ \delta b }\wedge \frac{ \delta }{ \delta \bar b }}
    @,label{Bracket})
  )
This defines some bracket on the space of field configurations.
This bracket being nonzero implies that the BRST operator is only nilpotent on-shell.
It is highly degenerate (only contains derivatives in the @f{b}- and @f{\overline{b}}-directions).

@subpage[3 "Nonlinearity of the space of deformations" #:tag "sec:Nonlinearity"]

The nonzero bracket leads to the following effect.
Let us deform by gauge fermion @f{\int \mu b + \bar{\mu} \bar{b}}.
This corresponds to the change of the worldsheet complex structure. For, example, the
matter field kinetic term deforms as follows:
@e[#:label "NaiveDeformationOfMatterKinTerm"
   ]{
     \int d^2z
     \left(
           \partial x \wedge \overline{\partial}x
           \right)
     \mapsto
     \int d^2z
     \left(
           \partial x \overline{\partial}x
           + \mu (\partial x)^2
           + \overline{\mu} (\overline{\partial} x)^2
           \right)
   }
But the nonzero bracket of Eq. (@ref{Bracket}) implies that @bold{@f{Q} also deforms}.
Therefore Eq. (@ref{NaiveDeformationOfMatterKinTerm}) is only valid in the first
order in @f{\mu} and @f{\overline{mu}}. The correct formula up to the second order is:
@e[#:label "DeformationOfMatterKinTerm"
   ]{
     \int d^2z
     \left(\partial x  \overline{\partial}x\right)
     \mapsto
     \int d^2z
     \left(
           \partial x  \overline{\partial}x
           + \mu (\partial x)^2
           + \overline{\mu} (\overline{\partial} x)^2
           - \mu\overline{\mu} \partial x \overline{\partial}x
           \right)
     }
This should be said as follows. The BRST-exact deformations sweep the space of kinetic terms
of the form:
@e[#:label "CorrectDeformation"
   ]{
   \int d^2 z \sqrt{\mbox{det}\;h_{\bullet\bullet}} h^{\alpha\beta} \partial_{\alpha} x\partial_{\beta} x
   }
while the most general expression would be:
@e[#:label "GeneralDeformation"
   ]{
     \int d^2 z a^{\alpha\beta} \partial_{\alpha} x\partial_{\beta} x
     }
This, in principle, has arbitrary symmetric tensor @f{a^{\alpha\beta}}. The ones of the
form Eq. (@ref{CorrectDeformation}) satisfy the nonlinear equation:
@e[#:label "UnimodularA"
   ]{
     \mbox{Det}\; a = 1
     }
where @f{\mbox{Det}} should be understood in the following sense. Notice that @f{a} is geometrically
a map @f{\Lambda^2 T\Sigma \rightarrow S^2T\Sigma}. This uniquely defines a map
@f{(\Lambda^2 T\Sigma)^{\otimes 2} \rightarrow (\Lambda^2 T\Sigma)^{\otimes 2}} which we call
@f{\mbox{Det} \;a}.
The space of BRST deformations is described by a nonlinear Eq. (@ref{UnimodularA}).

@subpage[3 "Partial on-shell condition" #:tag "sec:PartialOnShell"]

The bracket defined by Eq. (@ref{Bracket}) does not satisfy the Jacobi identity.
Can we define a Poisson bracket satisfying the Jacobi identity, out of the expansion of @f{S_{\rm BV}}?


One might try to say that  satisfies Jacobi identity on-shell and up to @f{Q} of something,
but this is very tricky. Although we can impose on-shell conditions, it is not reasonable
to expect the Poisson bivector be  tangent to on-shell conditions.

But at least in  the particular case of bosonic string, we can impose partial on-shell condition,
by asking that the action be stationary, but only  under the variations of @f{b} and @f{\overline{b}}:
@e[#:label "EqMforC"
   ]{
     \partial\overline{c} = \overline{\partial} c = 0
     }
The key observation is that the action depends on @f{b} and @f{\overline{b}} only linearly. Therefore
the bivector defined by Eq. (@ref{Bracket}) is tangent to Eqs. (@ref{EqMforC}).
The on-partial-shell bracket satisfies the Jacobi identity, in a rather trivial way:
@e[#:label "PiOnShell"
   ]{
     \pi_{os}
     =~
     \int d^2 z \left( \partial x(z,\overline{z}) \bar \partial x(z,\overline{z}) \right)
     \frac{ \delta }{ \delta b (z,\overline{z})}\wedge \frac{ \delta }{ \delta \bar b (z,\overline{z})}
     }
(by being ``essentially constant''). Moreover, they commute with @f{Q}, once we
impose our partial on-shell conditions. (This is  equivalent to the
                                              conformal invariance of Eq. (@ref{PiOnShell}).)


@subpage[2 "Higher order terms" #:tag "sec:NthOrder"]

Similarly there are higher brackets, which are all conformally invariant and
trivially satisfy the Jacobi identities:

@e{
   \int d^2z \; \partial x \overline{\partial} x \;
   {\delta\over\delta b(z,\overline{z})}
   \wedge\cdots\wedge
   {\delta\over\delta b(z,\overline{z})}
   \wedge
   {\delta\over\delta \overline{b}(z,\overline{z})}
   \wedge\cdots\wedge
   {\delta\over\delta \overline{b}(z,\overline{z})}
   }

They are all conformally invariant, as required by commutating with @f{Q}.

@page["Action of diffeomorphisms" #:tag "Diffeomorphisms" #:showtitle #t]

A vector field @f{v\in\mbox{Vect}\,\Sigma} acts on the BV phase space as an infinitesimal
diffeomorphism.  The action is generated by the Hamiltonian:
@e{
   H\langle v\rangle =
   \Delta \left(
                \int_{\Sigma} v^{\alpha} c^{\star}_{\alpha}
                \right)
   +
   \left\{
   \;
   \int_{\Sigma} v^{\alpha} c^{\star}_{\alpha}
   \;,\;
   \int_{\Sigma} v^{\alpha} c^{\star}_{\alpha}
   \right\}
   }
Notice that the second term is zero because @f{\{c^{\star},c^{\star}\}=0}.

We believe that this is a fundamental equation which should be present in any string
worldseet theory:
@(align
  r.l.n
  `(@,f{H\langle v \rangle \;=\;}
       @,f{\Delta i(v) + {1\over 2}\{ i(v),i(v) \}}
       "")
  `(@,f{\{H\langle v_1 \rangle, i(v_2)\} \;=\;}
       @,f{\left.{d\over dt}\right|_{t=0} i\left(e^{t[v_1,\_]} v_2\right)}
       "")
  )
where @f{i(v)} does not have to be linear in @f{v}.


@; ---------------------------------------------------------------------------------------------------
@(bystro-close-connection bystro-conf)
@disconnect[formula-database]

  
