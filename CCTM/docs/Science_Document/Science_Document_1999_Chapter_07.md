Chapter 7 
==============

NUMERICAL TRANSPORT ALGORITHMS FOR THE COMMUNITY MULTISCALE 

AIR QUALITY (CMAQ) CHEMICAL TRANSPORT MODEL IN GENERALIZED 

COORDINATES 

Daewon W. Byun, Jeffrey Young, and Jonathan Pleim

Atmospheric Modeling Division 

National Exposure Research Laboratory 
U.S. Environmental Protection Agency 

Research Triangle Park, NC 27711 

M. Talat Odman+ and Kiran Alapaty 

MCNC-Environmental Programs 

P.O. Box 12889, 3021  Cornwallis Road 
Research Triangle Park, NC 27709-2889 

ABSTRACT 

The transport l?rocesses in the atmosphere primarily consist of advection and diffusion, except 
for the mixing of pollutants by the parameterized subgrid-scale clouds.  In this chapter, numerical 
algorithms for advection, vertical diffusion, and horizontal diffusion implemented in the 
Community Multiscale Air Quality (CMAQ) chemical transport models are di.scussed.  To 
provide the CMAQ system with multiscale capability, we have formulated the transport 
processes, both advection and diffusion, in conservation (i.e., flux) forms for the generalized 
coordinate system.  Therefore the numerical transport algorithms implemented in CMAQ will  -
function under a wide variety of dynamical situations and concentration distribution 
characteristics.  Users can not only choose transport algorithms from optional modules available 
in CMAQ, but also are encouraged to experiment with their own algorithms to test different 
numerical schemes for air quality simulations. 

\* On assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 
Corresponding author address: Daewon W. Byun, MD-80, Research Triangle Park, NC 27711. 
E-mail: bdx@hpcc.epa.gov 

000n assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 

"l>resent Affiliation: Georgia Institute of Technology, Atlanta, GA. 


NUMERICAL TRANSPORT ALGORITHMS FOR THE COMMUNITY 

Chapter 7 
=========

MULTISCALE AIR QUALITY (CMAQ) CHEMICAL TRANSPORT MODEL IN 
GENERALIZED COORDINATES 

hi. this chapter, we study numerical algorithms for the transport processes in a turbulent 
atmosphere.  Many of the contents provided here are the results of a collaborative research 
project, the EPA's Cooperative Agreement CR822053-01 with MCNC-Environmental Programs 
(Exploratory Research on Air Quality Modeling Techniques: Research on Numerical Transport 
Algorithms for Air Quality Simulation Models), and other related in-house projects at EPA. 
Readers are referred to Alapaty et al. (1997), Byun (1999a, b), Byun and Lee (1999), and Odman 
(1998) for additional information. 

In principal, the transport process consists of advection and diffusion that cause the movement 
and dispersion of pollutants in space and time.  Transport of pollutants by the parameterized 
subgrid-scale cloud modules is not considered here.  We have assumed that the transport of 
pollutants in the atmospheric turbulent flow field can be described by means of differential 
equations with appropriate initial and boundary conditions.  In Eulerian air quality models, the 
transport process is modeled using numerical algorithms.  These numerical algorithms for the 
advection and diffusion processes must satisfy several propertfos that are essential for making 
useful air quality simulations.  As with all numerical methods, the numerical schemes for solving 
the transport equations must meet convergence conditions and correctly model the conservation, 
dissipation, and dispersion properties of the governing equations.  A numerical scheme is said to 
be convergent if the solution approaches the true solution of the corresponding partial differential 
equation as the grid spacing and time-step size become infinitesimally small.  Thus, a convergent 
numerical scheme can provide a numerical solution of any desired accuracy within finite precision 
bounds by reducing the grid spacing and the time-step size.  For linear equations, consistency and 
stability are both necessary and sufficient conditions for convergence (Lax's equivalence 
theorem).  In practice, machine precision and the computational resource availability limit the 
reduction of grid spacing and time-step size.  Therefore, numerical errors associated with using 
limited grid spacing and time-step sizes must be of concern. 

· 

There have been ~any studies on the numerical advection algori~s used in air quality models. 
The reason it ~ttracted so much attention is that the equation is hyperbolic in nature and spatial 
discretization of the solution generates a finite number of Fourier modes that travel at different 
speeds and leads to constructive and destructive interference.  If the high wave-number Fourier 
modes are damped significantly, then numerical diffusion becomes prevalent.  Solving the 
diffusion equation, on the other hand, is a lot safer because the stiffness matrix is diagonally 
dominant and  the .discretized solution is stable and sign preserving for a relatively wide range of 
conditions (Chock, 1999). 

Transport processes are of central importance in turbulent flow studies, and in the literature there 
are numerous transport algorithms that have different nwnerical characteristics and varying 
degrees of accuracy and computational complexity.  The skill needed here is to select appropriate 
numerical schemes that provide solutions with the desired accuracy at reasonable computational 
cost.  This document does not intend to provide an extensive review of the transport algorithms 
used in air quality modeling.  Instead, we describe several popular numerical schemes 
implemented in the Community Multiscale Air Quality Chemical Transport Model (CMAQ 
CTM or, hereafter,· CCTM), expecting users to choose the algorithms appropriate to their own 
study objectives.  We offer a few examples of good transport.algorithms and present some key 
numerical characteristics users should look for.  With this information, users can find the best 
algorithms through evaluation processes, and may even bring in their own algorithms to build a 
transport model for their applications.  To provide the CMAQ system with the multiscale and 
multi-pollutant capabilities, we strive to incorporate schemes that can function under a wide 
variety of dynamic situations and distribution characteristics (e.g., distributions of different 
primary species and secondary species are quite distinct).  Also, the schemes should be efficient 
in the use of computer time and storage.  Selected numerical transport algorithms for horizontal 
and vertical advection and for vertical and horizontal diffusion are described below.· 

## 7.1 Numerical Advection Algorithms 

Numerical advection algorithms for air quality models should satisfy several computational 
requirements. 

• 

• 

• 

• 

• 

They should be free of mass conservation errors to accurately account for pollutant 
sources and sinks. 

They should have small numerical diffusion to minimize the spread of a signal in every 
direction and the smoothing of spatial gradients. 

They should also have small phase errors since disturbances that propagate at different 
speeds produce spurious oscillations. 

Given initial positive concentrations, the schemes should be positive-definite (i.e., they 
should not produce negative concentrations. 

They should be monotonic (i~e., they should not produce new extrema). 

While it is essential that the schemes be positive-definite, this alone may not be sufficient 
because the monotonic property, for example, is just as desirable for air quality modeling. 

Numerical algorithms have not been able to satisfy all the requirements listed above, and they are 
imperfect, with varying degrees of accuracy.  Advection schemes with different properties 
introduce different errors, all of which are sources of uncertainty in air quality model predictions. 
Before recommending its use, it is critical to identify which of the computational properties a 
scheme possesses.  Because an advection scheme with all the desired properties is not currently 

·  available, a user needs to select a scheme with the most desirable properties and greatest 

efficiency to meet the needs of the application. 

7~1.1  Conservation Form Equation for Advection 

'111 

The atmospheric advection process is expressed in conservation (flux) form as: 

(7-1) 

• 

"' 

where cp,  is the concentration of trace species i coupled with the coordinate Jacobian.  Refer to 
Chapters 5 and 6 for the definition of symbols used in Equation 7-1.  For convenience, the 
advection process is decomposed into horizontal· and vertical adv.ection processes, with the 
fractional time-step implementation: 

(7-2) 

-

(7-3) 

,  v2  and v3  are contravariant components of wind velocity.  Splitting of the three(cid:173)

where v1
dimensional (3-D) advection into the horizontal and vertical components will lead some 
difficulties, such as the representativeness of the mass continuity and -setting up of proper 
boundary conditions for non-~rthogonal horizontal and vertical directions when simulating a 
region with complex topography. 

Many models further split the horizontal advection equation in two directions and solve for two 
one-dimensional equations, one in each direction, using the solutio~ of one as the initial condition 
of the other.  We refer to this scheme as a one-dimensional (1-D) algorithm.  Others solve the 
two-dimensional (2-D) form directly.  Although using 1-D schemes is very common, it has been 
found that problems can arise due to this additional splitting (Flatoy, 1993, and Odman and 
Russell, 1993).  Although 2-D schemes may be more desirable in this regard, fewer have been 
tested and they are often more cJifficult to implement and less computationally efficient than 1-D 
schemes.  Also, there are general conditions in which the splitting scheme is actually more stable 
and accurate than the non-splitting case for higher-order approximations because the splitting 
scheme intrinsically contains cross-spatial derivatives whereas the non-splitting scheme would 
not (Leith, 1965).  Yanenko (1971) has shown that time-splitting is second-order aecurate if the 
one-component advection operators commute.  Alternating the sequence of operations would be 
quasi-second-order accurate in the case of non-commutativity (Chock, 1999).  Here, only 1-D 
schemes will be discussed.  When using appropriately interpolated contravariant wind 
components, the 1-D advection in the generalized coordinate system is equivalent to the 1-D 
equation in the Cartesian coordinate system.  Therefore, it is sufficient here to discuss advection 
algorithms in Cartesian coordinates. 

The 1-D advection equation written in the Cartesian coordinate system is: 

acp 
ii+  ax  = 

(}(u<p) 

0 

(7-4) 

Equation 7-4 is the flux (or conservation) form and the quantity  Fx = u<p  is defined as the one(cid:173)
dimensional constituent flux.  The flux form is a natural choice here because it is based on the 
continuity equation without any assumptions on the atmospheric dynamics.  Maintaining the 
advection equation in flux form is key to providing transport schemes with multiscale and multi(cid:173)
pollutant capabilities.  A flux-form discretization of Equation 7-4 with first-order accuracy in 
time results in: 

n+l 

n 
<pj  = <pj  -

/lt  (Fn 
llx 
j 

j+1/2  -

) 
pn 
j-112 
' 

(7-5) 

where  Fj:1, 2  and  Fj"_112  denote the advective fluxes through the faces of cellj,  llt is the time-step 
and  L1xj =  xj+ 112  -xj_ 112  is the horizontal grid spacing.  To maintain numerical stability, and to 
accommodate other physical changes such as emissions input in a synchronized way, the time(cid:173)
step of 1-D advection should satisfy the Courant-Friedrich-Lewy (CFL) condition for hyperbolic 
equations: 

(7-6) 

where  f3j+ 112  = ju1+1, 2I ::. is the Courant number for advection.  This condition should be viewed 

J 

as a method defining a reference time scale for accommodating different physical processes in 
AQMs.  lfwe do not consider the synchronization of mass injection to the ~ell through other 
physical processes, the CFL condition can be determined sep_arately for individual advection 
schemes.  For certain schemes the restriction can be significantly less than one.  
And for advection alone, an implicit scheme may not have the Courant number restriction for stability 
(Chock, 1999). 

7.'1.2  Classfucation of Advection Schemes 

" 

, 

, 

I 

' 

I'''" 

,111,p 

"""'•II' 

, 

, 

Numerical advection schemes in the literature were developed using several different approaches 
(e.g., Chock and DUnker,  1983, and Chock, 1985, 1991).  Following Rood (1987), we classify 
these schemes based on the methoqs used in their formulations.  However, reviews in the 
literature may not capture the most recent developments in advection research.  Depending on· 
the methods used, the schemes may be classified as: 

I 

• 
• 
• 
• 
• 
• 

Finite diffe~ence schemes; 
Finite volume schemes; 
Flux corrected schemes; 
Lagrangian Schemes; 
Finite element schemes; or 
Spectral schemes . 

The distinction is somewhat arbitrary and only meant to convey the key intrinsic features of the 
scheme.  Current trends in advection scheme development show a merging of the methods to take 
advantage of the most desirable properties of several schemes.  For example, the Characteristic(cid:173)
Galerkin method (Childs and Morton, 1990) combines the best of the finite element and 
Lagrangian methods.  Flux corrections are being used in the framework of finite element and 
spectral schemes (Lehner et al., 1987).  Also, the classical finite difference schemes are being 
abandoned in favor of modem finite volume schemes.  Refer to Odman (1998) for details of the 
classification. 

' 

### 7.1.3  Description of Advection Schemes in CCTM 

In this section we describe the schemes that are available with the first release of the CCTM 
codes in the following order: the piecewise parabolic method (PPM), the Bott scheme (BOT), 
and the Y am~o-Blackman Cubic scheme (YAM). 

Odman (1998) provides additional descriptions of the Smolarkiewicz scheme (SMO), the 
accurate space derivative scheme (ASD) (Chock,1991, and Dabdub and Seinfeld,1994), the flux(cid:173)
corrected transport, the semi-Lagrangian method, and the chapeau function scheme with Forester 
filter.  These codes are no~ integrated into the CMAQ system yet, but along with other advection 
modules, will be aclded to the system in the near future. 

To simplify the discussion, we will consider a uniform (i.e., constant~= Ax i  ) and staggered 

grid (<fJ.j  represents the grid cell average of the concentration, while Uj+112  is the advection velocity 
defined at grid cell interfaces).  While discussing the finite-volume schemes (the piecewise 
parabolic method, the Bott scheme and the Y amartino scheme) below, we use the explicit flux 
formula presented in Equation 7-5.  Further, a nondimensional coordinate 1J is defined as 1J  = (x -
Xj-l12J!Ax,  so that, in grid cellj, 0S1J S  1.  Now, suppose that the concentration has a certain 
distribution <fJ.J{ TJ)  in each grid cell.  Depending on the direction of the velocity, the flux Fj+ 112  can 
be expressed as: 

Fj+l/2  = 

Uj+l/2  < 0 

(7-7) 

where /3j+ 112 is the Courant number at the right boundary of grid cellj. 

The conditions of high-order accuracy and freedom from spurious oscillations are difficult to be 
achieved simultaneously.  The usual way to satisfy one of these conditions without significant 
violation of the other is to introduce a correction mechanism.  Typically, this mechanism is 
provided by nonlinear flux-corrections, or by nonlinear filtering.  In advection schemes, such 
adjustments are either applied implicitly through the solution or explicitly as a subsequent step 
to the linear solution.  There is extensive literature on both solution algorithms (linear and 
nonlinear) and explicit nonlinear mechanisms. 

#### 7.1.3.1 Piecewise Parabolic Method (PPM) 

In the piecewise parabolic method (Colella and Woodward, 1984) the concentration distribution 
is assumed to be parabolic in any given grid cell.  In terms of the grid cell average concentration cpj 
and the predicted values of the parabola at the left and right boundaries of the cell cpLj and <pRj· 
this distribution can be written as: 

. 

(7-8) 

Since the initial cell average is known, the construction of the parabola involves the determination 
of the edge values.  First, an approximation to  <pat Xj+ 1/2 is computed subject to the constraint that its 
value js ~."'1tln the range of the values at the neighboring cells.  For the uniform  L1xj, a 

first guess for <pj+l/2  is estimated with: 

(7-9) 

I, 

"' 

,, 

In smooth parts of the solution away from extrema,  <pLj+ 1 =  <pRj =  <pj+ 112  so that the distribution 
is continuous at Xj+112·  In other parts, the cell boundary values are further modified so that cp  is 
monotonic on each grid cell.  This step introduces discontinuities at cell edges and yields a 
piecewise continuous global distribution for concentration.  There are two cases where the edge 
values are modified.  First, if <f'.j is a local extremum, then the distribution is assumed to be 

' 

' 

constant instead of parabolic.  The second case is when <f'.j  is between <pLj and <pRj•  but 
sWficiently close to one of the values so that the parabola may take on values outside the range, 
ahd lead to overshoots or undershoots.  In this case, to make the distribution monotonic, one of 
the edge values is reset so that the derivative of <p  (7])  is zero at the opposite edge. 

I 

, 

,, 

' 

II' 

I•,," 

,J!. 

The most distinctive feature of this monotonic scheme is that the nonlinear adjustments are 
purely geometric.  The numerical diffusion introduced by this scheme may be slightly higher than 
in some other schemes discussed here, but its monotonic property is desirable for photochemical 
IIlodeling purposes.  The scheme has been used in meteorological modeling (e.g., Carpenter et al., 
1990) and in~ q~ality models (Odman et al., 1993) including the CCTM.  The schem~ can be 
modified so ¢.at, in the neighborhood of a discontinuity, it produces a narrower profile.  This 
feature, kno'wn as steepening, avoids the smearing of sharp gradients.  Though Carpenter et al. 
(1990) did not recommend steepening for meteorological modeling, this fea~e may be beneficial 
in air quality modeling practice, where steep gradients may occur in the vertical direction. 
However, it should be noted that steepening is of no value and the PPM reverts to a lower order 
method in the case of sharp spikes or extreme values in a single cell such as emissions from point 
sources. 

1: 

'I 

' 

,, 

#### 7.1.3.2 Bott Scheme (BOT) 

~e numerical scheme introduced in Bott (1989) is a positive definite scheme with small 
numerical diffusion.  The distribution of the concentration within the cell is represented by a 
polynomial of order I as: 

<p.(TJ) =°"'a. kT]k 

J 

I 
£.J  J, 
k=O 

(7-10) 

The polynomial can be made area-preserving by requiring: 


(7-11) 

over a stencil of!+ 1 grid cells by varying the value of i.  The solution to this linear system yields 
the coefficients aj,k·  The coefficients obtained this way for a quadratic (l=2)  and quartic (l=4) 
together with those of the donor cell (or upwind) scheme, and Tremback's scheme with second(cid:173)
order polynomials (Tremback et al.,  1987), are listed in Table 7-1. 

Using Equation 7-7, integrating the polynomial of Equation 7-10 between appropriate limits, we 
arrive at a first estimate of the fluxes.  Finally, to make the scheme positive-definite, the total. 
outflux from cell} is limited by requiring that it should be positive and less than what the 
available mass in the cell would allow: 

0 < Fout  < Llx C. 
- ,   -1:!.t' 

(7-12) 

The outflux Fj0 ut is a combination of the boundary fluxes and its expression depends on the sign 
of the velocities.  In the CCTM implementation, we used. fourth-order polynomials as 
recommended by Bott (1989) except for the boundary cells.  The scheme is receiving increasing 
attention in current air quality models because of its high accuracy and low computational cost. 

Recently, a monoto.nic version of the scheme was also developed (Bott, 1992) and the time(cid:173)
splitting errors associated with the use of one-dimensional operators in multidimensional 
applications ~ere reduced (Bott, 1993).  Monotonicity is obtained by directly replacing the 
positive-definite flux limiter of the original approach by new monotone flux ~imiters as: 

min( <p;_1, <p;) :s; cp;+1 :s;  max( <pj_1, <p;) , if  u ~ 0 
min( <p;+t • <p;) :s; cp;+1 :s; max( <p;+i • <p;) , if  u < 0 

(7-13) 

Although the new flux limited Bott scheme yields monotonic results, there is an inherent mass 
conservation problem.  This problem is directly related to the flux limiting that takes place.  Near 
the leading edge of a sharp wave the use of second or higher order polynomials causes an 
underestimation of a certain advective flux, Fk-112·.  When this flux is not corrected it is. less than 
F1c+112,  and an undershoot occurs in cell k,  as experienced With the original algorithm (Bott, 1989). 
The motivation for the monotone flux limitation is to avoid such undershoots.  However, there 
are cases when the monotone flux limiter leaves the underestimated flux intact.  Instead of 
increasing the underestimated flux, the limiter reduces the advective flux downwind, F k:+ 112, in 
order to avoid an undershoot in cell k.  This eventually reduces the net flux out of the domain 
resulting in an accumulation of mass in the domain. 

Table 7-1.  Coefficients of the Polynomials Used in Each Scheme 
Bott-4 

Tremback-2 

Bott-2 

Donor 
Cell 

cpl 

#### 7.1.3.3 Yamartino-Blackman Cubic Scheme (YAM) 

Yamartino (1993) presents another finite volume scheme where the interpolating polynomial is a 
cubic spline: 

where  a0  =<pi 

a1 =di L1x 

a,_ = -! ( 'Pi+I  - 2<pj + <pj-1) + 3:x ( dj+I  - dj-1) 

a3 = ( <pi+I  -<pi-1)-1!; ( di+I + lQdi +di-I) 

"' The spline derivatives,  di, are obtained from the tridiagonal system: 

ad.  + (1-2a) d. +ad.  = 'Pi+1  -<pi-1 

2L1.x 

1-1 

J 

l+I 

(7-14) 

(7-15) 

(7-16) 

with a:=0.22826.  Note that a value of a:=O would correspond to explicit expressions of d .. 
J 

' 

The positivity of <p/1J)  is ensured by various mechanisms.  First, when <fJ.j  is a local minimum, a 
donor-cell scheme is used instead of the cubic spline.  Second, the spline is spectrally limited by 
the relation: 

EPA/600/R-991030 

k 

ak  $; !!___,  k = 1,2,3. 
a0 

k! 

(7-17) 

Third, a mass conservative flux renormalization is applied, where the fluxes are normalized with 
the ratio for the upwind cell of the cell concentration (i.e., concentration at 17=0) divided by the 
average concentration.  Finally, a mildly diffusive filter is applied in an attempt to block the 
depletion of donor cells.  Y amartino' s scheme is not monotonic and can generate new maxima. 

### 7.1.4  Treatment of Boundary Conditions 

Mathematically, the advection equation is a first-order hyperbolic partial differential equation, so 
it accepts only inflow boundary conditions.  Physically, the solution is not affected by the 
downwind concentrations, so no outflow boundary conditions should be imposed.  In practice, 
however, some outflow boundary conditions have to be imposed.  Often, the characteristics of a 
numerical advection scheme are affected by the boundary condition used. 

In the CCTM, a positive.,definite zero-flux outflow boundary condition with appropriate flow 
divergence restriction is used.  The zero-flux divergence condition at the boundary (flux gradient 
out of the boundary cell is set equal to the flux gradient into the cell, i.e., 
u1 ( <p1 - <fJo) I L1x = u2 ( <p2  - <p1) I L1x)  gives: 

(7-18) 

where <p0,  <pi.  and <fJ2  represent concentrations outside the computational domain, at the boundary 
cell, and first cell of the inner domain, respectively. u1 is wind at the outer boundary flux point 
and u2  is wind at the inner boundary flux point.  Refer to Figure 7-1  for the cell subscript 
definitions at the outflow boundary.  To prevent spurious boundary flux situations that are often 
associated with the zero-flux divergence boundary condition, the following constraints are 
applied.  When u1 is sufficiently small (for example smaller than 10-3 mis), or when the wind is 
divergent at the boundary cell (i.e., u1•  u2 < 0), a zero concentration gradient at the outflow 
boundary (i.e.,  <fJo  = <p1)  is imposed.  This boundary scheme can be used with any of the 
numerical advection algorithms implemented in CCTM. 

On the other hand, Odman (1998) introduced a "no condition at outflow boundary" [sic], for 
Bott's scheme where the scheme is modified to remove the need for the concentration <p  0 at the 
downwind cell.  A first-order polynomial (derived from <p  1 and <p 2)  is used in Cell 1 to compute 
the flux out of the domain.  A second-order polynomial (derived from <p  i. <p 2 and <p 3 is used in 
Cell 2 and a (cmrtJ:l-~rder polynomial is used in Cell 3.  Mathematically, this condition is more 
correct than tpe ot:hers.  However, the order of the polynomial is reduced to
one at the boundary 
while the other conditions use a second-order polynomial to compute the flux out of the domain. 
Because of the lack of generality of this approach, we have not implemented Odman' s boundary 
s~heme in th~.,CC::'TM.  The improved positive-definite zero-flux outflow boundary condition 
scheme essentially reproduces his results without having to rely on the modified advection 
algorithms ne.ar bOundary. 

'  ' ~ 

'I' 

1

I 

I 

, 

_..,_ 

<f'o 
x 

0 

<f'1 
x 

1 

x 

2 

3 

Figure 7-1.  Outflow boundary condition.  Cell 0 is outside the domain.  Vertical solid line 
denotes the domain boundary and the vertical dotted lines denote the cell interfaces. The 
advection sc~eme computes concentrations for Cells 1, 2, 3, and so on. 

### 7.1.5  Test of Algorithms with Idealized Linear Horizontal Flow Fields 

Typically, the performances of advection schemes are measured and compared with each other 
m;ing test cases with idealized flow fields.  These ideal flow tests have analytic solutions and 
Illay be very use~ for determining certain properties of the schemes.  Odman (1998) provides 
some evaluation results identifying schemes with desirable properties.  Here we summarize the 
results of the one-dimensional tests and rotating cone test. The evaluaiion and comparison of the 
schemes are based on the performance measures listed in Table 7-2.  Again, readers are referred to 
Odman (1998) for the details of the analysis. 

Table 7-2.  Summary of Performance Measures Used to Test the Effects of Numerical 
Advection.  <p;  and  <p;e  methods and exact concentrations.+ 
Peifonnance Measure 
peak ratio& 

Description 
Measure of peak preservation (best when  1.0) 

EPA/600/R-99/030 

Formula 
max(<p;) 
max(<p;e) 
min(<p;) 
max(<p;e) 

Background to peak ratio 

mass ratio 

Distribution ratio 

Measure ofripples introduced by a non(cid:173)
monotonic scheme 

Measure of mass conservation characteristic 
(best when 1.0) 

Measure of shape retention (best when 1.0) 

Average absolute error 

Measure of absolute difference (best when 0.0) 

root-mean square error 

Measure of distribution error (best when 0.0) 

To account for the total mass correctly, the concentration should be coupled with the Jacobian of the grid system 
& Peak ratio alone is not a meaningful criteria unless the positions of the computed and actual peaks are also given. 

#### 7.1.5.1 Advection of One-nhnensional Pulses 

Various tests have been conducted in the literati.ire with pulses of different shapes advected with 
uniform velocity.  We conducted tests using a Gaussian sngnal of exactly 88x width with 100 
ppm peak advected from cell 25 to cell 75  in a 100-cell uniform grid· domain.  Background values 
were set at 5 ppm.  Table 7-3 summarizes the value of various performance measures after 
advected the signal for a .distance of 508x at a Courant number of 0.25 (i.e., after 200 time-steps). 
The Accurate Space.Derivative (ASD) an~ Yamartino's schemes (YAM) preserved the peak. 
height very well.  _However, the distribution ratio has a lower value for YAM,. indicating 
distortions of the pulse's shape.  For the same reason, the average and RMS errors are larger than 
those of the ASD .. On all accounts, these two schemes p~rform much better, than the other 
schemes in this test.  Bott's scheme (BOT) ranks third overall, but large ripples are observed at 
leading and trailing edges of the pulse as indicated by the values below the background (as much 
as 4% of the peak height).  When the monotonic limiter is used (BOT-M), the ripples are 
eliminated but the peak retention performance deteriorates.  Also, a 2% increase in mass is 
observed.  PPM is somewhat more diffusive than BOT-M.  However, PPM performed better 
than BOT-Min minimizing the average absolute error and root mean square error and because it 
is intrinsically monotonic, did better in regard to shape retention as measured by the distribution 
ratio.  Smolarkiewicz's scheme (SMO) displays poorer performance than the other schemes. 


SMO produces ripples upwind from the pulse and leads to average and RMS errors larger than 
other schemes. 

'" 

Table 7-3.  Gaussian signal test (Abridged from Odman, 1998) 
Scheme 
PPM 
Peak Ratio 
0.69 
Background 
0.05 
Mass Ratio 
1.00 
Distribution 
0.79 
Average Error 
1.16 
0.17 
RMS Error 

BOT 
0.87 
0.01 
1.00 
0.93 
0.87 
0.18 

ASD 
0.99 
0.05 
1.00 
0.99 
0.08 
0.01 

0.74 
0.05 
1.02 
0.83 
1.38 
0.27 

BOT-M 

SMO 
0.61 
0.02 
1.00 
0.66 
2.21 
0.50 

YAM 
0.98 
0.05 
1.00 
0.92 
0.51 
0.12 

7.i,.5.2 Rota#ng Cone Test 

In this test, a cone-shaped puff is introduced into a rotational flow field and followed for a certain 
number of revolutions.  The exact solution is a rigid-body rotation of the puff without any change 
to its original shape.  Various errors can be revealed in this test.  For example, numerical diffusion 
(or dissipation) manifests itself in the drop of the peak height during rotation.  Also, by 
observing the location of the peak, one can determine the leading or lagging phase-speed errors. 

A 32x32 grid is used for this test (i.e.,  -16.6.x::; x::; + 16.6.x;  -16~y::; y::; + 16.Ay  L\x= L1y ).  A 
cone-shaped puff with peak concentration equal to 100 ppm and a base radius of 4.6.x is 
initialized suqh that its peak is located at [+8 L\x, O].  Note that the peak is not initially at a grid(cid:173)
cell center but a cell comer (i.e., there are four cells around the peak with the same average 
concentration).  The background concentration is set to 5 ppm.  To obtain a counterclockwise 
rotation around an axis passing through the center of the domain, the wind field is defined 
as u =-my and  v = mx.  The angular velocity, ro,  is adjusted so that the Cou'rant number of 
approximately 0.28 at the location of the peak of the puff. 

" 

I 

I: 

I! 

Table 7-4.  Rotating Cone Test (Abridged from Odman, 1998) 
Scheme 
Peak Ratio 
~ackground 
Mass Ratio 
Distribution 
Average Error 
RMS Error 

BOT 
0.87 
0.03 
1.00 
0.93 
0.46 
0.16 

ASD 
0.99 
0.06 
1.00 
0.96 
0.18 
0.05 

0.65 
0.06 
1.02 
0.83 
0.76 
0.30 

0.65 
0.06 
1.02 
0.83 
0.76 
0.30 

BOT-M 

BOT2D 

PPM 
0.61 
0.06 
1.00 
0.78 
0.54 
0.18 

SMO 
0.49 
0.02 
1.00 
0.64 
1.60 
0.51 

YAM 
0.99 
0.06 
1.00 
0.91 
0.33 
0.13 

ASD and YAM maintain the peak height and the overall shape of the cone better than the others. 
BOT performs third best in this test, but it yields non-monotonicity, which results in values 
below the background (as seen in a ring-shaped valley at the base of the cone).  BOT-M and 


PPM predict similar peak heights (65% and 61 %, respectively), but the shape distortions look 
very different in each case.  PPM has the worst peak clipping effect but the resulting shape has 
the smallest base span among the three schemes. SMO is clearly the most diffusive scheme; it 
also introduces a ripple upwind from the cone.  Table 7-4 summarizes the performance measures 
at the end of two rotations.  Since there is no shear in the flow field, BOT-Mand the two(cid:173)
dimensional version ofBott's scheme (BOT2D) (Bott, 1993) produce identical results.  Again, 
the mass conservation problem is revealed with BOT-M.  BOT preserves 87% of the peak height 
(third best after ASD and YAM), but it leads to ripples with an amplitude of 3% of the original 
peak height.  Performances ofBOT-M and PPM are comparable in predicting the peak.  But 
PPM produces the lower distribution ratio, smaller absolute average and RMS errors.  Notice 
that the comparison results obtained from this test are ve:ry similar to those of the Gaussian 
signal test.  Additional test results such as skew advection of a point-source plume and advection 
with shear flow are available in Odman (1998).  Also, effects of density distribution on the 
numerical advection are studied with a set of linear flows in Byun and Lee (1999).  The solvers 
integrated into the CCTM are BOT, PPM and YAM.  Although ASD has very high accuracy 
except for the 2 Ax wavelengths, ASD is neither strictly mass conservative nor monotonic.  It is 
also the most CPU-intensive scheme (taking about 4-5 times longer than BOT).  In addition to 
BOT' s overall performance, results reported in Odman (1998) for a broad series of tests showed 
that BOT had the best computational performance of all the schemes tested.  However, because 
of the concerns over the non-monotonicity about BOT and the mass conservation problem and 
diffusive nature of BOT-M, we chose PPM for a number of demonstration executions (Byun et 
al.,  1998).  Similar testing with BOT and YAM is underway.  We intend to integrate other 
methods into the CCTM at a later time. 

### 7.1.6  Vertical Advection 

Algorithms in the CCTM for vertical advection are essentially the same as those for the one(cid:173)
dimensional horizontal algorithms.  However, the vertical advection is performed in terms of the 
generalized vertical coordinate in CMAQ.  The contravariant vertical velocity component is used 
as the transport wind for the irregular vertical grid spacing (usually expanding with altitude) 
represented in the generalized coordinate.  For the irregular grid, the computational time-step 
should satisfy the CFL condition: 

At < 

.  { Agk-1 
k 

Agk 
v· k-112  v  k-112 

- mm  I "3 

I ' I "3 

Agk  Agk+I  } 
I ' I "3 
I 
v· k+112  v  t+112 

I ' I "3 

(7-19) 

As in the case of Cartesian representation, we assume there is no mass exchange by advection 
(i.e.,  v3 = 0) at the top and bottom boundaries of the model.  Therefore, there is no need to apply 
special algorithms for the boundary process.  Because vertical grid spacing is usually irregular in 
most air quality models, a scheme that can accommodate irregular spacing must be used for 
vertical advection.  This means that numerical algorithms used to represent the vertical advection 
may be different from that used for horizontal advection.  For example, when the ASD algorithm, 
which requires equal spacing in the computational domain, is used for horizontal advection, a 
numerical algorithm that allows irregular spacing (e.g., BOT and PPM) would need to be used for 
vertical advection. 

### 7.1.7  Adjustment of Mass Conservation Error 

' 

,,,11:' 

Recently, Byun (1999a, b) has highlighted the importance of dynami.c consi~tency in 
meteorological and air quality modeling for multiscale atmospheric applications.  Mass 
consistency quantifies how well the density and wind fields satisfy the continuity equation for 
air.  One of the furldamental requirements for the numerical transport algorithms used in air 
quality models is the conservation of trace species in the domain.  Ideally, the input 
meteorological data for air quality simulations should be mass consistent.  However, using 
n.umerical models with highly parameterized physical and cloud algorithms, inappropriate set of 
governing equations, misapplication of four-dimensional data assimilation (FDDA) schemes, or 
using incomplete objective analysis methods to characterize the atmosphere for a CTM could 
result in meteorological conditions that are not mass consistent.  In this situation, even precisely 
mass conserving numerical algorithms may fail to conserve trace species mass in the domain. 
Preferably, the mass inconsistency must be minimized before air quality simulation using a 
suitable diagnostic relation or a variational wi.tid field adjustment scheme as discussed in Byun 
(1999b) and in Chapter 5.  For certain vertical coordinates with appropriate dynamic 
assumptions, the diagnostic methods can be used within the CTM to provide mass consistent 
wind data.  This is accomplished usually by adjusting the vertical wind component for the 
advection process.  However, the variational methods are applied during the meteorological data 
preparation stage, instead of inside CTMs, mostly due to the computational efficiency reasons. 

• 

1 

•11,, 

I 

Whether the meteorological data are mass consistent or n~t, Byun (1999b) has shown that the 
trac~r mixing ~ati~ must be c~nserved as a p~econdition f~r .the trac~r mass conservation.  He has 
r~ported se~e~al adjustm~nt schemes used in current air quality models and.proposed a two-step 
time splitting numerical algorithm that satisfies the mixing ratio conservation equation.  In the 
event the meteorological data are not mass consistent, the mixing-ratio conservation scheme is 
demonstrated to be useful for photochemical air quality models where chemical production and 
loss terms are computed using molar mixing ratio.  For this purpose, Equation 7-1  should be 
modified as follows to conserve trace species: 

(Jq/ __ , = 
at 

(7-20) 


where  QP  is the density error term for the meteorological data.  This is a necessary condition for 

the tracer mass conservation.  The total tracer mass of the domain is conserved only when the 
additional condition that total air mass of the domain is conserved, i.e.: 

(7-21) 

where an  represents the boundary of a computational domain.  However, even this condition 
does not guarantee the cell base conservation of tracer mass except for the case of uniform mixing 
ratio, because  Jf J <p;  QP dV = Jf J q;  l.; QPdV :t= q;f Jf  .I.; QPdV  in general.  Because of this, the 

anm 

an 

P 

an  m 

correction methods should only be used for improving mass conservation characteristics of the 
numerical advection algorithms after the mass inconsistency is minimized .  Nevertheless, if the 
objective is to maintain the property of cell-based mixing ratio conservation, the mass 
conservation error caused by the inconsistency in meteorology data ( <p;  Qp ) can be corrected 

p 

with: 

(<p~Y°r = (<p~)T exp[J QP dt] 

I 

I 

p 

(7-22) 

where superscripts 'T' and 'cor' represent values after transport (advection) and after correction, 
respectively.  Table 7-5 summarizes several adjustment methods reported in Byun (1999b).  An 
adequate correction scheme for photochemical Eulerian AQMs should conserve the tracer mixing 
ratio at least even if wind and density fields are not m:ass consistent.  The correction scheme 
proposed and labeledA5 in Table 7-5 is expected to maintain conservation of mixing ratio up to 
machine precision.  Correction schemes Al and A2 ignore the effects of coordinate/grid structures 
during the advection process.  For Cartesian coordinates where the Jacobian is uniform and 
constant with time, the scheme A2 is equivalent to A5.  For a steady-state flow, schemes A3, A4, 
andA5 become identical.  Note that all the correction algorithms in Table 7-5 become the same for 
a steady state flow with uniform density distribution in Cartesian coordinates. 

Table 7-5.  Trace Gas Mass Correction Schemes 
Symbol  Correction Method 

Mass correction algorithm 

AO 
Al 

A2 

A3 

A4 

AS 

No correction 

AcivectJon of unity 

Advection of air density 

Advection of pl~ I m 2  with 
anelastic approximation 
Advection of pl~ I m 2  with 
fmite differencing of tendency tenn 
Advection of pl~ lm 2  with two(cid:173)
step time splitting 

<p  cor  = J!.L_<p  ,  <p  = 1.0 

J 

r 

r 

T 

T 
'Pr 
T 
PT 

<p cor  = PJ_ pint 

I 

(  J  I  2)"" - (  J  I  2)T 
<p1  < m 

- c1  < m 

exp 
[

(pJ  I m2)  (pl  I m2)T] 

{ 

• -

~ 
(pJ,tm  ). 

2 

(Note: Superscripts cor, int, and Trepresent corrected, interpolated, and advected quantities, respectively.) 

Byun and Lee (1999) compared the performance of the correction algorithms under several 
idealized two-dimensional linear flows and different density fields.  In addifam, the linearity of 
the advection processes was studied to show how well the different precursors from same 
sources can be advected without losing their integrity.  The characteristics of the correction 
procedures can be summarized as follows: 

• 

• 

• 

The correction scheme fixes mass conservation errors due to the time splitting and 
numerical algorithms as well as the mass inconsistent meteorological data input. 

The cqrrection scheme does not, however, improve inherent properti~s of the numerical 
advection scheme such as monotonicity, or numerical diffusion. 

In a budget study, such as in process analysis, the adjustment proces~ must be considered 
as an iµtegral part of the three-dimensional advection.  Horizontal advection, vertical 
advection, and the mass correction ste~ all together simulate the atm~spheric advection 
process. 

I 
I 

7.2 

Vertical Mixing Algorithms 

In this section, we describe turbulence closure schemes for the CCTM.  We describe numerical 
algorithms for vertical mixing and dry deposition, and discuss the details of the mathematical 
representations, integration time-steps and solvers.  Unless stated otherwise~ the vertical 
structures of meteorological and chemical transport models are assumed to be the same.  This is 
desirable because the interpolation of meteorological quantities in vertical direction would alter 
the original turbulent flux exchange characteristics. 

7.2.1  Closure Problem 

Because of the stochastic nature of atmospheric motion, the primitive equations in the set 
describing the atmosphere are averaged to form a set of deterministic equations before they can be 
solved numerically.  The decomposition of velocity components and concentrations into mean 
and turbulent terms and the application of ensemble avera.ging produces Reynolds flux terms in 
the species mass continuity equation.  Introduction of the Reynolds flux terms generates a new 
problem set in which the number of unknowns is larger than the number of equations.  This 
closure problem is caused by the attempt to represent nonlinear processes such as momentum 
advection using a linear decomposition such as the Reynolds decomposition.  We describe some 
approaches on the closure of the Reynolds flux terms below. 

7.2.1.1 Local Closure 

Local closure assumes that turbulence is analogous to molecular diffusion, i.e., that an unknown 
turbulence flux at any point in space is parameterized by values of known quantities at the same 
point (Stull, 1988).  First order closure retains the prognostic equations for only the mean 
variables such as wind, temperature, humidity, and trace gas concentrations while the second(cid:173)
order moments (Reynolds fluxes) are approximated.  An example of a local closure scheme is the 
approximation of Reynolds flux terms using a gradient transport theory, or a mixing length theory 
resulting in an eddy diffusion method.  One of the problems with the gradient transport theory is 
finding a rational basis for parameterizing the eddy diffusivity.  Also, the theory fails when 
eddies larger than the grid size are present, like they are in a convective boundary layer. 

The so-called one-and-a-half order closure retains the prognostic equations for the mean variables 
and adds equations for the variances of those variables.  The set of one-and-a-half order equations 
is obtained by simplifying the full second-order turbulence equations.  Instead of the velocity 
component variance equations, the turbulent kinetic energy (TKE) equation is often used.  By 
including the variance equations, we have increased the number of unknowns that need to be 
parameterized compared to the first-order closure approach.  However, the benefit is that the 
eddy diffusivity can be parameterized not only with the mean quantities but also with the TKE 
and the temperature variance which characterize turbulence intensity.  However, if an air quality 
model is based on one-and-a-half order closure in a true sense, the prognostic equations of the 
variances for the tracer concentrations should be included explicitly.  In practical Eulerian air 
quality models that deal with photochemical problems, additional prognostic variance equations 
for the tracer species are very expensive computationally.  Also, the additional closure problem 
must be dealt with by parameterizing the Reynolds average terms involved with the variances for 
tracer species.  Therefore, the one-and-a-half order closure for an air quality model often actually 
means that the diffusion equations for the tracer species are formulated. with first order closure, 
while eddy diffusivities (if gradient theory is applied for the closure) or turbulent fluxes (if non(cid:173)
local flux-bas~d closure is used) are estimated with the TKE information from a meteorological 
n1.odel with o~e-aP.<l-a-half order schemes for wind, temperature, and humidity. 

The set of sec~md--0rder turbulence equations includes all the second moment terms.  To derive 
these terms, parameterizations on a full set of third-order moments are required.  Similar to the 
first-ord~r cas,~~ th~ second-order closure approximates terms involving third moments.  Several 
basic closure assumptions such as down-gradient diffusion, return to isotropy, and turbulent 
dissipation in the inertial subrange are used in the parameterization of the third moment terms. 
These parameterizations must be valid, especially, for the scales of the energy-containing eddies 
that are sensitive to atmospheric stability.  Measurements of high-order moments in the real 
atmosphere are difficult because oflarge scatter in the direct flux measurements and because a 
long averaging time or a very large sample size of data is required because the events with a much 
lower probability of occurrence must be gathered to estimate higher-order moments using eddy(cid:173)
correlation methods.  For air quality applications, especially for a complex chemical reaction 
system, the technique requires too many ad-hoc assumptions that cannot be confirmed by 
observations ~r other theoretical reasoning.  In addition the second-order closure incurs a 
prohibitively high computational cost.  As before, we can solve the first-orcler tracer diffusion 
equations with the variances and covariances for wind components, temperature, and humidity 
from the second-order meteorological models.  For air quality applications, the true second-order 
closure formulation solves for the cross-species covariances explicitly.  Some researchers have 
attempted second-order closure for simple chemical mechanisms with a limited number of 
photochemically reactive species.  The introduction of additional parameterizations for the third(cid:173)
order moment terms among the tracer species themselves and wind components (about which we 
lack sufficient knowledge) and the added cost of solving for a large number of covariance terms 
make this scheme impractical and prohibitively costly for operational Eulerian photochemical 
models. 

7.2.1.2 Non-local  Closure 

Non-local closure recognizes that larger-size eddies can transport fluid across finite distances 
before the smaller eddies have a chance to cause mixing.  This advection-like concept is supported 
by observations of thermals rising with undiluted cores, finite size swirls of leaves or snow, and 
the organized circulation patterns sometimes visible from cloud photographs. 

"•111;: 

" 

,1 

I 

'' 

•i•h:ll' 

Two main approaches of non-local closure methods are the transilient turbulence theory and the 
spectral diffusion theory.  Both allow a range of eddy sizes to contribute to the turbulent mixing 
process.  The spectral diffusion theory attempts to simulate mixing process by transforming 
signals into a spectral space.  For example, a spectral diffusion model of Otte and Wyngaard 
(1996) represents the mean variables within the planetary boundary layer (PBL) by a truncated 
series of Legendre polynomials.  The first Legendre mode represents the layer average, and 
additional modes add structure to the vertical profiles.  Only a few modes are necessary to 
resolve vertical profiles comparable to high resolution diffusion models.  However, the need to fit 
a different number of spectral modes for each trace species makes the scheme less attractive for 
air quality application and thus it have not been considered here.  The transilient turbulence 
theory (e.g., Stull, 1988) is a general representation of the turbulent flux exchange process.  The 
Latin word transilient, meaning to jump over, is used since turbulent eddies that exist in the PBL 
can transport mass and momentum directly across several grid layers.  A variety of mixing 
processes can be modeled with the transilient scheme depending on the form of the transilient 
matrix.  Examples include complete mixing, top-down/botil:om-up mixing, asymmetric convection 
mixing, small-eddy mixing, cloud top entrainment, a detra:ining updraft core, patchy turbulence, 
no turbulence, or eddies triggered by the surface layer.  Non-local closure is most suitable for 
describing vertical turbulence mixing process, which should representturbulent diffusion and 
atmospheric transport by eddies of different sizes simultaneously. 

In the following, we describe only the first-order turbulent mixing schemes.  The true one-and-a(cid:173)
half and the second-order closure schemes are not discussed here.  We organize the description of 
vertical mixing algorithms based on the details of the turbulence parameterization; the eddy(cid:173)
diffusion form or the Reynolds-flux form.  Both the eddy-diffusion and the Reynolds-flux forms 
are capable of accommodating information from higher order turbulence closure for momentum 
and other meteorological parameters such as potential temperature and humidity.  The vertical 
diffusion modules in CMAQ will include two different ways of parameterizing the eddy 
diffusivity (using PBL similarity theory and using TK.E) and three flux form non-local algorithms 
(Blackadar, ACM, and Transilient Turbulence). 

7.2.2  Computing Vertical Mixing with the Eddy Diffusion Formulation: K-Theory 

The eddy diffusion algorithm in the CCTM computes the following: 

a<p; I  = _J_[  E-(k33 aqi )] 
ax3 
at 

ax3  -vrP 

. 

vdiff 

(7-23) 

7-21 

A16ooffi._991d~o 

where K33  is the contravariant vertical component of eddy diffusivity in the generalized 
coordinates.  The contravariant eddy diffusivity is related to the diffusivities in Cartesian 
coordinates as: 

(7-24) 

where J~ -t = l~l Here we focus on the parameterization of the eddy diffusivity,  Ka, in a 
generic Cartesian vertical coordinate z (geometric height h).  Parameterizatio~s of the horizontal 
eddy diffusivity are described later.  However, in the current CMAQ implementation, the term 
involving horizontal diffusivity kH in Equation 7-24 for the estimation of K33  is neglected. 
Evaluation o~ the effects of this simplification is left for future work. 
7.2.2.1 Paran;_eterization of Vertical Eddy Diffusivity  Kv.  with PBL Similarity Theory 

.. 1 

":11111:. 

'" 

I' 

11·':i 

, 

• 

" 

' 

There are several eddy diffusivity parameterizations using different similarity theories.  Since 
~ese ar~ somewh~t similar, we consider the formulations suggested by Businger et al. ( 1971) and 
Hass et al. (1991) to represent the turbulent process in the surface layer and'mixed layer. 
Previous studies (Chang et al.,  1987, and Hass et al.,  1991) indicated that this type of 
formulation can represent turbulent mixing in air quality models adequately.  With K-theory, we 
assume that kce ~pecies have non-dimensional profile characteristics similar to potential 
temperature~e, i.e., Kzz =  Kh.  We briefly describe the surface and boundary layer similarity 
theory used for the parameterization of eddy diffusivity for different stability regimes of the 
PBL below.  The stability regime is defined with a nondimensional number z/L, where z is the 
height above the ground and L is the Monin-Obukhov length. 

.. 

For the surfa9e layer, the non-dimensional profile functions of the vertical gradient of e are 
expressed as: 

i 

····. 

z 
</Jh  = Pro(l + f3h  L) 
<Ph  = h - r h ~ t 112 

L 

for moderately stable conditions (1  ;:: z/L ;:: 0) 

(7-25a) 

for unstable conditions (z/L < 0) 

(7-25b) 

where  Pr0  is .the Prandtl number for neutral stability and  f3h  and r h are coefficients of the profile 
functions determined through field experiments.  In addition, following Holtslag et al. (1990) we 

' 

7-22 

add a function for the very stable condition (z/L ~ 1) to extend the applicability of the swface 
layer similarity: 

Parameterizations for eddy diffusivity for the surface layer can be shown as: 

K 

ku.z 

_ 
h-----
<l>h(zl L) 

where u/* is the surface friction velocity. 

For the PBL (above the surface layer), eddy diffusivity is parameterized with: 

ku,.z(l- zl h)312 
K l= - - - - - -

' 

</>,,(zl L) 
K,. = kw.z(l- zl h) 

z 

for.  L> 0  (stable) 

z 
L 

for  -< 0  (unstable) 

. .  

(7-25c) 

(7-26a) 

(7-26b) 

(7-26c) 

In the above expressions, his the depth of the boundary layer, kthe Von Karman constant, and 
w* the convective velocity.  Refer to Chapter 12 for the method used to estimate the PBL height 
in the CMAQ modeling system. 

These parameterizations for Khare sensitive to the boundary-layer height (h) and surface-layer 
height. Therefore, when the vertical resolution is too coarse in the boundary layer, using a 
"representative" eddy diffusivity together with the mean-concentration gradient at the interface 
seems to be more appropriate for the estimation of the diffusive flux.  In fact, the diffusive flux 
across the interface can be estimated more accurately with the mean diffusivity and mean 
concentration gr~dient than with local di:ffu:~ivity and mea~ c~nc~ntration ~adient; the former has 
an error of O[(Ac;)2
"representative" eddy diffusivity at the layer interface, integrated eddy diffusivity formulas are 
used as in RADM and CMAQ.  They are summarized inthe following equations (Byun and 
Dennis,  1995) 

]  (at best) while the latter has.an error.of 9[Ac;] ..  To. _estiµiate, the  . 

• 

Surface Layer 

(a) Stable conditions: 

where  f3  = J!.L. 
1  Pr  L 

n 

(b) Unstable conditions: 

where a= - r,/L. 

•  Planetary Boundary Layer 

(a) Stable conditions: 

= [ 1 + a23-1 ri +(a4 -a2)rz ]-[ ~ + a23_\3 +(a4 -a2)1i] 
= (a6 -a4J _!_In (a+ 12)la-1jl] 
.. 
la+ 121(a + 1i) 

'l 2a 

where 1i  = (1- z1 I h)112

.. 

' 

,  r2  = (1- z2 /  h)1 12 ,  a2  = (1+8)18, and8 =  'f3hh  . 
fr0  L 

···· 

(b) Unstable conditions: 

K  = 

h 

kw 
-
Z2 

Z1  Zt 

3h 

Z2 

•  J<  -~)d  =kw [Z2  +z1  _  Z2  +ZiZ1  +z1] 

2 

2 

2 

. 

z  hz 

• 

2 

(7-27b) 

(7-27c) 

(7-28) 

In the free atmosphere above the mixed layer, turbulent mixing is parameterized using the 
formulatio~ as implemented in RADM (Chang et al.,  1987), in whlch vertical eddy diffusivities 
are represented as functions of the bulk Richardson number and wind shear in the vertical.  This 
formulation can be written as: 

' 

7-24 


(7-29) 

. 

. 

. 

. 

g 

~.ek 

where the Richardson number IS defined asRln  = e  s2  ~ 'Ko  IS the background value set at 

0 

k 

1 m2  s- 1,  and S  is the vertical wind shear.  Effects of the parameterization, Equation 7-29, on air 
quality simulation will be evaluated in near future.  Furthermore, not much is known of the eddy 
diffusivity formulas for strong stable condition (i.e., z/L > 1) and  therefore require further 
research in this area. 

7.2.2.2 Estimation of Vertical Eddy Diffusivity Using Turbulent Kinetic Energy 

Various forms of higher order closure schemes are becoming common in mesoscale meteorology 
models.  Typically, such models are referred to as TKE models.  The simplest form of a TKE 
model, which Mellor and Yamada (1974) referred to as level 2.5, has only one second order 
prognostic equation, for TKE itself.  The next level up in complication is the true one-and-a-half 
order closure model, level 3 in Mellor and Yamada (1974) nomenclature, which in addition to 
TKE includes prognostic equations for the turbulent variances of other relevant quantities such as 
temperature and humidity.  The TKE scheme in the latest version of MM5 can be run as either 
level 2.5 or level 3 where prognostic equations for temperature variance, moisture variance, and 
temperature-moisture covariance are added to the TKE equation (Burk and Thompson, 1989). 
Another similar form of a higher order closure model is known as TKE-E, which has prognostic 

equations for TKE and the turbulent dissipation rate (E).  An example of this type of model is 
described by Alapaty et al (1996). 

A common feature of TKE models, for level 2.5 or 3 or TKE-E, is that the turbulent fluxes of 
momentum, heat, and moisture are represented as local gradient diffusion similar to Equation 7-
23.  It is this characteristic that distinguishes 1.5 order closure from true 2nd order closure where 
the flux covariances are explicitly represented by prognostic equations.  Therefore, adaptation of 
a CTM to a meteorology model that includes TKE closure is essentially simple if the TKE fields 
are available to the CTM.  Only a small part of the TKE model need be reproduced in the CTM, 
namely the parameterization of eddy diffusion coefficients based on quantities already produced 
in the meteorology model.  For example, in the TKE-E model of Alapaty et al. (1996), the 
governing TKE equations in Cartesian coordinates are: 
- .. - .. au  -,,-,,av 
ik 

dE 
-=-u--v--w--u w  --v w  -
at 

dE 
ax 

dE 
ik 

()E 
ay 

ik 

7-25 


g - d(- 1-) 

+-e"w"-- w"E"+-p"w"  -e 

p 

e 

dz 

de 
-=-u--v--w-+- -u w  --v w  -+- w 
dt 

C1e (  -,-, -11  du 
dz 
E 

dze 

-,-, -,, dv 

g  -811  ") 

de 
dz 

de 
ax 

de 
ay 

2 

----c - we 

d  (-"-") 
' 

3  dz 

C2e
E 

(7-30a) 

(7-30b) 

where  u, v are horizontal wind components; 

E is the turbulent kinetic energy per urtlt mass; 
z is height; 
ev is the virtual potential temperature; 
e is the rate of TKE dissipation; 

, 
u"w"  v"w"  and .K.e"w" are turbulence fluxes of momentum components and heat flux· 

e 

, 

, 

p is the density; and 
p" is the fluctuating pressure. 

The constants CJ, c2, and c3 can be estimated following Detering and Etling (1985).  The first 
three terms on the right-hand-side of the first equation represent advection of turbulent kinetic 
energy, and the other terms, shear production, buoyancy production, turbulence transport and 
the dissipation, respectively.  Similarly, the first three terms on the right-hand-side of the second 
equation represent advection of e.  The rest of the terms represent the net rate change of e due to 
shear and buoyancy production, the rate c.hange of e related to the time-scale of turbulence, and 
the vertical transport, respectively.  The coefficients of eddy viscosity for momentum and h~at 
can be written as: 

K  =  C2E2 

m 

e 

K  = K  </Jm(zl L) 
h 
m  </>h(zl L) 

(7-31a) 

(7-31b) 

where Km  and  Kh  are the eddy exchange coefficients for momentum and heat, respectively.  E 

and E need to be provided by meteorological models in their native coordinates, thus the 
parameterization does not depend on the grid spacing explicitly.  Obviously, we are assuming 
here that the verti~al layer structure in the CTM is compatible with that used in the preprocessor 
meteorological model.  Once  Kh  is computed, chemical fluxes can be modeled assuming similarity 

7.;.26 


with heat flux using the same eddy diffusion numerical algorithm described below (section 
7.2.3.1).  The derivation of eddy coefficients in the Burk and Thompson (1989) model is 
algebraically more complex (see Mellor and Yamada, 1974, 1982) but based only on mean 
quantities plus TKE.  Therefore, given the TKE fields, which are output automatically from 
MM5 when the Burk and Thompson (1989) option is invoked,  Kh  can be re-diagnosed within 
the  CTM. 

7.2.2.3 Numerical Algorithm for Vertical Eddy Diffusion Modules 

As described in Chapter 5, the vertical eddy diffusion module must solve the following equation 
in terms of mixing ratio  q: 

dqj' 
at 

. 

vdifl 

= ~({(33 dii.;) + {(33 aln(,jfp) dii.; 

ax3 

ax3 , 

ax3 

ax3 

(7-32) 

where the vertical mixing is represented with the pure diffusion term and the turbulent flux 
exchange term, respectively. 

Numerical algorithm for the diffusion kernel 

In its generic form, the diffusion kernel solves for: 

(7-33) 

where  i;  is the generic vertical coordinate which increases with geometric height. To account for 
the loss process due to deposition in the lowest model layer (j= 1 ), dry deposition flux is 
considered as the flux boundary condition at ·the surface, i.e.: 

(7-34) 

where the geometric thickness of the lowest model layer is used for hdep-

The diffusion equation can be discretized for levelj  (1 < j  < N) in aN-layer model and time(cid:173)
steps  tn  and  tn+t  as: 

7-27 


where  7J  is a time-step weighting factor and 

(7-35a) 

For the lowest model layer, we need to account for the loss due to deposition process: 

-L\t :d [ 19q~+l + (1-19-)qn 

' 

dtp 

and for the top layer we have zero flux through the top boundary: 

(7-35b) 

(7-35c) 

Depending on 7J, the finite difference scheme is explicit ( t>=O), semi-implicit ( 7J=l/2), or fully 
implicit ( 7J=l).  In the current version of CMAQ, the semi-implicit (Crank-Nicholson) algorithm 
is implemented.  Equations 7-3Sa-c can be rearranged to yield a matrix equation: 

Aq=b 

(7-36) 

where A is tridiagonal whose coefficients (sub-diagonal componentai, diagonal component  di, 
and super-diagonal component  ci) whenj=l are given as:,. 

::,1 

''I' 

" 

' 

I' 

d  - 1' 

AQ A  v d 
- +---- + VL.lf-

I 

"' 

.,,,  K 
•} 
iMt 
L\~I  L\~1.!. 

2 

" 

hdtp 

7-28 

(7-37a) 


(7-37b) 

(7-37c) 

(7-37d) 

and forj=N: 

and for  2 ~ j  ~ N - 1: 

Coefficients of vector b are, for j= 1 and N: 

and for  2 ~ j  ~ N - 1: 

7-29 


+((1- fi)At [  KJ+i  ])  n  + ((1-~)At [  KJ-i  ])  n 

L\c;. 

J 

L\c;  I 

J+2 

q)+I 

L\~. 
J 

L\c;  1 

1-2 

qj-1 

(7-37e) 

The numerical algorithm solving the tridiagonal system is based on the Thomas algorithm.  Refer 
to Appendix 7 A.1  for details of the algorithm and stability characteristics. 

Numerical algorithm for the coordinate divergence kernel 

The coordinate divergence kernel solves for 

(7-38) 

where  V,,"  = K (Jin( f /5).  The differential equation is in advective from with the effective mass 

. 

I' 

a, 

"' 

': 

;, 

·,.::: .. ·' 

transfer velodty  Vm;x·  It can be solved with a vertical advection code.  Because most operational 
meteorological models rely on logarithmically spaced vertical layering based on sigma-p type 
coordinates,  Vmix  is expected to be small.  Currently, this component is not implemented in 
CMAQ.  However, a quantitative study is needed to assess the importance of this term. 

,·, 

• 

I 

:, 

• I '  

Integration time-steps 

A~cording to Oran and Boris (1987), any numerical algorithm for the diffusion equation (with 
equal grid spacing) should produce the followillg quantitative properties: 

'II' 

I' 

I, 

the total irltegral of q(c;,t)  should be conserved; 
the amplitude lq(c;,t)I should decay monotonically; 
there should be no phase errors introduced by the algorithm (for equal grid spacing); and 

• 
• 
• 
•  positivity should be preserved. 

Although the numerical solver algorithm for the semi-implicit scheme is stable fo the sense that 
the amplitude of the signal either decays or stays the same, the positivity condition may not be 
satisfied if we choose a large time-step for the integration, especially for signals with small 
Wavelengths.  For example, with equal grid spacing, we can use the Von Neuman stability 
analysis technique to demonstrate that some part of the short-wavelength spectrum shows a 
negative amplitude when the time-step is too long.  The semi-implicit scheme is positive definite 
given the rather stringent CPL condition, for a uniform vertical grid spacing: 

EP A/600JR-99J030 

2AtK 

/3 = (L1;)2  < 1.0 

For non-uniform grid spacing, we use the CPL condition: 

max {/3i} = max{  L1t  (. K1-112  +  K1+112  J} < 1.0 

L1~)  L1~J-1/2 

L1~J+l/2 . 

(7-39a) 

(7-39b) 

to ensure positive definiteness of the semi-implicit solution .. The internal time-step for vertical 
diffusion is thus determined in CMAQ with the following equation: · 

L1tvdiff  = min{L1t) 

where  L1t.  = L1;.(  K1-112  +  K1+112  J-1 

J 

J  L1;J-1/2 

L1;J+l/2 

7.2.3  Flux Form Representation of Vertical Mixing 

Vertical mixing can be represented in flux form as: 

(7-40) 

(7-41) 

where  F~ = ~~ is the turbulent flux represented in the generalized vertical coordinate  ;  , whose 
value increases monotonically with geometric height.  Here, the flux should be parameterized in  ~ 
coordinate instead of the generic height'coordmate.  nie cross directional.(w.r.t. generalized 
coordinate) diffusion terms, as well as the flux divergence due to grid spacing (second term in 
Equation 7-41 ), are neglected. 

Thus, the nwnerical solver kernel for the flux form vertical diffusion should solve for: 

aq 
aF~ 
-=---
dt 
a~· 

(7-42) 

when the source term is zero.  The flux form representation is extremely useful in describing the 
algorithms based on non-local closure.  Non-local closure recognizes that larger-size eddies can 
transport fluid across distances longer than the grid increment before the smaller eddies have a 
chance to cause mixing (Stull, 1988). 

To represent the turbulent mass exchange with the transilient parameterization, the boundary 
layer height must coincide with the height of layer interfaces of the vertical grid.  For most 
situations, the index for the boundary layer top (Lp)  is less than the total ntimber of model layers 
(i.e.,  LP< N).  With the transilient turbulence formulation, the new values of the trace species 
mixing ratio q due to turbulent mixing for a layerj at a future time  (t + L1t)  can be written as: 

LP 

qj(t + L1t) = ~>jk(t,L1t)qk(t) 

k=l 

(7-43) 

where  cjk  are the components of a transilient matrix and subscripts j  and k are indices of two 
different grid boxes (vertical layers) below boundary layer top in a column of atmosphere.  If we 
consider turbulent mixing between grid boxesj and k,  cjk  represents fraction of air mass ending in 
the grid boxjthat came from grid box k. The grid boxj is considered as the "destination" box 
while grid box k is considered as the "source" box.  Thus, the change in the tracer concentration 
due to turbulent mixing for grid boxj at a time interval  L1t  is a simple matrix multiplication with 
concentration from the source cell.  The transilient matrix representation is in fact applicable for 
any physical process that involves mass exchange among grid boxes in a column.  For example, 
convective cloud mixing can be represented by a transilient matrix as well, sim1lar to how mixing 
in a convective boundary layer is handled. 

. 

' 

The mass conservation requirements provide constraints for the coefficients of the transilient 
matrix.  The conservation of air mass requires that the sum over k of all mixing fractions be unity: 

(7-44) 

and the conservation of trace gas amount requires that the sum over j  of all mass-ratio weighted 
transilient coefficients be unity as well: 

(7-45) 

where  L1~i I L1~.c  represents the mass ratio (i.e., ratio oflayer thicknesses for mixing ratio q) 
between the source and destination boxes.  In order that transilient turbulence theory be useful, 
the coefficient~ should be determined using appropriate turbulent flux parameterizations. 

I 

7-32 

Consider how the mixing coefficients are related with the turbulent flux representations.  Because 
the transilient matrix describes the exchange of mass between grid boxes, the kinematic turbulent 
fluxes across the j-th level can be expressed, for a vertical layering with non-uniform grid spacing 
(Stull, 1993), for  2 ~ j  ~LP as: 


andforj=l as: 

(7-46a) 

(7-46b) 

The concentration (mixing ratio) of the lowest model layer, taking into account the deposition 
flux, is: 

(7-47) 

where  F;  is the flux at the sUrface. 

The transilient turbulent representation, as shown in Equation 7-43, is an explicit expression in 
which the magnitudes of mixing coefficients depend on the numerical integration time-step.  To 
use the transilient matrix representation as a general expression for the non-local turbulent closure 
methods, we need to relate Equation 7-43 with the semi-implicit representations of simple non(cid:173)
local closure algorithms such as in Blackadar (1978) and the Asymmetric Convection Model 
(Pleim and Chang, 1992).  Using Blackadar's definition of mik• which is the fraction of mass 
exchange between two levels j  and k per unit time, the turbulent fluxes at a given time t can be 
rewritten as: 

Fj(t) = Fj_I (t) + Li~j L,.mjk(t)[qj(t)-qk(t)] 

LP 

k=I 

(7-48) 

Note that  mik  = cik I Lit for an explicit method.  Substituting Equation 7-43 into the semi-implicit 
method, as in Equation 7-46, we get: 

q~+I = q~ _  Lit  [iJ(F.n+I  _ p,n+I) + (1- iJ)(Fn _ p,n  )] 

1 

Li~j 

1 

1 

1-I 

1 

1-I 

(7-49) 

7-33 

resulting in a general matrix equation: 

where the coefficients of P and Rare given as: 

(7-50) 

p D = 1 + f}Llt L mJk ;  Pik  = -fJt1tmik 

LP 

kc! 
k¢j 

j¢k 

r0  = 1- (1- tJ)t1t I,mik; ljk  = (1- tJ)t1tmik 

j-l>k 

Lp 

k:I 
bj 

If the matrix Pis nonsingular, we have a general expression for the transilient turbulence: 

• 

I 

I 

'I 

(7-51) 

The relationship between the coefficients of the transilient matrix and the mass exchange ratio 
among the grid boxes are somewhat complicated for the semi-implicit scheme.  However, the 
semi-implicit scheme becomes attractive for the closure algorithms with sparse P matrix (i.e., 
when the matrix inversion is not so expensive) because it allows longer integration time-steps 
than the explicit method.  In the following, we describe non-local flux-form atmospheric 
turbulence algorithms as a subset ~f the generalized transilient turbUlence representations. 

I 

7.2.3.1 Blackadar Non-local Scheme 

This scheme, first introduced by Blackadar (1978), has long been used as one of the PBL schemes 
in the Mesoscale Meteorology model generation 4 (MM4) and generation 5 (MM5).  The 
Blackadar model is a simple non-local closure scheme designed to simulate vertical transport by 
large convective eddies during conditions of free convection.  Therefore, this scheme is used only 
in the convective boundary layer and must be coupled with another scheme for non-convective 
conditions and above the boundary layer, such as K-theory.  In general, the flux-form diffusion 
algorithm can be written for the lowest layer as: 

(7-52a) 

(7-52b) 

where  mik  is the rate of mass exchange between two grid boxes in a column of the atmosphere 
(below boundary layer top) per unit time.  The convective mixing is assumed to be dominated by 
eddies of varying sizes but all having roots in the surface layer, each eddy exchanging a certain 
amount of its mass with the air around it as it ascends.  1he rate of change of mean potential 
temperature caused by the mass exchange in the mixed layer can be expressed as: 

(7-53) 

where w(c;J  is a weight function that accounts for the variation of exchange rate with height. 

The mass exchange rate,  Mu, can be estimated from conservation of energy, which requires the 
heat flux at any level to satisfy the equation: 

H  = H.ifc  -Mu f Cpdp(esfc -e)w(~' )J~d~ 

~ 

~sfr 

(7-54) 

where  H.efc  is the sensible heat flux leaving the surface layer and Cpd is the specific heat at 
constant pressure.  When the integration limit is extended to the top of the boundary layer, where 
His assumed to be zero, we can estimate Mu with: 

Mu= If.ifJ f Cpdp(esfc -e)w(/;)J~dl; 

~h 

~sir 

(7-55) 

Usually the weight function w is approximated to be un!ty In the mixed layer.  With the 
Blackadar scheme, the mixing algorithm is represented for the lowest model layer by: 

and for  2~j~LP by: 

dq. 

a:  =-Mu L1/;~ [q/t)- % (t)] 

L1/; 

(7-56a) 

(7-56b) 

where we used  m,k =Mu L1;k,  mi1 =Mu L1~1  ,  and all other components of mik  are zero. 

L1':>1 

L1':>j 

Finite difference representations of the above equations are: 

where we used ~h = L L1gk , and for  2 s:;  j  s:;  LP : 

LP 

k=I 

q;+

1 
-q'j =-?JM  L1g,  (qn+t -qn+t)-(1-?J)M  L1g,  (q~ -qn) 
L1t 

u  L1gj 

u L1gj 

1 

1 

J 

1 

After rearrangement, we obtain the following matrix equation: 

h. 
d2 
0 

0 

di 
e2 

el 

eL,. 

h 
0 

dj 
0 
0 

0 

0 
0 

!LP 
0 
0 

0 
dL 

p 

q~+I 
q;+I 

b, 
bz 

q'tl  = 

bj 

qn+I 
LP 

bL 

p 

0 

where the elements are defined with: 

(7-57a) 

(7-57b) 

(7-58) 

di  = 1 + 1J V tlL\t. + i} L\tMu  ( ~h - L1gl) 

htltp . 

L1gt 

The numerical algorithm to solve the sparse matrix system is similar to the Thomas algorithm for 
the tridiagonal system.  For the details of the numerical algorithm, refer to Appendix 7 A.2. 

(a) 

(b) 

Figure 7-2.  Schematics of the Blackadar Scheme (a) and the Asymmetric Convective Model (b) 

7.2.3.2 Asymmetric Convective Mixing 

The Asymmetric Convective Model (ACM), developed by Pleim and Chang (1992), is based on 
Blackadar's non-local closure scheme (Blackadar, 1978) but with a different scheme for 

downward mixing In the convective boundary layer (CBL).  Blackadar's scheme is based on the 
assumption that the turbulent mixing is isotropic (i.e., symmetric) in :the CBL.  However, 
observational ~yid~nce anc;l large-eddy simulation modeling studies indicate that mixing processes 
in a convective boundary layer are essentially asymmetric (i.e., ·turbulence is anisotropic; 
Sc;h~ann, 1989) with fast upward buoyant plumes and slow broad compensatory subsidence. 
Therefore the direct, non-local downward transport of the Blackadar scheme is replaced with 
layer by layer subsidence which increases in mass flux as it descends, like a cascading waterfall 
(Figure 7-2).  As with the Blackadar model, the ACM can only be used during convective 
conditions in the PBL.  For other stability regimes, one needs to rely on other schemes such as 
K-theory. 

" 

" 

, 

I 

, 

, 

, 

-

, 

Turbulent mixing in the PBL for any dynamic, or thermodynamic variables, or trace gas species 
concentrations can be represented in essentially the same way as in the above transilient 
parameterization.  Also, the conceptual design of :the ACM ailows Ioi considerable 
simplification.  Because the mass influx to the lowesf model layer is from the second layer only 
in ACM (refer to Figure 7-2), we can write the time rate change of mixing ratio as follows: 

(7-59a) 

.  (7-59b) 

where Mu represents upward mixing rate. MdJ represents downward mixing rate at layer j  and is 
defined as: 

(7-60) 

, 

' 

. 

The scheme can be represented in terms oftrruisilient ~xing rates (m1k), as shown in general fonii 
in Equation 7-48: 

. . 

. 

· 

(7-61) 

Therefore, by equating Equations 7-59b and 7-61, we see thatmi1 =Mu'* m1i, which shows that 
the transfer coefficients are asymmetric as expected, and  mi 1 + mi.i+I = Mdj, which demonstrates 
a recursive relationship for MdJ: 


M 

dj-1  - Mu  + 

M  L1~j+I 
~ 
L1~j 

di 

Finally, we rewrite the prediction equation in terms ofth1~ upward mixing rate: 

dq,  =-M ~h -~1 (q  -q )-3L._q 
dt 

2 

I 

I 

u  L1J: 
':.1 

h 
dtp 

EP A/600/R-991030 

(7-62) 

(7-63a) 

(7-63b) 

Note that this scheme does include the effects of vertical wind shear in generating turbulent 
mixing.  The magnitude of the mixing rates of the transilient matrix is based on the conservation of 
· 
sensible heat flux in the vertical direction: 

(7-64) 

Finite difference representations of the above equations are: 

q;+

1 

- q~ =-(~+Mu ~h - ~1 )ct9q~+1 +(I-~)qn 
Lit 

L1~1 

hdtp 

. 

+Mu ~h~~1~1 [t9q;+1 + (1-.~)q;] 

(7-65a) 

n+I 
qi  ;; qi  = Muf t9q~+I + (1- ~)qf] 

n 

{  ~. ~~J-1 )tiqt' + (1-1')qj] + ( ~~:.1).iq;:: +(I- 1')qj., J 

(7-65b) 

This results in a sparse matrix of the form: 


c, 

d2 
0 

0 

d, 

e2 

el 

, 
eL 

C2 

0 
0 

0 
0 

di 
0 
0 

0 
0 
0 

q~+I 

qt! 

bl 
b2 

q;+'  =  bj 

' 

CLP-I 
dL 
p 

qn+I 
LP 

bl 

p 

cl 

0 

(7-66) 

where: 

The numerical algorithm solving the sparse matrix system is presented in Appendix 7 A.3. 

7.2.3.3 Transilien.t Turbulence Parameterization 

The general computational paradigm for the transilient turbulence parameterization has been 
presented above.  In order to use the transilient turbulence concept for mixing trace gases, one 
needs to knovy the mass exchange coefficient matrix.  This is the closure pr~blem with this 
parameterization.  A couple of methods have been presented in the 1.iterature.  One method is 
based on the tkE equation (Stull and Driedonks, 1987, and Raymond and Stull, 1990) and the 
other is the one based on non-local Richardson number (Zhang and Stull, 1992).  In the following 
we describe briefly the TKE based scheme and discuss its associated difficulties. 

,  1" 

11:'1,' 

•  ~ 

ii•:,1 

'1 

l,11:'!· 

' 

1

I' 

• 

:' 

I'' 

'' 

, , 

, 

''," 

" 

, 

" 

",I., 

,,,,.  · 

' 

' 

• 

The horizontally homogeneous form of the TKE equation, Equation 7-30a is given as: 


aE 
-
at 

-au  -av  g -

= -u"w"--v"w"-+-fJ"w"-8 
. 

eo 

<k 

<k 

BP A/600/R-991030 

(7-67) 

Note that the pressure and turbulent transpo~ terms have been ignored.  After normalizing with 
E, the non-local analogy of the finite difference form.of Equation 7-67 can be written as: 

~ = 
A  E 

[(-u"w")  ( 

jk  L1u  + 

) 

·(-v"w")  ( 

) 

jk  Liv  + ...L 

(fJ"w") 

jk  - 8jk  L\t' 

] 

(7-68) 

Ejk 

Ejk 

jk 

Ejk 

jk  ej 

Ejk 

Ejk 

L1z 

L1z 

where the symbol  L\  represents temporal change while L1 represents spatial gradient.  To close 
the system, the unknown parameters are written in terms of known parameters by introducing 
three scaling parameters T0 ,  Ric, and D, which.are the time scale of turbulence, the critical 
Richardson number, and dissipation factor, respectively.  Thus, weighted kinematic fluxes can be 
written as: 

(-fJ"w"). 

__  __,1'-k -

T  (Lie) 

0 

Ejk 

- Ric 

L1z 

jk. 

Then Equation 7-68 can be rewritten as: 

•  ';..  !',.~ •, 

• ;.. 

. ; 

r 

.;  ' 

(7-69a) 

(7-69b) 

(7-69c) 

(7-70) 

To use Equation 7-70 for the generalized coordinate ;,'the.correspond.illg iayer heightslAz )Jk 
should be computed for  (A;) jk. 

Since we are dealing with fraction of masses that are .commg .from and g~ing t? different l~yers. 
(i.e.,j-:/:! k),  Jjk in the above equation is for off-diagonal elements only.  Diagonal elements in 
Equation 7-70, J}j, represent mass of air that remain in each layer without interaction with other 
layers.  Observations during convective conditions (Ebert et al.,  1989) indicated that turbulent 
eddies cause a well-mixed rather than convectively overturned boundary layer.  This requires 
that the values of the mixing potential elements (Jjk) should increase monotoi:llcally from the 
upper right-most element towards the main diagonal.  Further, to account forthe subgrid scale 
mixing in each layer, an independent parameter Y,.e1 was introduced.  Thus, the diagonal elements 
can be written as: 

I 

I 

(7-71) 

given values for T0 ,  Ric, and D.  Usually, Yref  is estimated based on observations (Stull, 1988). 
Finally, the off-diagonal elements of the transilient matrix are estimated with: 

" 

" 

I 

(7-72) 

where llYll.. is the infinite norm of matrix Y,  max {IYIJ.  The formulation presented in Raymond · 
and Stull (1990) and Alapaty et al. (1997) includes the additional weighting based on the mass in 
the layer for irregularly spaced grids.  However, we believe that Equation 7-72 should be valid 
e'V'.en for irregularly spaced grids when the constraint Equation 7-45 is satisfied.  Also, Stull 
( 1993) states that the formulation Equation 7-72 causes too much mixing near the surface and 
inclusion of the mass weighting in the formulation exacerbates the p~oblem further.  The diagonal 
elements of the transilient matrix can be computed by rewriting Equa~ion 7-44 as: 

cil = 1- fcik 

l•I 
l"'} 

(7-73) 

Once the transilient matrix is determined, the concentration due to turbulent processes in the 
boundary layer can be estimated from Equation 7-43.  The difficulties associated with this 
parameterization are: 

",I 

• 

• 

The scheme still depends on many free parameters (T0 ,  Ric, D, and Yref)  and they control 
the behavior of the mixing algorithm; and 

',:'" 

111' 

The time-s~eps should be such that the trace species mixing ratio cannot be negative 
because Eq~ation 7-43 is written in explicit form.  Althoughexplicit methods do not 
require matrix inversion, the time-step must be small enough to ensure positivity and 
numerical stability of the solution. 

I•"',.· 

" 

'" 

I 

11 

,,, 

7.3 

Horizontal Mixing Algorithms 

Unfortunately, our understanding of horizontal turbulence is limited due to the lack of adequate 
turbulence me1;1Surements as well as the scale dependency of the problem.  In earlier days of 
atmospheric modeling, the horizontal diffusion process was often ignored because the numerical 
diffusion associated with the advection algorithms used was large. For problems with large scales, 
such as regional to global transport studies, the coarse grid resolutions did not require an explicit 
implementation of horizontal diffusion.  However, with the advent of very accurate (i.e., less 
diffusive) numerical advection schemes and the emerging need for high resolution grids for urban 
scale problems, a good algorithm for horizontal diffusion is required.  The skill needed is 
balancing numerical diffusion associated with the advection schemes with the added explicit 
diffusion to model horizontal diffusion in the atmosphere.  A fundamental problem is that we do 
not know much about the expected magnitude of the actual horizontal diffusion.  In this section 
we will describe the numerical algorithm for the horizontal diffusion implemented in the CMAQ 
system. 

The horizontal diffusion process in the curvilinear coordinate system (See Equation 6-25' in 
Chapter 6) is given as: 

(7-74) 

There are not many choices for the horizontal diffusion parameterizations. Frequently, the 
horizontal turbulent fluxes are parameterized using eddy diffusion theory.  The contributions of 
the off-diagonal diffusion terms show up explicitly as in Equation 6.,.14 in Chapter 6.  Often, 
these off-diagonal terms are neglected in air quality simulations, and in the CMAQ 
implementation, we solve for diagonal terms only: 

ac; I  = _j_[  E-cK.11 aqi )]  _J__[  E-cK.22  aqi )] 
. 
a.x2 
at  hdiff 

a.xi  + ax2  "r P 

a.xi  "r P 

(7-75) 

The contravariant eddy diffusivity components are related to the Cartesian counterparts as 
K11  = mKxx  and  K22  = mKYY.  In practice, for Eulerian air quality modeling, we do not distinguish 
between eddy diffusivities in two different horizontal directions (i.e.,  Kxx  = KYY  = KH ).  For a 
Lagrangian simulation of atmospheric turbulence, the longitudinal. (following the plume 
movement) and lateral (perpendicular to the plume movement) dispersion are treated ~ifferently 
according to characteristics from the isotropic turbulence analysis.  Often the horizontal eddy 
diffusivity in the Cartesian coordinates is parameterized with the magnitude of the deformation in 
the gridded wind field.  For that case, one must be careful whether the wind data are represented 
in Cartesian coordinates or in the transformed coordinates. 
Unlike the vertical diffusion case, we do not separate the grid divergence terri:i from the diffusion 
e'!-uation.  An,,explicit solution method for Equation 7-75 is: 

• 

11+1 

(p  ),,,. q,,.,  - (p  ),,,.q,,lfl + (.1£')2  (p  )l+l,in 

11+1  _ 

L1 

n 

.. 

• 

n 

n 

l+l,,.(q,+l,m  -q,,,.)-(p  ),.m 

n 

/,,.(q,,llt  -q/-1,m) 

n 

-,.. 

*  n  Kll 

] 

' 

"t[ 

-,.. 
•  n  Klt 

(c  • )" 
, + (.1£2)2  p  /,.,+I 

Lit 

t22  < • 
l.m+I  q,,.,+I  - q,,,.  - p  /,in 

• )  <  • )"  t22 c • 

•  )] 
/,m  ql.m  - ql,n-1 

' 

(7-76) 

-,..-

1 

w!?,ere  K:~ = 2(K,'.~+I + K11.~) and  K,~! = 

- , . . -

A 

A 

1 
2 

A 

A 

(K,~ ...  + K,~).  At the boundary cells, a zero-gradient 

bqundary condition (Neumann) is applied.  Because Equation 7-76 is an explicit scheme, the 
time-step should be chosen to prevent numerical instability and to maintain positivity.  With an 
a~propriate Courant number for horizontal diffusion,  f3htliff"  the time-step can be determined 

I 

I 

with: 

,,,, 

Atl,uiiff = f3htliff 

(L1x)2 
(K"  t22) · 

,,.. 

,,. 

max 
'V(l,in) 

(7-77) 

At present  f3htliff= 0.3  and a uniform eddy diffusivity  KHl.11r= 4
grid resolution.  To compensate for larger grid sizes for coarser grids, the eddy diffusivity is 
modified to give: 

km  =  2000 m2/s is used for the 4-km 

K  I  - ( 4000)2 K  I 

H  "1r  -

(L1x)2 

H  "1r=4km 

(7-78) 

where  L1x  is in meters. 

Obviously, the above parameterization is too simple to be realistic in a variety of atmospheric 
conditions.  Also, depending on the numerical advection algorithms chosen, the artificial 
diffusivity can be quite different.  This calls for several in-depth studies on following two major 
issues: 

I 

(1) 

Quantification ofrealistic horizontal sub-grid scale diffusion. 

The simplest approach is to assume a space independent diffusivity (e.g., Kh =  50 m2/s). 
Smagorinsk.y (1963) formulated a horizontal diffusivity that accounts for diffusion due to 
distortion or stress in the horizontal wind field. For plumes which are several, kilometers or more 
across, the Briggs (1973) parameterizations of horizontal diffusion define th~ diffusivity as a 
constant times the transport wind speed.  The constant is usually based on the landuse (i.e., 
urban or rural) and the stability class (i.e., stable through unstable).  However, it is difficult to 
quantify what the horizontal eddy diffusivity should be appropriate for a variety of atmo.spheric 
conditions without more detailed wind field and turbulence information. 

(2)  Maintaining appropriate horizontal diffusion in the presence of numerical diffusion. 

Most methods for simulating advective transport in current models yield an effective numerical 
diffusivity much larger than physical horizontal diffusivities (Y amartino et al.,  1992).  Thus, the 
physical process may be outweighed by the numerical errors in the model.  A re-assessment of 
this issue is required when the resolution of the model changes or when the method for simulating 
advection is updated.  For idealized concentrations and wind fields distributions, we may be able 
to quantify the magnitude of the numerical diffusion in an advection scheme.  However, for the 
more general applications, estimating the magnitude of numerical diffusion with a specific 
advection scheme is almost impossible.  Refer to Odman (1997) for methodologies that quantify 
numerical diffusion errors associated with advection algorithms. 

7.4 

Conclusions 

In this chapter, we have described numerical advection and diffusion algorithms.  It has two 
purposes: to provide a description of the algorithms currently implemented in CCTM, and to 
describe the fundamental formulations that would guide future implementation of advection and 
diffusion modules.  We encourage the development of algorithms in conservation (i.e., flux) forms 
to ensure compatibility of new modules with existing ones. 

Because of the concerns over the non-monotonicity of BOT and YAM schemes and the mass 
conservation problem and diffusive nature ofBOT-M, we have used PPM for a number of 
demonstration executions (Byun et al.,  1998).  Similar testing with BOT and YAM is underway. 
We intend to integrate other methods into the CCTM at a later time. 

Also, we have identified several aspects in vertical and horizontal diffusion algorithms that 
require additional quantitative stUdies: 

• 
• 

• 

• 

Effects of the parameterization for the free troposphere; 
Importance of coordinate divergence term for vertical diffusion, in particular for the 
height-based constant coordinates; 
Characteristics  among competing algorithms for the vertical diffusion, such as TK.E and 
transilient turbulence schemes; and 
Practical and theoretiCal concerns with the horizontal diffusion algorithms. 

7.5 

References 

Alapaty, K., R. Mathur, and D.W. Byun, 1996: A modeling study of vertical diffusion of passive 
~d reactive trac~r~ using local- and nonlocal-closure boundary layer schemes. Air Pollution 
Modeling and Its Application XI, ed. S.E. Gryning and F. Schiermeier. 433-442. 

'""" 

,,, 

" 

, 

, 

"I 

Alapaty, K.A., J.E. Pleim, S. Raman, D.S. Niyogi, and D.W. Byun.  1997: Shnulation of 
atinospheric boundary layer processes using local- and nonlocal-closure schemes. J  Applied 
Meteor. Vol. 36. 214-233. 

Blackadar, A. K., 1978: Modeling pollutant transfer during daytime convection, Preprints, 
Fpwth Symposium on Atmospheric Turbulence,  Diffusion,  and Air Quality, Reno, Am. Meteor. 
Soc., 443-44 7. 

I 
I 

Bott, A.,  1989: A positive definite advection scheme obtained by nonlinear renormalization of 
the advective fluxes. Mon.  Wea.  Rev.  117, 1006-1015. 

Bott, A.,  1992: Monotone flux limitation in the area-preserving flux-form advection algorithm. 
Mon.  Wea.  Rev.  120, 2592-2602. 

Bott, A., 1993: The monotone area-preserving flux-form advection algorithm: reducing the time(cid:173)
splitting error in two-dimensional flow fields." Mon.  Wea.  Rev.  121. 2637-2641. 

Briggs, G. A., 1973: Diffusion estimates for small emissions. ATDL Contribution No. 79 
[Available from Atmospheric Turbulence and Diffusion Laboratory, Oak Ridge, Tenn.] 

Brooks, A. N., and T. J. R. Hughes, 1982: Streamline upwind/Petrov-Galerkin formulations for 
cpnvection d~min~ted flows with particular emphasis on the incompressible'Navier-Stokes 
equations. Comp.  Meth.  Appl. Mech.  Eng.  32, 199-259. 

· 

1'1•,I 

'i' 

1
1 

II' 

Ii 

111

I 
I 

Burk, S.D., and W.T. Thompson, 1989: A vertically nested regional numerical weather prediction 
model with second-order physics. Mon.  Wea.  Rev.  117, 2305-2324. 
Businger J. A.~ i C.  Wyrigaard, Y. Izumi, ~d E. F. Bradley, 1971:,Flux profile relationships in 
the atmospheric surface layer, J.  Atmos. Sci., 28, 181-189. 

I• 

" 

"' 

I 

Byun, D. W., 1999a: Dynamically consistent formulations in meteorological and air quality 
~odels for ~111ti-s~ale atmospheric applications: I. Governing equations in a generalized 
coordinate system. J.  Atmos. Sci., (in print). 

Bylli1, D. W.,  1999b: Dynamically consistent formulations in meteorological and air quality 
models for multi-scale atmospheric applications: II. Mass conservation issues. J.  Atmos. Sci., (in 
print). 

Bylli1, D. W., and R. L. Dennis, 1995: Design artifacts in Eulerian air quality models: Evaluation 
of the effects of layer thickness and vertical profile correction on surface ozone concentrations. 
Atmos.  Environ., 29, 105-126. 

Bylli1, D.W., and S.-M. Lee,  1999: Mass conservative numerical integration of trace species 
conservation equation: I Experiment with idealized two-dimensional flows.  (in preparation) 

Byun, D.W., J. Yolli1g., G.  Gipson., J. Godowitch., F. Binkowsk., S. Roselle, B. Benjey, J. 
Pleim, J.  Ching., J. Novak, C.  Coats, T. Odman, A.  Hamia, K.  Alapaty, R.  Mathur, J.  McHenry, 
U.  Shankar, S. Fine, A. Xiu, and C. Jang,  1998: Description of the Models-3 Community 
Multiscale Air Quality (CMAQ) model.  Proceedings of the American Meteorological Society 
78th Annual Meeting, Phoenix, AZ, Jan.  11-16, 1998. 

Carpenter, R. L., K. K. Droegemeier, P.R. Woodward, and, C. E., Hane, 1990: Application of 
the piecewise parabolic method (PPM) to meteorological modeling. Mon.  Wea.  Rev.  118, 586-
612. 

Chang, J.S., R.A. Brost, LS.A. Isaksen, S. Madronich, P.  Middleton, W.R. Stockwell, and C.J. 
Walcek,  1987: A three-dimensional Eulerian acid deposition model: Physical concepts and 
formulation, J.  ofGeophys. Res., 92, 14,681-700. 

Childs, P. N., and K.  W. Morton, 1990: Characteristic Galerkin methods for scalar conservation 
laws in one dimension. SIAM J.  Numer.  Anal.  27, 553-594. 

Chock, D. P., and A. M. Dunker, 1983: A comparison of numerical methods for solving the 
advection equation. Atmos.  Environ.  17(1 ),  11-24. 

Chock, D. P., 1985: A comparison of numerical methods for solving the advection equation - II. 
Atmos.  Environ.  19( 4), 571-586. 

Chock, D. P., 1991: A comparison of numerical methods for solving the advection equation-III. 
Atmos.  Environ.  25A(5/6), 853-871. 

Chock, D. P.,  1999: Personal communication. 

Colella, P., and, P.R. Woodward, 1984: The piecewise parabolic method (PPM) for gas(cid:173)
dynamical simulations. J.  Comp.  Phys 54, 174-201. 

Dabdub, D., and Seinfeld, J.  H., 1994: Numerical advective schemes used in air quality models -
sequential aJl.d parallel implementation. Atmos. Environ.  28(20), 3369-3385.' 

Detering, H.W., and D. Etling, 1985: Application ofE-e turbulence model to the atmospheric 
boundary layer. Bozmd.-Layer Meteor.,  33, 113-133. 

Ebert, E.E., U. Schumann, and R. B. Stull, 1989: Nonlocal turbulence mixing in the convective 
~undary layer evaluated from large-eddy simulation. J.  Atmos. Sci., 46, 2178-2207. 

Flatoy, F .,  1993: Balanced wind in advanced advection schemes when species with long lifetimes 
are transported. Atmos.  Environ.  27A(l2), 1809-1819. 

I 
I 
i 

'I 

1, 

"' 

""" 

Hass, H., H.J. Jacobs, M. Memmescheimer, A. Ebel, and J.S. Change, 1991: Simulation of a wet 
deposition case in Europe using the European Acid Deposition Model (EURAD).  Air Pollution 
Modelling and Its Applications, Vol. VIII (edited by van Dop H. and D.G. Steyn), Plenum Press, 
205-213. 

I 

' 

'" 

" 

' 

Holtslag, A. A., E. I. F. de Bruin, and H.-L. Pan, 1990: A high resolution air mass transformation 
model for shprt-1<µ1ge  weather forecasting. Mon.  Weather Rev., 118, 1561-1575. 

I 

Leith, C. C., 1965: Numerical simulation of the earth's atmosphere.  A chapter in Methods in 
Computational Physics.  Vol.  4,  Applications in Hydrodynamics.  p. I. ed., B. Alder, Academic 
l;lress, New York. 

'"' 

,, 

Lohner, R., K. Morgan, J. Peraire, and M. Vahdati, 1987: Finite element flux-corrected transport 
(FEM-FCT) for the Euler and Navier-Stokes Equations.  Finite Elements in Fluids.  Vol.  7 . John 
Wiley &  Sons.  lO:S-121. 

I 

Mellor, G.L: andT. Yamada, 1974: A hierarchy of turbulence closure models for planetary 
boundary layers, J. Atmos. Sci., 31, 1791-1806. 

• 

I 

Mellor, G.L, and'.f. Yamada, 1982: Development of a turbulence closure model for geophysical 
fluid problems. Reviews ofGeophysica and Space Physics, 20, 851-875. 

• 

I 

! 

Odman, M. T.,  1997: A quantitative analysis of numerical diffusion introduced by advection 
algorithms in air quality models. Atmos.  Environ.  31, 1933-1940. 

" 

Odman, M. T., 1998: Research on Numerical Transport Algorithms for Air Quality Simulation 
Models. EPA Report. EPA/660/R-97/142. [Available from National Exposure Research 
Laboratory, U.S. Environmental Protection Agency, Research Triangle Park, NC 27711]. 

Odman, M. T., and A.G. Russell, 1993: A nonlinear filtering algorithm for multi-dimensional 
finite element pollutant advection schemes. Atmos. Environ.  27A, 793-799. 

Odman, M. T., A. Xiu, arid D. W. Byun, 1995: Evaluating advection schemes for use in the next 
generation of air quality modeling systems, In Regional Photochemical Measurement and 
Modeling Studies, eds A.J. Ranzieri and P.A. Solomon, Vol. Ill, Air &  Waste Management 
Association, Pittsburgh, PA.  1386-1401. 

Oran, E., and J.  Boris, 1987: Numerical Solution of Reactive Flow, New York, Elsevier. 

Otte, M.J. and J.C. Wyngaard, 1996: A general framework for an "unmixed layer" PBL model. J. 
Atmos.  Sci., 53, 2652-2670. 

Pleim, J.E. and J.  Chang, 1992: Anon-local closure model for vertical mixing in the convective 
boundary layer.Atmos.  Envi., 26A, 965-981. 

Raymond, W.H. and R.B. Stull, 1990: Application oftransilient turbulence theory to mesoscale 
numerical weather forecasting. Mon.  Weather Rev., 118, 2471-2499. 

Rood, R. B., 1987: Numerical advection algorithms and their role in atmospheric transport and 
chemistry models. Rev.  Geophysics 25(1), 71-100. 

Schumann, U., 1989: Large-eddy simulation of turbulent diffusion with chemical reactions in the 
convective boundary layer. Atmos. Envi., 26A, 965-981. 

Smagorinsky, J.,  1983: General circulation experiments with the primitive equations:  1. The basic 
experiment. Mon.  Wea.  Rev.  91, 99-164. 

Stull, R.  B., 1988: An Introduction to Boundary Layer Meteorology.  Kluwer Academic, 666 pp. 

Stull, R. B., 1993: Review of non-local mixing in turbulent atmosphere: Transilient turbulence 
theory. Bound-Layer Meteor.,  62, 21-96. 

Stull, R. B., and A.G.M. Driedonk, 1987: Applications of the transilient turbulence 
parameterization to atmospheric boundary layer simulations. B~und. -Layer Meteor.,  40, 209-
239. 

Tremback, G. J., Powell, J., Cotton, W.R. and Pielke, R. A., 1987: The forward-in-time 
upstream advection scheme: extension to higher orders . .Nfon.  Wea.  Rev.  115, 540-555. 

Yamartino, R. J.,  1993: Nonnegative, conserved scalar transport using grid-cell-centered, 
spectrally constrained Blackman cubics for applications on a variable-thickness mesh. Mon.  Wea. 
Rev.  121, 753-763. 

Yamartino, R.J., J.S. Scire, G.R. Carmichael, and Y.S. Chang, 1992: The CALGRID mesoscale 
photochemical grid model-Part I.  Model formulation, Atmos. Environ.  26A, 1493-1512. 

'"'''''' 

' 

,, 

I 

Yanenko, N. N., 1971: The Method of Fractional Steps. Spring-Verlag, New York,  160pp. 

Zhang, Q., al1.d Stull, R. B. Stull, 1992: Alternative nonlocal descriptions of boundary-layer 
evolution. J. Atmos. Sci., 49, 2267-2281. 

This chapter is taken from Science Algorithms of the EPA Models-3 Community 
Multiscale Air Quality (CMAQ) Modeling System, edited by D. W. Byun and J. K. S. 
Ching, 1999. 

; 

11',•1 

Appendix 7 A  Numerical Solvers for Diffusion Equations 
==============

In this appendix we describe numerical procedures for eddy diffusion, the Blackadar mixing 
scheme, and the asymmetric convective model. 

7 A.1  Stability of Tridiagonal Solver 

The eddy diffusion formulation requires the solution of the linear equation 

Aq=b 

where: 

(7A-1) 

A= 

C1 
d2 

Cl;i 

di 
a2 
0 

0 
0 

0 
C2 
d3 

0 
0 

0 
0 
C3 

0 
0 
0 

an-I 
0 

dn-1 
an 

Cn-1 
dn 

q=(q1  q2 

q3 

qn(; and 

b=(h1  h2 

b3 

bn(• 

The system Equation 7 A-1  can be solved by the Thomas algorithm (Gaussian elimination of a 
tridiagonal matrix without pivoting) followed by back substitution.  Assume that the following 
stage of the elimination has been reached: 

(7A-2a) 

(7A-2b) 

(7A-3a) 

(7A-3b) 

Forj=2,3; .. n-1, eliminating  qi-I  from Equations 7A-2a,b leads to: 

where 

a.c.  1 
a. = d. - -1.......1:::._ 
a 

i 

i 

j-1 

f3  = b. - ajf3j-I 
a. 
l 
,  1-I 

l 

T!}e last pair ~f simultaneous equations are: 

lXn-lqn~I + Cnqn  = l3n-I 
anqn-1 + dnqn  = bn 

Eliminating  qn-I  gives: 

, 

""II 

,.", 

,,, 

(7A-3c) 

(7A-4a) 
(7A-4b) 

(7A-5a) 

atd with Equation 7A-3a, we can obtain the solution by back substitution, i.e.,j = n-l, n-2;··,1: 

(7A-5b) 

The algorithn1 described above is stable for the tridiagonal system if: 

(i)  di> 0,  a1 <0, andc1 < O; 
(ii)  d1 > -(aj+I + Cl~I) forj=l,i, ;··n-i, defining  C0  =an = 0; and 
(iii) d1 >-(a1 +c1)  forj=l,2, ;··n-1, defining  a1 =en-I =0. 

~.~ first two cond~tions e~sure tp(!.t the forward eliminatiqn is stable and tQ.e  first and third 
conditions ensure that the back substitution is stable. 

T? prove that., ~e forward elimination procedure is stable, it is necessary to show that the moduli 
of the multipliers  m1 = -a1 I a1_1  used to eliminate qi, q2,  •••  are positive and less than or equal to 
one.  From Equation 7A-2, we get: 

I 

and 

(7A-6a) 

(7A-6b) 

(7A-7a) 

Similarly, we have: 

EPA./600/R.-991030 

since  d2 > -( ll:i + c1)  •  In this way, we can show that  0 < mi < 1 for j= 1, 2, · ··n-1. 

For back substitution, we can write: 

(7A-7b) 

(7A-8) 

I 

' 

' 

n-1 

with  a  = c  = 0 fior;"=l  2  ···n-1 
There will be no build-up of errors in the back substitution process if !Pi+! I < 1, where: 

• 

Now, 0 < p2  = -c1Id1 <1, since  a1 =0 and  d1 >-c1  by hypothesis.  Then: 

P  -

3-

-c 
2 

d2  +a2P2 

As  -c2  > 0,  0<p2 <1, and  d2 > -a2  > 0, it follows that: 

(7A-9a) 

(7A-9b) 

(7A-10) 

Similarly, we can show that 0<pi<1 forj=l, 2;··n-1. 

7 A.2  Solver for Blackadar Scheme 

The Blackadar scheme requires solving the sparse linear matrix equation_ Aq=b of the form 

A= 

di 
e2 
e3 

en-I 
en 

ln-1 

h  A 
0 
0 
di 
~  0 
0 

0 
0 

0 
0 

dn-1 
0 

In 
0 
0 

0 
dn 

(7A-11) 

The solver for this system uses a similar numerical procedure as for the tridiagonal system.  The 
e.l~me~~ of 1h,e Hnear set of ~quations are related as follows: 

dlql + "L.'1q1 = b, 

n 

)=2 

and for  2 S. j  S. n: 

'li'',,,,I 

By substitu~g q1 with  q1 for eachj, we get: 

~.d for  2 S. j  ·.;;; n : ·· 

111 

(7A-12a) 

(7A-12b) 

(7A-13a) 

(7A-13b) 

Note that Equation 7A-13b involves neither a fonvard nor a backward substitution loop. 

1A.3  Sofye[ for Asymmetric Convective Model (ACM) 

The Asymmetric Convective Model requires solving the linear matrix equation Aq=b with a 
sparse matrix of the form: 
r 

, 

i. 

A= 

C2 

d2 
0 

di 

e2 

e3 

en-I 

en 

0 

C2 

d3 

0 
0 

0 

0 
0 
0 

dn-1 
0 

Cn-1 

dn 

0 

C3 

0 
0 

EPA/600/R-991030 

(7A-14) 

The solver for this system is based on a numerical procedure .. simil~ to the tridiagonal solver. 
The relation among the variables qi are given as: 

and for  2 $. j  $. n - 1: 

(7A-15a) 

(7A-15b) 

(7A-15c) 

Assume that the following stage of the elimination has been reached for  1 $. j  $. n - 1 : 

where: 

a.=e.--1 a. 1 

l-

l 

[3 . = b. _ _  l 

[J.  I 

l(cid:173)

l 

d. 

cj-1 
d. 

cj-1 

l 

l 

(7A-16a) 

(7A-16b) 

(7A-16c) 

with  a 1 = d 1  and /31 = b1•  From Equation 7 A-16a for  j  = n - 1 and Equation 7 A-15b,  q1 is found 
to be: 

and all other qis are computed with: 

-
j+I  -

q 

[3.  -a.qi 
l 

l 

ci 

7-55 

(7A-17a) 

(7A-17b) 

for  1 ~ j  ~ n - 1.  The final substitution stage can be implemented either in a forward or backward 
sweep. 


Chapter 9
=============

GAS-PHASE CHEMISTRY 

Human Exposure and Atmospheric Sciences Division 

Gerald L. Gipsolll * 

National Exposure Research Laboratory 
U.  S.  Environmental Protection Agency 
Research Triangle Park, NC 27711, USA 

Jeffrey 0. Young•• 

Atmospheric Modeling Division 

National Exposure Research Laboratory 
U.S. Environmental Protection Agency 
Research Triangle Park, NC 27711, USA 

ABSTRACT 

This chapter describes the manner in which gas-phase chemistry is treated in the Models-3 
Community Multiscale Air Quality (CMAQ) modeling system.  The CMAQ system currently 
includes two chemical mechanisms -- RADM2 and CB4 -- with plans to incorporate a third -- the 
SAPRC97 mechanism -- in the near future.  Each of these mechanisms is described, and the 
manner in which the first two are  linked to the aqueous chemistry and aerosol formation 
processes is discussed.  Enhanced isoprene chemistry that has been included in the RADM2 
mechanism is also described, and  procedures for entering new chemical mechanisms in the 
CMAQ system are addressed.  The representation of reaction kinetics in the CMAQ system and 
the numerical  modeling of gas-phase chemistry are also presented.  The CMAQ system currently 
employs two numerical solvers, SMVGEAR and a variant of the QSSA method. The numerical 
procedures used in each are presented, and the relative computational efficiencies of each on 
different computing platforms are not.ed. 

•Corresponding author address: Gerald L. Gipson, MD-80, Research Triangle Park, NC 27711.  E-mail: 
ggb@hpcc.epa.gov 

··on assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 
