<!-- BEGIN COMMENT --> 

[<< Previous Chapter](CMAQ_Science_Ch_09.md) - [Home](README.md) - [Next Chapter >>](CMAQ_Science_Ch_11.md)

<!-- END COMMENT -->

AEROSOLS IN MODELS-3 CMAQ 

Chapter 10 
=============

Francis S. Binkowski" 

Atmospheric Modeling Division 

National Exposure Research Laboratory 
U.S. Environmental Protection Agency 

Research Triangle Park, North Carolina 27711 

ABSTRACT 

The aerosol module of the CMAQ is designed to be an efficient and economical depiction of 
aerosol dynamics in the atmosphere.  The approach taken represents the particle size distribution 
as the superposition of three lognormal subdistributions, called modes.  The processes of 
coagulation, particle growth by the addition of new mass, particle formation, etc. are included. 
Time stepping is done with analytical solution to the differential equations for the conservation 
of number and species mass conservation.  The module considers both PM2.5 and PM JO  and 
includes estimates of the primary emissions of elemental and organic carbon, dust and other 
species not further specified.  Secondary species considered are sulfate, nitrate, ammonium, 
water and organic from precursors of anthropogenic and biogenic origin.  Extinction of visible 
light by aerosols represented by two methods, a parametric approximation to Mie extinction and 
an empirical approach based upon field data.  The algorithms describing cloud interactions are 
also included in this chapter. 

"On assignment from the National Oceanic and Atmospheric Administration, U.S.  Department of Commerce. 
Corresponding author address: Francis S.  Binkowski, MD-80, Research Triangle Park, NC 27711.  E-mail: 
fzb@hpcc.epa.gov 

## 10.0  AEROSOLS IN MODELS-3 CMAQ 
Inclusion of aerosol particles in an air quality model presents several challenges.  Amo~g these 
are the differences between the physical characteristics of gases and particles.  In treating gases 
in an air quality model, the size of the gas molecules is not usually of primary importance.  In 
contrast, particle size is of primary importance.  The interaction between condensing vapors and 
the target particle depends in an important way on the particle size in relation to the mean free 
path in the atmosphere.  For gases, once the concentration is known, the corresponding number 
of molecules is known.  This is not the case for particles.  Thus, including aerosol particles in an 
air quality model means choosing how the total number, total mass, and size distribution of the 
particles is represented.  Oncl:'. this choice is made, then important physical anq chcm:iical 
processes involving particles must be represented.  Particles may be emitted into t~.e air by 
natural processes such as wind blowing dust from a desert.  Human activities may disturb the soil 
to allow wind to blow soil particles off the ground.  Sea salt particles come into the atmosphere 
by wind driven waves on the sea surface.  Volcanic activity is another source of particles for both 
the troposphere and the stratosphere.  Particles can be made in the atmosphere directly from 
chemical reaction.  The most important example of this is the transfonnation of sulfur dioxide, a 
by-product of fossil  fuel  combustion, into sulfate particles.  Hydroxyl radicals
•• ~ttas,~ the ,sulfu~. 
dioxide and make sulfuric acid that then may nucleate in the presence of water vapor and 
.:
ammonia to produce new particles.  If there are particles already present in the atmosphere, the · 
new sulfate may condense on the existing particles or nucleate to form new particles depending 
upon conditions which are only recently beginning to be understood.  Reactions of organic 
precursors such as natural monoterpenes and anthropogenic organic species with ozone and other 
oxidants or radicals make new species that condense on: existing particles or make new particlt!!) 
depending upon conditions.  Combustion sources emit particles composed of mixtures of organic 
carbon and elemental carbon.  The exact mixture of organic and elemental carbon is a strong 
function of the conditions of combustion.  Once these particles are in the air, they may grow by 
condensing of species upon them as has already been mentioned.  For a large group of particles 
made in the air, i.e., secondary particles, growth may be related to relative humidity because of 
water condensing on the particles.  Another gas-particle interaction is the chemical equilibration 
of species within or on the surface of a particle with gases and vapors within the air.  Unlike 
gases, particles coagulate, e.g., collide and form a particle whose mass and volume are the sums 
of the masses and volumes of the colliding particles.  Thus, adding particles to an air quality 
model means adding a new set of physical processes. 

11111 

1 

In designing the aerosol component of CMAQ the following assumptions were made.  Any 
representation of particles had to be consistent with observations of particles. The representation 
had to be mathematically and numerically efficient to  minimize computer time.  And finally the 
representation had to be usable for regional to urban simulations.  These assumptions led to a 
choice of two methods.  The first method would be to model particle behavior in set of bins of 
increasing size.  This approach is quite popular and is described originally by Gelbard et. al. 
(1980) and more recently by Jacobson (1997). The second approach, the one chosen for 
implementation in CMAQ, is to follow Whitby (1978) and model the particles as a superposition 
of lognormal subdistributions called modes. The sectional method using the discrete size bins 
requires a large number of bins to capture the size distribution.  If one wishes to model several 
chemical components then the number of components is multiplied by the number of bins. This 
leads to a very large number of variables that must be added to an air quality model to capture 
particle behavior. In the modal approach, using the three modes suggested by Whitby(l 978), 
only three integral properties of the distribution, the total particle number concentration, the total 
surface area concentration, and the total mass concentration of the individual chemical 
components in each of the three mode. The current approach differs from that taken by 
Binkowski and Shankar (1995) where the sixth moment was chosen as a third in integral 
property in place of the second moment. That moment as chosen because of a mathematical 
simplification (see Whitby and McMurry,  1997). The mathematical simplifications of the modal 
method allow analytical solutions to be used for the aerosol dynamics. The current approach uses 
numerical quadratures to calculate all of the coagulation terms. The numerical quadratures were 
compared with the analytical expressions exhibited in Whitby et al.  (1991) and are accurate to six 
decimal places. The choice of using numerical quadratures was made to reduce the memory 
requirements associated with a variable geometric standard deviation and because the second 
moment unlike the sixth moment does not have an analytical form. 

The aerosol component of the CMAQ is derived from the Regional Particulate Model (RPM) 
{Binkowski and Shankar,  1995) which in, turn, is based upon the paradigm of the Regional Acid 
Deposition Model (RADM), an Eulerian framework model (Chang et al.,  1990).  The particles 
are divided into two groups, which are fine particles and coarse particles.  These groups generally 
have separate source mechanisms and chemical characteristics.  The fine particles result from 
combustion processes and chemical production of material that then condenses upon existing 
particles or forms new particles by nucleation.  The coarse group is composed of material such as 
wind-blown dust and marine particles (sea salt).  The anthropogenic component of the coarse 
particles is most often identified with industrial processes.  The common EPA nomenclature used 
in air quality refers to PM25 (particles with diameters less than 2.5 µm)  and PM 10  (particles with 
diameters less than 1 Oµm).  Note that PM10  includes PM 2.5•  Thus, in the present context, coarse 
particles are those with diameters between 2.5 and  10 µm.  Then, the mass of the coarse particles 
is the difference between the masses in PM 10  and PM25 • 

As already noted, the aerosol particle size distribution is modeled using the concepts developed 
by  Whitby (1978).  That is, PM25 is treated by two interacting subdistributions or modes.  The 
coarse particles form a third mode.  Conceptually within the fine group, the smaller (nuclei or 
Aitken), i-mode represents fresh particles either from nucleation or from direct emission, while 
the larger (accumulation),j-mode represents aged particles.  Primary emissions may also be 
distributed between these two modes.  The two modes interact with each other through 
coagulation.  Each mode may grow through condensation of gaseous precursors; each mode is 
subject to wet and dry deposition.  Finally, the smaller mode may grow into the larger mode and 
partially merge with it.  These processes are described in the following subsections.  The 
chemical species treated in the aerosol component are fine species sulfates, nitrates, ammonium, 
water, anthropogenic and biogenic organic carbon, elemental carbon, and other unspecified 
material of anthropogenic origin.  The coarse-mode species include sea salt, wind-blown dust, 
and other unspecified material of anthropogenic origin.  Because atmospheric transparency or 
visual range is an important air quality related value, the aerosol component also calculates 
estimates of visual range and aerosol extinction coefficient. 


## 10.1  Aerosol Dynamics 

The particle dynamics of this aerosol distribution are described fully in Whitby et al. (1991) and 
Whitby and McMurry 1997); therefore, only a brief summary of the method is given here. 

(Note: In the following equations repeated subscripts are not summed.) 

### 10.1.1  Modal Definitions 

Given a lognormal distribution defined as 

n(lnD) =  ~ exp[-O.s(

2n  In cr g 

ln ~) 2 

1
n cr g 

(10-1) 

where N is the particle number concentration, D  the particle diameter, and Dg and crg the 
geometric mean dicµneter an.? standard deviation of the distribution, respectively.  The kth 
moment of the dis~ibution is defined as 

Mk=  [  DknQnD) dQnD) 

with the result 

Mk= N Dg  exp  Tin  crg · 

k 

2 

l 

k 
2 

[ 

(10-2) 

(10-3) 

M 0  is the total number, N, of aerosol particles within the ~ode suspended in a unit volume of air. 
Fork= 2, the moment is proportional to the total particulate surface area within the mode per , 
unit volume of air. Fork= 3, the moment is proportional to the total particulate volume within 
the mode per unit volume of air. The constant of proportionality between M 2 and surface area is 
n; the constant of proportionality between M3  and volume is n/6. Note that the ge9metric 
,,,1111 
standard deviation is the same no matter which moment is selected. M 3  is determined from the .. 
nine distinct fine aerosol species (including water) listed in Table I 0-1  as follows: 

nmax 

M3i=  L  <p7 
()Pn 

n= I  TC 

M=~"1t 

nmax  <pn} 
n~I  ()Pn 

3j 

(10-4a) 

(10-4b) 

where cp?  and  cpnJ  are the species mass concentrations of the nth species in each mode in 
[µg  m·3

], Pn is the average density of the nth species.  The third moment for  the coarse mode is 

10-4 

obtained in a similar manner.  Given a value of third moment concentration and number 
concentration, the geometric mean standard deviation and the ge~metric mean diameter for each 
mode is diagnosed from 

· 

EP A/600/R-99103 0 

ln

2

cr g = ~(21n(M3) -3 In(~ )  -In (MJ 

3_ 

Dg -

M3 
r 9  2 

~· 

Nexp  2ln crg 

(IO-Sa) 

(10-Sb) 

The prediction equations for number, second moment and species mass are given in Section 
10.1.4. 

### 10.1.2  New Particle Production by Nucleation 

The CMAQ aerosol component has a choice of two particle production mechanisms, those of 
Harrington and Kreidenweis (l 998a,b) and Kulmala et al.  (1998). Both of these methods predict 
the rate of increase of the number of particles, J,  (in number per unit volume per unit time) by 
the nucleation from sulfuric acid vapor. In order to predict the rate of increase on new mass and 
new second moment an assumption about particle size is necessary. Following work by Weber et 
al.  (1997), it is assumed that the new particles are 3.5 nm in diameter. Weber et al. reported 
measurements of the concentration of particles that are in the size range 2.7 to 4.nm. For 
simplicity we have chosen 3.5 nm as a representative diameter. 

Using either of these methods, the production rate of new particle mass'[ µg m-3 s-1

]  is then 

d  Mass  =  ~ pdi.s J 

dt 

and that for number [ m-3  s-1 
d Num  =J 

dt 

]  is 

and that for second moment [ m2 m~3 s-1  ] is 
d  Mz  =di  J 

dt 

3.5 

(10-6a) 

(10-6b) 

(10-6c) 

where d3.5 is the diameter of the 3.5 nm particle and pis the density of the particle (taken as 
sulfuric acid) at ambient relative humidity (Nair and Vohra, 1975). 

### 10.1.3  Primary Emissions 

The EPA emission inventoryfor PM2.s  ~d PM 10  does not currently contain inforil1ation about  " 
neither size distribution nor chemical speciation.  In the CMAQ work, the assumption is that the 
major part of PM2.5 particulate mass emissions are in the accumulation mode with a small 
fraction in the Aitken mode; i.e. a fraction of 0.999 of PM2_5  is assumed to be in the accumulation 
mode and the remaining fraction, 0.001, is assigned to the Aitken mode.  Sensitivity studies V{!,ll 
be conducted to evaluate this assumption. In order to estimate the emissions rate for number and 
second moment fro1:11  the mass emissions rate an assumed mass size distribution is required. It is 
convenient to express the emission rate for number, Eo, and ..  that for second moment E2 in terms 
of a total emissions rate for third moment. This is shown schematically as follows where En  is' 
the mass emissions rate for species n  and Pn is the density for that species 

" I   ,1•11:1,'ll 

'"""""""""' 

" " ' '  

'"""' 

"'" 

"" 

' 

' 

" 

E,n~ (~)(::) 

,::11 

Eo= 

l 
L E3ri 
9 

11

\ 
Divexp  -2ln cr g) 

2 

( 

E2= 

LE3n 
II( 

\ 
Dgvexp  -~ln2cr g) 

where the sum is taken over all emitted species. 

1111111 

(10-7a) 

,(1" 

(lQ-7b) .. 

.., 

!II' 

(10-7c) 

':11 

In Equation 10-7b,c, Eo and E2 schematically represent the emissions rates for the various 
modes. In Section 10.1.4, the nomenclature used to represent the emissions rate for number for 
each of the three modes will be respectively Eoi, Eoj. and Eocor· 

We have chosen values of 0.3  µm for the geometric mean diameter for mass, D  , and 2.0 for the 
geometric standard deviation, cr g  for the accumulation mode. The corresponding 1:'.,~.ues for th.~ 
Aitken mode are 0.03  µm and 1.7, and those for the coarse mode are 6 µm and 2.2. 

The current emissions inventory estimates that 90% of PM10  is fugitive dust, and that 70% of this 
dust consists of PM2_5  particles. The paradigm adopted for the CMAQ is that fugitive dust is a 
coarse mode phenomenon with a tail that overlaps the PM2.s  range. Therefore, 90% of PMu) 
emissions are assigned entirely to the coarse mode species ASOIL. Sulfate emissions are treated 
differently in CMAQ than in RPM. In RPM sulfate emissions were treated as particles and 
distributed between the Aitken and accumulation modes. In CMAQ, the photochemical module 
has sulfate emissions incorporated into the chemical solver. Thus, the production rate for sulfuric 
acid will include direct emissions of sulfate. This rate is passed from the photochemical module 
to the aerosol module. Assigning fractional amounts of emitted PM2.5 and PM 10  to the specific 
species in Table 10-1  is a matter of ongoing discussions with those responsible for preparing the 
national emissions inventory. 

### 10.1.4  Numerical Solvers 

The numerical solvers for the two fine particle modes in the Models-3 aerosol component have 
been modified from those in RPM, which followed from Whitby et al. (1991).  The major 
difference is that the RPM solvers linearized the quadratic term for intramodal coagulation in the 
equation for modal number concentration.  The new solvers in CMAQ retain this quadratic term. 

The number concentrations for the Aitken and accumulation modes are denoted as Ni and Nj 
respectively.  Intramodal coagulation coefficients are functions only of the geometric mean 
diameters and geometric standard deviations for each mode and are denoted as Foii and FOJj-

Similarly, the intermodal coagulation coefficient for coagulation between the Aitken and 
accumulation modes is FOij·  For simplicity the following coefficients are defined. 

For the Aitken mode: 

a·  =Fo··  b·=N·Fo··  and 
l 

ll• 

lj• 

') 

l 

dNum 

.  dNum 

c i  = 

dt  + Eoi, with ~ from  (I 0-6b ); 

and for the accumulation mode: 

aJ = F O;J•  and CJ  =  Eoj 
The emissions rates for number concentration are Eoi and Eoj and are set to values determined 
for each mode from Equation 10-7b. 

We may now write for the particle number concentrations 

I  =c  -a. 

BN. 
at 
~  j 

I 

I 

N2  -b N.; and 
. 

I 

I 

BN. 
a(  =c Ta 1N1 . 

2 

(I0-8a) 

(10-8b) 

Equation 10-8a, a Riccati type equation and Equation 10-8b, a logistics type equation, have 
different analytical solutions depending upon whether Ci and CJ  are zero or nonzero. These 
analytic solutions are used in the CMAQ solver with the coefficients being held constant over 
one model time step.  In discussing the analytical solutions to Equations 10-8a and b, subscripts 
will be omitted for simplicity 

The solution to Equation 10-8a for Ci* 0 is of the form 

N{t)  =  r,  + r2P exp(Dlj 
a[ 1 + P exp(Dt)] 

iil1::: 

where 
" 
D=(b

2
+4ac)  ,r,=b'+D,r2- -~, 

2ac 

1 

-

p =  _ 

" 
b+ D 

(  ' )  ,, 
r 1 -a N  to 
r
-aNt
) 
0
2

. 

For Ci  = 0, the solution to Equation !p-8a is of the form 

'I 

i' 

' 

1i::, 

~ 

N(t)  = 

bNQ 0)exp (-bt) 

: 
b + aN(t0)[ 1 -exp (-bt)] 

,i'ili  111 

:11: 

11ll1,, 

,, 

,:111;' 

"11111' 

11:11 

:1, 

111·1' 

1111,,111' 

1111 
111, 

The solution to Equation 10-8b when Cj * 0 is of the same form as that to Equation 10-8a except 
b  = 0.  The solution when Cj = 0, known as Smoluchowski's solution, is: 

The equations for the prediction of second moment, M2,  in the Aitken and accumul.~tion mod~s 
are both of the form 

,, 

' ' ~ 11 

' , 11111,, 1 ' 

11 

"' 

' , 1111 

" 

I 

8M Tt =  p2 - L2M2; 

with solutions of the form 

M,~) ~ ~: + [ M,(tJ- ~]exp (- L,t). 

In these equations, production of second moment is denoted by P2 and loss by Li .For the Aitken 
mode, the production term includes the rate of second moment increase by new particle 
formation from Equation 10-6c, condensational growth (Equation 7a of Binkowski and Shank:ar, 
1995) and by primary emissions from Equation 10-7c.  The loss term accounts for the loss of 
second moment by intramodal coagulation, as well as including the transfer of second moment to 
the accumulation mode by intermodal coagulation. For the accumulation mode, the production 
term includes the transfer of second moment by intermodal coagulation, condensational growth 
(Equation 7b of Binkowski and Shankar, 1995) and the contribution of primary emissions from 
Equation 10-7 c. The loss term accounts for intramodal coagulation. 

It is important to note that the history variable in CMAQ is the modal surface area, which, as 
already noted, is n time the second moment. For convenience, however, within the internal 
aerosol subroutines, the second moment is the treated. Before returning to the main CMAQ 
routines, the second moment is multiplied by n.  That is why species number 23  and 24 in Table 
10-1  are identified as modal surface areas. It is also important to note that the surface area 
predicted by CMAQ is the surface area for spherical particles and may not represent the true 
surface area available in nonspherical particles or in porous particles such as carbon soot. 
Empirical correction factors may be needed for use of CMAQ surface area predictions in certain 
applications. 

The equations for mass concentration of species n may be written as: 

8cp" 
0Tt1  = P/'  - L<p 1!; and 

I 

I 

8<p" 
o1=P'~, J 

(10-9a) 

(10-9b) 

I 

I 

I 

i 

't' I 

r y  f  31) 

where  P"  =  ,n" + E':  + R"Q  and  L.  =  NM 17.  .. /  M  . 
3t 
with  <i>7  =  d ~~s from Equation 10-6a, when n denotes sulfate, and where  E; and  E;' are the 
emission rates and Rn is the gas-phase production rate for species n.  The factors O.i and O.j, 
defined by Equations A 17 and A 18 of Binkowski and Shankar ( 1995) represent the fractional 
apportionment of condensing species.  F3if is the coa~ulation coefficient for the third moment. 

Note that the loss of mass in Equation 10-9a is a gain of mass in Equation 10-9b.  This represents 
the transfer of mass by intermodal coagulation.  There is no such transfer of number in Equations 
10-8a,b because of the convention that when a smaller particle coagulates with a larger particle 
there is a loss of number from the population of smaller particles, but no gain of number in the 
population of larger particles.  There is, however, a transfer of mass.  Equations 10-9a and b have 
an analytic solution holding the coefficients constant for the time step of the form: 

cp(t)  = ~ + [ cpQJ- ~]exp (-Lt). 

The solution to Equation 10-9b are by an Euler forward step once again holding the production 
terms constant over that time step. 

The equation for the prediction of coarse mode mass is 

8<p" cor  =Ell 
-or-
wr 

The solution is by an Euler forward step. The equation for coarse mode number is similar 
because coagulation is ignored for the coarse mode, and is also solved by an Euler forward step. 

### 10.1.5  Mode Merging by Renaming 

In Binkowski and Shankar (1995), the Aitken mode diameters grew over the simulation periqd to 
become as large as those in the accumulation mode.  While this is probably true in nature, it  , 
violates the modeling paradigm that tw~ modes of distinct siz~ r~nges alway~~~x~~,~r This 
······· 
phenomenon can be modeled by mode merging as follows.  The Aitken mode approaches the 
accumulation mode by small increments over any model time step when particle growth and 
nucleation are occurring.  Thus, an algorithm is needed that ti:ansfers numbe~}tµd ,,mass 
" 
concentration from the Aitken mode to the accumulation mode when the Aitken mode forcing 
exceeds the accumulation mode forcing and the number of particles in the accumulation mod~ is 
no larger than that in the Aitken mode. 

',•''ill 

olll'llllll 

""""' 

I 

, 

,::.,, 

i;,;:!!1·

1 

Th~s algorithm. is formulated as fo~ows (Binkowski et al.,  19?6):  ~en Equ.,~~i?n~"':l0-10 is 
satisfied, the diameter of overlap,  d , for the modal number distnbut10ns can be calculated 
· · 
exactly.  Given this diameter, the fraction of the total number of Aitken mode particles greater 
than this diameter is easily calculated from the complementary error function 

,iii 

I' 

'" 

"'ill• 

.,,: 

F11u111  = 0.5[ 1 + erfc (x,,,1111)] 

,  where 

I' 

=  In (di dgn~ 
filn ( cr gJ 

x 
"'"" 

,,. 

(10-IOa) 

and dgni  is the geometric m.ean diame~er for the Aitken mode number distri9.~tio?. 

The number concentration corresponding to these particles is transferred to the accumulation  ···· 
mode, a processes denoted here as renaming the particles.  A similar process is U§,~d to transfor 
mass (third moment) concentration and surface area (second moment) concentration from the" 
Aitken to the accumulation mode using the complementary error function corresponding to the 
third moment. 
.,, 

' 

··  "' 

'"" 

·· 

·· 

Fk  =  0.5[ 1 + erfc (xk)]  , 

where  xk =  x11 11111  -

kin (crg) 

= 

(i'O-lOb) 

For numerical stability, the transfer of number and mass is limited so that no more than one half 
of the Aitken mode mass may be transferred at any given time step. 

Th.  · 

is is accomp 1s  e 

1 ·  h  d b 

·  · 

y reqmnng  at 

th  3 In ( cr gJ 

fi  ~ x,,,,m  • 

The fraction of the total number and surface area (k= 2) and mass (k=3) remaining in the Aitken 
mode is calculated from the error function of the overlap diameters as: 

10-10 

<1>1111111  = 0.5[ 1 + erf (x;,,,m)] 
<Dk= 0. s[ 1 + erf(x.J] 

EPA/600/R-99/030 

(10-lOc) 

(10-lOd) 

Using these fractions, Aitken and accumulation mode number and mass concentrations are 
updated as 
0 = N; + F111111,Ni 

(10-11 a) 

n_  n+F  n 
3<p i 

<pj  - <pj 

M2rM2;+F2M2i 

N=<D  N. 

I 

11Ulll 

I 

n_ <D  n 
3<p  i 

cp i  -

Mii= <D2M2i 

(10-11 b) 

(10-llc) 

(10-lld) 

(10-lle) 

(10-1 lf) 

This method of particle renaming is analogous to the procedure discussed by Jacobson (1997) 
where particles are reassigned in the moving center concept of a bin model.  When the particles 
grow beyond the boundaries of their size bin, they are reassigned to a larger bin and averaged 
with the new bin. 

## 10.2  Aerosol Dry Deposition 

The rate of dry deposition of particle zero111  and third moment to the earth's surface provides the 
lower boundary condition for the vertical diffusion of aerosol number and species mass, 
respectively.  The method of doing this follows the RPM approach with the following 
exceptions.  In RPM total fine mass was deposited.  In CMAQ the species mass in each mode is 
deposited separately using the dry deposition velocity for the third moment.  The impaction term 
is omitted for the coarse mode particles in both the zero•h and third moment dry deposition 
velocities.  See Binkowski and Shankar (1995) Equations A25 through A34 for details. 

##10.3  Cloud Processing of Aerosols 

Clouds are formed when the relative humidity reaches a value at which existing aerosol particles 
are activated. That is, they pass through a potential barrier and grow rapidly from a few 
micrometers to several micrometers to become cloud droplets (cloud nucleation). Soluble gases 
are then dissolved into the cloud droplets where aqueous-phase chemical equilibria and reactions 
occur. The attack on dissolved sulfur dioxide by hydrogen peroxide produces a dissolved sulfate 
species (oxidation of Sulfur (IV) to Sulfur (VI)).  Because these processes are very complex in 
detail and occur at subgrid scale, most cloud modeling in mesoscale meteorological models and 
in air quality models uses simplified parametric approaches to model the effect of clouds rather 
than modeling the clouds directly. This approach was used in RADM and RPM and is applied in 
the first version of CMAQ. 

,'I,,, 

The assumptions for aerosol,"behavior in clouds are: 

• 

• 

• 

• 

• 

The Aitken ( i ) mode forms interstitial aerosol which is scavenged by the cloud droplets. 
All three integral properties of the Aitken mode respond to in-cloud scavenging. 

, 

,. 

,,, 

,. 

'" 

,,,, 

" 

''I 

'1!1 111·,, 

"' 

The accumulation (j) mode forms cloud condensation nuclei and thus is  distributed as 
aerosol within the cloud water. Mass and number in this mode may be lost through 
precipitation. Mass but not number is increased by in-cloud scavenging of the Aitken 
mode. 

All new sulfate mass produced by aqueous production is added to the accumulation 
mode, but the number of accumulation mode particles is unchanged as is the geometric 
standard deviation, crg, of the accumulation mode processes  (cf Leaitch, 1996 for 
cumulus clouds). 

::~'  ·· 

The assumption about the accumulation mode geometric standard deviation means that 
the surface area of the accumulation mode is reconstucted from  the new mass and new 
number in the accumulation mode at the end of the cloud lifetime. 

,,,  , 

The aerosol is mixed vertically by the same mechanisms mixing other species. The wet 
removal of aerosol is proportional to wet removal of sulfate (See Chapter 11). 

The limitations are: 

• 

• 

The cloud process modules are similar to those of RPM and RADM with cloud droplet 
number concentrations being modeled by an empirical fit to data from Bower and 
Choularton (1992). 

Cloud droplet size distributions are lognormal with cr  set to  1.2. Using the cloud liquid 
water content and the cloud droplet number concentrition, the geometric mean cloud 
droplet diameter, d  can be calculated. 

. 

S' 

The mathematical approach begins with an extension (Binkowski and Shankar,  1994; Shankar 
and Binkowski, 1994) of Slinn's (1974) two-step model as used by Chaumerliac (1984). 

The in-cloud scavenging of interstitial Aitken mode number, surface area and mass 
concentration, Yak•  may be r~presented by: 
' 

.,, 

dyak 
dt= -a.kYak 

po-12) 

10-12 

with solution 

Yak(t +  't  ~ =  y ak(o)  exp  (-a 't 
) 
k  c/d 

cl<!} 

EPA/600/R-99/030 

(10-13) 

where ak (k = 0,2,3) is the attachment rate for interstitial aerosol concentration.  The attachment 
rate is assumed to be held constant over the cloud lifetime 'tc/d·  The initial values Yak(O)  are 
determined after cloud mixing (see Equations 11-4 and 11-5). 

The cloud water aerosol concentration is represented by 

dYck 
dt= 8k3  akYak+ PJ - f38kOY ck>  k-¢ 2 

( 

~ 

(10-14) 

where  13  is the precipitation removal rate, and P is the production of new sulfate mass by aqueous 
chemistry.  The first Kronecker delta indicates that only mass (k=3) is increased for the 
accumulation mode by chemical production and in-cloud scavenging. The second Kronecker 
delta indicates that only number (k=O)  is removed by the precipitation removal term in this form. 
Mass is removed explicitly in the cloud processor. 

The attachment rates, ak, using the form recommended by Pruppacher and Keltt (1978) and 
including an enhancement factor for the settling velocity of the cloud droplets, Vdc  are given by: 
ak = 2nm 1cD /k(l  + 0. 5 Pek1

,  k = 0,2,3;. 

(10-15) 

3
) 

'

Where 
m1 c=N,d<texp[~1n2(crJ]. 
Ne  and  dd  are the cloud droplet number concentration and geometric mean 
diameter respectively. 

Pek =  v3dd  is a Peclet number. 

DP< 

The polydisperse diffusivity is given by 

(10-16) 

(10-17) 

" 
( 
Dk= 

P 

kbT 

) 
37tUPairDg 

x 

{xp( (-2~ + 

1

)  Jn'cr,) + I.246Kn, exp( (-4~ + 4) ln2cr ,)} 

and is the same form as that for dry deposition algorithm 
(see Binkowski and Shankar, 1995, Equation A29). 

(10-18) 

The precipitation removal rate for number is given by 

11111, 

1 

p 
=  ~ SO 

[ 
( 

4] i11i/ 

4  wetdep 

[8SO] 
[  J .\CU• 
+  SO 

+  8SO 

[ 

) 
4]  prod 

111·1 

,,(10-19) 

11!i:1! 

where  i;;cld  is the cloud lifetime, [ 8S04]wetdep  is the change in sulfate concentration due to 
precipitation loss, and [ S04]init  is the sulfate concentration at the beginning of the cloud 
lifetime,  [S04]scav  is the amount of sulfate added from in-cloud scavenging of Aitken mode 
sulfate; [ 8S04]prod  is the amount of new sulfate produced by aqueous chemistrJ. 
·· 

1
111:::1,,, 

'

,, 

1'' 

## 10.4  Aerosol Chemistry 

11111111". 

'li11:llllliiiil1, 

The aerosol chemical species are listed in Table 10-1.  The secondary species sulfate is produced 
by chemical reaction of hydroxyl radical on sulfur dioxide to produce sulfuric acid that may 
condense on existing particles or nucleate to form new particles.  Emissions of fresh primary 
sulfate are treated in the gas-phase chemistry component, and this contributes to the total change 
in sulfate from the chemistry component.  This is a change from RPM where primary sulfate 
emissions were treated as a source of new mass and new particle number.  Other inorganic 
species such as (anunonia ~d n.itric acid) are equilibrated ~ith .the aerosols.  '1

"',' 

'" 

" 

'I 

An assumption of the model is that organics influence neither the water content nor the ionic  I 
strength of the system; however, this assumption may not be valid for many atmospheric 
aerosols.  Although much progress has been made (e.g. Saxena et al.,  1995; ~,,~:Xe~,~ and 
Hildemann,  1996), sufficient basic data are not yet available to treat the system in a more 
complete and correct way.  Over continental North America for PM2.s, sea salt and soil particles 
are not considered in the equilibria.  Thus, for the initial release of CMAQ, only the equilibriyp1 
of the sulfate, nitrate, anunonium and water system is considered.  The equilibria and the 
associated constants are based upon Kim et al.  (1993a) and shown in Table 10-3. 

The aerosol water content is computed using the ZSR method (see Kim et al.,  1993a)  from: 

(10-20) 

], and mno  is the molality [moles kg'3

where  Wis the aerosol liquid water content [kg m'3
], Mn  is the atmospheric concentration of the 
nth species [moles m'3
], of the nth spec~~s at a va.lue of ....... . 
water activity (fractional relative humidity) of aw.  The values for the molality as a function of 
water activity are calculated from laboratory data from Giauque et aL  (1960), Tang and 
Munkelwitz (1994), and Nair and Vohra (1975).  The ZSR meth()d is used in a s9giewh(lt 
different way than usual.  The water content of sulfate aerosols depends strongly upon the ionic 
ratio of ammonium to sulfate.  This ratio varies from zero for sulfuric acid to 2.0 for ammonium 
sulfate with intermediate values of 1.0 for anunonium bisulfate, and  1.5 for letovicite.  The usual 
method would span this range with a single expression; however, Spann and Richardson (1985) 
have shown that this is not correct.  They proposed a modification whic.h resqltect in a correction 
term.  A very similar result is obtained by using the ZSR method between the ranges of the ionic 
ratio of sulfuric acid to ammonium bisulfate, ammonium bisulfate to letovicite, and letovicite to 
anunonium sulfate.  The binary activity coefficients are computed using Pitzer's method and the 
Bromley method is used for the multicomponent activity coefficients in the aqueous solution (see 
Kim et al.,  1993a) for details. 

Two regimes of anunonium to sulfate ionic ratio are considered.  The anunonia deficient regime 
(in which the ionic ratio of anunonium to total sulfate ion is less than two) leads to an acidic 
aerosol system with very low concentrations of dissolved nitrate ion which depend very strongly 
on ambient relative humidity.  The second regime is one in which the anunonium to sulfate ratio 
exceeds two, the sulfate is completely neutralized, and there is excess anunonia.  If there is nitric 
acid vapor in the system, it will dissolve in the aqueous particles along with the excess anunonia 
and produce abundant nitrate. 

For cases when the relative humidity is so low that the aerosol liquid water content comprises 
less than 20 percent of the total aerosol mass, and the ionic ratio of anunonium to sulfate is 
greater than two, "dry ammonium nitrate" aerosol is calculated with the following equilibrium 
relationship: 

NH 4 N0 3(s) <=>  NH 3(g) + HN0 3(g) 

(I 0-21) 

The value of the equilibrium constant is taken from Mozurkewich (1993) as noted in Table 10-2. 

Precursors of anthropogenic organic aerosol (such as alkanes, alkenes, and aromatics) react with 
hydroxyl radicals, ozone, and nitrate radicals to produce condensable material.  Monoterpenes 
react in a similar manner to produce biogenic organic aerosol species.  The rates of production of 
sulfuric acid and the organic species are passed from the photochemical component to the aerosol 
component.  The formation rates of aerosol mass (in terms of the reaction rates of the precursors) 
are taken frorri  Pandis et al.  (1992).  These factors are given in Table 10-3. 

## 10.5  Visibility 

Visibility is usually defined to mean the furthest distance one can see and identify an object in 
the atmosphere.  For a detailed presentation on the concepts of visibility, see Malm (1979).  In a 
perfectly clean atmosphere composed only of nonabsorbent gases, the only process restricting 
visibility during daylight is the scattering of solar radiation from the molecules of the gases.  This 
is known as Rayleigh scattering.  Scattering is usually represented by a scattering coefficient.  If 
absorption is also occurring in addition to scattering, an absorption coefficient may also be 
defined.  The sum of the scattering and absorption coefficients is called the extinction coefficient. 
If absorption is not occurring, the extinction coefficient is defined to be equal to the scattering 
coefficient.  The visibility in an atmosphere in which Rayleigh scattering is the only optical 
process active may be taken as a reference.  A  useful index for quantifying the impairment of · 
visibility by the presence of atmospheric aerosol particles is the deciview (Pitchford and Malm, 
1994).  The deciview index, deciV, is given as 


( '3cxl) 
deciV =  10 In  TIN 

(10-22) 

where the value of 0.01  [km-I]  is taken as a standard value for Rayleigh exti:q,9Hqu~  ~e aero~,~l 
extinction coefficient, 13ext [km-I], must be calculated from ambient aerosol characteristics such 
as index of refraction, volume concentration and size distribution. 

The extinction coefficient at a wavelength of A,  for aerosol may be expressed as 

,, 

11

A 
l-'ex1  -

- 3 7t  f  Qexl  d V  di 

2').., J-ro ex dlna  na' 

where the particle distribution is given in a lognormal form as 

dV  _ 
d~n a  - VT  "ff)  exp  - A In  av. 

[ 

(A\ Ia 

2(  a)] 

, 

(10-23) 

(10-24) 

where  a  =  JJl2 
').., 

rtD8 v 

av=~ 

I 

A  = 2 ln2cr g 

Vr is the total particle volume concentration, Qexf, the Mie extinction efficiency factor,  is a 
function of a  and the index of refraction of the particles.  Willeke and Brockmann (1977) 
showed that the behavior of the extinction coefficient is a smooth function of the geometric mean 
diameter for the volume distribution Dgv, and the index of refraction.  This smooth characteristic 
implies that an accurate approximation to the Mie efficiency can be used in its place to reduce a 
very computationally intensive task.  The method of Evans and Fournier (1990), a highly 
accurate approximation, is used to calculate Qext· 

,,,, 

•,II 

111 

"' 

1, 

I 

Because routine measurements of aerosol species mass concentrations are often available, but 
particle size distribution information is not, an additional method of calculating extinction has 
also been included.  This is an empirical approach known as reconstructed extinction.  The 
method is explained by Malm et al. (1994).  The formula used here is a slight modification of 
their Equation 12 (Sisler,  1998). 

, ,, 

,,  ' 

' , '~. '" 

''"'\ 

'  ""'I; 

ll"':i  ' 

, 

I 

':1!1:,  '"'' 

'11 

11 

11111 

;I 

Pext [ 1/lrm] =  0.003* f(rh)*{  [ammonium sulfate]+ [ammonium nitrate]} 

""" 

' 

(10-25) 

+ 0.004 *[organic mass} 
+ 0.01 *[Light Absorbing Carbon]+  0.001 *[fine soil] 
+ 0.0006*[coarse mass] 

In implementing this method, ammonium sulfate and ammonium nitrate were taken as the sum of 
ammonium, plus sulfate, plus nitrate.  Organ~c mass was taken as the sum of all organic species. 
Light absorbing carbon was taken as elemental carbon.  Fine soil was taken as the unspeciated 
portion of PM2.5 emitted species, and the coarse mass term was not implemented in CMAQ at 
this time.  The reason for not implementing coarse mass was that the uncertainty in the emissions 
was deemed to be too large at the present time.  The relative humidity correction, f(rh), is 
obtained from a table of corrections with entries at one- percent intervals.  The methodology for 
the corrections is given in Malm et al. (1994). 

## 10.6 Summary 

The CMAQ aerosol component is a major GXtension of the RPM.  Addition of the coarse mode 
and primary emissions now allow both PM2.5  and PM 10  to be treated.  Ongoing work will 
improve the representation of the production of secondary organic aerosol (SOA) material by 
including a version of the method of Pankow (1994a,b) as discussed by Odum et al. (1996).  This 
method, based upon laboratory experiments, calculates the yield of SOA as a function of the 
amount of organic material already in the particle phase. 

Kleeman et al.  ( 1997) have shown that various source types have size and species information 
that may be looked upon as a source signature. This assumes the availability of such source 
characteristics for the entire modeling domain. As noted in Section 10.1.3, there are ongoing 
discussions with those responsible for the national emissions inventory. As more information 
becomes available, identification of source signatures may be possible for a larger domain than 
the Los Angeles area, and an effort similar to Kleeman et al. (1997), albeit using a modal 
approach, might be undertaken.  Other planned improvements for primary particles are the 
inclusion of marine aerosol as well as a better treatment of fugitive dust. 

Future plans also include an intensive effort to evaluate the CMAQ aerosol component using 
atmospheric observations from selected field studies in which aerosol particles were observed. 
Comparison with routine visual range observations during the field study periods will provide an 
additional method of evaluation. 

## 10.7  References 

Binkowski F.  S., and U.  Shankar, The regional particulate model  1.  Model description and 
preliminary results. J  Geophys.  Res.,  100, D12, 26191-26209,  1995. 

Binkowski F.  S., and U.  Shankar, Development of an algorithm for the interaction of a 
distribution of aerosol particles with cloud water for use in a three-dimensional eulaeian air 
quality model, Presentation at the Fourth International Aerosol Conference, Los Angeles, CA, 
Aug. 29 - Sept. 2, 1994. 

Binkowski, F.  S., S.  M. Kreidenweis, D.  Y.  Harrington, and U.  Shankar, Comparison of new 
particle formation mechanisms in the regional particulate model, Presentataion at the Fifteenth 
Annual Conference of the American Association for Aerosol Research, Orlando Florida, 
October 14-18, 1996. 

Bower, K. N. and T.  W.  Choularton, A parameterisation of the effective radius of ice free clouds 
for use in global climate models. Atmos.  Res.,  27, 305-339, 1992. 

Bowman, F. M., C.  Pilinis, and J.  H.  Seinfeld, Ozone and aerosol productivity ofreactive 
organics, Atmos.  Environ., 29, 579-589, 1995. 

Chang, J.  S., F. S.  Binkowski, N. L. Seaman, D.  W.  Byun, J. N. McHenry, P.  J.  Samson,\\'. R. 
Stockwell, C. J.  Walcek, S.  Madronich, P.  B. Middleton, J.E. Pleim, and H. L. Landsford, The 
regional acid deposition model and engineering model, NAP AP SOSff Report 4,  in National 
Acid Precipitation Assessment Program, Acidic Deposition: State of Science and Technology, 
Volume I,  Washington, D.C., 1990 .. ,, 

Chaumerliac, N., Evaluation des Termes de Captation Dynamique dans un Modele 
Tridimensionel  a Mesoechelle de  Lessivage de L 'Atmosphere,  These Presentee a L'Universite de 
Clermont II, U.E.R. de Recherche Scientifique et Technique, 1984. 

Evans, T. N. and G.  R.  Fournier, Simple approximation to extinction efficiency valid over all 
size ranges. Appl.  Optics, 29, 4666-4670, 1990. 

Gelbard, F., Y.  Tambour, and J.  H.  Seinfeld, Sectional representations for simulating aerosol 
dynamics. lour.of Colloid and Interface. Sci., 76, 541-556,  1980. 

Giauque, W.  F., E.W. Hornung, J.E. Kunzler, and T.  R.  Rubin, The thermodynamics of aqueous 
sulfuric acid solutions and hydrates from  15 to 300 K,  j  Amer.  Chem.  Soc.,  82, 62-67,  1960. 

Harrington, D.  Y.  and S.  M.  Kreidenweis, Simulations of sulfate aerosol dynamics: Part I model 
description, Atmos.  Environ.,  32,  1691-1700, 1998a. 

Harrington, D.  Y. 'and S.  M.  Kreidenweis, Simulations of sulfate aerosol dynamiCs: Part II  model 
intercomparison, Atmos.  Environ.,  1701-1709, 1998b. 
Jacobson, M.Z. Development and application of a new air pollution modeling system-II. Aerosol 
module structure and design, Atmos.  Environ., 31, 131-144, 1997. 

Kim,Y. P., J.  H.  Seinfeld, and  P. Saxena, Atmospheric gas-aerosol equilibrium I. 
Thermodynamic model, Aerosol Sci.  and Technol.,  19,  157-181, 1993a. 

Kim,Y.  P., J.  H.  Seinfeld, a~d P.  S~ena, Atmospheric gas-ae:osol equilibri~m I~. An~lysis ~l 
common approximations and activity coefficient calculation methods, Aerosol Sci.  and Teclmol., 
19,  182-198, 1993b. 

Kleeman, M.J., G.R. Cass and A.  Eldering, Modeling the airborne particle complex as a source(cid:173)
oriented external mixture. J.  Geophys. Res.,  102, 21355-21372,  1997. 

Kulmala, M.  , A. Laaksonen, and Liisa Pirjola, Parameterization for sulfuric acid/water 
nucleation rates. J  Geophys.  Res.,103, 8301-8307,  1998. 

Leaitch, W.  R., Observations pertaining to the effect of chemical transformation in cloud 
on the anthropogenic aerosol size distribution, Aerosol Sci.  and Technol., Vol. 25, pp 157-173, 
1996. 

Malm, W. C., Considerations in the measurements of visibility, J  Air Pollution Control Assoc., 
29,  1042-1052,  1979. 

Malm, W.  C., J.  F. Sisler, D. Huffman, R.  A. Eldred, and T. A.  Cahill, Spatial and seasonal 
trends in particle concentration and optical extinction in the United States, J  Geophys.  Res.,  99, 
-.p47-1370, 1994. 

McElroy, M.  W., R.  C. Carr, D.S. Ensor, and G. R.  Markowski, Size distribution of fine 
particles from coal combustion, Science, 215,  13-19, 1982. 

Middleton, P.  B. and C.  S. Kiang, A kinetic model for the formation and growth of secondary 
sulfuric acid particles, J  Aerosol Sci., 9, 359-385, 1978. 

Mozurkewich, M. The dissociation constant of ammonium nitrate and its dependence on 
temperature, relative humidity, and particle size. Atmos.  Environ., 27A, 261-270,  1993. 

Nair, P.  V. N. and K.  G.  Vohra, Growth of aqueous sulphuric acid droplets as a function of 
relative humidity, J  Aerosol Sci.,  6, 265-271,  1975. 

Odum, J.  R., T. Hoffman, F.  Bowman, D.  Collins, R.C. Flagan, and J.H Seinfeld, Gas/particle 
partitioning and secondary organic aerosol yields. Environ.  Sci.  Technol.,  30, 2580-2585,  1996. 

Pandis, S.  N., R.  A. Harley, G.  R.  Cass, and J.  H.  Seinfeld, Secondary organic aerosol formation 
and transport, Atmos.  Environ., 26A, 2269-2282,  1992. 

Pankow, J.  F .,  An absorption model of gas/particle partitioning of organic compounds in the. 
atmosphere. Atmos.  Environ.,  28, 185-188,  1994a. 

Pankow, J.F., An absorption model of gas/particle partitioning involved in the formation of 
secondary organic aerosol, Atmos.  Environ.,  28, 189-193,  l 994b. 

Pitchford, M. L.  and W.  C. Malm, Development and applications of a standard visual index, 
Atmos.  Environ.,  28,  1049 - 1054, 1994. 

Pratsinis, S.  E., Simultaneous aerosol nucleation, condensation, and coagulation in aerosol 
reactors, J  Colloid Interface Science, 124,  417-427, 1988. 

Pruppacher, H.  R.  and J.  D. Klett, Microphysics of Clouds and Precipitation, Reidel, Dordrecht, 
Holland,  1978. 

Saxena, P.  and L. Hildemann, Water-soluble organics in atmospheric particles: a critical review 
of the literature and application of thermodynamics to identify candidate compounds, J  Atmos. 
Chem.,  24, 57-109, 1996. 

Saxena, P., L. M.Hildemann, P.H. McMurry, and J.  H.  Seinfeld, Organics alter hygroscopic 
behavior of atmospheric particles, J.  Geophys.  Res.,  100,  18755 - 18770, 1995. 

Seinfeld, J.  H., Atmospheric Chemistry and Physics of Air Pollution, Wiley, New York,  1986. 

Shankar, U. and F. S. Binkowski, Sulfate aerosol wet deposition in a three-dimensional Eulerian 
air quality modeling framework, Presentation at the Fourth International Aerosol Conference, 
Los Angeles, CA, Aug. 29 - Sept. 2,  1994. 

Sisler, J.  Personal Communication 

SHnn, W. G. ~., R~te-limiting aspects of in-cloud scavenging, J.  Atmos.  Sci., 31,  1172-
1173, 1974. 

Spann, J.  F. and C.  B. Richardson, Measurement of the water cycle in mixed ammonium acid 
sulfate particles, Atmos.  Environ.,  19, 919-825, 1985. 

Tang, I.N. and H.R. Munkelwitz, Water activities, densities, and refractive indices of aqueous 
sulfates and sodium nitrate droplets of atmospheric importance, J.  Geophys.  Res., 99,  18801-
18808, 1994. 

Van Dingenen, R. and F. Raes, Determination of the condensation accommodation coefficient of 
sulfuric acid on water-sulfuric acid aerosol, Aerosol Sci.  Technol.,  15,  93-106,  1991. 

Weber, R.J., J.J. Marti, P.H. McMurry, F.L. Eisele, D.J. Tanner, and A. Jefferson, Measurements 
of new particle formation and ultrafine partilce growth rates at a clean continental site. J. 
Geophys.  Res.,102, 4375-4385, 1998. 

Wesely, M. L., D.R. Cook, R.  L.  Hart, and R.  E.  Speer, Measurement and parameterization of 
particulate sulfur dry deposition over grass. J.  Geophys.  Res., 90,  2131-2143,  1985. 

\Vexler, A. S., F. W. Lurmann, and J.  H.  Seinfeld, Modeling urban and regional aerosols: I. 
Model development, Atmos.  Envion., 28, 531-546,  1994. 

l11f'I 

'!I: 

, 

Whitby, K. T., The physical characteristics of sulfur aerosols, Atmos.  Environ.,  12,  135-159, 
1978. 

\Vhitby, E. R.and  P.H. McMui-ry, Modal aerosol dynamics modeling, Aero;ol Sci.  and 
Techno/.,  27, 673-688,  1997. 

Whitby, E. R., P.H. McMurry, U.  Shankar, and F.  S.  Binkowski, Modal Aerosol Dynamics 
lilodeling, Rep. 600/3-91/020, Atmospheric Research and Exposure Assessment Laboratory, 
U.S. Environmental Protection Agency, Research Triangle Park, N.C., (NTIS PB91- 16172?/AS), 1991. 

Willeke, K. and J.  E.  Brockmann, Extinction coefficients for multimodal atmospheric particle 
size distributions, Atmos.  Environ.,  11, 995 - 999, 1977. 

Youngblood, D.A. and S.M. Kreidenweis, Further development and testing of a bimodal aerosol 
dynamics model. Colorado State University, Department of Atmospheric Sciences Report No. 
550,  1994. 

Table 10-1  Aerosol Species Concentrations 

Units:  mass [ µg m·3 

], number [ # m·3 

] 

I 

!llh' 

'II' 

{al} 
(82} 
{"~} 
{ a4} 
( aS} 
{ n6} 
{ a7} 
{ a8} 
{ a9} 
{~io} 
{all} 
{al2} 
{a13} 
{al4} 
{alS} 
{al6} 
{al7} 
{a18} 
{al9} 
{ a20} 
{a21} 
{a22} 
{a23} 
{a24} 
{a25} 
{a26} 

AS04J 

Accumulation mode sulfate mass 

AS04I 

ANH4J 

ANH4I 

AN03J 
AN031 

AORGAJ 

Aitken mode sulfate mass 

Accumulation mode ammonium mass 
Aitken' mode ammonium mass 

Accumulation mode nitrate mass 
Aitke~" mode aerosol nitrate mass 
Accumulation mode anthropogenic secondary organic mass 

AORGAI 

Aitken mode anthropogenic secondary organic mass 

1111''' 

AORGPAJ 
'I" AORGPAI 
AORGBJ 
'I 
.,,,  AdRGBI 

".'II' 

AECJ 
AECI 

A25J 

A25I 

ACORS 

ASE AS 

ASOIL 

Accumulation mode primary oq~anic mass 

Aitken mode mode primary organic inass 

Accumulation mode secondary biogenic organic mass 
Aitken mode biogenic second~ biogenic organic mass 
Accumulation mode elemental carbon mass 
Aitken mode elemental carbon mass 

Accumulation mode unspecified anthropogenic mass 

Aitken mode unspecified anthropogenic mass 

Coarse rriode unspecified anthropogenic mass 

Coarse mode marine mass 

Coarse modt.::  soil-derived mass 

NUMA TKN  Aitken mode number 

NUMACC  Accumulation mode number 
NUMCOR 

Coarse mode number 

SRF A TKN  Aitken mode surface area 

SRF ACC 

Accumulation mode surface area 

AH20J 

AH20I 

Accumulation mode water mass 

Aitken mode water mass 

Table 10-2.  Equilibrium Relations and Constants 
(Kim et al.,  l 993a) 

Equilibrium Relation 

Constant 

K(298.15) 

a 

b 

Units 

HSO;( aq) ¢:> H+(aq) + so:-(aq) 

NH3(g) ¢:>  NH 3(~q) 

NH3(aq) + Hp(aq) ~ NH;(aq)  +  OH-(aq) 

HN03(g) ¢:> H+( aq) + N03 (aq) 

....... 
0 
I 
N 
(.,.> 

NH4N0 3(s) ¢:> NHig) + HN03(g) 

(H +][ SO!-]rH+Yso~-

[Hso~]rHSO~-
[NH3(aq)]YNH3 

PNH3 

[NH; ][ oH-Jy NH·Y oH-
[ NH3(aq))yNHaa w 

[H+][No;]yH+YN03 

PHN03 

PNH3PHN03 

l.015E-02 

8.85 

25.14 

mol I kg 

57.639 

13.79 

-5.39 

mol /kg atm 

l.805E-05 

-1.50 

26.92 

mol I kg 

2.511E06 

29.17 

16.83 

moI2 I kg2 atm 

5.746E-l 7# 

-74.38# 

6.12# 

atm2 

H20(aq) ¢:> H+(aq)  +OH  (aq) 

[ H+][oH- ]rH+YoH-

l.OIOE-14 

-22.52 

26.92 

moI2 I kg2 

The constants a and b are used in the following to adjust for ambient temperature 

aw 

K ~ K(r,)e+(~ -I) + b(1 + In(.?f )--¥} r, ~ 298.15 [KJ] 

#These values are only used by Kim et al. (1993a,b).The values used in the CMAQ are from Mozurkewich (1993): 

K ~exp (118.87 - ·  - 6.025 ln (r)) 

where Mozurkevich reports in nanobars squared.  This yields a value for the equilibrium constant of 43.11  [nb2

]  at 298.15 K. 

Table 10-3.  Organic Aerosol Yields in Terms of Amount of Precursor Reacted 
(From Pandis et aL  (1992) and Bowman et al.  (1995)) 

Gas-Phase Organic  Species 

Aerosol Yield 

[µg m"3 

/  ppm(reacted)] 

CS  and higher alkanes 

Anthropogenic internal alkenes 

monoterpenes 

toluene 

xylene 

cresol 

380 

247 

740 

424 

342 

221 

II", 

'II 

I 

i 

11111,,, 

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_Science_Ch_09.md) - [Home](README.md) - [Next Chapter >>](CMAQ_Science_Ch_11.md)<br>
CMAQ Science Document (c) 1999<br>

<!-- END COMMENT -->

