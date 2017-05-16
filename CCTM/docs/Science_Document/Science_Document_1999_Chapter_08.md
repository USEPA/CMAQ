Chapter 8
=================

GAS-PHASE CHEMISTRY 

Since atmospheric chemistry plays a major role in many air pollution problems, the representation 
of chemical interactions among atmospheric constituents is often an essential element of an air 
quality model.  All important chemical transformations relevant to the problem being studied must 
be included to make accurate predictions of ambient pollutant concentrations.  Many atmospheric 
pollutants or their precursors are emitted as gases and interact primarily in the gaseous phase. 
However, some important atmospheric processes such as acid deposition and the formation of 
aerosols involve tlie interaction of constituents in the gas, liquid, and solid phases, so 
transformations taking place in all three phases often need to be represented.  For computational 
efficiency, these processes are usually modeled separately.  This approach has been adopted in the 
Chemical Transport Model (CTM) that is part of the Community Multiscale Air Quality (CMAQ) 
modeling system (hereafter referred to as the CCTM). This section addresses the modeling of gas(cid:173)
phase transformatfons alone in the CCTM.  Descriptions of the linkages of gas-phase constituents 
With aer9sols i;md :mth aqueous chemistry are discussed below and in Chapters 10 and 11, 
respectively.  A potential future improvement fo the CCTM would involve more closely coupling 
t1w .. 9hemical fu.tet.~ctions taking place in all three phases.  Nevertheless, the current formulation 
still enables the investigation and assessment of environmental problems using a multi-pollutant, 
o~e-atmosphere modelmg concept. 
· 

· 

,1,11.  L' 

,,I 

11'1'"1 

·.1·:"11111.1 

·:, 

• 

,, 

' 

,, 

,; 

11• 111' 

1,, 

Interactions in the gas-phase are represented in air quality models by means of chemical 
mechanisms.  The CMAQ system currently includes two base chemical mechanisms that have 
been developed primarily to address issues associated with urban and regional scale ozone 
formation and acid deposition -- the CB4 (Gery et al.,  1989) and RADM2 (Stockwell et al.,  1990) 
l!lechanisms. Variants of these two mechanisms have been develop~d for the CMAQ system to 
provide the n~ces~~ linkages to 'ilie aerosol and aqueous chemistry processes.  Current plans 
also call for adding a third mechanism -- the SAPRC-97 mechanism (Carter, 1997).  Although 
these mechanisms should be adequate for many air pollution applications, it may be necessary to 
modify or even replace these mechanisms to address some issues.  to facilitate changing 
mechanisms and adding new ones, the CMAQ system has been equipped with a generalized 
chemical mechanism processor.  It must be emphasized, however, that supplemental data for other 
CMAQ processors may be required when one of the predefined mechanisms is modified or 
replaced. This is addressed in more detail section 8.2.5. 

The remainder of this chapter addresses different aspects of the representation of gas-phase 
chemistry in the CMAQ system.  The first section includes background information on chemical 
mechanisms and provides the rationale for including the predefined chemical mechanisms in the 
CMAQ system.  The subsequent section describes the predefined chemical mechanisms as 
implemented in the CMAQ system and addresses adding new mechanisms or changing existing 
ones.  This is followed by a description of reaction kinetics as it relates to the CCTM 
representation, and the fmal section describes the mathematical procedures used internally in the 
CCTM to solye the equations that arise from the mathematical representation of gas-phase 
chemistry. 

8.1 

Background 

A chemical mechanism is a collection of reactions that transforms reactants into products, 
including all important intermediates.  Chemical mechanisms developed for air quality modeling 
are highly condensed, parameterized representations of a true chemical mechanism. They include 
artificial species and operators, and many of the mechanism reactions are parameterizations of a 
large set of true atmospheric reactions. In some cases, mechanism reactions may include elements 
which have no physical significance (e.g., products with negative stoichiometry).  While it would 
be difficult to design a generalized mechanism processor to handle all possible parameterizations 
used in various condensed mechanisms, some degree of generalization in the CMAQ system is 
achieved by using special conventions for entering chemical mechanisms.  First, all reactions in the 
mechanism are treated as if they are elementary, and the stoichiometric coefficients for all 
reactants must be one.  Since all reactions are assumed to be elementary, a reaction can have no 
more than three reactants.  These conventions permit the reaction rate to be derived directly from 
the stoichiometric equation, thereby simplifying the mathematical representation of the reactions. 
Other conventions adopted· for the CMAQ generalized mechanism processor are included in 
Section 8.3  and in Chapter 15. 

Mechanism species can be divided into two categories: inorganic and organic.  The number of 
important inorganic species is relatively small, and they are almost always represented explicitly in 
chemical mechanisms.  The important inorganic species included in these mechanisms are ozone, 
nitric oxide, nitrogen dioxide, nitric acid, nitrous acid, hydrogen peroxide, sulfur dioxide, and 
several radicals formed through their interactions with other species.  Although most of the 
chemical reactions involving these species are common to all mechanisms, some differences do 
exist.  For example, some of the mechanisms omit a few reactions because they are normally 
minor pathways and thus do not affect modeling results significantly.  Also, different rate 
constants may be used  for some reactions, especially those that are photolytic.  The 
representation of organic species usually differs more substantially, however.  Some species in the 
mechanism represent real organic compounds, but others represent a mixture of several different 
compounds. The manner in which the grouping of organic compounds is carried out typically 
distinguishes one mechanism from another, and that is described next.  In this chapter, the phrase 
mechanism species is used to refer to any species in the gas-phase mechanism, regardless of 
whether it is an explicit species or not. 

Although explicit mechanisms have been developed for many organic compounds, the resultant 
number of reactions and species needed to represent their atmospheric chemistry is too large to 
model efficiently in photochemical grid models such as the CCTM.  In addition, explicit 
mechanisms have not yet been developed for most organic compounds, thereby requiring that 
some reaction pathways be postulated.  Thus, both compression and generalization are necessary 
when depicting organic reactions.  Although chemical mechanisms differ in the manner in which 
organic species are represented, the mechanism developer usually chooses some distinguishable 
organic property to group similar organics into classes that reduce both the number of mechanism 
species and reactions.  The three most common representations include the lumped structure 

8-3 

I 
I 

technique, the surrogate species approach, and the lumped species method.  In the lumped 
structure appr6'ach, organic compounds are apportioned to one or more mechanism species on the 
basis of chemical bond type associated with individual carbon atoms (Whitten et al., 1980).  In the 
surrogate species method, the chemistry of a single species is used to represent compounds of the 
same class (e.g., Lurmann et al., 1987).  Generalized reactions are then written bas~d on the 
hypothetical model species.  The lumped species method is very similar to the surrogate species 
approach, but various mechanism parameters associated with a particular surrogate are adjusted 
to account for variations in the composition of the compounds being represented by the surrogate 
species (e.g., Carter, 1990, and Stockwell et al., 1990). 

·I· 

The construction of a compact chemical mechanism necessarily introduces varying levels of 
distortion, generalization, and omission in the representation of atmospheric chemistry (Jeffries, 
19'9s).  Alihough"mechamsms are routinely tested using results obtained from environmental 
· 
chamber experiments, the data are often insufficient to resolve uncertainties associated with some 
of the chemical representations.  For example, Carter (1990) noted that much is unknown about 
several important reaction types, and that their representations" ... continue to be largely 
speculative or are based on empirical models derived from fits to environmental chamber data." 
Further, rate constants for some reactions are either unknown completely, or significant 
dfoagreemerit exists as to their accuracy.  Several studies have been conducted to compare 
different chemicafmechanisms (e.g., Leone and Seinfeld,  1985; Hough, 1988; and Dodge, 1989). 
These comparisons revealed that the mechanisms often yield results that are similar for some 
species.  This could indicate that the fundamental atmospheric chemistry is fairly well understood 
for these species, or that the mechanisms were derived from the same experimental kinetic or 
nl~94anistic q~ta, .. which may or may not be accurate.  Larger differences tend to occur for those 
species for which the atmospheric chemistry is more uncertain.  thus, it is often difficult to assess 
the relative merits of different mechanisms when applied to any one situation.  Therefore, the 
CMAQ system includes the capability to use more than just one chemical mechanism. 

Given the inherent uncertainties in existing chemical mechanisms, alternate approaches for 
representing gas-phase chemistry are needed.  One approach being explored involves decreasing 
unc::ertainties a,~soqiated with the simplifications that are introduced to reduce mechanism size. 
'fh.is approach is based on the concept that much of the information needed for an expanded 
chemical representation does not necessarily have to be included in the mechanism explicitly, but 
ra,tl:ier can be wairjJained in atpdliary variables linked to a :relatively small number of core species 
that are inclugedi!l the m~chanism (Jeffries et al.,  1993).  Thus, it may be possible to expand 
chemical representations without greatly increasing the size of the basic mechanism, and this 
could be a future enhancement to the treatment of gas-phase chemistry in the CMAQ system. 

8.2 

Chemical Mechanisms in the CMAQ System 

This section includes summary descriptions of the two basic chemical mechanisms included in the 
CMAQ system -- the CB4 (Gery et al.,  1989) and the RADM2 (Stockwell et al., 1990).  Since the 
SAPRC-97  mechanism (Carter, 1997) is to be added in the near future, some discussion of it is 
also included.  These mechanisms require that information be supplied to the CCTM in a form 
that is unique for each mechanism, and this is carried out in several sub-systems incorporated in 
the CMAQ system.  These include the emissions processing system which generates emissions for 
key mechanism species; the initial conditions/boundary conditions processor that generates 
ambient starting and boundary concentrations for mechanism species; and the photolysis rate 
processor that produces mechanism specific photolysis rates.  The reader is referred to the 
chapters describing those sub-systems for a description of the treatment of mechanistic data, and 
to the mechanism references for a more detailed description of each chemical mechanism. 

In addition the base mechanisms, both the base CB4 and RADM2 mechanisms have been 
modified in the CMAQ system to provide necessary linkages for aerosol and aqueous chemistry 
processes, and the RADM2 mechanism has also been modified to create two new mechanism 
variants that include enhanced isoprene chemistry representations.  Note that the existing sub(cid:173)
systems provide all of the necessary information for the extensions to the base mechanism.  The 
modifications to the base mechanisms are discussed below in the section on mechanism 
extensions.  Complete listings of all mechanisms currently available in the CMAQ system are 
included in Appendix 8A. The last portion of this section briefly discusses changing the base 
mechanism or their variants or adding new mechanisms to the CMAQ system. 

8.2.1  CB4 Mechanism 

The CB4 mechanism is a lumped structure type that is the fourth in a series of carbon-bond 
mechanisms, and differs from its predecessors notably in the detail of the organic compound 
representation.  It has been used in models such as EP A's Empirical Kinetic Modeling Approach 
(EPA, 1989) and Regional Oxidant Model (Lamb, 1983), and in versions IV and V of the Urban 
Airshed Model (EPA,  1991  and SAI, 1993).  The CMAQ implementation of the basic CB4 
mechanism includes 36 species and 93  reactions, including 11  photolytic reactions. 

The CB4 uses nine primary organic species (i.e., species emitted directly to the atmosphere as 
opposed to secondary organic species formed by chemical reaction in the atmosphere).  Most of 
the organic species in the mechanism represent carbon-carbon bond types, but ethene (ETH), 
isoprene (ISOP) and formaldehyde (FORM) are represented explicitly.  The carbon-bond types 
include carbon atoms that contain only single bonds (PAR), double-bonded carbon atoms (OLE), 
7-carbon ring structures represented by toluene (TOL), 8-carbon ring structures represented by 
xylene (XYL), the carbonyl group and adjacent carbon atom in acetaldehyde and higher molecular 
weight aldehydes represented by acetaldehyde (ALD2), and non-reactive carbon atoms (NR). 
Many organic compounds are apportioned to the carbon-bond species based simply on the basis 
of molecular structure.  For example, propane is represented by three PARs since all three carbon 
atoms have only single bonds, and propene is represented as one OLE (for the one carbon-carbon 
double bond) and one PAR (for the carbon atom with all single bonds).  Some apportionments are 
based on reactivity considerations, however.  For example, olefins with internal double bonds are 
represented as ALD2s and P ARs rather than OLEs and P ARs.  Further, the reactivity of some 
compounds may be lowered by apportioning some of the carbon atoms to the non-reactive class 
NR (e.g., ethane is represented as 0.4 PAR and 1.6 NR).  Apportioning rules have been 
es~blished fo~ hundreds of organic compounds, and are built into the emissions processing sub(cid:173)
systems to produce the appropriate emission rates for the CB4 mechanism species. 

. 

I 
I 

• 

' 

The CB4 mechanism described by Gery et al. (1989) has undergone several changes since its 
publication.  In 1991, the PAN rate constants were changed and a termination reaction between 
th~)C02 operator and the H02 radical were added.  Subsequently, terminal reactions for the 
X02N operator were also added.  An updated CB4  isoprene chemistry mechanism based on the 
work of Carter (1996) was developed in 1996.  All of these changes have been incorporated in the 
CMAQ version.  It should also be noted that the original CB4 mechanism incorporated simple 
Arrhenius type rate constant expressions that were derived from more complex expressions for 
temperature and. pressure dependent rate constants.  Since the top of the CCTM domain may 
e~tend toh~ights that makes pressure dependencies important, the CMAQ version incorporates 
the original expressions rather than the derived ones .. 

8.2.2  RADM2 Mechanism 

The RADM2 mechanism is a lumped species type that uses a reactivity based weighting scheme to 
adjust for lumping (Stockwell et al.,  1990).  It has evolved from the original RADMl mechanism 
(Stockwell,  1986), and is employed in version 2 of the Regional Acid Deposition Model (Chang 
et al., 1987).  The base mechanism as implemented in the CMAQ system contains 57 model 
s~cies and 158 reactions, of which 21  are photolytic. 

I 

In RADM2, the primary organics are represented by 15 mechanism species, five of which are 
explicit because of their high emission rates or because of special reactivity considerations 
(methane, ethane, ethene, isoprene, and formaldehyde ).  The other ten represent groups of 
organic comp0ilnds aggregated on the basis of their reactivity with the hydroxyl radical (HO) 
and/or their molecular weights.  To account for varying reactivities of the different organics that 
are lumped into a single group, emissions of each organic within a group are weighted by a 
reactivity factor (F) that is computed as the ratio of the fraction of emitted organic compound that 
reacts to the fo1cti.on of the mechanism species that reacts: 

1
111,, ' ~ 111 
I '  ' 

" " " ' 

' I 

I  - exp( -~0 Emiss  J [HO]  dt) 
F  =  ~~~~~~~~~~-
1  - exp( -~o Mech  j[HO]  dt) 

(8-1) 

The integral term is estimated from a daily average integrated HO radical concentration of 110 
ppt min that was derived from RADM simulations  (Stockwell et al., 1990).  Note that F 
approaches unity if the reactivity of the emitted organic nearly equals that of the mechanism 
species or ifthe reactivities of both are very large.  As with CB4, the RADM2 lumping and 
weighting rules have been built into the CMAQ emission processing system. 

The implementation of the RADM2 mechanism in the CMAQ system is almost identical to that 
described by Stockwell et al. (1990), with only two minor modifications. First, the reaction of HO 
with cresol (CSL) was reformulated as follows to eliminate negative stoichiometry in the 
mechanism: 

From:  HO + CSL - 0.1  H02 + 0.9 X02 + 0.9 TC03  - 0.9 HO 

To: 

HO + CSL - 0.1  H02 + 0.9 X02 + 0.9 TC03 

HO+CSL- CSL 

0.9 kHo+CSL• 

(Note that negative stoichiometry is permitted in the CMAQ system, but was removed here for 
consistency with previous implementations ofRADM2.)  Second, the concentration of methane in 
the CCTM is assumed to be constant at 4.5xl013  molecules/cc.  Thus, methane was removed as a 
reactant in the reaction of OH with methane and the corresponding rate constant changed from 
second-order to pseudo first-order using the assumed CCTM methane concentration. 

8.2.3  SAPRC-97 Mechanism 

The SAPRC-97 mechanism (Carter, 1997) employs the lumped surrogate species approach, but 
offers the capability to incorporate semi-explicit chemistry of selected organics.  The SAPRC 
series of mechanisms evolved from the "ALW" mechanism of Atkinson et al. (1982).  SAPRC-97 
is similar to its predecessors SAPRC-90 and SAPRC-93, but incorporates improvements to 
aromatic chemistry and updates to reactions of many individual organic compounds.  Although 
many of the reactions for organic compounds are generalized and incorporate non-explicit 
species, product yield coefficients and rate constants are tabulated for over 100 individual organic 
compounds.  Thus, each of these organics can be modeled individually by including their semi(cid:173)
explicit chemistry in the mechanism.  Due to computational constraints, however, the full set of 
organic compounds cannot be incorporated In an Eulerian model.  For this situation, the 
mechanism is condensed by lumping individual organic compounds into groups with 
corresponding rate constants and product yield coefficients that have been weighted by mole 
fractions of the individual organics.  The mole fractions are typically derived from emission 
inventory data used in the model simulation.  Thus, unlike the previous mechanisms, the SAPRC-
97 mechanism can potentially change with each application since new rate constants and product 
yield coefficients can be computed for each application. 

The SAPRC-97 mechanism has been develop.ed with supplemental software to facilitate 
constructing mechanisms of varying levels of condensation.  Documentation of the procedures 
include three distinct levels of detail, differing primarily in the number of organic species that are 
included in the mechanism (Carter, 1988).  Since no decisions have made on the form of the 
mechanism that will be added to the CMAQ system, listings for this mechanism are not included 
in Appendix 8A.  Documentation will be provided when the CMAQ version is made available 
however. 

8.2.4  Extended Mechanisms 

I 

Each gas-phase chemical mechanism has been linked to aqueous chemistry and to aerosol 
formation processes.  Since these linkages required some modifications to the' original gas-phase 
mechanisms, di.fferent versions of the same mechanism were created for modeling gas-phase 
ch~mistry alone or for modeling gas-phase chemistry with or without aerosols and/or aqueous 
chemistry.  Different versions of the mechanisms are distinguished by means of a special naming 
conventi.on.  G~-phase mechanisms that have not been modified are referred to by their base 
name (e.g., CB4, RADM2, and SAPRC when the latter is available).  Mechanisms that have been 
modified to account for aerosol production have their names appended with "_ AE", mechanisms 
mcidified for aqueous chemistry are appended with "_AQ", and mechanisms modified for both are 
appended with "_AE_AQ".  Thus, CB4_AE_AQ refers to the CB4 gas-phase mechanism that has 
been modified to include linkages to both aerosols and aqueous chemistry. A second set of 
RA.DM2 gas-phase mechanisms that incorporates new isoprene chemistry has also been included 
in the CMAQ system. These mechanisms include either"_ CIS 1" or "_ CIS4" 1n their names to 
denote that the mechanism incorporates enhanced isoprene chemistry.  Methods used to develop 
the extended mechanisms are described below according to the three types of 'extensions: aerosol, 
aqueous chemistry, and isoprene chemistry. 

s.2.4.1 Aerosol Extensions 

'" 

I 

A major pathway leading to the formation of aerosols is the oxidation of sulfur dioxide (S02)  to 
sul,fate, primarily by the gas-phase reaction of S02 with the hydroxyl radical (OH).  All 
mechanisms in the CMAQ system incorporate this reaction.  Because organics are represented 
differently in the base mechanisms, however, aerosol formed from the reactions of organic 
COIJlpounds must be handled somewhat differently.  In the CCTM, organic aerosol formation is 
quantified using aerosol yields, i.e., µgm· 3  of aerosol produced per ppm of organic reacted with 
OH~ ozone or nitrate radical (N03).  The yields used in the CCTM are those reported by Bowman 
et al. (1995) that were derived from the work of Pandis et al. (1992).  These yields are given in 
terms of the SA,PRC-90 chemical mechanism species, so some adjustments were required to adapt 
th~.rn to the C!y!AQ mechanisms.  In the CMAQ system, aerosol production is assumed to occur 
from reactions involving five different generic organic groupings.'  Individual mechanism species 
are then mapped to these general groupings to obtain the aerosol yields.  The five generic groups 
are defined as:  1) long-chain alkanes; 2) alkyl-substituted benzenes such as toluene and xylene; 3) 
cresol and phenols; 4) long-chain olefins; and 5) monoterpenes.  Note that the' aerosol yields vary 
significantly among these five groups, so it is important to map the organic species in each 
m~.~hanism to ~e proper aerosol production group. Jhe remainder of this section describes the 
mapping that is used for the CMAQ base mechanisms and how the aerosol production rates are 
determined from the gas-phase reactions.  The derivation of the yields used in the CCTM and the 
manner in which they are used in the aerosol module are described in Chapter I 0. 

To apply the aerosol yields, the amount of reactant consumed by reaction must be determined for 
several mechanism, species.  In the CMAQ system, this is accomplished by using "counter" species 

I 

i 

that have been added as products to those reactions involving the mechanism species of interest 
(e.g., Bowman ~t al.,  1995).  These counter species are es~entially "dummy" species with no 
physical significance, and are not subjected to any other model process such as advection or 
diffusion.  Thus, changes in their concentrations reflect the effect of chemical reaction alone. 
Also, their inclusion in the mechanisms does not affect basic gas-phase chemistry since they do 
not interact with any of the other species in the mechanism. 

Special procedures are used in the CCTM to determine aerosol production from monoterpenes 
since their aerosol yields are relatively large and they are either lumped with other organic 
compounds into a single mechanism species or are apportioned among several mechanism species. 
The approach involves tracking the rate of reaction of monoterpenes separately from the rates of 
their mechanistic representation.  The CMAQ emission processor generates emissions for 
monoterpenes as a unique species in addition to lumping or apportioning the emissions into the 
appropriate mechanism species. Whenever aerosols are being modeled, the unique monoterpene 
species is included in the mechanisms and is modeled as a separate species.  As with the counter 
species, however, the monoterpene species is incorporated in the mechanisms such that it does 
not affect the basic gas-phase chemistry.  This is described in more detail below.  A p~tential 
future modification to the CMAQ system would involve incorporating a more explicit 
representation of monoterpenes in the base mechanisms that would eliminate the need for this 
special treatment and would also improve the chemical representation of these species in the gas(cid:173)
phase mechanisms (e.g., Stockwell et al.,  1997). 

• 

RADM2_AE.  Much of the linkage between the lRADM2 mechanism and aerosol 
formation is relatively straightforward.  Aerosol production from S02 and long-chain 
alkanes is derived from the amount of S02 and HC8 reacted with OH, respectively. 
Similarly, aerosol production from alkyl-substituted benzenes is derived from the sum of 
the TOL and XYL reactions with OH.  The  production from phenols and cresols is based 
upon the sum of the CSL reactions with OH and N03•  Thus, special counter species 
named SULAER, HC8AER, TOLAER, XYLAER, and CSLAER have been added to 
track these reactions. 

In the RADM2 mechanism, both monoterpenes and other olefinic compounds are lumped 
into the mechanism species OLI.  As noted above, however, monoterpenes are modeled 
separately whenever aerosols are modeled.  The monoterpenes are represented in the 
RADM2 mechanism by the species TERP, and the following reactions are added: 

TERP+HO 

TERPAER+HO 

TERP+N03 

TERPAER+N03 

TERPAER+03 

These reactions use the same rate constants as the reaction of OLI witJ.:i these species, and 
have the TERPAER counter species added to track the throughput of this reaction.  Note 
that concentrations of the reactants OH, N03  and ozone are unaffected by these reactions 
since their production equals their loss. 

· 

The final pathway for aerosol production in the RADM2 mechanism is via reaction of 
long-chain olefins with OH, N03 and ozone.  The RADM2 mechanism species OLI is used 
as the surrogate for long-chain olefins, and thus a counter species product named 
OLIAER is added to each of the reactions ofOLI with OH, N03 and ozone.  Since OLI 
includes both monoterpenes and other olefins, however, OLIAER tracks the reaction rate 
of both.  The amount of long-chain olefins reacted is determined by subtracting the 
concentration of the counter species TERP AER from that of OLIAER. The yield of 
aerosols from long-chain olefins is then applied to this difference to obtain aerosol 
production by this pathway. 

' 

• 

i 
I 

CB4_AE.  Since the CB4 gas-phase mechanism is structure-based, individual organic 
molecqles are often disaggregated and assigned to more than one mechanism species.  For 
example, long-chain alkenes are apportioned to both the PAR and OLE mechanism 
species.  Thus, many of the organic mechanism species contain fragments of molecules, 
and the identity of the original contributing organic compound is lost. As a result, it is not 
possible to ascertain with certainty the amount of long-chain alkanes and alkenes reacting 
in the CB4 mechanism, and thus aerosol production via these pathways is omitted.  The 
production of aerosols from the reactions of toluene, xylene, and cresol is included, 
however, by tracking the amounts of TOL, XYL, and CRES that react using the counter 
species TOLAER, XYLAER, and CSLAER.  The manner in which aerosol production 
from monoterpenes is modeled is identical to that used in the RADM2 mechanism. 
Monote!penes are modeled Independently as the mechanism species TERP' with rate 
constants for the reactions of TERP with OH, 0 3,  and N03  set to the same values as those 
used in the RADM2 mechanism extension. 

' 

8.2.4.2 Isoprene Extensions 

I 

Over the past few years, the importance of isoprene in ozone formation has become a major 
concern.  Its representation in the original gas-phase mechanisms was substantially condensed, 
partially because of computational resource considerations and partialiy due to significant 
uncertainties about the pathways of its reaction products.  Recent mechanistic and environmental 
chamber studies have led to a greater understanding of its atmospheric chemistry and thus 
improved mechanistic representations (Carter and Atkinson, 1996).  In the CMAQ system, two 
different levels of more detailed isoprene chemistry have been included in the RADM2 
me~panism, and these are referred to as the one-product and the four-product Carter isoprene 
mechanisms (Carter, 1996).  Both are condensed forms of the more detailed mechanism 
developed by Carter and Atkinson (1996).  Since this detailed mechanism may be too large to use 
in full-scale Eulerian modeling studies, Carter condensed the detailed mechanlsm to two levels of 
detail: one in which isoprene products are represented by four products, and one in which only 
one product is used.  The four-product mechanism is the lesser condensed of the two, and 
includes the explicit representation of many of the isoprene's unique products (e.g., methacrolein, 
methyl vinyl ketone, and methacrolein's PAN analogue).  The one-product form lumps the major 
products into a single species, thereby yielding a more compact albeit less explicit mechanism.  As 
noted above, these two mechanisms are named RADM2_CIS4 and RADM2_CIS1, and both have 
been linked to aerosols and aqueous chemistry as well.  It should also be noted that the isoprene 
chemistry incorporated in the CMAQ CB4 mechanism corresponds to the Carter I-product form, 
but the 4-product form is not available for the CB4 mechanism in the CMAQ system 

8.2.4.3 Aqueous Chemistry Extensions 

The base RADM2 mechanism does not have to be modified to link it to the aqueous chemistry 
processes since the aqueous processes in the CMAQ system are similar to those incorporated in 
the original RADM model. As described in Chapter 15, other aspects of the linkages require a 
separate mechanism with a unique name. The linkages to aqueous chemistry do require some 
minor changes to the CB4 mechanism however ..  These changes were based on a variant of the 
CB4 mechanism developed for acid deposition modeling by Gery et al. (1987).  In this version, 
the following product species that were omitted in the base CB4 mechanism are included: formic 
acid, acetic acid, peroxyacetic acid, and methylhydroperoxide (MHP).  Since these species are 
proc:Iucts only, their inclusion in the mechanism does not affect the concentrations of any of the 
other mechanism species.  It should be noted, however, that the concentration ofMHP in this 
modified mechanism represents an upper limit for two reasons. First, known decomposition 
pathways for it are not included in the mechanism.  Second, the production ofMHP will be 
overstated since it is produced by an operator that includes radicals other than the methylperoxy 
radical (Gery et al.,  1987). 

8.2.5  Changing or Adding Mechanisms in CMAQ 

As noted in the introduction to this chapter, the CMAQ system has been instrumented with a 
generalized chemical mechanism processor to facilitate making changes to existing mechanisms or 
adding new mechanisms.  The pro'?edures for altering or adding a new mechanism are described in 
EPA (1998), and will not be repeated here.  It should be emphasized, however, that the addition 
of a new mechanism will likely require modifications to the previously mentioned subsystems that 
provide key mechanism-specific information, i.e., emissions, initial/boundary condition, and 
photolytic rate processors.  Changes to an existing mechanism would also likely require 
modifications to these processing subsystems if new organic species are added or if an alternative 
organic grouping scheme is implemented.  If changes are limited such that they affect only the 
reactions of intermediate and/or product species, however, these subsystems may not need to be 
changed at all, and the modifications can then be implemented solely within the generalized 
chemical mechanism processor.  For example, the modifications to the base mechanisms to 
provide linkages to aerosol and aqueous chemistry and to expand isoprene chemistry that were 
described previously did not require any major changes to the other processors except to add a 

8-11 

II' 


'" 

'111,. 

' 

I 

,'1,,:1 

' 

photolysis rate for acrolein in the photolytic rate processor for the CIS 1 and CIS4 versions of 
RADM2. 

,,,: 

,,,I,,,' 

i 
I 

' 

I 
I 

Although the CMAQ system provides a convenient tool for making mechanism changes, some 
caution should be exercised in modifying existing CMAQ mechanisms.  The mechanisms currently 
in the CMAQ system have been evaluated outside of the CMAQ system using environmental 
chamber data andlor more detruled chemical mechanism representations.  An§ proposed changes 
to .i::~actions or .reac~.ions rates that significantly affect model predictions should normally be 
subjected to similar independent testing before being introduced into the CMAQ system and 
subsequently used in modeling applications. Thus, it would be expected that the introduction of 
most changes to a mechanism in the CMAQ system would only be performed by a researcher who 
is experienced in atmospheric chemistry and is familiar with the base mechanism..  Finally, it 
should be noted that the existing CMAQ mechanisms are fully specified.  In most instances, it will 
only be necessary for a user to choose one of the existing mechanisms for their application, and it 
will not be necessary to make any changes to that mechanism. 

' 

8.3 

Reaction Kinetics 

' 
i 

The rates of chemi~al reaction determine whether a species is formed or destroyed by gas-phase 
ch~mistry.  sin:~e the CMAQ syste~ treats all reactions as if they are elementary, the laws of 
reaction kinetics can be used directly to develop mathematical expressions for the rates of each 
chemical reaction.  This section describes the rate expressions and the forms of the rate constants 
that are used in those expressions, with special emphasis placed on the conventions used in the 
CMAQ system. The reader may also wish to refer to Chapter 15 and EPA ( 1998) for details on 
how mechanism data are entered in the CMAQ system. 

8.3.1  Reaction Rates 

The rate of a chemical reaction l (r1)  can be expressed as the product of a rate constant (k1)  and a 
term that is dependent on the concentrations of the reactants: 

' 

' 

illl 

r 1  =  k1 f( concentration)  . 

I 

ii.'ll,'' 

I' 

I 

i 

(8-2) 

For. elementary reactions, the concentration dependent term is simply the product of reactant 
concentrations, and the rate of reaction takes one of the following forms: 

"''I 

'''Ill'"' 

•I'' 

for  first-order  reactions 
for  second-order  reactions 
for  third-order  reactions 

(8-3) 

,,, 

" 

"  .. ,, 

.,,.. 

.I 

wq~re C1,  C2,and C3 refer to the concentration ofreactants 1, 2 and 3, respectively.  Note that 
when a species reacts with itself, the concentration dependent term includes the species 
concentration squared.  Thus, the rate for the reaction NO +NO + 0 2  ...  2NO:z  is equal to 
k[NO][N0][02]  and not k[N0][02]. 

Several important ter-molecular reactions involve 0 2 and/or N 2 which mediate those reactions by 
absorbing energy from exothermic bi-molecular reactions.  When either N2 or 0 2  serves this role, 
the third body is usually referred to as "M", where M =  N2 + 0 2• Since their concentrations are 
relatively stable in the atmosphere, some mechanism developers convert second- or third-order 
reactions that include these species to a reaction one order lower by multiplying the higher-order 
rate coefficient by the concentration ofM, 0 2,  or N2•  The CMAQ convention is to include third(cid:173)
body reactants in the reaction rate calculations if they are explicitly shown in the reaction, and to 
omit them if they are not shown or included only as comments.  For example, consider the 
bimolecular reaction: 0 1D + 0 2  - 0 3P + 0 2•  If the reaction is written in this form, the reaction is 
assumed to be second-order and the CCTM will use the appropriate concentration for 0 2 in the 
rate computation.  If the reaction is written as  0 1D  - 0 3P (or as 0 1D {+ 0 2}  ...  0 3P, where here 
the braces denote a comment), the reaction rate will be assumed to be first-order and the CCTM 
will not include the 0 2  concentration in the reaction rate calculation.  In the latter case, the 
mechanism developer must specify a pseudo first-order rate constant for the reaction.  The same 
convention also applies to H20. 

8.3.2  Rate Constant Expressions 

As shown in Equation 8-3, the rate of reaction is related to the concentration term by a constant 
of proportionality k1•  The rate constant k1 can take many forms depending upon the characteristics 
of the reaction.  One important class of uni-molecular reactions involves the absorption of radiant 
energy and subsequent dissociation of the reactant into product species.  The rate constants for 
these types of reactions are functions of the incident radiant energy and properties of the 
absorbing molecule, such as the absorption cross section and the quantum yield.  In the CMAQ 
system, these rate constants are calculated by the photolytic rate processor, and the details of 
these calculations are described in Chapter 14.  The remainder of the reactions are classified as 
thermal, and their rate constants are typically functions of temperature and sometimes pressure. 
The calculation of these rate constants is discussed below. 

To facilitate incorporating rate constant information for thermal reactions, the CMAQ generalized 
mechanism processor (discussed in Chapter 15) has been designed to accept the standard rate 
constant expressions used in NASA (1997).  Rate constant information is most often supplied in 
ems units (i.e., gas concentrations in molecules/cc and time in seconds), but some mechanisms use 
mixing ratio units (i.e, gas concentrations converted to mixing ratios in parts per million and time 
in minutes).  The CMAQ generalized mechanism reader is designed to accept either, but they 
must be consistent throughout the mechanism (i.e., the same units must be used for all rate 
constant forms that can be expressed in either set of units).  Some rate constant expressions (e.g., 
falloff expressions and other special forms discussed below) can be expressed only in ems units, 
however, and must always be in these units even when mixing ratio units are being used for all 
other types of rate constants.  The CCTM will automatically perform the necessary units 
conversions during the model simulation. Nevertheless, since the CMAQ domain typically extends 
through the entire troposphere, ems units are usually preferred because differences in number 
density differences with height are explicitly accounted for with those units.  · 

I 
I 
I 

Descriptions of the forms of rate constant expressions currently used in the CMAQ system are 
presented next. 

· 

i 
I 

• 

• 

• 

Arrhenius Equation.  Many rate constants exhibit a temperature dependence that 
corresponds to the Arrhenius equation: 

· 

I 

k  =A eC-EIT) 

(8-4) 

where A. is the pre-exponential factor, Eis the activation energy divided by the gas 
c0nsta.Q~ R, and Tis the temperature in degrees Kelvin.  For this form of reaction, either 
cins or mixing ratio units may be used, and only A and E need to be specified. 

.  . 

' 

i 
i 

Temperature Dependent A-factors.  For some reactions, the temperature dependence of 
t4e pre-exponential factor can become significant, and the Arrhenius equation does not 
hold.  These rate constant expressions can often be put in the following form that is 
available in the CMAQ system: 

· 

' 

k  =A (T/300)8  ~(-EIT) 

(8-5) 

where A, E, and Tare defined as above, and B is an empirically derived constant that 
provides a best fit to the data (Pitts-Finlayson and Pitts, 1986).  For this form, either set of 
units can be used, and only A, E and B need to be specified. 

I 
I 

Falloff Expressions.  Several ter-molecular reactions exhibit pressure dependencies that 
can be significant when modeling atmospheric chemistry.  These can be especially 
important when modeling from the troposphere through the stratosphere.  In these cases, 
the rate constant increases with increasing pressure.  In effect, the behavior of these 
reactions approaches second-order at high pressure and third-order at low pressure. 
Equation 8-6 gives an effective second-order rate constant for the falloff region between 
these two limits. 

' 

' 

k 

k0 [M] 

F{I  +  [N- 1!og(k0[M]/k.))2)-1 

= - - - - - c 

1  +  k0 [M]lk .. 

(8-6) 

In Equation 8-6, k0 and k .. are the low- and high-pressure limiting rate constants, 
respectively, and are calculated using the temperature dependent A-factor form described 
above .... Jh~ parameters Fe and N are also reaction specific, but for atmospheric conditions 
are very often 0.6 and LO respectively (Firllayson-Pitts and Pitts, 1986). · For this type of 
rate constant expression, A, E, and B must be specified for both k0 and k .. , and only ems 

i 
I 

8-14 

I 
' 

units are allowed. If the parameters N and Fe are not specified, the values listed above will 
be used. 

• 

Special Forms.  The following special rate constant forms are also in general use and 
have been included in the CMAQ system: 

EP A/600/R-99/030 

k  =  k1  +  Is [M] 

k  =  k  +  ( 

o 

/s [M] 

1  +  /s[M]/k2 

) 

k  = A  ( 1.0  +  0.6P) 

(8-7) 

(8-8) 

(8-9) 

Equation 8-7 is used for the rate constants of the reactions forming hydrogen peroxide 
from hydroperoxy radicals (H02 + H02  ....  H20 2 and H02 + H02 + H20  ....  H20J.  Equation 
8-8 is used for the reaction of the hydroxyl radical with nitric acid (HO + HN03  ....  N03 + 
H20), and Equation 8-9 is used for the reaction of the hydroxyl radical with carbon 
monoxide (HO + CO ....  H02 + C02). In these equations, k0,  k1,  k2,  and k3 are calculated 
using the Arrhenius equation, and A and E must be specified for each, with A given in ems 
units.  In Equation 8-9, Pis the atmospheric pressure in atmospheres andA can be 
specified in either set of units. 

• 

Reverse Equilibria Forms.  The CMAQ system also includes a special reverse 
equilibrium form for first-order decomposition reactions.  With these types of reactions, 
the equilibrium constant is input in a form similar to the Arrhenius equation.  Thus, the 
rate constant can be expressed as follows: 

k  =  k  I  Ae(-EIT} 

'f 

. (8-10) 

In Equation 8-10, k1 is the rate constant for the fo1ward reaction forming a species, and the ' 
denominator is an Arrhenius-like form for _the equilibrium constant. These reaction rate 
coefficient types are used, for example,  for the decomposition of pernitric acid and 
nitrogen pentoxide (i.e., HN04 ....  H02 + N02  and. N20 5  ....  N02 + N03). In the CMAQ 
system, A, E, and the corresponding forward reaction must be specified.  Either set of 
units may be used with this form. 

8.4  Mathematical Modeling 

This section describes the mathematical modeling concepts used in the CCTM to simulate gas(cid:173)
phase chemical reactions.  The first sub-section describes the fundamental equations that must be 
solved and some of the difficulties encountered in obtaining solutions to them. The next two sub(cid:173)
sections describe the two gas-phase chemistry solvers that are currently available in the CCTM. 
The last sub-section summarizes some of the important solver characteristfos. 

8.4.1  Governing Equations 

"II 

1' 

,1: 

A~ described iri Chapter 5, operator splitting allows gas-phase chemistry to be de-coupled from 
physical processes· such as advection, diffusion and deposition, and, as noted in the introduction to 
this chapter, gas-phase chemistry is modeled separately from aerosol formation and aqueous 
chemistry.  As a consequence, continuity equations for each gas-phase mecharusm species can be 
formulated anc::l. solved independently on a cell-by-cell basis.  By using the killetics laws for 
elementary reactions and applying a mass balance to each species, the following equation for the 
rate of change of each species concentration can be derived for a single cell: 

I 

l 

dC; 
dt 

=P;-L;C1 

m, 

pi  =E 

/=I 

V1,lrl 

where 

and 

L,CJ=  L  rt 

n, 

/=I 

(8-11) 

(8-12) 

(8-13) 

In Equations 8-11 through 8-13, C, is the concentration of species i,  V; 1 is the stoichiometric 
coefficient for species i in reaction /, and r1 is the rate ofreaction /.  The sum l =  l...m1 is over all 
reactions in which species i appears as a product, and the sum I= l ... n1 is over all reactions in 
which species i appears as a reactant. 

• Equation 8-11  states that the change in species concentration is equal to the chemical production 
of that species minus its chemical loss, and it is the fundamental species continuity equation for 
gas-phase chemistry that is solved in the CCTM.  If the concentration of species i is known at 
some particular time, its concentration can be computed at a later time by solving Equation 8-11. 
Since the production and loss terms contain references to other species concentrations, however, 
Equation 8-11 must be solved as part of a coupled set of ordinary differential 'equations. 

It should also be noted that the CCTM contains an option for including emissions in either the 
ve:rtical diffusiOn process or in gas-phase chemistry.  When emissions are included in gas-phase 
chemistry, the fundamental form of the Equation 8-11  is not altered since an emission source term 
is simply a zeroth:order production  rate.  Thus, for the discussions that follow, the production 
term P, is assumed to include an emission source term if species i is emitted and emissions are 
in9luded in gas-phase chemistry. 

8-16 

The system of non-linear, ordinary differential equations (OD Es) arising from Equation'8-11 for N 
species can be expressed as follows: 

EP A/600/R-99/030 

dC. 
- '  =  P.(c,t)  - L.(c,t) C.  = J;1(c,t) 
dt 

I 

I 

I 

with the initial conditions: 

i  =  1,2,. .. ,N 

(8-14a) 

(8-14b) 

where c is the vector of species concentrations and N is the total number of species in the 
chemical mechanism.  Numerical "marching" methods are typically employed to obtain 
approximate solutions for this class of problem.  In these methods, the concentrations of all 
species are given at the starting point and a solution is computed at selected time intervals (i.e., 
time steps) using the right hand side of Equation 8-14a.  Two sources of difficulty arise in 
obtaining numerical solutions to these equations as they apply to atmospheric chemistry problems. 
First, the system is nonlinear because the production and loss terms include second- and third(cid:173)
order reactions.  Second, the system of equations is "stiff" because of the widely varying time 
scales of the chemical reactions and complex interactions among species.  A stiff system can be 
described mathematically as one in which all the eigenvalues of the Jacobian matrix of Equation 8-
14a are negative, and the ratio of the absolute values of the largest-to-smallest real parts of the 
eigenvalues is much greater than one.  Systems are typically termed stiff if the latter ratio is 
greater than 104
the system very stiff (Gong and Cho,  1993). 

•  For atmospheric chemistry problems, the ratio is often greater than 10 10

,  making 

The stiffness problem coupled with the fact that these equations must be solved for tens of 
thousands of cells in a typical modeling application require that special numerical methods be 
employed.  The use of standard explicit methods is often precluded because relatively small time 
steps are required to maintain numerical stability and obtain accurate solutions.  On the other 
·hand, classical implicit methods that are both accurate and stable have not often been used 
because of high computational demands.  As a result, several special techniques have been 
developed to obtain reasonably accurate solutions in a computationally efficient manner.  At 
present, two solution techniques are available in the CCTM: the implicit Sparse-Matrix 
Vectorized Gear algorithm (SMVGEAR) and a variant of the explicit Quasi-Steady State 
Approximation (QSSA) method.  Each of these is described in detail below.  Although each of 
these techniques, as well as others that have been used in atmospheric ~hemistry models, have 
been designed to be computationally efficient, they still consume 50 to 90% of the total CPU time 
used in a model simulation.  Thus, obtaining a numerical solution to Equation 8-14a,b is normally 
the most computationally intensive portion of the CCTM. 

8.4.2  SMVGEAR 

I 

·111 

Numerical solvers based on the algorithm developed by Gear (1971b) have traditionally been used 
to obtain accurate solutions to stiff ODE problems.  The technique is implicit in method, does not 
amplify erro~~ from one step to another and incorporates automatic step size and error control.  In 
fas~~ solvers b .. ~ed.~n this method have often been used to evaltiate .?ther .fastrr solutio!1 meth()ds 
for accuracy (e:g.,Odman et al.,  1992; Gong and Cho,  1993; Dabdub and Seinfeld,  1995; and 
S~ylor and Ford, 1995).  Past versions of this code have rarely been installed 'in Eulerian models, 
hq,vever~ ~ecapse Qfthe high computational ~ost.  Jacobson and Turco (1994~ have modified the 
Gear algorithm to obtain considerable speedups on vector computers.  The SMVGEAR algorithm 
is highly vectorized to improve computational performance on vector computers and it 
incorporates special sparse matrix techniques to increase computational efficiency.  Further 
enhancements have been obtained by ordering the cells for processing.  Each of these are 
described further below.  Since the technique is based on Gear's original algorithm, it is briefly 
described first.  For more details on the Gear method, the reader is referred to Gear (1971a) and 
Gear (1971b). 

h," 

1111!!!11111' 

,, 

"'" 

, 

, 

I 

, 

, 

'" 

8.4.2.1  Standard Gear Algorithm 

The Gear algorithm is one of a class of methods referred to as backward differentiation formulae 
(BDF).  The generalized BDF that forms the basis for Gear's method can be expressed as follows: 

I 

(8-15) 

w~ere n refers to the time step, h is the size of the time step, p  is the assumed order, /30  and ~ are 
scalar quantities that are functions of the order, and f( cmtn)  is the vector of production and loss 
terms defined by the right hand side of Equation 8-14a.  The method is implicit since 
concentiations at the desired time step n depend on values of the first derivatives contained in 
f( cn,tn) that are functions of the concentrations at the same time.  The order of the method 
corresponds to the number of concentrations at previous time steps that are incorporated in the 
summation on the right hand side of Equation 8-15. 

, 

""''" 

"' 

I 

'" 

,,, 

I 
I 

To facilitate changing step size and estimating errors, the multi-step method i~ Equation 8-15 is 
transformed to a multi-value form in which information from only the previo{is step is retained, 
but information on higher order derivatives is now used (Gear,  1971b).  In this formulation, the 
solution to Equation 8-14a,b is first approximated by predicting concentrations and higher order 
deriyatives at the end of a time step for each species using the following matrix equation: 

I 

z 
1,n,(O) 

= Bz 

1,n- I 

(8-16) 

8-18 

where zi =  [c;,  he';, ... , hpc/PJ/p!]T, the subscript n,(O) refers to the prediction at the end of time step 
n, and the subscript n-1  refers to values obtained at the end of previous time step (or the initial 
conditions when n =  1 ).  B is the Pascal triangle matrix, the columns of which contain the 
binomial coefficients: 

EPA/600/R-991030 

1  1  1  1  ... 

2  3 

3 

...  p-1  p 
... 

B 

0 

p 

(8-17) 

The prediction obtained from Equation 8-16 is then corrected by solving for z;,n  such that the 
following relations hold for all species: 

(8-18) 

In Equation 8-18, r is a vector of coefficients that is dependent on the order, but r 2,  the element 
corresponding to the first derivative location in z,  is always equal to one.  Thus, the correct value 
of c;,n  is obtained when the calculated value of c  ~.n equalsi(cmtn) in Equation 8-18.  An 
approximate solution for C;,n  is obtained by applying Newton's method to the system of equations 
that correspond to the first equation in 8-18 for all species.  This leads to the following corrector 
iteration equation: 

where m refers to the Newton iteration number, the vector f(cn,tn)  is calculated using 
concentrations computed for the m-th iteration, oc is a vector containing the most recent 
estimates of first derivatives, I is the identity matrix, and J  is the Jacobian matrix whose entries 
are defined as: 

(8-19) 

i,j  =  1,2, .. .N 

(8-20) 

At the end of each iteration, the vector containing the first derivatives ( oc) is updated, but higher 
order derivatives in z need not be computed until convergence is achieved. 

After convergence is achieved, the local truncation error for each species, e;, is given by: 

'1111'' 

,I., 

1111' 

,II 

' 

:11•11 

, 

' 1 

I• 

11 

:• 

"111 

)  term and approximating fr 1c/p+i) 
Th.e error is estimated in the algorithm by neglecting the O(J.r2
from the backward. difference of lt'c/P>/p!  which can be calculated using the last components of the 
vJ6tors Z;~m) arid ~;:'~-!  de:fined abo~e.  These error estimates are used to control accuracy and to 
cq,!,lllge both the time step size and the order of the method when warranted. 

!"""', 

, 

, '::· 

I 

(8-21) 

,;111"11' 

,, 

" 

I 

1
' 

Although several variants of the basic Gear algorithm have been developed, the fundamental 
computational scheme can be described generically as follows.  At the begiruling of any 
integration interval, the order is set to one and the starting time step is either calculated or 
selected by the user.  Each time step is initiated by predicting concentrations at the end of the time 
step using Equation 8-16.  Corrector iterations are then carried out using Equation 8-19 until 
prescribed convergence criteria are achieved or non-convergence is deemed to have occurred. 
when convergence is achieved~ the error is computed using the approximation for Equation 8-21. 
If the error is within prescribed limits, the solution for the step is accepted and the step size and 
orger to be us~.cl iq,Jhe next step are estimated.  The size of the time step is estimated for the 
current order, the next lowest order, and the next highest order using error estimates derived from 
Equation 8-21  for the next step.  From these, the largest time-step size and its corresponding 
order are then selected for use in the next step.  If either the convergence or error test fails, the 
integration is restafied from the beginning ofihe failed time step after re-evaluating the Jacobian 
m~trix, reduci.Ilg the size of the time step, and/or lowering the order. 

' 

Th~ individual operations described above are normally handled automatically in Gear algorithms. 
To reduce comput(ltional demands, the algorithms also utilize several empirically based rules.  For 
example, the Jacobian matrix is only updated after a prescribed number of successful steps have 
been completed, if the order changes, or if a convergence or error test failure occurs.  In the 
Newton iterations, progress towards convergence is monitored and the iterations halted if the 
progress is judged insufficient or if three complete iterations have been performed without 
convergence being achieved.  To maintain numerical stability, changes to the size of the time step 
and the order are allowed no more than once every p+ 1 steps for a p-th order method. 

I 

I 

8.4.2.2 Vectorized Gear Algorithm 

Jacobson and Turco (1994) have modified the Gear algorithm to incorporate additional 
computational ~fficiencies that can achieve speedups on the order of 100 on vector computers. 
A~cmt half of tlie improvement is attributed to ellhanced vectorization, and h~lf to improved 
ma;trix operations.  Because of the improved matrix operations, SMVGEAR also runs faster than 
traditional Gear solvers on non-vector machines, but the greatest benefit will be obtained with 
vector machines.  The major enhancements incorporated in SMVGEAR are now described in 
m~re detail. 

' 

"' 

,, 

In the conventional application of the Gear-type algorithm, the method is applied to each grid cell 
individually.  With this implementation, the length of the innermost loops in most computations 
corresponds to the number of species, which is typically on the order of 30 to 100.  In 
SMVGEAR, the modeling domain is divided into blocks of cells, and the Gear algorithm is 
applied to each cell within a block simultaneously.  With this structure, the length of the innermost 
loops for most calculations is equal to the number of cells in a block.  Substantial improvements in 
vectorization can therefore be obtained if the block size is larger than the number of species. 
Jacobson and Turco (1994) found that a block size on the order of 500 cells achieves about 90% 
of the maximum vectorization speed on a Cray C-90 computer.  The use oflarger block sizes may 
not substantially increase computational speed and may incur some additional penalties.  For 
example, memory requirements increase with increasing block size.  Furthermore, the size of a 
time step in SMVGEAR is the same for each cell within a block and is based on the time-step 
estimate for the stiffest cell in the block.  Limiting the block size can therefore reduce excess 
calculations that need to be performed for the less stiff cells.  Jacobson (1995) also achieved 
computational savings by ordering the cells by stiffness before dividing them into blocks.  Each 
block then tends to contain cells of similar stiffness, thereby reducing excess computations for 
some cells.  Jacobson found excess computations were reduced by about a factor of two, and that 
these reductions more than offset the additional work incurred with calculating and sorting the 
cells by stif:fiiess. 

Much of the computational intensity associated with the Gear method arises from the matrix 
operations that are needed to perform the Newton iterations in the corrector step.  Jacobson and 
Turco have introduced two techniques to improve the efficiency of these operations.  First, all 
known cases of multiplication by zero in matrix multiplication and decomposition are eliminated. 
This is particularly beneficial for atmospheric chemistry problems since the Jacobian matrices are 
almost always sparse (i.e., they contain a large number of zero entries).  However, decomposition 
techniques that are applied to these matrices often result in substantial fill-in, thereby reducing the 
benefits of employing sparse-matrix techniques.  To maintain maximum sparsity after the 
decomposition operation, SMVGEAR orders the species in the Jacobian such that those with the 
fewest partial derivative terms are located in the top rows, and those with the most are in the 
bottom rows.  At the very beginning of the program, the ordering is done and a symbolic 
decomposition is performed to identify multiplies by zero.  Since the computations associated 
with the matrix operations are determined entirely by the structure of the chemical mechanism, it 
is necessary to do this only once and the results can then be applied to every cell uniformly. 

The SMVGEAR algorithm has been implemented in the CCTM with minor changes to the 
original algorithm but extensive changes to the original computer code.  The code changes arose 
from linking the algorithm to the generalized chemistry processor used in the CMAQ system, and 
developing a driver routine specific to the CCTM structure.  The only significant change to the 
algorithm involved modifying the code to eliminate the possibility of obtaining negative 
concentrations.  With the standard Gear algorithm, negative concentrations can occur when a 
species is rapidly depleted, although the magnitudes of these concentrations are extremely small. 
In the CCTM implementation, a lower bound on allowable concentrations is applied, and the rates 
of change and the Jacobian matrix are modified to reflect that no changes in concentration are 
occurring when the lower bound is reached.  Comparisons with the standard Gear algorithm show 
virtually no differences in species concentrations above the lower bound, but a small penalty in 
computational performance is incurred. Nevertheless, the approach insures a positive-definite 
solution. 

' 

' 

The computational performance of SMVGEAR is also affected by the error tolerances used for 
the Newton iteration convergence tests and the local truncation error tests.  Error control in 
SM\f GEAR 1~ similar to that used. in LSODE (Hindmarsh,  1980).  Both a relative and an absolute 
tolerance must be specified. In their discussion of ODE solvers, Byrne and Hindmarsh (1987) 
relate the relative error tolerance to the number of accurate digits and the absolute error tolerance 
to the noise level (i.e., the size of the largest concentration that can be neglected).  If r is the 
number of accurate digits required, then Byrne and Hindmarsh suggest setting the relative 
tqlerance to  ~g·Cr+ 1>,  The absolute error tolerances cannot be specified as generically because 
particular model applications may require different accuracies for the mechanismn species.  In the 
CCTM implementation of SMVGEAR, the relative tolerance and absolute tolerances have been 
preset to 10·3 and 10·9 ppm, respectively.  However, these values can be changed by the user 
relatively easily in the CCTM as described in EPA (1998). 

8.4.3  QSSA Solver 

The QSSA solver is a low order, explicit solver that exhibits good stability for stiff systems. 
Although less accurate than the Gear solver, it is still a reasonably accurate, fast solver that is 
especially suitable for large scale grid models.  There are actually many versions of solvers that go 
by the name "QSSA" (e.g., Mathur et al.,  1998).  The solver developed for the CCTM is a 
predictor/corrector version based on the one developed by Lamb and used in the Regional 
Oxidant Mod~.l (Lamb,  1983, and Young et al.,  1993). 

. 

i 

I  '":I' 

'" 

The QSSA method originates from assuming integration time steps sufficiently small such that in 
Equation 8-11, the production and loss rate terms Pi and L; can be considered constant. If the 
Jacobian is diagonaily dominant, this assumption may be valid as Llt ...  0, and the time step 
solution at tn+ 1 =  tn+ Llt can be written formally as: 

c,  :  c,  +  ( c.  - c.  ) e 

• 

In 

1. 

-l t.1 

I 

(8-22) 

where  C1_  =  P, I Li  and  C;.  is the solution at  tn-

The CCTM QSSA makes no a priori assumptions about reaction time scales.  For example, there 
ru;:~ no assume:<:l  st~ady states.  However, the algorithm separates the numerical computation into 
either an Euler step, a fully explicit integration, or an asymptotic evaluation based on 
photochemical lifetimes estimated from an initial, predictor calculation of P; and L;.  The cut-offs 
and equations for each predictor step are: 

I 

8-22 

Eulerstep: 

LJ!::..t  ~  0.01 

Explicit: 
Asymptotic:  Li /:,.t  l=:  10.0 

0.01  -<  Li /:,.t  -<  10.0 

n 

C  = C1  +  (P.  +  L.C1  ) 
C.  = C  +  ( C. 
- C.  ) e 
C.  =  C. 

I. 

In 

I,. 

I 

I 

n 

I 

I 

I 

1_ 

EPA/600/R-991030 

(8-23) 

The CMAQ QSSA algorithm proceeds in three stages: an optimal time step determination, a 
predictor evaluation, and a corrector evaluation.  In the first stage, an optimal chemistry time step 
interval Lit is determined at time tn  based on a tolerance parameter, A.,  such that 

I c.  - c.  I ~  )., c  . 

I 

1n 

1n 

Substituting Equation 8-22 into 8-24 gives: 

c 
'n 

IC.  - c  I 

'• 

1n 

/:,.t  ~  - -

1 
L 

I 

In 

[ 

I  - A 

l 

. 

(8-24) 

(8-25) 

The algorithm attempts to weight the time step determination against species whose 
concentrations are very small compared to the primary oxidants. These usually include some of 
the fastest reacting radicals. The following quantity is defined as 

(8-26) 

Then the following quantity is computed for only those species that satisfy Ca> eC,est ( e= 0.01): 

(8-27) 

(8-28) 

ot  =  __  n 

).Ca. 
p 
ex 

ex 

The tolerance parameter A.  is controlled by the rate at which the key NO species concentrations 
are changing; if they are changing too rapidly, the tolerance is tightened, otherwise it is relaxed: 

A.= 

{ 

0.001,  if d[NO] I [NO] 

dt 

0.005,  otherwise 

2:  0.5%  per  minute 

(8-29) 

After determiriing a time scale for each species a, .Lit is set to min{ ota}.  For computational 
efficiency, .Lit is further constrained to be no less than one second and of course is also constrained 
to be no greater than the total integration time. 

· 

In the predictor step, the species concentrations are updated ( C; _.  C; * ) with the optimal time step 
u~,i11g the Euler-step, explicit or asymptotic calculations described above.  Once  c,· is calculated, 
QSSA computes new production and loss rate coefficients P; • and L; ·, respectively. 

I 
' 

,. 

In the corrector step, the final production rate is set as the average of the initial and predictor 
values,  ft,  =  ( P/  +  P1 )  I 
the same cut-offs based on L1 that were determined in the predictor step: 

, and the new concentration C; for time  tn+t  =  tn + .Lit is computed using 

I 

Eulerstep: 

Explicit: 

Asymptotic: 

I 

'n 
P  IL•+ (C.  - P./L.*)e-L,"At 

C1  =  C.  +  ( P  +  L, • C.  )  llt 
C 
c,  =  P/ L;. 

1
11 

In 

i 

I 

: 

I 

I 

I 

(8-30) 

The algorithm has been optimized for vector computers by moving the grid cell loops into the 
innermost position (Young et al.,  1993) as is done in SMVGEAR described above.  To minimize 
storage requirements, grid cell blocking has been implemented wherein blocks of cells are handed 
off to the solver in sequence. The CMAQ QSSA has also been optimized for the Cray T3D by 
utilizing vario'us coding techniques aimed specifically at that architecture. Some of these 
optimizations are described in Chapter 19. 

· 

I 

8.4.4  Summary 

Gear type solvers have generally been considered the most accurate for gas chemistry, 
representing ''exact solutions" (provided the controlling numerical tolerances are sufficiently 
tight). Until the advent of SMVGEAR, however, it has not been feasible to use these solvers in 
Eulerian models.  SMVGEAR is designed to run optimally on high end vector computers such as 
the Cray C90, but its use on scalar machines may be impractical. Although less accurate, the 
QSSA solver may be more suitable for those types of computers. 

· 

The issue of ~ccuracy versus computational speed is a continuing concern (Mathur et al.,  1998), 
p~icularly siiice the availability of high-end machines like the Cray C90 is limited.  The CMAQ 
QSSA solver presents a reasonable, numerically efficient alternative and, although not considered 
as accurate as a Gear-type solver, may be sufficiently accurate for modeling, taking into 
consideration the uncertainties of the other numerical modeling components.  Accuracy can be 
somewhat improved by using shorter integration steps in the solver, but then computational work 
mounts, defeating the purpose. For the CMAQ QSSA, accuracy will be compromised when the 
ODE system is very stiff and the system Jacobian strays from diagonal dominance.  Nevertheless, 
the trade-off between solution efficiency and accuracy may still warrant its use in these cases. 

Finally, both the SMVGEAR and the QSSA solvers have been incorporated in the CMAQ system 
with the predefined accuracy controls that were described in the previous two sections.  Of 
course, these error controls can be changed by the user if desired, but that will of course affect 
both the prediction accuracy and the efficiency of obtaining a solution.  Note also, that either 
solver can be used with any of the CMAQ chemical mechanisms that were described in section 
8.2. 

8.5 

References 

Atkinson R., Lloyd A.  C.  and Winges L. (1982) An updated chemical mechanism for 
hydrocarbon/NO:x/S02 photooxidations suitable for inclusion in atmospheric simulation models. 
Atmos. Environ., 16, 1,341-1,355. 

Bowman F. M., Pilinis C., and Seinfeld J.  (1995) Ozone and aerosol productivity of reactive. 
Atmos. Environ., 29, 579-589. 

Byrne G.  D. and Hindmarsh A. C.  (1987) Stiff ODE solvers: a review of current and coming 
attractions. J.  Comput.  Phys.  70, 1-62. 

Carter W. P. L. (1988) Appendix C Documentation of the SAPRC Atmospheric Photochemical 
Mechanism Preparation and Emissions Processing Programs for Implementation in Airshed 
Models. Final Report for California Air Resources Board Contract No. A5-122-32. 

Carter W. P. L. (1990) A detailed mechanism for the gas-phase atmospheric reactions of organic 
compounds. Atmos.  Environ.  24A, 481-515. 

Carter W. P. L. (1996) Condensed atmospheric photooxidation mechanisms for isoprene. Atmos. 
Environ.  24, 4,275-4,290. 

Carter W. P. L. and Atkinson R. (1996) Development and evaluation of a detailed mechanism for 
the atmospheric reactions of isoprene and NOx. Int.  J.  Chem.  Kinet. 28, 497-530. 

Carter W. P. L., Luo D.  and Malkina I. L. (1997) Environmental Chamber Studies for 
Development of an Updated Photochemical Mechanism for Reactivity Assessment. Final Report 
for California Air Resources Board Contract No. 92-345, Coordinating Research Council, Inc., 
Project M-9 and National Renewable Energy Laboratory, Contract ZF-2-12252-07. 

c;J:)l;ll1g J.  S., Brost R. A., Isaksen I.  S. A., Madronich S., Middleton P., Stockwell W.R. and 
WSleck C.J. (1987) A three-dimensional Eulerian acid deposition model: physical concepts and 
formulation,.!.  geophys.  Res., 92, 14,681  - 14,700. 

Dabdub D. and Seinfeld J.  H. (1995) Extrapolation techniques used in the solution of stiffODEs 
associated with chemical kinetics of air quality models. Atmos. Environ.  29, 403-410. 

I)odge M.C. (1989) A comparison of three photochemical oxidant mechanisms,.!. geophys.  Res., 
94, 5,121-5,136. 

I 

EPA (1989) Procedures for Applying City-specific EKMA. EPA-450/4-89-012. 

''''"" 
i 

EPA (1991) User's Guide for the Urban Airshed Model, Volume I: User's Manual for UAM 
(CB4). EPA-~,50/4-90-007a. 

I 

El'A (1998) EPA Third-Generation Air Quality Modeling System, Models-3 Volume 9b User 
Manual. EPA-600/R-98/069b. 

I 

Gear C. W.  (1971a) Numerical Initial Value  Problems in Ordinary Differential Equations. 
Prentice-Hall, Englewood Cliffs, NJ. 

I 
'I" 

Gear C.  W.  (1971 b) The automatic integration of ordinary differential equatfons. Comm.  ACM 
14, 176-179. 

I 

Gery M. W., Morris R. E., Greenfield S.  M., Liu M. K., Whitten G. Z., and Fieber J. L.  (1987) 
Development of a Comprehensive Chemistry Acid Deposition Model (CCADM). Final Report for 
.IJ:iteragency Agreement DW 14931498, U.  S. Environmental Protection Agency and U.  S. 
Department of Interior. 

: : ~ i ! : : ' : : ; " ' ' 

~ : : : : '  : 1

11 
: 

Gery M. W., Whitten G. Z., Killus J.P. and Dodge M. C.  (1989) A photochemical kinetics 
mechanism for ur~an and regional scale computer modeling . .!.  geophys.  Res. 94,  12,925-12,956. 

I ' 

' I 

I 

,' I  " 

I 

' 

"' 

I i 

' " 

Gong W. and Cho H.  R. (1993) A numerical scheme for the integration of the gas-phase chemical 
rate equations in three-dimensional atmospheric models. Atmos.  Environ.  27A, 2,147-2,160. 

Hindmarsh A'.C.  (1980) LSODE and LSODI, two new initial value ordinary differential equation 
solvers. ACM News!. 15, 10-11. 

· 

Hough A. (1988) An intercomparison of mechanisms for the production of photochemical 
oxidants . .!.  geophys. Res.,  93, 3,789-3,812. 

Jacobson.~.!'~q,,Jurco ~,~,, P.  (1994) SMVGEAR: A sparse-matrix, vectorized Gear code for 
alinospheric models. Atmos.  Environ.  28, 273-284. 

Jacobson M.  (1995) Computation of global photochemistry with SMVGEAR II. Atmos.  Environ. 
29, 2,541-2,546. 

Jeffries H. E. (1995) Photochemical Air Pollution. Chapter 5 in Composition,  Chemistry,  and 
Climate of the Atmosphere. Ed. H.B. Singh, Von Nostand-Reinhold, New York. 

Jeffries H. E., Gery M. and Murphy K.  (1993) Advanced Chemical Reaction Mechanisms and 
Solvers for Models-3. Progress Report for EPA Cooperative Agreement CR-820425. 

Lamb, R.G. (1983) A Regional Scale (1000 km) Model of Photochemical Air Pollution.  Part I -
Theoretical Formulation.  EPA-600/3-83-035, U. S.  Environmental Protection Agency, Research 
Triangle Park, NC. 

Leone J.A. and Seinfeld  J.H. (1985) Comparative analysis of chemical reaction mechanisms for 
photochemical smog. Atmos.  Environ., 19, 437-464. 

Lurmann F. W., Carter W. P. L.  and Coyner  L.A. (1987) A Surrogate Species Chemical 
Reaction Mechanism for Urban Scale Air Quality Simulation Models Volume 1. EPA-600/3-87-
014a. 

Mathur R., Young J.  0., Schere K. L., and Gipson G. L. (1998) A Comparison of Numerical 
Techniques for Solution of Atmospheric Kinetic Equations. Atmos.  Environ., 32, 1,535-1,553. 

NASA (1997). Chemical Kinetics and Photochemical Data for Use in Stratospheric Modeling 
Evaluation No.  12. JPL Publication 97-4, Jet Propulsion Laboratory, Pasedena, CA. 

Odman, M. T., Kumar N. and Russell A.G. (1992) A comparison of fast chemical kinetic solvers 
for air quality modeling. Atmos.  Environ.  26A, 1,783-1,789. 

Pandis S. N., Harley R. A., Cass G. R.  and Seinfeld J.  H.(1992) secondary aerosol formation and 
transport. Atmos.  Environ.  26A, 2,269-2,282. 

Pitts-Finlayson B.J. and  Pitts J.N. (1986) Atmospheric Chemistry: Fundamentals and 
Experimental Techniques, J.  Wiley and Sons, New York. 

SAI (1993) Systems Guide to the Urban Airshed Model (UAM-V). Systems Applications 
International, San Rafael, CA. 

Saylor, R. D. and Ford G.D. (1995) On the comparison of numerical methods for the integration 
of kinetic equations in atmospheric chemistry and transport models. Atmos.  Environ.29, 2,585-
2,593. 

Stockwell W. R, Middleton P.  and Chang J.  S.  and Tang X.  (1990) The second generation 
regional acid deposition model chemical mechanism for regional air quality modeling. J.  geophys. 
Res.  95 (dlO),  16,343-16,367. 

', 

"' 

111,' 

'11,, 

111111:1""' 

'", 

",1,1, 

ii1j:;,,11111, 

i' 

,,11:,1111' 

,, 

,l1 

Stockwell W.R (1986) A homogeneous gas phase mechanism for use in a regional acid 
deposition model, Atmos. Environ., 20,  1,615-1,632. 

' 
I 

I, 

,, 

Stockwell W.R., Kirchner F., Kuhn M., and S.  Seefeld (1997) A new mechanism for regional 
atmospheric chenifatry modeling. J.  geophys.  Res.  102 (D22), 25,847-25,879. 1 

I 
I' 

Whitten G.  Z., Hogo H.  and Killus J.P. (1980) The carbon bond mechanism:  a condensed kinetic 
mechanism for photochemical smog. Envir. Sci.  Technol.  14, 690-701. 

I 

Young J.  0., Sills E., Jorge D. (1993) Optimization of the Regional Oxidant Model for the Cray 
Y-MP. EPN6QO/R'.'"94-065, U.S. Environmental protection Agency, Research Triangle Park, 
NC. 

, 

This chapter is taken from Science Algorithms of the EPA Models-3 Community 
Multiscale Air Quality (CMAQ) Modeling System, edited by D. W. Byun and J. K. S. 
Ching, 1999. 

Appendix SA Chemical Mechanisms Included in the CMAQ System 

Table SA-I.  CB4 Mechanism Species List 

Table 8A-2.  RADM2 Mechanism Species List 

Table 8A-3.  CB4 Mechanism 

Table 8A-4.  CB4  AE Mechanism 

Table 8A-5.  CB4_AQ Mechanism 

111111,1!,11' 

Table 8A-6.  CB4  AE  AQ Mechanism 
Table 8A-7.  R.ADM2 and RADM2  AQ Mechanisms 

,"'111.1'  ~  7 

Ii 

'11 

-

~,' 

'', 

I 

-

-

11111:',I 

1 

"'· 

Table 8A-8.  RADM2  AE and RADM2  AE  AQ Mechanisms 

-

-

Table 8A-9.  RADM2  CISI and RADM2  CISI  AQ Mechanisms 

'

1 

::;;:,,1: 

'I, 

,,Ill:",::·,', 

1,, 

'"" 

'·:w'1

; , , , ,  

" 

11·!' 

"' 

-

-

Table 8A-10.  RADM2_CIS1_AE and RADM2_CIS1_AE_AQ Mechanisms 

Table 8A-1 l.  RADM2_CIS4 and RADM2_CIS4_AQ Mechanisms 

Table 8A-12.  RADM2_CIS4_AE and RADM2_CIS4_AE_AQ Mechanisms 

Notes to Tables 8A-3 through 8A-12: 

a) 

b) 

c) 

The mechanism listings are divided into two parts. The first lists the reactions and the 
second lists the rate constant expressions. 

. . 

The  parameters  for  the  rate  constants  are  given  in ems  units.  Rate  constants 
calculatedin ems units for T=298 °K and P= 1 atm are shown in the rightmost column 
of these listings. 

For photolytic  reactions,  photo  table  refers  to  the  photolysis  rates  described  in 
Chapter 14.  The rate constant for all photolytic contains a zero entry in these tables, 
but is calculated in the CCTM as the product of the scale factor and the photolysis 
rate that is calculated by the CMAQ photolysis rate processor. 

d) 

The falloff rate expression referred to in these tables is Equation 8-6 in Section 8 .3 .2. 

Table 8A".l 

CB4 Mechanism Species List 

Nitrogen Species 
NO 
N02 
HONO 
N03 
N205 
HN03 
PNA 

Oxidants 

ru(cid:173)

H202 

Sulfur Species 
S02 
SULF 

Atomic Species 
0 
om 

Nitric oxide 
Nitrogen dioxide 
Nitrous acid 
Nitrogen trioxide 
Nitrogen pentoxide 
Nitric acid 
Peroxynitric acid 

Ozone 
Hydrogen peroxide 

Sulfur dioxide 
Sulfuric acid 

Oxygen atom (triplet) 
Oxygen atom (singlet) 

Odd Hydrogen Species 
OH 
H02 

Hydroxyl radical 
Hydroperoxy radical 

Carbon oxides 

8-29 

co 

PAR 
ETH 
OLE 
TOL 
XYL 
!SOP 

Carbon monoxide 

Paraffin carbon bond (C-C) 
Ethene (CH2=CHi) 
Olefinic carbon bond (C=C) 
Toluene {C6H4-CH1) 
Xylene (C6Hs-{CH3)2) 
lsoprene 

Carbonyls and phenols 
FORM 
ALD2 
MGL Y 
CRES 

Formaldehyde 
Acetaldehyde and higher aldehydes 
Methyl glyoxal  (CH3C{O)C(O)H) 
Cresol and higher molecular weight  phenols 

Organic nitrogen 
PAN. 
NTR 

Organic Radicals 
C203 
ROR 
CRO 

Peroxyacyl nitrate (CH3C(O)OON02) 
Organic nitrate 

Peroxyacyl radical (CH1C(O)OO') 
Secondary organic oxy radical 
Methylphenoxy radical 

Qperttors 
X02 
X02N 

NO-to-N02 Operation 
NO-to-nitrate operation 

Products of organics 
T02 
Of EN 

ISPD 

Toluene-hydroxyl radical  adduct 
.  High molecular weight aromatic 
''  oxidation ring fragment 

Pro.~,µcts of isoprcne reactions 

Socc;ies added for aerosols 
SULAER 
TO~R 
xYLAER 
Oi~R . 
TERPAER 
TERP 

Counter species for H2S04 production 
Counter species for toluene reaction 
Couiiterspecies for xylene reaction 
COU!Jter species for cresol reaction 
Counter species for tcrpene reaction 
Monoterpenes 

SP!icies added for aqueous chemistry 
FACD 
AACD 
P~~D 
UMHP 

Formic acid 
Acetic and higer acids 
Pero~y acetic acid 
Upper limit ofmethylhydropcroxide 

,,•I 

Table 8A-2 

RADM2 Mechanism Species List 

OP! 
OP2 
PAA 

Organic acids 
ORAi 
ORA2 

Methyl hydrogen peroxide 
Higher organic peroxides 
Peroxyacetic acid 

Formic acid 
Acetic and higher acids 

Peroxv radicals from alkanes 
M02 
ETHP 
HC3P 
HCSP 
HC8P 

Methyl peroxy radical 
Peroxy radical formed from ETH 
Peroxy radical formed from HC3 
Peroxy radical formed from HCS 
Peroxy radical formed from HC8 

. Peroxy radicals from alkenes 
OL2P 
OL TP 
OLIP 

Peroxy radical formed from OL2P 
Peroxy radical formed from OL TP 
Peroxy radical formed from OLIP 

Peroxy radicals from aromatics 
TOLP 
XYLP 

Peroxy radical formed from TOL 
Peroxy radical formed from XYL 

Peroxy radicals with carbonyl groups 
AC03 
KETP 
TC03 

Acetylperoxy radical 
Peroxy radical formed from KET 
H(CO)CH=CHC03 

Nitrogen Species 
NO 
N02 
HONO 
N03 
N205 
HN03 
HN04 

Oxidants m(cid:173)

H202 

Sulfur Species 
S02 
SULF 

Atomic Species 
03P 
OID 

Nitric oxide 
Nitrogen dioxide 
Nitrous acid 
Nitrogen trioxide 
Nitrogen pentoxide 
Nitric acid 
Peroxynitric acid 

Ozone 
Hydrogen peroxide 

Sulfur.dioxide 
Sulfuric acid 

Oxygen atom (triplet) 
Oxygen atom (singlet) 

Odd Hydrogen Species 
HO 
H02 

Hydroxyl radical 
Hydroperoxy radical 

Carbon oxides 
co 

Carbon monoxide 

Alkanes 
ETH 
HC3 
HCS 
HC8 

Alken es 
OL2 
OLT 
OLI 
ISO 

Aromatics 
TOL 
XYL 
CSL 

Carbonyls 
HCHO 
ALO 
KET 
GLY 
MGLY 
DCB 

. 

Ethane 
Alkanes w/ 2.7xJ0·13 > kott < 3.4x10·12 
Alkanes w/ 3.4xJ0·12  > k0 "  < 6.8xto·12 
Alkanes w/ koH > 6.8xJ0·12 

Peroxy radicals involving nitrogen 
X02 
XN02 
OLN 

NO-to-N02 Operator 
NO-to-nitrate operator 
N03-alken adduct 

Species added for aerosols 
SULAER 
HCSAER 
OLIAER 
TOLAER 
XYLAER 
CSLAER 
TERP AER 
TERP 

Counter species for H2S04  production 
Counter species for HC8 reaction 
Counter species for OLI reaction 
Counter species for toluene reaction 
Counter species for xylene reaction 
Counter species for cresol reaction 
Counter species for terpene reaction 
Monoterpenes 

Ethene 
Terminal olefins 
Internal olefins 
Isoprene 

Toluene and less reactive aromatics 
Xylene and more reactive aromatics 
Cresol and other hydroxy substituted 
aromatics 

Formaldehyde 
Acetaldehyde and higher aldehydes · 
Ketones 
Glyoxal 
Methyl glyoxal 
Unsaturated dicarbonyl 

Organic nitrogen 
PAN 
TPAN 
ONIT 

Peroxyacetyl nitrate and higher PANs 
H(CO)CH=CHC03N02 
Organic nitrate 

Organic peroxides 

Table 8A-3.  CB4 Mechanism 

Reaction  List 
>----------------------------------------------------------------------< 
{  1}  N02 

+ 

0 

NO 
03 
N02 
NO 
N03 
N02 
N03 
0 
010 
0 
0 

2.000*0H 
H02 
OH 

0.890*N02 
2.000*N02 

NO 
N205 
2.000*HN03 

N03 
2.000*N02 
2.000*HONO 
HONO 
OH 
N02 
NO 
HN03 
N03 
OH 
PNA 
H02 
N02 
H202 
H202 

2 .. OOO*OH 

H02 
H02 
H02 
2.000*H02 
co 
OH 
HN03 
C203 
C203 
C203 
X02 
FORM 
N02 
H02 
PAN 
C203 
2.000*X02 
0.790*FORM 

+ 

+ 

+  0.790*0H 
X02 
0.870*X02 

+  0.110*AL02 

+  0.890*0 

+  O.llO*NO 

+ 

+ 

+ 

+ 

+ 

+ 

+ 
+ 

+ 
+ 
+ 

N02 

N02 

NO 

N02 

N02 

N02 

co 
co 

H02 
H02 
OH 

HN03 
+ 
+  2.000*H02 

+ 

X02 

+ 
+ 

+ 

+ 

co 
co 

co 

FORM 

N02 

+ 
+  2.000*FORM 
+  0.790*X02 

+  2.000*H02 
+  0.790*H02 

FORM 
+ 
+  0.130*X02N 
+  0.760*ROR 

H02 
+ 
+  0.110*H02 
- O.llO*PAR 

8-32 

<.... 

2)  0 

{  3}  03 

I :~ ~ 

{  6)  0 
( 
7}  03 
{  8)  03 
( 
9}  03 
(  10}  010 
{  11}  010 
f ~~l  ~~O 
{  14}  03 
(  15}  N03 
(  16)  N03 
(  17)  N03 
((  18}  N03 

+  hv 
+  [02] 
+  NO 
+  N02 
+  N02 
+  NO 
+  N02 
+  hv 
+  hv 
+  [N2] 
+  [02] 
+  [H20] 
+  OH 
+  H02 
+  hv 
+  NO 
+  N02 
+  N02 

19}  N205  +  [H20] 

+  [02] 
+  [H20] 

+  [H20] 

+  NO 
+  N02 
+  NO 
+  hv 
+  OH 
+  HONO 
+  N02 
+  HN03 
+  NO 
+  N02 

{  20}  N205 
{  21)  NO 
{  22}  NO 
{  23)  OH 
{  24}  HONO 
(  25}  HONO 
{  26)  HONO 
(  27)  OH 
(  28)  OH 
((  29}  H02 
30}  H02 
{  31)  PNA 
(  32)  PNA 
+  OH 
+  H02 
(  33}  H02 
+  H02 
{  34}  H02 
+  hv 
{  35}  H202 
+  OH 
{  36}  H202 
(  37)  co 
+  OH 
(  38}  FORM  +  OH 
{  39)  FORM  +  hv 
{  40}  FORM  +  hv 
(  41)  FORM 
+  0 
+  N03 
(  42}  FORM 
+  0 
{  43}  ALD2 
+  OH 
{  44}  ALD2 
{  45)  ALD2 
+  N03 
{  46)  ALD2  +  hv 

{  47}  C203  +  NO 

{  48)  C203  +  N02 
(  49}  PAN 
. {  so}  C203  +  C203 
(  51)  C203  +  H02 

52}  OH 
53}  PAR 

+  OH 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 

--> 
--> 
--> 
--> 

--> 
--> 

Table 8A-3.  CB4 Mechanism 

EPA/600!R-99/030 

1.100*ALD2  +  0.960*X02 

+  0.940*H02 
+  0.040*X02N  +  0.020*ROR 

- 2.100*PAR 
H02 
NTR 

0.630*ALD2  +  0.380*H02 

+  0.280*X02 

+  0.300*CO 
+  0.220*PAR 
FORM 

+ 

·  H02 

O.SOO*ALD2 
+  0.440*H02 
PAR 
0.910*X02 
ALD2 
FORM 

+ 

+  1.700*H02 
X02 

+  0.220*ALD2 
FORM 

0.080*X02 
+  O.S60*T02 
0.900*N02 
+  0.100*NTR 
CRES 
0.400*CRO 

+  0.300*0PEN 

CRO 
NTR 
0.700*H02 

{  S4}  ROR 

SS}  ROR 
S6}  ROR 
S7}  OLE 

+  N02 
+  0 

S8}  OLE 

+  OH 

S9}  OLE 

+  03 

60}  OLE 

+  N03 

61}  ETH 

+  0 

62}  ETH 

+  OH 

63}  ETH 
64}  TOL 

+  03 
+  OH 

6S}  T02 

+  NO 

66}  T02 
67}  CRES 

+  OH 

68}  CRES 
69}  CRO 
70}  XYL 

+  N03 
+  N02 
+  OH 

71}  OPEN 

+  OH 

72}  OPEN 
73}  OPEN 

+  hv 
+  03 

74}  MGLY 
7S}  MGLY 
76}  ISOP  +  0 

+  OH 
+  hv 

77}  ISOP  +  OH 

--> 

--> 
--> 
--> 

--> 

--> 

--> 

--> 

--> 

--> 
--> 

--> 

--> 
--> 

--> 
--> 
--> 

--> 

--> 
--> 

--> 
--> 
--> 

--> 

+  0.200*FORM  +  0.020*X02N 
+  0.200*0H 
+ 

X02 

+ 

ALD2 
J?AR 

+  0.740*FORM  +  0.330*CO 
+  0.220*X02 
+  0.100*0H 

+  o. 09o•:m2N  + 
+ 
+ 

PAR 
+  0.700*X02 
+  0.300*0H 
+  1.S60*FORM  + 

FORM 
N02 
co 

H02 

+  0.420*CO 
+  0.120*H02 
+  0.360*CRES  +  0.440*H02 

+  0.900*H02 

+  0.900*0PEN 

H02 
+ 
+  0.600*X02 

+ 

HN03 

+  0.600*H02 

+  0.200*CRES 
+  0.300*T02 
+  2.000*H02 

co 

+ 
+  0.700*FORM 
+  0.080*0H 

+  O.SOO*X02 
+  0.800*MGLY  +  1.100*PAR 

+ 

X02 
C203 
C203 

+  2.000*CO 
+ 
+ 

FORM 
H02 

0.030*ALD2  +  0.620*C203 

+  0.030*X02 
+  0.760*H02 
X02 
C203 

+  0.690*CO 
+  0.200*MGLY 
C203 
+ 
H02 
+ 

co 
+ 
+  0.2SO*X02 
+  0.2SO*PAR 
0.912*ISPD  +  0.629*FORM  +  0.991*X02 

0.7SO*ISPD  +  O.SOO*FORM 
+  0.2SO*C203 

+  0.2SO*H02 

78}  ISOP  +  03 

--> 

0.6SO*ISPD  +  0.600*FORM  +  0.200*X02 

+  0.912*H02 

+  0.088*X02N 

{  79}  ISOP  +  N03 

+  NO 
+  X02 

80}  X02 
81}  X02 
82}  X02N  +  NO 
+  OH 
83}  S02 
84}  S02 
8S}  X02 
+  H02 
86}  X02N  +  H02 
87}  X02N  +  X02N 
88}  X02N  +  X02 
89}  ISPD  +  OH 

--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

+  0.066*H02 
+  0.1SO*ALD2 

+  0 .266.*0H 
+  0.3SO*PAR 
0.200*ISPD  +  0.800*NTR 
+  0.200*N02 

+  0.800*H02 
+  2.400*PAR 
N02 

+  0.200*C203 
+  0.066*CO 
X02 
+ 
+  0.800*ALD2 

NTR 
SULF 
SULF 

+ 

H02 

1.S6S*PAR 
+  O.S03*H02 
+  0.273*ALD2  +  0.498*C203 

+  0.167*FORM  +  0.713*X02 
+  0 .. 334*CO 

+  0.168*MGLY 

{  90}  ISPD  +  03 

--> 

0.114*C203 

+  0.1SO*FORM  +  0.8SO*MGLY 


Table 8A-3.  CB4 Mechanism 

+  0.154*H02 
+  0.020*ALD2  +  0.360*PAR 

+  0.268*0H 

91}  ISPD  +  N03 

92}  ISPD  +  hv 

{  93}  ISOP  +  N02 

+  0.064*X02 
+  o.225•co 

--> 

--> 

+  0.925*H02 
+  0.075*C203  +  0.075*X02 

0.357*ALD2  +  0.282*FORM  +  1.282*PAR 
+  o.850*NTR 
+  0.075*HN03 
+  0.067*ALD2  +  0.900*FORM 
+  1. 033*H02 

+  0.643*CO 

+  0.700*X02 

0.333*CO 

+  0.832*PAR 
+  0.967*C203 

--> 

0.200*ISPD  +  0.800*NTR 

+  0.800*H02 
+  2.400*PAR 

+  0.200*NO 

+ 
+  0.800*ALD2 

X02 

~----------------------------------------------------------------------< 

i. 

Rate  Constant 
Rate  Expression 
a"••···············•"ii:::it1::1zl:z=z====·=====:z==============::==============:::==========;::sm::=== 
{O.OOOOOE+OO} 
k(  1)  uses  photo  table  N02_CBIV88 
{1.37387E-14} 
~(  2)  is  a  falloff  expression  using: 

,  scaled  by  1.00000E+OO 

I 

kO 
kinf  • 
F  •  0.60, 

~.OOOOE-34  *  (T/300)**(-2.30) 
~,0000E-12  •  (T/3oo)••(  o.oo) 

n 

s 

1.00 

k(  3)  • 
k(  4)  • 
k( 

1.ilOooE-12  •  exp(  -1370.0/Tl 
9.3000E-12 

5)  is  a  falloff  expression  using: 

k(  6)  is  a  falloff  expression  using: 

kO 
kinf  • 
F  •  0.60, 

kO 
kinf  • 
F  •  0.60, 

9.0000E-32  *  (T/300)**(-2.00) 
2.lOOOE-11  • 
(T/300)**(  (l.00) 

n  =  1.00 

9.0000E-32  *  (T/300)**(-1.50) 
3.0000E-11  *  (T/300)**(  0.00) 

n  •  1.00 

7)  • 

1.2000E-13  *  exp(  -2450.0/T) 

9)  uses  photo  table  0301D_CBIV88 

k( 
k(  8)  uses  photo  table  N02_CBIV88 
k( 
k(  10)  • 
k(  11)  • 
k(  12) 
k(  13)  • 
k(  14)  • 
k(  15)  uses  photo  table  N02_CBIV88 
1.3000E-11  *  exp( 
250.0/T) 
k(  16)  • 
k(  17)  • 
2.5000E-14  *  exp(  -1230.0/Tl 
k(  18)  is  a  falloff  expression  using: 

1.SOOOE-11  *  exp( 
3.2000E-11  *  exp( 
2.2000E-10 
1.6000E-12  *  exp( 
1.4000E-14  *  exp( 

-940.0/T) 
-580.0/T) 

107.0/T) 
67.0/T) 

,  scaled  by  5.30000E-02 
,  scaled  by  1.00000E+OO 

,  scaled  by  3.39000E+Ol 

kO 
kinf  • 
F  •  0.60, 

2.2000E-30  *  (T/300)**(-4.30) 
1.5000E-12  *  (T/300)**(-0.50) 

n  =  1.00 

1.3000E-21 

k(  19)  • 
k(  20)  •  k(  18)  I  Keq,  where  Keq  =  2.700E-27  *  exp(  11000.0/T) 
k(  21)  • 
~<  22)  • 
k(  23)  is  a  falloff  expression  using: 

3.3000E-39  *  exp( 
~.~oobi~4o 

530.0/T) 

kO 
kinf  • 
F  •  0.60, 

6.7000E-31  *  (T/300)**(-3.30) 
3.0000E-11  *  (T/300)**(-1.00) 

n  =  1.00 

k(  24)  uses  photo  table  N02_CBIV88 
k(  25)  • 
k(  26)  • 
k(  27)  is  a  falloff  expression  using: 

6.6000E-12 
1.0000E-20 

2.60btiE-30  *  (T/300)**(-3.20) 
2.4000E-11  *  (T/300)**(-1.30) 

kO 
kinf  • 
F  • 

0.60, 

n  •  1.00 

8-34 

,  scaled  by  1.97500E-Ol 

{1.81419E~14} 
{9.30000E-12} 
{1.57527E-12} 

{1.66375E-12} 

{3.22581E-17} 
{o.oooooE+oo} 
{o.oooooE+oo} 
{2.57757E-11} 
{4.00676E-11} 
{2.20000E-10} 
{6.82650E-14} 
{1.99920E-15} 
{O.OOOOOE+OO} 
{3.00805E-11} 
{4.03072E-16} 
{1.26440E-12} 

{1.30000E-21} 
{4.36029E-02} 
{1.95397E-38} 
{4.39999E-40} 
{6.69701E-12} 

{O.OOOOOE+OO} 
{6.60000E-12} 
{1.00000E-20} 
{1.14885E-11} 

Table 8A-3.  CB4 Mechanism 

EPA/600/R-991030 

k(  28)  is  a  special  rate  expression  of  the  form: 

(1  +  k3[M]/k2)},  where 

7.2000E-15  *  exp( 

k  = kO  +  {k3[M]  I 
ko 
785.0/T) 
k2  =  4.1000E-16  *  exp(  1440.0/Tl 
k3  =  1.9000E-33  *  exp( 
725.0/T) 
3.7000E-12  *exp( 

k(  29) 
240.0/T) 
k(  30)  is  a  falloff  expression  using: 

kO 
2.3000E-31  *  (T/300)**(-4.60) 
kinf  =  4.2000E-12  *  (T/300)**(  0.20) 
F  =  0.60, 

n  =  1.00 

k(  30)  I  Keq,  where  Keq  =  2.100E-27  *  exp(  10900.0/T) 
380.0/T) 
1.3000E-12  *  exp( 
5.9000E-14  *  exp(  1150.0/T) 
2.2000E-38  *  exp(  5800.0/T) 

uses  photo  table  HCHOmol_CBIV88.,  scaled  by  2.55000E-Ol 

3 .1000E-12  *  exp ( 
1.5000E-13  *  (1.0  +  0.6*Pressure) 
1. OOOOE-11 

-18·7. O/T) 

uses  photo  table  HCHOrad_CBIV88 
uses  photo  table  HCHOmol_CBIV88 
3.0000E-11  *exp(  -1550.0/T) 
6.3000E-16 
1.2000E-11  *  exp( 
7.0000E-12  *  exp( 
2.5000E-15 

-986.0/T) 
250. O/T) 

uses  photo  table  ALD_CBIV88 

,  scaled  by  l.OOOOOE+OO 
,  scaled  by  1.00000E+OO 

,  scaled  by  1.00000E+OO 

{1.47236E-13} 

{8.27883E-12} 
{1.48014E-12} 

{9.17943E-02} 
{4.65309E-12} 
{2.79783E-12} 
{6.23927E-30} 
{0.00000E+OO} 
{1.65514E-12} 
{2.40000E-13} 
{1. OOOOOE-11} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{1.65275E-13} 
{6.30000E-16} 
{4.38753E-13} 
{1.61972E-11} 
{2.50000E-15} 
{O.OOOOO!l:+OO} 
{1.90766E-11} 
{9.41356E-12} 
{4.23268E-04} 
{2.50000E-12} 
{6.50000E-12} 
{3.54242E-01} 
{8.10000E-13} 
· {2 .19325E+03} 
{ 1. 60000E+03} 
{1.50000E-11} 
{4.04572E-12} 
{2.82173E-11} 
{1.19778E-17} 
{7;70000E-15} 
{7.01080E-13} 
{7.94340E-12} 
{1. 89105E-18} 
{6.18715E-12} 
{8.10000E-12} 
{4.20000E+OO} 
{ 4 .10000E-11}. 
{2.20000E-11} 
{1.40000E-11} 
{2.50901E-11} 
{3.00000E-11} 
{O.OOOOOE+OO} 
{1.00858E-17} 
{1.70000E-11} 
{O.OOOOOE+OO} 
{3.60000E-11} 
{9.97368E-11} 
{1.28512E-:l7} 
{6.73819E-13} 
{8.10000E-12} 
{l.33359E-12} 

k(  31) 
k(  32) 
k(  33) 
k(  34) 
k(  35) 
k(  36) 
k(  37) 
k(  38) 
k(  39) 
k(  40) 
k(  41) 
k(  42) 
k(  43) 
k(  44) 
k(  45) 
k(  46) 
k(  47) 
k(  48) 
k(  49) 
k(  SO) 
k(  51) 
k(  52) 
k(  53) 
k(  54) 
k(  55) 
k(  56) 
k(  57) 
k(  58) 
k(  59) 
k(  60) 
k(  61) 
k(  62) 
k(  63) 
k(  64) 
k(  65) 
k(  66) 
k(  67) 
k(  68) 
k  (  69) 
k(  70) 
k(  71) 
k(  72) 
k(  73) 
k(  74) 
k( 
k( 
k( 
k( 
k( 
k( 
k( 

-180.0/T) 
3.4900E-11  *exp( 
380.0/T) 
2.6300E-12  *exp( 
2.0000E+16  *  exp(-13500.0/T) 
2.SOOOE-12 
6.5000E-12 
1.1000E+02  *  exp(  -1710.0/T) 
8.1000E-13 
1.0000E+15  *  exp(  -8000.0/T) 
1.6000E+03 
1.5000E-11 
1.2000E-11  *  exp( 
5.2000E-12  *  exp( 
1.4000E-14  *  exi;>( 
7.7000E-15 
l.OOOOE-11 
2.0000E-12 
1.3000E-14 
2.1000E-12 
8.1000E-12 
4.2000E+OO 
4.1000E-11 
2.2000E-11 
1.4000E-11 
1.7000E-11  *  exp( 
3.0000E-11 

-792.0/T) 
411. O/T) 
-2633.0/T) 
322.0/T) 

-324. O/T) 
504.0/T) 
-2105.0/T) 

*·exp( 
*  exp( 
*  exp( 
*  exp( 

116. O/T) 

c 

c 

uses  photo  table  HCHOrad_CBIV88 
-500.0/T) 

5.4000E-17  *  exp( 
1.7000E-11 

3.6000E-11 
2.5400E-11  *exp( 

75)-uses  photo  table  HCHOrad_CBIV88 
76) 
77) 
407.6/T) 
78)  =  7.8600E-15  *  exp(  -1912.0/T) 
79) 
-448.0/T) 
80) 
81) 

3.0300E-12  *  exp( 
8.1000E-12 
1. 7000E-14  *  exp (  1300. O/T) 

,  scaled  by  9.04000E+oo. 

,  scaled  by  9.64000E+OO 


Table 8A-3.  CB4 Mechanism 

160.0/T) 

8.lOOOE-12 
4.3900E-13  *  exp( 
1.3600E-06 
7.6700E-14  *  exp(  1300.0/Tl 
7.6700E-14  *  exp(  1300.0/T) 
1.7300E-14  *  exp(  1300.0/T) 
3.4SOOE-14  *exp(  1300.0/T) 
3.3606!;;-11 
7.llOOE-18 
1.ooooE-15 

{8.lOOOOE-12} 
k(  82)  • 
{7.51005E-13} 
k(  83)  • 
{1.36000E-06} 
k(  84)  • 
{6.01684E-12} 
k(  85)  • 
{6.01684E-12} 
k(  86)  • 
{1.35712E-12} 
k(  87)  • 
{2.70640E-12} 
k(  88)  • 
k(  89)  • 
{3.36000E-11} 
{7.llOOOE-18} 
k(  90)  • 
{1.oooooE-15} 
~(  91)  • 
k(  92)  uses  photo  table  ACROLEIN 
{o.oooooE:+oo} 
{1.49000E;19} 
k(  93)  • 
········--···==•=2========~================================================~=== 

1.4900E-19 

,  scaled  by  3.60000E-03 

'111111 

Table 8A-4.  CB4  AE Mechanism 

EP A/600/R-99/030 

l}  N02 

+ 
+ 

+ 
+ 
+ 

2.000*0H 
H02 
H02 
H02 
2.000*H02 
co 
OH 
HN03 
C203 
C203 
C203 
X02 
FORM 
N02 
H02 
PAN 
C203 
2.000*X02 
0.790*FORM 

+ 

+ 

+  0.790*0H 
X02 
0.870*X02 

+  O.l.10*ALD2 
1.100*ALD2 
- 2.l.OO*PAR 

8-37 

+ 

0 

[H20] 

[H20] 

+  0.890*0 

+  O.llO*NO 

NO 
N205 
2.000*HN03 

NO 
03 
N02 
NO 
N03 
N02 
N03 
0 
OlD 
0 
0 

2.000*0H 
H02 
OH 

0.890*N02 
2.000*N02 

+  hv 
+  [02] 
+  NO 
+  N02 
+  N02 
+  NO 
+  N02 
+  hv 
+  hv 
[N2] 
+ 
+  [02] 
+ 
+  OH 
+  H02 
+  hv 
+  NO 
+  N02 
+  N02 
+ 

Reaction  List 
>----------------------------------------------------------------------------------< 
{ 
{  2}  0 
{  3}  03 
{  4}  0 
s}  0 
{ 
{ 
6}  0 
{ 
7}  03 
{  8}  03 
{ 
9}  03 
{  10}  OlD 
{  11}  OlD 
{  12}  OlD 
{  13}  03 
{  14}  03 
{  15}  N03 
{  16}  N03 
{  17}  N03 
{  18}  N03 
{  19}  N205 
{  20}  N205 
{  21}  NO 
{  22}  NO 
{  23}  OH 
{  24}  HONO 
{  25}  HONO 
{  26}  HONO 
{  27}  OH 
{  28}  OH 
{  29}  H02 
{  30}  H02 
{  31}  PNA 
{  32}  PNA 
{  33}  H02 
{  34}  H02 
{  35}  H202 
{  36}  H202 
{  37}  co 
{  38}  FORM 
{  39}  FORM 
{  40}  FORM 
{  41}  FORM 
{  42}  FORM 
{  43}  ALD2 
{  44}  ALD2 
{  45}  ALD2 
{  46}  ALD2 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

N03 
2.000*N02 
2.000*HONO 
HONO 
OH 
N02 
NO 
HN03 
N03 
OH. 
PNA 
H02 
N02 
H202 
H202 

+  OH 
+  H02 
+  H02 
+  hv 
+  OH 
+  OH 
+  OH 
+  hv 
+  hv 
+  0 
+  N03 
+  0 
+  OH 
+  N03 
+  hv 

+  NO 
+  N02 
+  NO 
+  hv 
+  OH 
+  HONO 
+  N02 
+  HN03 
+  NO 
+  N02 

HN03 
+ 
+  2.000*H02 

+  [02] 
[H20] 
+ 

+ 

+ 

+ 

+ 

+ 

+ 

co 
co 

H02 
H02 
OH 

NO 

N02 

N02 

N02 

co 
co 

co 

N02 

N02 

+ 
+ 

+ 

+  [H20] 

47}  C203 

+  NO 

48}  C203 
49}  PAN 
so}  C203 
51}  C203 

52}  OH 
53}  PAR 

54}  ROR 

+  N02 

+  C203 
+  H02 

+  OH 

--> 

--> 
--> 
--> 
--> 

--> 
--> 

--> 

+ 

X02 

+ 

FORM 

N02 

+ 
+  2.000*FORM 
+  0.790*X02 

FORM 
+ 
+  O.l.30*X02N 
+  0.760*ROR 
+  0.960*X02 
+  0.040*X02N 

+  2.000*H02 
+  0.790*H02 

H02 
+ 
+  O.ll.O*H02 
- O.ll.O*PAR 
+  0.940*H02 
+  0.020*ROR 

Table 8A-4.  CB4_AE Mechanism 

,,1,1, 

{  ss}  ROR 
{  56}  ROR 
{  57}  OLE 

{  SB}  OLE 

59}  OLE 

+  N02 
+  0 

+  OH 

+  03 

60}  OLE 

+  N03 

{  Gl}  ETH 

+  0 

62}  ETH 

63}  ETH 
64}  TOL 

65}  T02 

{  66}  T02 
{  67}  CRES 

68}  CRES 
69}  CRO 
70}  XYL 

+  OH 

+  03 
+  OH 

+  NO 

+  OH 

+  N03 
+  N02 
+  OH 

71}  OPEN 

+  OH 

72}  OPEN 
73}  OPEN 

+  hv 
+  03 

74}  MGLY 
75}  MGLY 
76}  ISOP 

+  OH 
+  hv 
+  0 

77}  ISOP 

+  OH 

{  78}  ISOP 

+  03 

79}  ISOP 

+  N03 

{  80}  X02 
'{  81}  X02 
' {  82}  X02N 
{  83}  S02 
{  84}  S02 
{  es}  xo2 
(  86}  X02N 
{  87}  X02N 
{  88}  X02N 
{  89}  ISPD 

+  NO 
+  X02 
+  NO 
+  OH 

+  H02 
+  H02 
+  X02N 
+  X02 
+  OH 

(  90}  ISPD 

I 

11, 

+  03 

--> 
--> 
--> 

--> 

--> 

--> 

--> 

--> 

--> 
--> 

--> 

--> 
--> 

--> 
--> 
--> 

H02 
NTR 

0.630*ALD2 

+  0.300*CO 
+  0.220*PAR 
FORM 
H02 

+ 

O.SOO*ALD2 
+  0.440*H02 
PAR 
0.910*X02 
ALD2 
FORM 
+  1. 700*H02 
X02 

+ 

+  0.220*ALD2 
FORM 
0.080*X02 
+  0.560*T02 
0.900*N02 
+  O.lOO*NTR 
CRES 
0.400*CRO 

+  0.300*0PEN 

CRO 
NTR 
0.700*H02 
+  O.SOO*MGLY 
+ 

XYLAER 
X02 
C203 
C203 
0.030*ALD2 

+ 

--> 

--> 
--> 

+  0.030*X02 
+  0.760*H02 
X02 
C203 
0.7SO*ISPD 

+  0.2SO*H02 

--> 
--> 
--> 

--> 

0.912*ISPD 

+  0.380*H02 
+  0.200*FORM 
+  0.200*0H 
+ 

ALD2 
PAR 

+  0.740*FORM 
+  0.220*X02 

+  0.090*X02N 

PAR 
+  0.700*X02 
+  0.300*0H 
+  1.560*FORM 

+  0.420*CO 
+  0.360*CRES 
+ 
+  0.900*H02 

TOLAER 

H02 
+ 
+  0.600*X02 
+ 
+ 

CSLAER 
HNOJ 

+  0.280*X02 
+  0.020*X02N 

' 

+ 

X02 

+  0.3JO*CO 
+  O.lOO*OH 

+ 
+ 
+ 

+ 

FORM 
N02 
co 

H02 

+  0.120*H02 
+  0.440*H02 

.j.  0.900*0PEN 

+  0.600*H02 

+ 

CSLAER 

+  O.SOO*X02 
+  1.100*PAR 

+  0.200*CRES 
+  0.300*T02 

FORM 
H02 

+  2.000*CO 
+ 
+ 
+  0.620*C203 
+  0.690*CO 
+  0.200*MGLY 
C203 
+ 
+ 
H02 
+  O.SOO*FORM 
+  0.2SO*C203 
+  0.629*FORM 
+  0.088*X02N 
+  0.600*FORM 
+  0.266*0H 
+  0.3SO*PAR 
+  O.SOO*NTR 
+  0.200*N02 

+  2.000*H02 

co 

+ 
+  0.700*FORM 
+  O.OSO*OH 

co 
+ 
+  0.2SO*X02 
+  0.2SO*PAR 
+  0.991*X02 

+  0,200*X02 
+  0.20!J*C203 
+  0.066*CO 
+ 
X02 
+  0.800*ALD2 

+  0.912*H02 

0.6SO*ISPD 

+  0.066*H02 
+  0.1SO*ALD2 
0.200*ISPD 

+  0.800*H02 
+  2.400*PAR 
N02 

--> 

--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 

NTR 
SULF 
SULF 

+ 
+ 

H02 
SULAER 

+ 

SULAER 

1.565*PAR 
+  0.503*H02 
+  0.273*ALD2 
0.114*C203 

+  0.154*H02 

+  0.167*FORM 
+  0.334*CO 
+  0.498*C203 
.+  O. lSO*FORM 
+  0.268*0H 

+  0.713*X02 
+  0.168*MGLY 

+  0. SSO*MGLY 
+  0.064*X02 
+  0.360*PAR 
+  0.282*FORM 
+  0.643*CO 
+  0.075*X02 
+  0.067*ALD2 
+  1.033*H02 

+  0.225*CO 
+  1.282*PAR 
+  0.850*NTR 
+  0.075*HN03 
+  0.900*FORM 
+  0.700*X02 

+  0.800*NTR 
+  0.200*NO 

+ 
+  0.800*ALD2 

X02 

--> 

+  0.020*ALD2 
0.357*ALD2 

+  0.925*H02 
+  0.075*C203 

--> 

0.333*CO 

+  0.832*PAR 
+  0. 967*C203 
0.200*ISPD 

+  o.8oo*Ho2 
+  2.400*PAR 

Table 8A-4.  CB4  AE Mechanism 

91}  ISPD 

+  N03 

92}  ISPD 

+  hv 

" 

94}  TERP 
95}  TERP 
96}  TERP 

+  OH 
+  N03 
+  03 

93}  ISOP 

+  N02 

--> 

>------------------------------·----------------------------------------------------< 

--> 
--> 
--> 

TERPAER  + 
TERPAER  + 
TERPAER  + 

OH 
N03 
03 

Rate  Expression 

Rate  Constant 

==============aa============m=========================================a======~a 

,  scaled  by  1.00000E+OO 

{O.OOOOOE+oo} 
{1.37387E-14} 

{l.81419E-14} 
{9.30000E-12} 
{1.575211\:-12} 

{1.66375E-12} 

{3.22581E-17} 
{O.OOOOOE+OO} 
{o.oooooE+oo} 
{2.57757E-11} 
{4.00676E-11} 
{2.20000E-10} 
{6.82650E-14} 
{1.99920E-15} 
{O.OOOOOE+OO} 
{3.00805E-ll} 
{4.03072E-16} 
{1.26440E-12} 

{l.30000E-21} 
{4.36029E-08} 
{1.95397E-38} 
{4.39999E-40} 
{6.69701E-12} 

{O.OOOOOE+OO} 
{6.60000E-12} 
{l.OOOOOE-20} 
{l.14885E-11} 

k(  1)  uses  photo  table  N02_CBIV88 
k(  2)  is  a  falloff  expression  using: 

=  6.0000E-34  *  (T/300)**(-2.30) 
kO 
kinf  =  2.8000E-12  *  (T/300)**(  0.00) 
F  =  0.60, 
. 

n  =  1.00 

k(  3)  =  1.8000E-12  *  exp(  -1370.0/T) 
k(  4)  =  9.3000E-12 
k( 

5)  is  a  falloff  expression  using: 

=  9.0000E-32  *  (T/300)**(-2.00) 
kO 
kinf  =  2.2000E-11  *  (T/300)**(  0.00) 
F  =  0.60, 

n  =  1.00 

k( 

6)  is  a  falloff  expression  using: 

n  =  1.00 

=  9.0000E-3~  ~  (T/300)**(-1.50) 
kO 
kinf  =  3.0000E-11  *  (T/300)**(  0.00) 
F  =  0.60, 
7)  =  1.2000E-13  *  exp(  -2450.0/T) 
8)  uses  photo  table  N02_CBIV88 

k( 
k( 
k(  9)  uses  photo  table  03010  CBIV88 
k(  10)  =  l.8000E-ll  *  exp( 
k(  11)  =  3.2000E-11  *  exp( 
k(  12)  =  2.2000E-10 
. 
k(  13)  =  l.6000E-12  *  exp( 
k(  14) 
l.4oooE-14  *  exp( 
k(  15)  uses  photo  table  N02  CBIV88 
k(  16)  =  l.3000E-11  *  exp(-
250.0/T) 
k(  17)  =  2.5000E~l4  *  exp(  -1230.0/T) 
k(  18)  is  a  falloff  expression  using: 

-940.0/T) 
-580.0/Tl  · 

-107.0/T) 
67.0/T) 

c 

=  2.2000E-30  *  (T/300)**(-4.30) 
kO 
kinf  =  l.5000E-12  *  (T/300)**(-0.50) 
F  =  0.60, 

1.00 

n 

c 

,  scaled  by  5.30000E-02 
,  scaled  by  l.OOOOOE+OO 

,  scaled  by·3.39000E+Ol 

k(  19)  =  l.3000E-21 
k(  20)  = k(  18)  I  Keq,  where  Keq  =  2.700E-21  *  exp(  11000.0/T) 
k(  21)  =  3.3000E-39  *  exp( 
k(  22)  =  4.4000E-40 
k(  23)  is  a  falloff  expression  using: 

530.0/T) 

=  6.7000E-31  *  (T/300)**(-3.30) 
kO 
kinf  =  3.0000E-11  *  (T/300)**(-1.00) 
F  =  0.60,.  n  =  1.00 

k(  24)  us.es  photo  table  N02_CBIVB8 
k(  25)  =  6.6000E-12 
k(  26)  =  l.OOOOE~20 
k(  27)  is  a  falloff  expression  using: 

kO 

=  2.6000E-30  *  (T/300)**(-3.20) 

,  scaled  by  l.97500E-Ol 

Table 8A-4.  CB4  AE Mechanism 

I 

kinf  =  2.4000E~ll  *  (T/3p0l**(-l,30) 
F  =  0.60, 

n  =  1.00 

i 

k(  28)  is  a  special  rate  expression  of  the  form: 

k  =  kO  +  {k3 [Ml  ,I  (1  +  k3 (Ml  /k2)}.  where 
kO  =  7.2000E-1S  *  exp( 
78S.O/T) 
k2  =  4.lOOOE-lG  *exp(  1440,0/Tl 
k3  =  1.9000E-3J  *  exp( 
72S.O/Tl 
k(  29)  =  3.7000E-12  *exp( 
240.0/T) 
k(  30)  is  a  falloff  expression  using: 

=  2.3000E~31 *  (T/~,,00)**(-4.60) 
kO 
kinf  =  4.2000E~l2  *  (T/300)**(  0.20) 
F  =  0.60, 

n  =  1.00 

,  scal'ed  by  2.SSOOOE-01 

-986.0/T) 
2SO.O/T) 

,  scaled  by  l.OOOOOE+OO 

l.1000E+02  *  exp(  -1710.0/T) 

l.SOOOE-13  *  (1.0  +  0.6*Pressure) 

l.OOOOE+lS  *  exp(  -8000.0/T) 

,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 

k(  31)  = k(  30)  I  Keq,  where  Keq  =  2.lOOE-27  *  exp(  10900.0/T) 
k(  32)  = 
380.0/T) 
l.3000E-12  *  exp( 
k(  33)  =  S.9000E-14  *  exp( 
llSO.O/T) 
k(  34)  =  2.2000E-38  *  exp(  SBOO.O/T) 
k(  3S)  uses  photo  table  HCHOmol_CBIVBB 
k(  36)  =  3.lOOOE-12  *  exp( 
-187.0/T) 
k(  37)  = 
k(  38)  =  l.OOOOE-11 
k(  39)  uses  photo  table  HCHOrad_CBIVBB 
k(  40)  uses  photo  table  HCHOmol_CBIVBB 
k(  41)  =  3.0000E-11  *exp(  -lSSO.O/T) 
k(  42)  =  6.3000E-16 
k(  43)  =  1.2000E-11  *exp( 
k(  44)  =  7.0000E-12  *  exp( 
k(  4S)  =  2.SOOOE-lS 
k(  46)  uses  photo  table  ALD_CBIVBB 
k(  47)  =  3.4900E-ll  *  exp( 
-180.0/T) 
k(  48)  =  2.6300E-12  *  exp( 
380.0/T) 
k(  49)  =  2.0000E+l6  *  exp(-13SOO.O/Tl 
k(  SO)  =  2.SOOOE-12 
k(  Sl)  =  6.SOOOE-12 
k(  S2)  = 
k(  S3)  =  B.lOOOE-13 
k(  S4)  = 
k(  SS)  =  l.6000E+03 
k(  S6)  =  l.SOOOE-11 
k(  S7)  =  l.2000E-ll  *  exp( 
k(  SB)  =  S.2000E-12  *  exp( 
k(  S9)  = 
l.4000E-14  *  exp( 
k(  60)  =  7.7000E-1S 
k(  61)  = 
l.OOOOE-11  *  exp( 
k(  62)  =  2.0000E-12  *  exp( 
k(  63)  = 
l.3000E-14  *  exp( 
k(  64)  =  2.lOOOE-12  *  exp( 
k(  65)  =  B.lOOOE-12 
k(  66)  =  4.2000E+OO 
k(  67)  =  4.lOOOE-11 
k(  68)  =  2.2000E-ll 
k(  69)  =  l.4000E-ll 
k(  70)  = 
k(  71)  =  3.0000E-11 
k(  72)  uses  photo  table  HCHOrad_CBIVBB 
k(  73)  =  5.4000E-17  *  exp( 
-500.0/T) 
k(  74)  =  1.7000E-11 
k(  75)  uses  photo  table  HCHOrad_CBIVBB 
k(  76)  =  3.6000E-11 
k(  77)  =  2.5400E-11  *  exp( 
407.6/T) 
k(  78)  =  7.8600E-1S  *  exp(  -1912.0/Tl 
k(  79)  =  3.0300E-12  *exp( 
-448.0/T) 

-792.0/T) 
411. O/T) 
-2633.0/T) 
322.0/T) 

-324.0/T) 
504.0/Tl 
-2105.0/T) 

l.7000E-11  *  exp( 

116. O/T) 

,  scaled  by  9.04000E+OO 

,  scaled  by  9.64000E+OO 

{1.47236E-l.3} 

'II 

{B.27883E-l.2} 
{l..480l.4E-l.2} 

{9.l.7943E-02} 
{4.6S309E-12} 
{2. 79783E-12} 
{6.23927E-30} 
{O.OOOOOE+OO} 
{l.6SS14E-12} 
{2.40000E-13} 
{l..OOOOOE-11} 
{O.OOOOOE+oo} 
{O.OOOOOE+oo} 
{l.6S27SE-13} 
{6.30000E-16} 
{4.387S3E-13} 
{1.61972E-ll.} 
{2.SOOOOE-15} 
{O.OOOOOE+OO} 
{l.90766E-l.l} 
{9.4l.3S6E-12} 
{4.23268E-04} 
{2.SOOOOE-12} 
{6.SOOOOE-12} 
{3. S4242E-,Ol} 
{B.lOOOOE-13} 
{2.19325E+03} 
{ 1. 60000E+03} 
{l.SOOOOE-11} 
{ 4. 04S72E-12} 
{2.82173E-ll} 
{l.1977BB-17} 
{7.70000E-l.S} 
{7.0lOBOB-13} 
{7.94340E-l.2} 
{l.8910SE-l.8} 
{6.l.871SE-l.2} 
{B.lOOOOE-12} 
{4.20000E+OO} 
{4.lOOOOE-11} 
{2.20000E-ll} 
{l.40000E-ll} 
{2.S0901E-ll} 
{3.00000E-11} 
{O.OOOOOE+oo} 
{l.OOBSBE-17} 
{l..70000E-l.l} 
{O.OOOOOE+OO} 
{3.60000E-ll} 
{9.97368E-ll.} 
{1.28Sl2E-17} 
{6.73819E-13} 

,, 

' 

1

Table 8A-4.  CB4  AE Mechanism 

EPA/600/R-99/030 

l..3600E-06 

l.60.0/T) 

l..7000E-l.4  *exp(  1300. O/T) 

k(  80)  =  8.l.OOOE-l.2 
k  (  Bl.)  = 
k(  82)  =  8.l.OOOE-l.2 
k(  83)  =  4.3900E-l.3  *  exp( 
k(  84)  = 
k(  85)  =  7.6700E-l.4  *exp(  1300. O/T) 
k(  86)  =  7.6700E-l.4  *  exp( 
l.300.0/T) 
k(  87)  = 
l..7300E-l.4  *  exp(  1300.0/T) 
k(  88)  =  3.4500E-l.4  *  exp(  1300. 0/T) 
k(  89)  =  3  .3600E-l.l. 
k(  90)  =  7.l.l.OOE-l.8 
k(  9l.)  = 
l.. OOOOE-l.5 
k(  92)  uses  photo  table  ACROLEIN 
k(  93)  = 
l..4900E-l.9 
l..0700E-l.l.  *  exp( 
k(  94)  = 
549. O/T) 
k(  95)  =  3. 2300E-l.l.  *  exp ( 
-975.0/T) 
k(  96)  =  7.2900E-l.5  *  exp(  -l.136.0/T) 

{8.l.OOOOE-12} 
{l..33359E-l.2} 
{8.l.OOOOE-l.2} 
{7.5l.005E-l.3} 
{l..36000E-06} 
{6.0l.684E-l.2} 
{6.0l.684E-l.2} 
{l..357l.2E-l.2} 
{2.70640E-l.2} 
{3.36000E-l.l.} 
{7.l.l.OOOE-l.8} 
{l.OOOOOE-15} 
{0.00000E+OO} 
{l..49000E-l.9} 
{6.75269E-l.l.} 
{l..22539E-l.2} 
{l..6ll.25E-l.6} 

,  scaled  by  3.60000E-03 

8-41 

,''II' 

,:,1' 

,,'111, 

1
" 

1111!!

EP A/600/R-99/030 

Table 8A-5.  CB4_AQ Mechanism 

[H20] 

+ 

+ 

l}  N02 

--> 

,, 

1 

+ 

0 

--> 

--> 

+  0.890*0 

+  0.110*NO 

0.890*N02 
2.000*N02 

2 .. 000*0H 
H02 
OH 

NO 
N205 
2.000*HN03 

NO 
03 
N02 
NO 
N03 
N02 
N03 
0 
010 
0 
0 

+  hv 
+  [02] 
+  NO 
+  N02
+  N02, 
+  NO 
+  N02 
+  hv  I 
+  hv  I 
[N2] 
+ 
+  [02] 
[H29] 
+ 
+  OH 
+  H02 
+  hv  , 
+  NO 
, 
+  N02, 
+  N02 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
+  [02] 
+  [H20]  --> 
--> 

Reaction  List 
>----------------------------------------------------------------------< 
{ 
{  2}  o· 
{  3}  03 
{  4}  0 
{  5}  o 
{  6}  0 
{  7}  03 
{ 
0}  03 
{  9}  03 
{  10}  010 
{  11}  010 
{  12}  010 
{  13}  03 
{  14}  03 
{  15}  N03 
{  16}  N03 
{  17}  N03 
{  18}  N03 
{  19}  N205  + 
{  20}  N205 
+  NO 
{  21}  NO 
+  N02 
{  22}  NO 
+  NO 
{  23}  OH 
{  24}  HONO  +  hv 
{  25}  HONO  +  OH 
{  26}  HONO  +  HONp 
{  27}  OH 
{  28}  OH 
{  29}  H02 
{  30}  H02 
{  31}  PNA 
+  OH 
{  32}  PNA 
+  H02 
{  33}  H02 
+  H02 
{  34}  H02 
{  35}  H202  +  hV  I 
{  36}  H202  +OH, 
{  3 7}  co 
+OH  I 
{  38}  FORM  +  OH 
{  39}  FORM  +  hV  I 
{  40}  FORM  +  hv, 
{  41}  FORM  +  0 
{  42}  FORM  +  N03 
{  43}  AL02  +  0 
I 
{  44}  AL02  +OH, 
+  N03 
{  45}  ALD2 
{  46}  AL02  +  hV  I 

--> 
--> 
+  [H20]  --> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

N03 
2.000*N02 
2.000*HONO 
HONO 
OH 
N02 
NO 
HN03 
NOJ 
OH 
PNA 
H02 
N02 
H202 
H202 

+  N02 
+  HN03 
+  NO 
+  N02 

HN03 
+  2.000*H02 

--> 
--> 

--> 
--> 

--> 
--> 
--> 

+ 

+ 

+ 

+ 

H02 
H02 
OH 

N02 

N02 

NO 

N02 

N02 

N02 

--> 

--> 

co 
co 

co 

+ 
+ 

+ 

co 
co 

+ 
+ 

2. OOO*OH· 
H02 
H02 
H02 
2.000*H02 
co 
OH 
+ 
HN03 
+ 
C203  + 
C203 
C203  + 
X02 
FORM 
N02 
H02 
PAN 
C203  + 

+ 

·--> 

--> 

+ 

+ 

47}  C203  +NO.: 

,, 

48}  C203  +  N02. 
49}  PAN 
so}  c203  +  C203 
51}  C203  +  H02 

52}  OH 
53}  PAR 

+OH, 

--> 

--> 

--> 
--> 
--> 
--> 

--> 

X02 

+ 

FORM 

N02 

2.000*X02 
0.790*FORM  +  0.790*X02 

+  2.000*FORM  +  2.000*H02 
+  0.790*H02 

+  0.790*0H 
X02 
0.870*X02 

+  0-.110*AL02 

+  0.210*PACO 
H02 
+ 
+  0.130*X02N  +  0.110*H02 
·+  0. 760*ROR 
- 0.110*PAR 

FORM  + 

Table 8A-5.  CB4_AQ Mechanism 

EPA/600/R-99/030 

S4}  ROR 

SS}  ROR 
S6}  ROR 
S7}  OLE 

+  N02 
+  0 

SS}  OLE 

+  OH 

59}  OLE 

+  03 

60}  OLE 

+  N03 

61.}  ETH 

+  0 

62}  ETH 

+  OH 

63}  ETH 

+  03 

64)  TOL 

+  OH 

6S}  T02 

+  NO 

66}  T02 
67}  CRES 

+  OH 

68}  CRES 
69}  CRO 
70}  XYL 

+  N03 
+  N02 
+  OH 

71}  OPEN 

+  OH 

72}  OPEN  +  hv 
73}  OPEN  +  03 

74}  MGLY  +  OH 
+  hv 
7S}  MGLY 
76}  ISOP  +  0 

77}  ISOP  +  OH 

78}  ISOP  +  03 

{  79}  ISOP  +  N03 

+  NO 
+  X02 

so}  xo2 
81.}  X02 
82}  X02N  +  NO 
+  OH 
83}  S02 
84}  $02 
+  H02 
as}  xo2 
86}  X02N  +  H02 
87}  X02N  +  X02N 
88}  X02N  +  X02 
89}  ISPD  +  OH 

--> 

--> 
--> 
--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 
--> 

--> 
--> 
--> 

--> 

--> 
--> 

--> 
--> 
--> 

--> 

--> 

--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

l..l.OO*ALD2 
- 2.l.OO*PAR 
H02 
~TR 

+  0 .960*X02 
+  0.940*H02 
+  0.040*X02N.  +  0.020*ROR 

0.630*ALD2  +  0.380*H02 

+  0.280*X02 

+  0.300*CO 
+  0.220*PAR 

+ 

+  0.200*FORM  +  0.020*X02N 
+  0.200*0H 
FORM  + 
-
H02 
+  0.740*FORM  +  0.330*CO 
+  0.220*X02 
+  O.l.OO*OH 

ALD2 
PAR 

X02 

+ 

O.SOO*ALD2 
+  0.440*H02 
+  0.200*FACD  +  0.200*AACD  -
+  0.090*X02N  + 
-
PAR. 
+ 
+  0.700*X02 
+ 
+  0.300*0H 
+  l..S60*FORM  + 

0.9l.O*X02 
ALD2 
FORM 

+  l.. 700*H02 
X02 

+ 

PAR 
FORM 
N02 
co 

H02 

+  0.420*CO 

+  0.120*H02 

+  0.360*CRES  +  0.440*H02 

+  0.900*H02 

+  0.900*0PEN 

+  0.220*ALD2 
FORM 
+  0.400*FACD 

O.OSO*X02 
+  O.S60*T02 
0.900*N02 
+  0.100*NTR 
CRES 
0.400*CRO 

+  0.300*0PEN 

CRO 
NTR 
0.700*H02 

H02 
+ 
+  0.600*X02 

+ 

HN03 

+  0.600*H02 

+  0.200*CRES 
+  0.300*T02 
+  2.000*H02 

+  O.SOO*X02 
+  O.SOO*MGLY  +  l..l.OO*PAR 

+  2.000*CO 

+ 

X02 
C203  + 
C203 
+ 

FORM 
H02 

+ 

co 

0.030*ALD2  +  0.620*C203  +  0.700*FORM 

+  O.OSO*OH 

+  0.690*CO 
+  0.030*X02 
+  0.200*MGLY 
+  0.760*H02 
C203 
X02 
+ 
C203  + 
H02 

0.7SO*ISPD  +  O.SOO*FORM 

co 
+ 
+  0.2SO*X02 
+  0.2SO*C203  +  0.2SO*PAR 
0.9l.2*ISPD  +  0.629*FORM  +  0.99l.*X02 

+  0.2SO*H02 

+  0.912*H02 

+  0.088*X02N 

0. 6.SO*ISPD  +  0.600*FORM  +  0.200*X02 
+  0.200*C203 
+  0.066*CO 
X02 
+ 
+  O.SOO*ALD2 

+  0.066*H02 
+  0.1SO*ALD2  +  0,.3SO*PAR 
0.200*ISPD  +  O.SOO*NTR 
+  0.200*N02 

+  0.266*0H 

+  O.SOO*H02 
+  2.400*PAR 
N02 

NTR 
SULF  + 
S'OL.F 
UMHP 

H02 

l..56S*PAR 
+  O.S03*H02 
+  0.273*ALD2  +  0.498*C203 

+  O.l.67*FORM  +  0.7l.3*X02 
+  0.334*CO 

+  O.l.68*MGLY 


Table 8A-S.  CB4  AQ Mechanism 
{  90}  ISPD  +  03 

--> 

{  91}  ISPD  +  N03 

{  92}  ISPD  +  h'lf 

{  93}  ISOP  +  N02 

0.114*C203  +  0.150*FORM  +  0 .. 850*MGLY 

+  0.154*H02 
+  0.020*ALD2  +  0.360*PAR 

+  0.268*0H 

+  0.064*X02 
+  0.225*CO 

,,--> 

--> 

+  0.925*H02 
+  0.643*CO 
+  0.075*C203  +  0.075*X02 

0.357*ALD2  +  0.282*FORM  +  1. 282*PAR 
+  O.!l50*NTR 
+  0.075*HN03 
+  0.067*ALD2  +  0.900*FORM 
+  1.033*H02 

+  0.700*X02 

0.333*CO 

+  0.832*PAR 
+  0.967*C203 

--> 

0.200*ISPD  +  O.BOO*NTR 

+  O.BOO*H02 
+  2 AOO*PAR 

+  0.200*NO 

+ 
+  0.800*ALD2 

X02 

>-----------------~--------------------------------~-------------------< 

,  scaled  by  1.00000E+OO 

Rate  Expression 

I 

" 

I 

n  =  1.00 

n  =  1.00 

k( 
k( 

k( 
k( 
k( 

k( 

1)  uses  photo  table  N02_CBIV88 
2)  is  a  falloff  expression  using: 
=  6.0000E"'.34  *  (T/300)**(-2.~0) 
kO 
k~nf =  2.8000E~12  *  (T/~OO)**(  o.~Ol 
F  =  0.60, 
3)  =  1.BOOOE-12  *  exp(  -1370.0/T) 
4)  =  9.3000E-12 
5)  is  a  falloff  expression  using: 
=  9.0000E~32  *  (T/300)**(-2.00) 
kO 
kinf  =  2.2000E~ll  *  (T/300)**(  0.00) 
F  =  0.60, 
6)  is  a  falloff  expression  using:· 
=  9.0000E~32  *  (T/300)**(-1.SO~ 
kO 
kinf  =  3.0000E~ll  *  (T/300)**(  0.00) 
F  =  0.60, 
7)  = 
8)  uses  photo  table  N02_CBIV88 
9)  uses  photo  table  0301D_CBIVBB 

k( 
k( 
k( 
k(  10)  =  l.BOOOE-11  *  exp( 
k(  11)  =  3.2000E-11  •exp( 
k(  12)  =  2.2000E-10 
k(  13)  =  1.6000E-12  *exp( 
k (  14)  =  1. 4000E-14  *  exp ( 
k(  15)  uses  photo  table  N02_CBIV88 
k(  16)  =  1.3000E-ll  *  exp( 
250.0/T) 
k(  17)  =  2.5000E-14  *  exp(  -1230.0/T) 
k(  18)  is  a  falloff  expression  using: 

l.2000E-13  *  exp(  -2450.0/T) 

-940.0/T) 
-580. O/T) 

107.0/T) 
67.0/T) 

n  =  1.00 

I 

,  scaled  by  5.30000E-02 
,  scaled  by  1.00000E+OO 

,  scaled  by  3.39000E+Ol 

Rate  Constant 

{O.OOOOOE+oo} 
{1.37387E-14} 

{1.81419E-14} 
{9.30000E-12} 
{1.57527E-12} 

{l.6637SE-i

12} 

{3.22581E-17} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{2.57757E-11} 
{4.00676E"'.ll} 
{2.20000E-10} 
{6.826SOE-14}  " 
{1.99920E-15} 
{ 0. OOOOOE+OO}  " 
{3.00BOSE-11} 
{4.03072E-16} 
{1.26440E-12} 

11
1

11

:

· 

''I"' 
'1"11 
"" 

{l.30000E-21} 
{4.36029E-02} 
{l.95397E:38} 
{4.39999E.:.40} ::: 
{6 .69701E-12} .. 

{O.OOOOOE+oo} 
{6.60000E-12} 
{l.OOOOOE-20} 
{l.14885E-ll} 

=  2.2000E-3o  *  !T(300l**(-4.30l 
ko 
kinf  =  1.5000E-12  *  (T/300)**(-0.50) 
F  =  0.60, 

. 
n  =  1.00  . 

r·' 
k(  19)  =  1.3000E-21 
k(  20)  = k(  18)  I  Keq,  where  Keq  =  2.700E-27  *  exp(  11000.0/T) 
k(  21)  =  3.3000E-39  *exp( 
k(  22)  =  4.4000E-40 
k(  23)  is  a  falloff  expression  using: 

530.0/T) 

. 

=  6.7000~-31  *  (~/30d)**(-3.30) 
kO 
kinf  =  3.0000,-11  *  (T/300)**(-1.00) 
F  =  0.60, 

n  =  1.00 

k(  24)  uses  photo  table  N02_CBIVBB 
k(  25)  =  6.6000E-12 
k(  26)  =  1.0000E-20 
k(  27)  is  a  falloff  expression  using: 

ko 
=  2.6oOoE-30  *  <T/3oo)**(-3.2ol 
kinf  =  2.4000E:-11  *  (T/300)**(-1.30) 

' 

,  scaled  by  1.97500E-Ol 

Table 8A-5.  CB4_AQ Mechanism 

EPN600/R-99/030 

k(  28)  is  a  special  rate  expression  of  the  form: 

(1  +  k3[M]/k2l},  where 

n  =  1.00 

F  =  0.60, 
k  = ko  +  {k3[M] 
kO  =  7.2000E-15  *  exp( 
785.0/T) 
k2  =  4.1000E-16  *  exp(  1440.0/T) 
k3  =  1.9000E-33  *  exp( 
725.0/T) 
k(  29)  =  3.7000E-12  *  exp( 
240.0/T) 
k(  30)  is  a  falloff  expression  using: 

I 

=  2.3000E-31  *  (T/300)**(-4.60) 
kO 
kinf  =  4.2000E-12  *  (T/300)**(  0.20) 
F 

n  =  1.00 

0.60, 
= k(  30)  I  Keq,  where  Keq  =  2.100E-27  *  exp(  10900.0/T) 

{1.47236E-13} 

{8.27883E-12} 
{l.48014E-12} 

{9.17943E-02} 
{4.65309E-12} 
{2.79783E-12} 
{6.23927E-30} 
{O.OOOOOE+OO} 
{1.65514E-12} 
{2.40000E-13} 
{1.00000E-11} 
{O.OOOOOE+OO} 
{0.00000E+OO} 
{l.65275E-13} 
{6.30000E-16} 
{4.38753E-13} 
{l.61972E-ll} 
{2.5C°OOOE-15} 
{o.OOOOOE+OO} 
{1. 90766E-11} 
{9.41356E-12} 
{4.23268E-04} 
{2.SOOOOE-12} 
{6.SOOOOE-12} 
{3.54242E-Ol} 
{8.lOOOOE-13} 
{2.19325E+03} 
{ 1. 60000E+03} 
{1.SOOOOE-11} 
{4.04572E-12} 
{2.82173E-11} 
{1.19778E-l 7} 
{7.70000E-15} 
{7.01080E-13} 
{7.94340E-12} 
{1.89105E-18} 
{6.18715E-12} 
{8.10000E-12} 
{4.20000E+OO} 
{4.lOOOOE-11} 
{2.20000E-11} 
{1.40000E-11} 
{2.50901E-11} 
{3.00000E-11} 
{o.oooooE+oo} 
{1.00858E-17} 
{1. 70000E-11} 
{O.OOOOOE+OO} 
{3.60000E-ll} 
{9.97368E-11} 
{1.28512E-17} 
{6.73819E-13} 
{8.10000E-12} 

3.1000E-12  *  exp( 
l.5000E-13  *  (1.0  +  0.6*Pressure) 
1. OOOOE-11 

2.5000E-15 

-986.0/T) 
250.0/T) 

uses  photo  table  ALD_CBIV88 

uses  photo  table  HCHOrad_CBIV88 
uses  photo  table  HCHOmol  CBIV88 
3.0600E-11  *  exp(  -15SO.O/T) 
6.3000E-16 

-180.0/T) 
3.4900E-11  *  exp( 
380.0/T) 
2.6300E-12  *  exp( 
2.0000E+16  *  exp(-13500.0/T) 
2.5000E-12 
6.5000E-12 

1.3000E-12  *  exp( 
380.0/T) 
5.9000E-14  *  exp(  1150.0/T) 
2.2000E-38  *  exp(  5800.0/T) 
uses  photo  table  HCHOmol_CBIV88 
-187.0/T) 

c 
k(  31) 
k(  32) 
k(  33) 
k(  34) 
k(  35) 
k  (  36) 
k(  37)  = 
k(  38) 
k(  39) 
k(  40) 
k(  41) 
k(  42) 
k(  43)  =  1.2000E-11  *  exp( 
k(  44)  =  7.0000E-12  *  exp( 
k(  45) 
k(  46) 
k(  47) 
k(  48) 
k(  49) 
k(  50) 
k(  51) 
k(  52)  =  1.1000E+02  *  exp(  -1710.0/T) 
k(  53)  =  8.1000E-13 
1.0000E+15  *  exp(  -8000.0/T) 
k(  54) 
k(  55) 
1.6000E+03 
k(  56) 
1. 5000E-11 
-324.0/T) 
1.2000E-11  *  exp( 
k(  57) 
5.2000E-12  *  exp( 
504.0/T) 
k(  58) 
1.4000E-14  *  exp(  -2105.0/T) 
k(  59) 
7.7000E-15 
k(  60) 
k(  61) 
1.0000E-11 
2.0000E-12 
k(  62) 
1.3000E-14 
k(  63) 
k(  64) 
2.1000E-12 
k(  65) 
8.1000E-12 
4.2000E+OO 
k(  66) 
k(  67) 
4.1000E-11 
k(  68)  ~  2.2000E-11 
k(  69) 
1.4000E-11 
k(  70) 
1.7000E-11  *  exp( 
3.0000E-11 
k(  71) 
k(  72) 
k(  73) 
k(  74) 
k(  75)  uses  photo  table  HCHOrad_CBIV88 
k(  76)  =  3.6000E-11 
k(  77) 
k(  78) 
k(  79)  "' 
k(  80) 

2.5400E-11  *  exp( 
407.6/T) 
7.8600E-15  *  exp(  -1912.0/T) 
3.0300E-12  *exp( 
-448.0/T) 
8.1000E-12 

uses  photo  table  HCHOrad_CBIV88 
-500.0/T) 

5.4000E-17  *  exp( 
1. 7000E-11 

*  exp( 
*  exp( 
*  exp( 
*  exp( 

-792.0/T) 
411. O/T) 
-2633.0/T) 
322.0/T) 

116. O/T) 

,  scaled  by  2.55000E-01 

,  scaled  by  1.00000E+oo 
,  scaled  by  l.OOOOOE+OO 

,  scaled  by  l.OOOOOE+OO 

,  scaled  by  9.04000E+OO 

,  scaled  by  9.64000E+OO 

I 

160 .O/T) 

Table 8A-5.  CB4_AQ Mechanism 
k(  81)  =  1.7000E-14  *exp(  1300. O/T) 
k(  82)  =  8.lOOOE-12 
k(  83)  =  4.3900E-13  *  exp( 
..  1.3600E-06 
k(  84) 
k(  85)  =  7.6700E-14  *  exp(  1300. O/T) 
k(  86)  =  7.6700E-14  *  exp(  1300. O/T) 
k(  87)  =  1.7300E-14  *  exp(  1300.0/T) 
..  3.4500E-14  *exp(  1300.0/T) 
k(  88) 
..  7.llOOE-18 
k(  89)  =  3.3600E-1l 
k(  90) 
k(  91)  =  1. OOOOE-15 
k(  92)  uses  photo  table  ACROLEIN 
k(  93)  =  1.4900E-19 

"'"' 

{1.33359E-12} 
{8.lOOOOE-12} 
{7.51005E-13} 
{1.36000E-06} 
{6.01684E-12} 
{6.01684E-12} 
{1.35712E-12} 
{2.70640E-12} 
{3.36000E-1l} 
{7.llOOOE-18} 
{1. OOOOOE-15} 
{O.OOOOOE+OO} 
{1.49000E-19} 

,  scaled  by  3.~0000E-03 

==ma===m======•===~========~,~==~=====•=================2=================•••==• 

,, 
'11'1 

Table 8A-6.  CB4_AE_AQ Mechanism 

EP A/600/R-99/030 

1}  N02 

+ 

0 

+  0.890*0 

+  0.110*NO 

NO 
N2os· 
2.000*HN03 

3}  03 
4}  0 
s}  o 
6}  0 
7}  03 
8}  03 
9}  03 

NO 
03 
N02 
NO 
N03 
N02 
N03 
o 
010 
0 
0 

2.000*0H 
H02 
OH 

0.890*N02 
2.000*N02 

+  hv 
+  [02] 
+  NO 
+  N02 
+  N02 
+  NO 
+  N02 
+  hv 
+  hv 
+  [N2] 
+  [02] 
+  [H20] 
+  OH 
+  H02 
+  hv 
+  NO 
+  N02 
+  N02 
+ 

[H20] 

Reaction  List 
>--------------------------~-------------------------------------------------------< 
{ 
{  2}  0 
{ 
{ 
{ 
{ 
{ 
{ 
{ 
{  10}  010 
{  11}  010 
{  12}  010 
{  13}  03 
{  14}  03 
{  15}  N03 
{  16}  N03 
{  17}  N03 
{  18}  N03 
{  19}  N205 
{  20}  N205 
{  21}  NO 
{  22}  NO 
{  23}  OH 
{  24}  HONO 
{  25}  HONO 
{  26}  HONO 
{  27}  OH 
{  28}  OH 
{  29}  H02 
{  30}  H02 
{  31}  PNA 
{  32}  PNA 
{  33}  H02 
{  34}  H02 
{  35}  H202 
{  36}  H202 
{  37}  CO 
{  38}  FORM 
{  39}  FORM 
{  40}  FORM 
{  41}  FORM 
{  42}  FORM 
{  43}  ALD2 
{  44}  ALD2 
{  45}  ALD2 
{  46}  ALD2 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
·--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

N03 
2.000*N02 
2.000*HONO 
HONO 
OH 
N02 
NO 
HN03 
N03 
OH 
PNA 
H02 
N02 
H202 
H202 

+  OH 
+  H02 
+  H02 
+  hv 
+  OH 
+  OH 
+  OH 
+  hv 
+  hv 
+  0 
+  N03 
+  0 
+  OH 
+  N03 
+  hv 

+NO 
+  N02 
+NO 
+  hv 
+  OH 
+  HONO 
+  N02 
+  HN03 
+  NO 
+  N02 

2.000*0H 
H02 
H02 
H02 
2.000*H02 

+ 
HN03 
+  2.000*H02 

+  [02] 
+ 

[H20] 

+ 

+ 

+ 

+ 

+ 

+ 

CO 
CO 

H02 
H02 
OH 

NO 

N02 

N02 

N02 

N02 

N02 

CO 
CO 

CO 

+  [H20] 

47}  C203 

+  NO 

48}  C203 
49}  PAN 
so}  c203 
51}  C203 

52}  OH 
53}  PAR 

54}  ROR 

+  N02 

+  C203 
+  H02 

+  OH 

--> 

--> 
--> 
--> 
--> 

--> 
--> 

--> 

+ 
+ 

+ 
+ 
+ 

+ 
+ 

+ 

+ 

X02 

+ 

FORM 

N02 

+ 
+  2.000*FORM 
+  0.790*X02 
+  0.210*PACD 
FORM 
+ 
+  O.l30*X02N 
+  0.760*ROR 
+  0.960*X02 
+  0.040*X02N 

+  2.000*H02 
+  0.790*H02 

H02 
+ 
+  0.1lO*H02 
- O.llO*PAR 
+  0.940*H02 
+  0.020*ROR 

CO 
OH 
HN03 
C203 
C203 
C203 
X02 
FORM 
N02 
H02 
PAN 
C203 
2.000*X02 
0.790*FORM 

+ 

+ 

+  0.790*0H 
X02 
0.870*X02 

+  O.llO*ALD2 
1.lOO*AL02 
- 2.lOO*PAR 

Table 8A-6.  CB4_AE_AQ,Mechanism 

I 

SS}  ROR 
56}  ROR 
57}  OLE 

58}  OLE 

S9}  OLE 

60}  OLE 

61}  ETH 

62}  ETH 

63}  ETH 

64}  TOL 

6S}  T02 

66}  T02 
67}  CRES 

68}  CRES 
69}  CRO 
70}  XYL 

71}  OPEN 

72}  OPEN 
73}  OPEN 

74}  MGLY 
7S}  MGLY 
76}  ISOP 

I 

+  N92 
+  o, 

'I 

+  OH I 
+  o~ 

I 

+  N03 

+  o' 
I 
+  OH I 
+  03 

I 

+  Oi:i 

I 
+  NO 

+  Oi:i 

I 

+  N03 
+  N92 
+  Oi:i 

I 

+  01;1 

+  hv 
+  03 

+  OH 

+  hy 
+  0 

77}  ISOP 

+  OH 

78}  ISOP 

+  03 

80}  X02 
Bl}  X02 
82}  X02N 
83}  $02 
84}  $02 
BS}  X02 
86}  X02N 
87}  X02N 
88}  X02N 
89}  ISPD 

+  NO 
+  X02 
+  NO 
+  OJ:! 

I 

+  H02 
+  H02 
+  X02N 
+  x92 
+  OH 

{  90}  ISPD 

+  o~ 

{  79}  ISOP 

+  N93 

--> 

--> 
--> 
--> 

--> 

-~> 

H02 
NTR 

0.630*ALD2 

+  0.300*CO 
+  0.220*PAR 
FORM 
H02 

+ 

O.SOO*ALD2 
+  0.440*H02 
+  0.200*FACD 

--> 

--> 

--> 

--> 

--> 

--> 

--> 
--> 

--> 
--> 
--> 

+ 

0.9lO*X02 
ALD2 
FORM 
+  l.700*H02 
X02 

+  o:220*ALD2 
FORM 
+  0.400*FACD 

0.080*X02 
+  O.S60*T02 
0.900*N02 
+  0.100*NTR 
CRES 
0.400*CRO 

+  0.300*0PEN 

CRO 
NTR 
0,700*H02 

+  O.SOO*MGLY 
+ 

XYLAER 
X02 
C203 
C203 
0.030*ALD2 

+ 

--> 

--> 
--> 

+  0.030*X02 
+  0.760*H02 
X02 
C203 
0.7SO*ISPD 

+  0. 250*H02 

0.912*ISPD 

+  0.9l2*H02 

--> 
--> 
--> 

--> 

--> 

0.6SO*I~PD 

+  0.066*H02 
+  0.1!:>0*ALD2 
0.200*ISPD 

+  0.800*H02 
+  2.400*PAR 
N02 

NTR 
SULF 
SULF 
UMHP 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

l.S6S*PAR 
+  O.S03*H02 
+  0.273*ALD2 
-->  O.ll4*C203 

8-48 

+  0.380*H02 
+  0.200*FORM 
+  0.200*0H 
+ 

ALD2 
PAR 

+  0.740*FORM 
+  0.220*X02 
+  0.200*AACD 
+  0.090*X02N 

PAR 
+  0.700*X02 
+  0.300*0H 
+  1.560*FORM 

+  0.280*X02 
+  0.020*X02N 

+ 

X02 

+  0.330*CO 
+  O.lOO*OH 

+ 
+ 
+ 

+ 

PAR 
FORM 
N02 
co 

H02 

+  0.420*CO 

+  0. l20*H0,2 

+  0.360*CRES 
+ 
+  0.900*H02 

TOLAER 

+ 
H02 
+  0.600*X02 
+ 
+ 

CSLAER 
HN03 

+  0.440*H02 

+  0.900*0PEN 

+  0.600*H02 

+ 

CSLAER 

+  O.SOO*X02 
+  1.100*PAR 

+  0.200*CRES 
+  0.300*T02 

FORM 
H02 

+  2.000*CO 
+ 
+ 
+  0.620*C203 
+  0.690*CO 
+  0.200*MGLY 
C203 
+ 
+ 
H02 
+  0.500*FORM 
+  0.250*C203 
+  0.629*FORM 
+  0.088*X02N 
+  0.600*FORM 
+  0.266*0H 
+  0.3SO*PAR 
+  O.SQO*NTR 
+  0.200*N02 

+  2.000*H02 

co 

+ 
+  0.700*FORM 
+  O.OBO*OH 

co 
+ 
+  0.250*X02 
+  0.250*PAR 
+  0.991*X02 

+  0.200*X02 
+  0.200*C203 
+  0.066*CO 
x92 
+ 
+  0.800*ALD2 

+ 
+ 

H02 
SULAER 

+ 

SULAER 

+  0.167*FORM 
+  0.334*CO 
+  0.498*C203 
+  O.l50*FORM 

+  0.7l3*X02 
+  O.l.68*MGLY 

+  0.850*MGLY 

Table 8A-6.  CB4_AE_AQ Mech~ism. 

{  91}  ISPD 

+  N03 

--> 

+  0.154*H02 
+  0.020*ALD2 
0.357*ALD2 

+  0.925*H02 
+  0. 075*C203 

{  92}  ISPD 

+  hv 

--> 

0.333*CO 

{  93}  ISOP 

+  N02 

94}  TERP 
95}  TERP 
96}  TERP 

+  OH 
+  N03 
+  03 

--> 

--> 
--> 
--> 

+  O.B32*PAR 
+  0.967*C203 
0.200*ISPD 

+  0.800*H02 
+  2.400*PAR 

TERPAER  + 
TERPAER  + 
TERPAER  + 

OH 
N03 
03 

EPA/600/R-99/030 

+  0.26B*OH 
+  0.360*PAR 
+  0.2B2*FORM 
+  0.643*CO 
+  0.075*X02 
+  0.067*ALD2 
+  1. 033*H02 

+  0.064*X02 
+  0.225*CO 
+  l.2B2*PAR 
+  O.B50*NTR 
+  0.075*HN03 
+  0.900*FORM 
+  0.700*X02 

+  0.800*NTR 
+  0 .. 200*NO 

+ 
+  0.800*ALD2 

X02 

>--------------------------------------------------------------------------~-------< 

,  scaled  by  l.OOOOOE+OO 

Rate  Constant 

{O.OOOOOE+OO} 
{l.37387E-14} 

{l.81419E-14} 
{9.30000E-12} 
{1.57527E-12} 

{1.66375E-12} 

{3.22581E-17} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{2.57757E-11} 
{4.00676E-11} 
{2.20000E-10} 
{6.B2650E-14} 
{l.99920E-15} 
{O.OOOOOE+OO} 
{3.00805E-ll} 
{4.03072E-16} 
{l.26440E-12} 

{l.30000E-21} 
{4.36029E-02} 
{l.95397E-38} 
{4.39999E-40} 
{6.69701E-12} 

{o.oooooE+oo} 
{6.GOOOOE-12} 
{1.00000E-20} 
{1.14BB5E~ll} 

,  scaled  by  5.JOOOOE-02 
'  scaled  by  l.OOOOOE+OO 

,  scaled  by  3.39000E+Ol 

,  scaled  by  l.97500E-Ol 

8~49 

Rate  Expression 

1)  uses  photo  table  N02_CBIV8B 
2)  is  a  falloff  expression  using: 

n  =  1.00 

=  6.0000E-34  *  (T/300)**(-2.30) 
kO 
kinf  =  2.8000E-12  *  (T/300)**(  0.00) 
F  =  0.60, 
3)  = 
4)  =  9.3000E-12 
5)  is  a  falloff  expression  using: 

l.8000E-12  *  exp(  -1370.0/T) 

=  9.0000E-32  *  (T/300)**(-2.00) 
kO 
kinf  =  2.2000E-ll  *  (T/300)**(  0.00) 
F  =  0.60, 

n  =.  1.00 

k( 
k( 

k( 
k( 
k( 

k( 

6)  is  a  falloff  expression  using: 

n  =  1.00 

=  9.0000E-32  *  (T/300)**(-1.50) 
kO 
kinf  =  3.0000E-11  * (T/300)**(  0.00) 
F  =  0.60, 
7)  =  1.2000E-13  *exp(  -2450:0/T) 
8)  uses  photo  table  N02_CBIVBB 
9)  uses  photo  table  0301D_CBIVBB 

k( 
k( 
k( 
k(  10)  = 
l.BOOOE-11  *  exp( 
k(  11)  =  3.2000E-ll  *exp( 
k(  12)  =  2.2000E-10 
k( 
k( 
k( 
k( 
k( 
k( 

13)  = 
-940.0/T) 
14)  = 
-580.0/T) 
. 
15)  uses  photo  table  N02_CBIVBB 
16)  =  1.3000E-11  *  exp( 
250.0/T) 
17)  =  2.5000E-14  *  exp(  -1230.0/T) 
18)  is  a  falloff  expression  using: 
kO 
kinf  = 
F  =  0.60, 

=  2.2000E-30  *  (T/300)**(-4.30) 
l.5000E-12  *  (T/300)**(-0.50) 

l.6000E-12  *  exp( 
l.4000E-14  *  exp( 

107.0/T) 
67.0/T) 

n  =  1.00 

l.3000E-21 

k(  19)  = 
k(  20)  = k(  18)  I  Keq,  where  Keq  =  2.700E~27  *exp(  11000.0/T) 
k(  21)  =  3.3000E-39  *  exp( 
k(  22)  =  4.4000E-40 
k(  23)  is  a  falloff  expression  using: 

530.0/T) 

0.60, 

=  6.7000E-31  *  (T/300)**(-3.30) 
kO 
kinf  =  3.0000E-11  *  (T/300)**(-1.00) 
F  = 
k(  24) 
k(  25) 
k(  26) 
k(  27) 

is  a  falloff  expression  using: 

uses  photo  table  N02_CBIVB8 

6.6000E-12 
l.OOOOE-20 

n  =  1.00 

EPA/600/R-99/030 

Table 8A-6.  <;B4_AE_AQ Mechanism 

I 

= 

2.6000E-30  *  (T/300)**(-3.20) 
kO 
kinf  =  2.4000E-11  *  (T/300)**<-1.30) 
F  =  0.60, 

n  ~  1.00 

k(  28)  is  a  special  rate  expression  of  the  form: 

(1  +  k3[M]/k2)},  where 

/ 

k  =  kO  +  {k3[MJ 
kO  =  7.2000E-i5  *  exp( 
785.0/T) 
k2  =  4.1000E-l6  *  exp(  1440.0/T) 
k3  =  l.9000E-J3  *  exp( 
725.0/Tl 
k(  29)  =  3.7000E-12  *exp( 
k(  30) 

240.0/T) 
i.s  a  fallC)ff  expression  using: 
=  2.3ocici•-i1  *  .(T/300)**(-4.60) 
kO 
kinf  =  4.2000E-12  *  (T/300)**(  o.20J 
F  =  0.60, 

n  ~  '1.00  ,,,, .. ,,, 

l.3000E-12  *exp( 

l.OOOOE-11 

-986.0/T) 
250.0/T) 

,  scaled  by  l.OOOOOE+OO 

,  scaled  by  2.55000E-01 

,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 

k(  31)  =  k(  30)  I  Keq,  where  Keq  =  2.lOOE-27  *  exp(  10900.0/T) 
k(  32)  = 
380.0/T) 
k(  33)  =  5.9000E-14  *  exp(  1150.0/T) 
k(  34)  = 
2.2000E-38  *  exp(  5800.0/T) 
k(  35)  uses  photo  table  HCHOmol_CBIV88 
k(  36)  =  3.lOOOE-12  *exp( 
-187.0/T) 
k(  37)  =  l.5000E-13  *  (1.0  +  0.6*Pressure) 
k(  38)  = 
k(  39)  uses  photo  table  HCHOrad_CBIV88 
k(  40)  uses  photo  table  HCHOmol_CBIV88 
k(  41)  =  3.0000E-11  *exp(  -1550.0/T) 
k(  42)  =  6.3000E;l6 
k(  43)  =  l.2000E-ll  *  exp( 
k(  44)  =  7.0000E-12  *  exp( 
k(  45)  =  2.5000E-15 
k(  46)  uses  photo  table  ALD_CBIV88 
k(  47)  =  3.4900E-ll  *  exp( 
-180.0/T) 
k(  48)  =  2.6300E-12  *  exp( 
380.0/T) 
k(  49)  =  2.0000E+l6  *  exp(-13500.0/T) 
k(  50) 
k(  51) 
k(  52) 
k(  53) 
k(  54) 
k(  55) 
k(  56) 
k(  57) 
k(  58) 
k(  59) 
k(  60) 
k(  61) 
k(  62) 
k(  63) 
k(  64) 
k(  65) 
k(  66) 
k(  67) 
k(  68) 
k(  69) 
k(  70) 
k(  71) 
k  (  72) 
k(  73) 
k(  74) 
k(  75)  uses  photo  table  HCHOrad_CBIV88 
k(  76)  =  3.6000E-ll 
k(  77) 
k  (  78) 

2.5000E-12 
6.5000E-12 
l.lOOOE+02  *  exp(  -1710.0/T) 
8.lOOOE-13 
l.OOOOE+l5  *  exp(  -8000.0/T) 
l.6000E+03 
1. 5000E-ll 
-324. O/T) 
l.2000E-ll  *  exp( 
504.0/T) 
5.2000E-12  *  exp( 
l.4000E-14  *  exp(  -2105.0/T) 
7.7000E-15 
l.OOOOE-11  *  exp( 
2.0000E-12  *  exp( 
l.3000E-14  *  exp( 
2.lOOOE-12  *  exp( 
8.lOOOE-12 
4.2000E+OO 
4.lOOOE-11 
2.2000E-ll 
1.4000E-ll 
l.7000E-ll  *  exp( 
3.0000E-11 

uses  photo  table  HCHOrad_CBIV88 
-500.0/T) 

2.5400E-ll  *  exp( 
407.6/T) 
7.8600E-15  *  exp(  -1912.0/T) 

-792. O/T) 
411. O/T) 
-2633.0/T) 
322.0/T) 

5.4000E-17  *  exp( 
l.7000E-ll 

,  scaled  by  9.04000E+OO 

,  scaled  by  9.64000E+OO 

116.0/T) 

8-50 

,,,,11,,1111111, 

:111111 

{1.47236E-13} 

{8.27883E-12} 
{1.48014E~l2} 

{9.17943E-02} 
{4.65309E-12} 
{2.79783E-12} 
{6.23927E-30} 
{O.OOOOOE+OO} 
{1.65514E-12} 
{2.40000E-13} 
{1.00000E-11} 
{O.OOOOOE+oo} 
{O.OOOOOE+OO} 
{1. 6527SE-13} 
{6.30000E-16} 
{4.38753E-13} 
{1. 61972E-11} 
{2.50000E-15} 
{O.OOOOOE+OO} 
{1.90766E-11} 
{9.41356E-12} 
{4.23268E-04} 
{2.50000E-12} 
{6.50000E-12} 
{3.54242E-01}, 
{8.lOOOOE-13} 
{2.19325E+03} 
{l.60000E+03} 
{l.50000E-1J.} 
{4.04572E-12} 
{2.82173E-11} 
{1. 19778E-17}  "'' 
{7.70000E-15}•' 
{7.01080E-13} 
{7.94340E-12} 
{ 1. 89105E .. ::: .. 18} 
{6.18715E:.12}',;,  ., 
{8.lOOOOE-12} 
{4.20000E+oo} 
{4.l.OOOOE-11} 
{2. 20000E-11} HI· 
{1.40000E-11} 
{2.50901E-11} 
{3.00000E-J.J.} 
{O.OOOOOE+OO} 
{1.00858E-17} 
{1.70000E-11} 
{O.OOOOOE+OO} 
{3.60000E-l.l} 
{9.97368E-1J.} 
{1.28512E-17} 

Table 8A-6.  CB4_AE_AQ Mechanism 

EP A/600/R-99/030 

8.lOOOE-12 
l.7000E-14  *  exp(  1300.0/T) 

-448.0/T) 

160.0/T) 

k(  79)  =  3.0300E-12  *exp( 
{6.73819E-13} 
k(  80)  = 
{8.lOOOOE-12} 
k(  81)  = 
{1.33359E-12} 
k(  82)  =  8.lOOOE-12 
{8.lOOOOE-12} 
k(  83)  =  4.3900E-13  *  exp( 
{7.51005E-13} 
k(  84)  =  1.3600E-06 
{1.36000E-06} 
k(  85)  =  7.6700E-14  *  exp(  1300.0/T) 
{6.0l684E-12} 
k(  86)  =  7.6700E-14  *  exp(  13 00. O/T) 
{6.01684E-12} 
k(  87)  = 
l.7300E-14  *  exp(  1300. O/T) 
{1.35712E-12} 
k(  88)  =  3.4500E-14  *  exp(  1300. O/T) 
{2.70640E-12} 
k(  89)  =  3.3600E-ll 
{3.36000E-11} 
k(  90)  =  7.llOOE-18 
{7.llOOOE-18} 
k(  91)  =  1. OOOOE-15 
{l.OOOOOE-15} 
k(  92)  uses  photo  table  ACROLEIN 
{O.OOOOOE+OO} 
k(  93)  = 
l.4900E-19 
{l.49000E-19} 
l.0700E-ll  *  exp( 
k(  94)  = 
{6.75269E-11} 
k(  95)  =  3.2300E-ll  *exp( 
{1.22539E-12} 
{1.61125E-16} 
k(  96)  " 
=============================================================================== 

549 .O/T) 
-975.0/T) 
7.2900E-15  *exp(  -1136.0/T) 

,  scaled  by  3.60000E-03 

Table 8A-7.  RADM2 and RADM2_AQ Mechanisms 

• 

I 

9}  H202 

NO 

NO 
N02 
N02 

03P 

+ 

+ 
+ 
+ 

+ 

Reaction  List 
>----------------------------------------------------------------------< 
+  hv  ' 
1 }  N02 
{ 
+  hv 
2}  03 
{ 
+  hv 
3}  03 
{ 
4 }  HONO  +  hv 
{ 
{  s}  HN03  +  hv 
{  6)  HN04  +  hv 
+  hv 
{  7)  N03 
+  hv 
{  8)  N03 
{ 
+  hv 
{  10}  HCHO  +  hv 
+  hv 
{  11}  HCHO 
+  hv 
{  12}  ALO 
+  hv 
{  13}  OPl 
+  hv 
{  14)  OP2 
+  hv 
{  15}  PAA 
{  16}  KET 
+  hv 
+  hv 
{  17}  GLY 
+  hv 
{  18}  GLY 
+  hv 
{  19}  MGLY 
{  20}  DCB 
+  hv 
{  21)  ONIT  +  hv 

03 P 
OlD 
03P 
HO 
HO 
H02 
NO 
N02 
2.000*HO 
CO 
Ho'2 
M02 
HCHO 
ALD 
M02 
AC03 

- - > 
--> 
--> 
- - > 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

0.130*HCHO  +  l.870*CO 
0.450*HCHO  +  l.SSO*CO 
H02 

+  0.800*H02 
+ 
+ 
+ 

CO 
TC03 
H02 

+ 
+  0.020*AC03 
+  0.800*KET 

H02 
H02 
H02 
H02 
HO 
ETHP 

CO 
CO 
HO 
HO 

+ 
+ 
+ 
+ 
+ 
+ 

+ 
+ 
+ 
+ 

{  22}  03P 
'{  23}  03P 
{  24}  OlD 
{  25}  OlD 
{  26}  OlD 
{  27}  03 
{  28}  03 
{  29}  03 
{  30}  H02 
{  31}  H02 
(  32)  HN04 
{  33}  H02 
{  34)  H02 
{  35}  H202 
{  36}  NO 
{  37}  NO 
{  3 8}  03 
{  39}  N03 
{  40}  N03 
{  41}  N03 
{  42}  N03 
{  43)  N205 
{  44)  N205 
{  45}  HO 
{  46}  HO 
{  47}  HO 
{  48}  HO 
{  49}  HO 
{  so}  co 
{  51}  HO 
{  52}  ETH 
{  53}  HC3 

54}  HCS 
ss}  HC8 
56}  OL2 
57}  OLT 

[H20] 

+ 
[M] 
+  N02 
[N2J 
. + 
+  [02J 
+ 
+NO  ' 
+  HO 
+  H02 
+  NO 
+  N02, 

+  H02 
+  H02 
+  H<? 
+  HO 
+  NO 
+  N02, 
+NO  I 
+  N02
+  H02 
+  N02 

I 

1 

[H20] 

+ 
+  N02: 
+  HN03 
I 
+  HNOf 
+  H02 
I 
+  S02 
+  HO 

+  HO 
+  HO 

+  HO 
+  HO 
+  HO 
+  HO 

+  [02] 

+  [02] 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
+  [H20]  --> 
.--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 
--> 
--> 
--> 

+ 

AC03 
0.980*H02 
0.200*ALD 
N02 
03 
NO 
03P 
03P 

+ 

+ 

HO 

N02 

2.000*HO 
N02 
H02 
HO 
N02 
HN04 
H02 
H202 
li202 
H02 
HONO 
2.000*N02 
N03 
2.000*N02 

NO 
HN,03 
N205 
N02 

+ 

+ 

N02 

N03 

·2. OOO*HN03 
HN03 
N03 
N02 

+ 

H02 

SULF 
H02 
M02 
ETHP 
0.830*HC3P 

+  0.075*ALD 

+  0.1'70*H02 
+  0.025*KET 
HC5P  +  0.250*X02 
HC8P  +  0.7SO*X02 
OL2P 
OLTP 

8-52 

+  0.009*HCHO 

Table 8A-7.  RADM2 and RADM2_AQ Mechanisms 

EP A/600/R-99/030 

+  0.250*CSL 
+  0.170*CSL 
+  0.900*X02 

+  0.250*H02 
+  0.170*H02 
+  0.900*TC03 

+ 

co 

+  2.000*CO 
co 
+ 

+  0.500*HCHO 
+  0.500*ALD 

+  0.500*HO 
+  0.500*HO 

+ 

X02 

+ 
+ 

+ 

N03 
N02 

N02 

OLIP 
0.750*TOLP 
0.830*XYLP 
0.100*H02 
CSL 
H02 
AC03 
KETP 
H02 
AC03 
TC03 

0.500*M02 
0.500*HC3P 
AC03 
HCHO 
HC3P 
OLTP 
PAN 
AC03 
TPAN 
TC03 
HCHO 
0.750*ALD 

N02 
+ 
H02 
+ 
+  0.250*KET 
+  0.036*0NIT  +  0.964*N02 
+  0.690*KET 
+  0.920*H02 
+  1.060*KET 
+  0.240*0NIT  +  0.760*N02-
H02 

0.380*ALD 
+  0.920*N02 
0.350*ALD 

1.600*HCHO  + 

N02 

+ 
+  0.090*HCHO 
+  0.964*H02 
+  O.OSO*ONIT 

+  0.040*HCHO 
+  0.760*H02 
+ 
N02 

+ 

+  0.200*ALD 
ALD 
N02 
H02 
+  0.100*KET 
M02 
N02 

+ 

HCHO 

+ 

H02 

+  1.450*ALD 
+ 
N02 
+ 
N02 
+  0.920*H02 

+  0.280*HCHO 

+  0.890*GLY 

+  0.110*MGLY  +  0.050*AC03  +  0.950*CO 
+  2.000*X02 
N02 
+  0.160*GLY 
N02 
+  0.806*DCB 
.  ALD 

H02 
+ 
+  0.700*DCB 
+ 
H02 

+  0.170*MGLY 

+  0.450*MGLY 

H02 
N02 
ALD 
HN03 
HN03 
H02 
AC03 
TC03 
XN02 

N02 
+ 
H02 
+ 
+  2.000*N02 
co 
+ 

+  2.000*CO 
co 
+ 

+  0.500*CSL 

+  HO 
{  58}  OLI 
+  HO 
{  59}  TOL 
+  HO 
{  60}  XYL 
+  HO 
{  61}  CSL 
+  HO 
{  62}  CSL 
+  HO 
{  63}  HCHO 
+  HO 
{  64}  ALD 
+  HO 
{  65}  KET 
+  HO 
{  66}  GLY 
{  67}  MGLY  +  HO 
+  HO 
{  68}  DCB 
+  HO 
{  69}  OP1 
+  HO 
{  70}  OP2 
+  HO 
{  71}  PAA 
+  HO 
{  72}  PAN 
{  73}  ONIT  +  HO 
+  HO 
{  74}  ISO 
{  75}  AC03 
+  N02 
{  76}  PAN 
{  77}  TC03 
{  78}  TPAN 
+  NO 
{  79}  M02 
{  so}  HC3P  +  NO 

+  N02 

81}  HC5P  +  NO 

82}  HCSP  +  NO 

83}  OL2P  +  NO 

84}  OLTP 

+  NO 

85}  OLIP  .+NO 

86}  AC03 
+  NO 
87}  TC03  +  NO 

88}  TOLP  +  NO 

89}  XYLP  +  NO 

{  90}  ETHP 
{  91}  KETP 
{  92}  OLN 
{  93}  HCHO 
{  94}  ALD 
{  95}  GLY 
{  96}  MGLY 
{  97}  DCB 
{  98}  CSL 
{  99}  OL2 
{100}  OLT 
{101}  OLI 
{102}  ISO 
{103}  OL2 

+  NO 
+  NO 
+  NO 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  03 

{104}  OLT 

+  03 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 

--> 

--> 

--> 

--> 

--> 
--> 

--> 

--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 

+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 

MGLY 
HCHO 
H02 
AC03 
HN03 
HN03 
HN03 
HN03 
OLN 
OLN 
OLN 
OLN 
HCHO 
+  0.120*H02 

0.530*HCHO 
+  0.200*0RA1 
+  0.220*M02 

8"."53 

{105}  OLI 

+  03 

--> 

0.180*HCHO 

+  0.400*0RA1 

+  0.420*CO 

+  0,. 500*ALD 
+  0.200*0RA2 
.f.  0.100*HO 
+  0.720*ALD 

+  0.330*CO 
+  0.230*H02 

+  0.100*KET 

Table 8A-7.  RADM2 and RADM2  AQ Mechanisms 

{106}  ISO 

+  03 

{107}  H02 
{108}  H02 
{109}  H02 
{110}  H02 
{111}  H02 
{112}  H02 
{113}  H02 
{114}  H02 
{115}  H02 
{116}  H02 
{117}  H02 
{118}  H02 
{119}  H02 
{120}  H02 
{121}  M02 
{122}  M02 
{123}  M02 

+  M02 
+  ETHP 
+  HC3P 
+  HC5P 
+  HC8J? 
+  OL21\' 
+  OLTP 
+  OLII\' 
+  KETP 
+  ACO~ 
+  TOLP 
+  XYLJ? 
+  TCO~ 
+  OLN 1 
+  M02, 
+ETH!? 
+  HC31;' 

I 

{124}  M02 

+  HCSP 

{125}  M02 

+  HC8lf' 

{126}  M02 
{127}  M02 
{128}  M02 

{129}  M02 
{130}  M02 

+  OL2Ji' 
+·  OLTJi' 
+  OLI!f' 

+  KETP 
+  ACOf 

{131}  M02 

+  TOL!f' 

{132}  M02 

+  XYL!f' 

{133}  M02 

+  TC03 

{134}  M02 

+  OLN, 

{135}  ETHP 

+  AC03 

{136}  HC3P 

+  AC03 

{137}  HCSP 

+  ACO~ 

{138}  HC8P  +  AC03 

{139}  OL2P  +  AC03 

{140}  OLTP  +  AC03 

{141}  OLIP 

+  AC0;3 

{142}  KETP 

+  ACO~ 

{143}  AC03 
{144}  AC03 

I 
+  AC0;3 
+  TOLP 

i 

--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 

--> 

--> 
--> 
--> 

--> 
--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 
--> 

+  0.230*CO 
+  0.260*H02 

0.5~0*HCHO 
+  0.200*0RA1 
+  0.22°0*M02 
OPl 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
PAA 
OP2 
OP2 
OP2 
ONIT 
1.500*HCHO 
0.750*HCHO 
0.840*HCHO 

+  0.060*0RA1 
+  0.140*HO 
+  0.500*ALD 
+  0.200*0RA2 
+  O.lOO*HO 

+  0.290*0RA2 
+  0.310*M02 
+  0.330*CO 
+  0.230*H02 

+ 
H02 
+ 
H02 
+  0.770*ALD 

+  0.750*ALD 
+  0.260*KET 

H02 

0.770*HCHO  +  0.410*ALD 

+  0.7SO*KET 

H02 

0.800*HCHO  +  0.460*ALD 

+  1.390*KET 

+ 

+ 

+ 

H02 

1.550*HCHO  +  0.3SO*ALD 
1.250*HCHO  +  0.7SO*ALD 
0.890*HCHO  +  0.725*ALD 

+ 
+ 
+ 

H02 
H02 
H02 

+  O.SSO*KET 

0.750*HCHO 
HCHO 
+  0.500*0RA2 
HCHO 
+  0.700*DCB 
HCHO 
+  2.000*H02 

0 ... 590*HCHO 
+  O.SOO*ORA2 
+  0.475*CO 

1. 750*HCHO 

+ 

N02 
ALD 

+  0.750*MGLY 
+  0.500*H02 

H02 
+ 
+  O.SOO*M02 

+  0.170*MGLY 
+  2.000*H02 
+  0.45o*l'1GLY 

+  0.160*GLY 

+  0.806*DCB 

+  0.445*GLY 
+  0.025*AC03 
X02 
+ 
+  0.500*H02 

+  0. 055*MGLY 
+  0.460*H02 

+ 

ALD 

+  0.500*H02 

+  O.SOO*M02 

+  O.SOO*ORA2 
0.770*ALD 
+  0.500*M02 
0.410*ALD 
+  0.500*M02 
0.460*ALD 
+  0.500*M02 

+  0.260*KET 
+  O.SOO*ORA2 
+  0.750*KET 
+  O.SOO*ORA2 
+  1.390*KET 
+  O.SOO*ORA2 
O.BOO*HCHO  +  0.600*ALD 

+  0.500*H02 

+  O.SOO*H02 

+  O.SOO*H02 

+  0.500*H02 

+  0.500*M02 
ALD 
+  O.SOO*M02 
0.725*ALD 
+  0.500*H02 

+  0.500*0RA2 
+  O.SOO*HCHO  +  O.SOO*H02 
+  O.SOO*ORA2 
+  0.550*KET 
+  0.500*M02 
MGLY  +  0.500*H02 

+  0.140*HCHO 
+  O.SOO*ORA2 
+  O.SOO*M02 

+  0.500*0RA2 

2.000*M02 
M02 

+  0.170*MGLY 

+  0.160*GLY 

8-54 

Table 8A-7.  RADM2 and RADM2_AQ Mechanisms 

{145}  AC03 

+  XYLP 

{146}  AC03 

+  TC03 

{147}  AC03 

+  OLN 

+  0.700"DCB 
M02 
H02 
M02 

+ 

+ 
+  0.450*MGLY  +  0.806*DCB 

H02 

+  0.920*H02 

+  0.890*GLY 

+  0 .110•'MGLY  +  0.05Q*AC03  +  0.950*CO 
+  2. OOO•'X02 

+ 

HCHO  + 
N02 

ALD 
+  0.500*M02 
2. OOO'•HCHO  +  2.000*ALD 

+  0.500*0RA2 

--> 
--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

+  2.000*N02 

OP2 
HCHO  + 
M02 

{148}  OLN 
{149}  X02 
{150}  X02 
{151}  X02 
{152}  X02 
{153}  X02 
{154}  XN02 
{155}  XN02 
{156}  XN02 
{157}  XN02 
{158}  XN02 
>----------------------------------------------------------------------< 

+  OLN 
+  H02 
+  M02 
+  AC03 
+  X02 
+  NO 
+  N02 
+  H02 
+  M02 
+  AC03 
+  XN02 

N02 
ONIT 
OP2 
HCHO  + 
M02 

H02 

H02 

'  scaled  by  1.00000E+OO 
,  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  l.OOOOOE+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  l.OOOOOE+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  1·. OOOOOE+OO 
'  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  l.OOOOOE+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  l.OOOOOE+OO 
,  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 

Rate  Expression 
Rate  Constant 
====c=======c====================c============================================= 
k(  1)  uses  photo  table  N02_RADM88 
{O.OOOOOE+OO} 
k(  2)  uses  photo  table  0301D_RADM88 
{O.OOOOOE+OO} 
k( 
3)  uses  photo  table  0303P_RADM88 
{O.OOOOOE+OO} 
k(  4)  uses  photo  table  HONO_RADM88 
{O.OOOOOE+OO} 
k( 
5)  uses  photo  table  HN03_RADM88 
{O.OOOOOE+OO} 
6)  uses  photo  table  HN04_RADM88 
k( 
{O.OOOOOE+oo} 
7)  uses  photo  table  N03NO_RADM88 
k( 
{O.OOOOOE+OO} 
k( 
8)  uses  photo  table  N03N02_RADM88 
{O.OOOOOE+OO} 
k( 
9)  uses  photo  table  H202_RADM88 
{O.OOOOOE+oo} 
k(  10)  uses  photo  table  HCHOmol_RADM88 
{O.OOOOOE+OO} 
k(  11)  uses  photo  table  HCHOrad_RADM88 
{O.OOOOOE+OO} 
k(  12)  uses  photo  table  ALD_RADM88 
{O.OOOOOE+OO} 
k(  13)  uses  photo  table  MHP_RADM88 
{O.OOOOOE+OO} 
k(  14)  uses  photo  table  HOP_RADMBB 
{O.OOOOOE+OO} 
k(  15)  uses  photo  table  PAA_RADM88 
{O.OOOOOE+oo} 
k(  16)  uses  photo  table  KETONE_RADM88 
{O.OOOOOE+OO} 
k(  17)  uses  photo  table  GLYform_RADM88 
{O.OOOOOE+OO} 
k(  18)  uses  photo  table  GLYmol_RADMBB 
{O.OOOOOE+OO} 
k(  19)  uses  photo  table  MGLY_RADMBB 
{O.OOOOOE+OO} 
k(  20)  uses  photo  table  UDC_RADM88 
{O.OOOOOE+OO} 
k(  21)  uses  photo  table  ORGNIT_RADM88 
{O.OOOOOE+OO} 
k(  22)  =  6.0000E-34  *  (T/300)**(-2.30) 
{6.09302E-34} 
120.0/T) 
k(  23)  =  6.5000E-12  *  exp( 
{9.72293E-12} 
k(  24)  =  1.8000E-11  *  exp( 
110.0/T) 
{2.60365E-11} 
k(  25)  =  3.2000E-11  *exp( 
70.0/T) 
{4.04730E-11} 
k(  26)  =  2.2000E-10 
{2.20000E-10} 
k(  27)  =  2.0000E-12  *  exp(  -1400.0/T) 
{ 1. 82272E-14} 
k(  28)  = 
l.6000E-12  *  exp( 
-940.0/T) 
{6.82650E-14} 
k(  29)  =  1.1000E-14  *  exp( 
-500.0/T) 
{2.05452E-15} 
k(  30)  =  3.7000E-12  *exp( 
{8.27883E-12} 
240.0/T) 
k(  31)  is  a  falloff  expression  using: 
{1.39058E-12} 

=  1. BOOOE-31  *  (T/300)**(-3.20) 
kO 
kinf  =  4.7000E-12  *  (T/300)**(-l.40) 
F  =  0.60, 

n  =  1. 00 

k(  32)  =  k(  31)  I  Keq,  where  Keq  =  2.lOOE-27  *  exp(  10900.0/T) 
k(  33)  is  a  special  rate  expression  of  the  form: 

{8.62399E-02} 
{3.01634E-12} 

k  =kl+  k2[MJ,  where 

Table 8A-7.  RADM2 and RADM2_AQ Mechani~ms 

' I 
I 

kl  =  2.2000E-13  *  exp( 
l.9000E-33  *  exp( 
k2  = 

I 

620.0/T) 
980.0/T) 

k(  34)  is  a  special  rate  expression  of  the  form: 

{6.7890SE-30j 

k  =kl+  k2[Ml;  where 
kl  =  3.0BOOE-J4  *  exp(  2820.0/T) 
k2  =  2.6600E-S4  *  exp(  3180.0/Tl 
k(  3S)  =  3.3000E-12  *exp( 
-200.0/T) 
k(  36)  is  a  falloff  expression  using: 

=  7.ooooE-31  *  !T/300)**(-2.60) 
l.SOOOE-11  *  (T/300)**(-0.SO) 

kO 
kinf  = 
F  =  0.60, 

I 

n  ~  1.00 

k(  37)  =  3.3000E-39  *  exp( 
l.4000E-13  *  exp( 
k(  38)  = 
k(  39)  = 
l.7000E-ll  *  exp( 
k(  40)  =  2.SOOOE-14  *  exp( 
k(  41)  = 
k(  42)  is  a  falloff  expression  using: 

S30. O/T) 
-2SOO.O/T) 
lSO.O/T) 
-1230.0/T) 

2.SOOOE-12 

=  2.2000E-3o  *  !T/300)**(-4.30) 
l.SOOOE-12  *  (T/300)**(-0.SO) 

kO 
kinf  = 
F  =  0.60, 

I 

n  ~  1.00 

k(  43)  =  k(  42)  I  Keq,  where  Keq  = 
k(  44)  =  2.0000E-21 
k(  4S)  is  a  falloff  expression  using: 

kO 
=  2.6000E-30  *  (T/300)**(-3.20) 
kinf  =  2.4000E-11  *  !T/300)**(-1.30) 
F  =  0.60, 

n  =  1.00 

l.lOOE-27  *  exp(  11200.0/T) 

k(  46)  is  a  special  rate  expression  of  the  form: 

k  = kO  +  {k3[M]  I 
kO  =  7.2000E-is  *exp( 
7BS.O/T) 
k2  =  4.1000E-l6  *  exp(  1440.0/T) 
k3  = 
72S.O/T) 

l.9000E-33  *  exp( 

(1  +  k3[M]/k2J},  where 

380.0/T) 
k(  47)  = 
l.3000E-12  *  exp( 
230.0/T) 
k(  48)  =  4.6000E-ll  *  exp( 
k(  49)  is  a  falloff  expression  using: 

=  3.00QOE-31  *  (T/300)**(-3.30) 
l.SOOOE-12  *  (T/300)**(  0.00) 

kO 
kinf  = 
F  =  0.60, 

n  =  1.00 

l.SOOOE-13  *  (1.0  +  0.6*Pressure) 

-S40.0/T) 
-380.0/T) 
-380.0/T) 
411.0/T) 
S04.0/T) 
549.0/T) 
322.0/T) 
116.0/T) 

k(  SO)  = 
k(  Sl)  =  2.8300E+Ol  *  (T/300)**(  2.00)  *exp(  -1280.0/T) 
k(  S2)  =  1.2330E-12  *  (T/300)**(  2.00)  *exp( 
-444.0/T) 
l.S900E-ll  *  exp( 
k(  S3)  = 
k(  S4)  = 
l.7300E-ll  *  exp( 
k(  SS)  =  3.6400E-ll  *  exp( 
k(  S6)  =  2.lSOOE-12  *  exp( 
k(  S7)  =  S.3200E-12  *  exp( 
k(  SB)  =  1.0700E-ll  *exp( 
k(  S9)  =  2.lOOOE-12  *  exp( 
k(  60)  =  l.8900E-ll  *  exp( 
k(  61)  =  4.0000E-11 
k(  62)  =  9.0000E:-01  *  k(  61) 
k(  63)  =  9.0000E-12 
k(  64)  =  6.8700E-12  *exp( 
l.2000E-ll  *  exp( 
k(  6S)  = 
k(  66)  = 
l.lSOOE-11 
k(  67)  = 
l.7000E:-ll 
k(  68)  =  2.BOOOE-11 
l.OOOOE-11 
k(  69)  = 
k(  70)  =· 
l.OOOOE-11 
k(  71)  = 
l.OOOOE-11 
k(  72)  =  6.16SOE-13  *  (T/300)**(  2.00)  *exp( 
k(  73)  = 

2S6.0/T) 
-74S.O/Tl 

l.SSOOE-11  *  exp( 

-S40.0/T) 

-444.0/T) 

8-56 

{l.68671E-12} 
{4.87144E-12} 

{l.95397E::_38}. 
{3.18213E-17} 
{2.Bl225E-ll} 
{4.03072E-16} 
{2.SOOOOE-12} 
{1.26440E-12} 

{5.47034E-02} 
{2.00000E-21} 
{l.1488SE-ll} 

{1.47236E-l3} 

{4.65309E-12} 
{9.95294E-ll} 
{ 8. BB848E-13}  ·:: 

{2.40000E-13} 
{3.80672E-Ol} 
{2.74210E-13} 
{2.S9669E-12} 
{4.83334E-12} 
{l.01696E-ll} 
{8.53916E-12} 
{2.BB684E-ll} 
{6.75269E-ll} 
{6.1871SE-12} 
{2.78943E-ll}. 
{ 4. OOOOOE-11} .... 
{3.60000E-ll} 
{9.00000E-12} 
{1. 62197E-ll} 
{9.8S020E-13} 
{1. lSOOOE-11} 
{l.70000E-ll} 
{2.BOOOOE-11} 
{1. OOOOOE-11} 
{l.OOOOOE-11} 
{l. OOOOOE-11} 
{l.3710SE-13} 
{2.53137E-12};. 

Table 8A-7.  RADM2 and RADM2_AQ Mechanisms 
k(  74)  =  2.5500E-ll  *  exp( 
409,0/T) 
k(  75)  =  2.8000E-12  *  exp( 
181.0/Tl 
k(  76)  =  l.9500E+l6  *  exp(-13543.0/T) 
k(  77)  =  4.7000E-12 
k(  78)  =  l.9500E+l6  *  exp(-13543.0/T) 
k(  79)  =  4.2000E-12  *  exp( 
180.0/Tl 
k(  80)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  81)  =  4.2000E-12  *  exp( 
180.0/Tl 
k(  82)  =  4.2000E-12  *  exp( 
180.0/T) 
180.0/T) 
k(  83)  =  4.2000E-12  *  exp( 
k(  84)  =  4.2000E-12  *  exp( 
180.0/Tl 
k(  85)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  86)  =  4.2000E·12  *  exp( 
180. O/'l') 
k(  87)  =  4.2000E-12  *  exp( 
180. O/'l') 
k(  88)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  89)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  90)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  91)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  92)  =  4.2000E-12  *  exp( 
180. O/T) 
k(  93)  =  6.0000E-13  *  exp(  -2058.0/Tl 
k(  94)  =  1.4000E-12  *  exp(  -1900.0/T) 
k(  95)  =  6.0000E-13  *  exp(  -2058.0/T) 
k(  96)  =  1.4000E-12  *  exp(  -1900.0/T) 
k(  97)  =  l.4000E-12  *  exp(  -1900.0/Tl 
k(  98)  =  2.2000E-ll 
k(  99)  =  2.0000E-12  *  exp(  -2923.0/T) 
k(lOO)  = 
l.OOOOE-11  *  exp(  -1895.0/T) 
k(lOl)  =  3.2300E·ll  *  exp( 
-975.0/T) 
k(l02)  =  5.8100E-13 
k(103)  =  1.2000E-14  *exp(  -2633.0/T) 
k  (104)  =  l.3200E-14  *  exp(  -2105.0/T) 
k(105)  =  7.2900E-15  *  exp(  -1136.0/T) 
k(106)  =  l.2300E-14  *exp(  -2013.0/T) 
k(l07)  =  7.7000E-14  *  exp(  1300. O/T) 
k(108)  =  7.7000E-14  *  exp(  1300.0/T) 
k(l09)  =  7.7000E-14  *exp(  1300.0/T) 
k (110)  =  7.7000E-14  *  exp(  1300.0/T) 
k(lll)  =  7.7000E-14  *  exp(  1300.0/T) 
k (112)  =  7.7000E-14  *  exp(  1300. O/T) 
k(113)  =  7.7000E-14  *  exp(  1300. O/T) 
k(114)  =  7.7000E-14  *  exp(  1300. O/T) 
k(115)  =  7.7000E-14  *  exp(  1300. O/T) 
k(ll6)  =  7.7000E-14  *  exp(  1300.0/T) 
k(ll7)  =  7.7000E-14  *  exp(  1300.0/T) 
k (118)  =  7.7000E-14  *  exp(  1300.0/T) 
k(119)  =  7.7000E-14  *  exp(  1300. O/T) 
k  (120)  =  7.7000E-14  *  exp(  1300. O/T) 
k  (121)  =  1.9000E-13  *  exp( 
220.0/T) 
k  (122)  =  l.4000E-13  *  exp( 
220.0/T) 
k(l23)  =  4.2000E-14  *  exp( 
220.0/Tl 
k  (124)  =  3.4000E-14  *  exp( 
220.0/T) 
k (125)  =  2.9000E-14  *  exp( 
220,0/T) 
k (126)  = 
220.0/T) 
l.4000E-13  *  exp( 
k (127)  = 
220.0/T) 
l.4000E-13  *  exp( 
k (128)  =  1.7000E-14  *  exp( 
220.0/T) 
k(129)  =  1.7000E-14  *  exp( 
220.0/T) 
k  (130)  =  9.6000E-13  *  exp( 
220.0/T) 
l.7000E-14  *  exp( 
k  (131)  = 
220.0/T) 
k  (132)  = 
l.7000E-14  *  exp( 
220. 0/'l'l 
k(133)  =  9.6000E-13  *  exp( 
220.0/T) 
k(134)  =  l.7000E-14  *  exp( 
220. O/T) 

8-57 

{1.00601E-10} 
{5.13974E-12} 
{3.57235E-04} 
{4.70000E-12} 
{3.57235E-04} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{6.01030E-16} 
{2.38307E-15} 
{6.01030E-16} 
{2.38307E-15} 
{2.38307E-15} 
{2.20000E-11} 
{1.09940E-16} 
{ 1. 73099E-14} 
{1.22539E-12} 
{5.81000E-13} 
{1. 74559E-18} 
{1.12933E-17} 
{l.61125E-16} 
{l.43295E-17} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12}. 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{3.97533E-13} 
{2.92919E-13} 
{8.78758E-14} 
{7.11376E-14} 
{6.06762E-14} 
{2.92919E-13} 
{2.92919E-13} 
{3.55688E-14} 
{3.55688E-14} 
{2.00859E-12} 
{3.55688E-14} 
{3.55688E-14} 
{2.00859E-12} 
{3.55688E-14} 

',II"  'II 

11 

II 

I 

Table 8A-7.  RADM2 and RADM2_AQ Mechanisms 
k(l.35)  =  3.4000E-l.3  *  exp( 
k(l.36)  = 
l..OOOOE·l.3  *  exp( 
k(l.37)  =  8.4000E-l.4  *  exp( 
k (l.38)  =  7.2000E-l.4  *  exp( 
k(l.39)  =  3.4000E-l.3  *exp( 
k(l.40)  =  3.4000E-l.3  *  exp( 
k(l.4l.)  =  4.2000E-l.4  *  exp( 
k(l.42)  =  4.2000E-l.4  *  exp( 
l..l.900E-l.2  *  exp( 
k(l.43)  = 
k(l.44)  =  4.2000E-l.4  *  exp( 
k(l.45)  =  4.2000E-l.4  *  exp( 
k(l.46)  = 
l..l.900E-l.2  *  exp( 
k(l.47)  =  4.2000E-l.4  *  exp( 
k(l.48)  =  3.6000E-l.6  *  exp( 
k(l.49)  =  7.7000E-l.4  *  exp{ 
l..7000E-l.4  *  exp( 
k(l.50)  = 
k(l.5l.)  =  4.2000E-l.4  *  exp( 
k(l.52)  =  3.6000E-l.6  *  exp( 
k(l.53)  =  4.2000E-l.2  *  exp( 
k(l.54)  =  4.2000E-l.2  *  exp( 
k(l.55)  =  7.7000E-l.4  *  exp( 
k(l.56)  = 
l.7000E-l.4  *  exp( 
k(l.57)  =  4.2000E-l.4  *  exp( 
k(l.58)  =  3.6000E-l.6  *exp( 
=================:======~=.~1.~~=:;================='.===•======.=====m•m:•i=f.,,~m=•mm=csi,~r:· ,11, ,  

{7.l.l.376E-l.3} 
{2.09228E-l.3}  '' 
{ l.. 75752E-l.3} 
{ J.. 50644E.:.J.3}  11 
{7 .l.l.376E·l.3}  !' 
{7.l.l.376E-l.3} 
{8.78758E~l.4} 
{8.78758E-l.4} 
{2.4898l.E-l.2} 
{ 8. 78758E·l.4}  ... 
{8.78758E·l.4} 
{2.4898l.E-l.2} 
{8.78758E-l.4} 
{7.5322l.E-l.6} 
{6.04038E-l.2} 
{3.55688E-l.4} 
{8.78758E-14} 
{7. 5322l.E·l.6}  1 'II'" 
{7.68378E-l.2} 
{7.68378E-l.2} 
{6.04038E-l.2}  11111 
{3.55688E-l.4} 
{8.78758E-14} 
{7.53221E-16} 

220.0/T) 
220.0/T) 
220.0/T) 
220.0/T) 
220.0/T) 
220.0/T) 
220.0/T) 
220. O/T) 
220.0/T) 
220.0/T) 
220.0/T) 
220.0/T) 
220.0/T) 
220.0/T) 
l.300. O/T) 
220.0/T) 
220.0/T) 
220.0/T) 
l.80. O/T) 
180.0/T) 
l.300. O/T) 
220.0/T) 
220. O/T) 
220.0/T) 

1 'il 

'',,;11 

I 

I 

I 

Table 8A-8.  RADM2_AE and RADM2_AE_AQ Mechanisms 

Reaction  List 
>-----------------.-----------------------------------------------------------------< 

+ 

NO 

1)  N02 
2)  03 
3}  03 
4}  HONO 
S}  HN03 
6}  HN04 
7)  N03 
8)  N03 
9)  H202 
10)  HCHO 
11)  HCHO 
12)  ALD 
13}  OP1 
14)  OP2 
15)  PAA 
16)  KET 
17}  GLY 
18}  GLY 
19)  MGLY 
20)  DCB 
21)  ONIT 

+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 

"+ 

(02] 

+  [H20] 

+ 

(02] 

22)  03P 
23)  03P 
24)  01D 
25)  01D 
26}  01D 
27}  03 
28)  03 
29)  03 
30)  H02 
31)  H02 
32)  HN04 
33}  H02 
34)  H02 
35}  H202 
36)  NO 
37)  NO 
38)  03 
39)  N03 
40)  N03 
41}  N03 
42)  N03 
43)  N205 
44)  N205 
45)  HO 
46)  HO 
47)  HO 
48)  HO 
49)  HO 
so}  co 
51)  HO 
52)  ETH 
53)  HC3 

54)  HCS 
ss}  HC8 
56)  OL2 
57)  OLT 

[H20] 

+ 
[M] 
+  N02 
+  [N2] 
+  (02] 
+ 
+  NO 
+  HO 
+  H02 
+  NO 
+  N02 

+  H02 
+  H02 
+  HO 
+  HO 
+  NO 
+  N02 
+  NO 
+  N02 
+  H02 
+  N02 

[H20] 

+ 
+  N02 
+  HN03 
+  HN04 
+  H02 
+  802 
+  HO 

+  HO 
+  HO 

+  HO 
+  HO 
+  HO 
+  HO 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 
--> 
--> 
--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

+ 

03P 
01D 
03P 
HO 
HO 
H02 
NO 
N02 
2.000*HO 
co 
H02 
M02 
HCHO 
ALD 
M02 
AC03 
0.130*HCHO 
0.4SO*HCHO 
AC03 
0.980*H02 
0.200*ALD 
N02 
03 
NO 
03P 
03P 

2.000*HO 
N02 
H02 
HO 
N02 
HN04 
H02 
H202 
H202 
H02 
HONO 

2.000*N02 
N03 
2.000*N02 

NO 
HN03 
N205 
N02 

2.000*HN03 
HN03 
N03 
N02 

SULF 
H02 
M02 
ETHP 
0.830*HC3P 

+  0.075*ALD 
HCSP 
HC8P 
OL2P 
OLTP 

8.:.59 

+ 
+ 
+ 

+ 

NO 
N02 
N02 

03P 

H02 
H02 
H02 
H02 
HO 
ETHP 

+ 
+ 
+ 
+ 
+ 
+ 
+  1:870*CO 
+  1.SSO*CO 
+ 
H02 
+  0.020*AC03 
+  O.BOO*KET 

+ 
+ 
+ 
+ 

co 
co 
HO 
HO 

+  0.800*H02 
co 
+ 
+ 
TC03 
+ 
H02 

+ 

+ 

+ 

+ 

+ 

HO 

N02 

N02 

N03 

H02 

+ 

SULAER 

+  0.170*H02 
+  0.02S*KET 
+  0.2SO*X02 
+  0.750*X02 

+  0.009*HCHO 

+ 

HC8AER 

Table 8A-8.  RADM2  AE and RADM2  AE  AQ Mechanisms 

' 

·~ 

-

I 
I 

-

--> 
--> 

CLIP 
O. 750*'.l"OLP 

.+ 
+  0.250*CSL 

OLIAER 

+  0.250*H02 

,,·111111 

11·,1111 

TOLAER 

0.830*XYLP 

XYLAER 

+  O.l.70*CSL 

+  O.l.70*HQ2 

O.l.OO*H02 

+  0.900*X02 

+  0.900*TC<;l3 

{  58}  OLI 
{  59}  TOL 

60}  XYL 

+  HO 
+  HO 

+  HO 

61.}  CSL 

+  Hp 

{  62}  CSL 
{  63}  HCHO 
{  64}  ALD 
{  65}  KET 
{  66}  GLY 
{  67}  MGLY 
{  68}  DCB 
{  69}  OPl. 
{  70}  OP2 
{  71}  PAA 
{  72}  ?AN 
{  73}  ONIT 
{  74}  ISO 
{  75}  AC03 
{  76}  PAN 
{  77}  TC03 
{  78}  TPAN 
{  79}  M02 
{  so}  HC3P 

{  Bl.}  HC5P 

+  HO 
+,HO 
+  HO 
+  HP 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  Hp 
+  ~p 
+  Hp 
+  N02 

I 

+  Np2 

+  NO 
+  NO 

+  NO 

82}  HCBP 

+  NO 

83}  OL2P 

84}  OLTP 

+  NO 

' 

+  ~o 

85}  OLIP 

+  l~O 

86}  AC03 
87}  TC03 

' 

+  ~o 
+  NO 

88}  TOLP 

89}  XYLP 

{  90}  ETHP 
{  91.}  KETP 
{  92}  OLN 
{  93}  HCHO 
{  94}  ALD 
{  95}  GLY 
{  96}  MGLY 
{  97}  DCB 
{  98}  CSL 

{  99}  OL2 
{l.00}  OLT 
{l.Ol.}  OLI 
{l.02}  ISO 
{l.03}  OL2 

t 

l\iO 

+  l\iO 

+  N:O 
+  l\iO 
+  l\iO 
+  l'!03 
+  N:03 
+  N:03 
+  ~03 
+  ~03 
+  l'!03 

,. 

+  l'!03 
+  l'f03 
+  1i03 
+  1'!'03 
+  q3 

+ 

+ 

+ 

--> 

--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

CSLAER 
CSL 
H02 
AC03 
KETP 
H02 
1).C03 
TC03 

0.500*M02 
0.500*HC3P 
AC03 
HCHO 
Hc:JP 
P:LTP 
J?llN 
AC03 
'TPAN 
TC03 
HCHO 
0.750*ALD 

+  0.036*0NIT 

--> 

--> 

0.380*ALD 
+  0.920*N02 
0.350*ALD 

+  0.240*0NIT 
l.. 600*HCHO 
+  0.20.0~ALD 
ALD 
N02 
H02 
.+  O.l.OO*KET 
!>102 
N02 

.+ 

--> 

-->. 

--> 

-->. 
--> 

. +  0. l.l.O*MGLY 

~~> 

--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 
--> 
--> 
-.-> 
.--> 

+  2.000*X02 
N,02 
+  O.l.60*GLY 
N02 
+  0.806*DCB 
ALD 
MGLY 
HCHO 
H02 
AC,P3 
HN03 
HN03 
HN03 
HN03 

+  0.500*CSLAER 

OLN 
OLN 
OLN 
OLN 
HCHO 
+  O.l.20*H02 

8-60 

+ 

co 

+  2.000*CO 
co 
+ 

+  0.500*HCHO 
+  0.500*ALD 

+  O.SOO*HO 
+  O.SOO*HO 

+ 
+ 

+ 

N03 
N02 

N02 

N02 
+ 
+ 
H02 
+  0.250*KET 
+  0.964*N02 
+  0.690*KET 
+  0.920*H02 
+  l.. 060*KET 
+  0.760*N02 
+ 
H02 

+ 

X02 
,,, 

:1': 
'""':1 

N02 

+ 
+  0.090*HCHO 
+  0.964*H02 
+  0.080*0NIT 

+  0.040*HCHO 
+  0.760*H02 
+ 
NQ2 

+ 

HCHO 

+ 

H02 

+  0.2110·~£,!"0 

+  0.890*GLY 
+  0.950*CO 

+  O.l.?O*f1<;,:;LY 

+  0.450*MGLY 

+ 
N02 
+ 
H02 
+  2.000*N02 
co 
+ 

+  2.000*CO 
co 
+ 

+  0.500*CSL 

+  l..450*ALD 
+ 
N,P2 
+ 
N02 
+  0.920*H02 
+  0.050*AC03 

+ 
H02 
+  0.700*DCB 
+ 
H02 

+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 

+ 

H02 
N02 
ALD 
HN03 
HN03 
H02 
AC03 
TC03 
XN02 

OLIAER 

+  0.400*0RAl. 

+  0.420*CO 

'"I' 

:111i,11:1:;.' 

,11 

;1 

'',111, 

Table 8A-8.  RADM2_AE and RADM2~AE_AQ Mechanisms 

EP N600/R-99/030 

{104}  OLT 

+  03 

{105}  OLI 

+  03 

{106}  ISO 

+  03 

{107}  H02 
{108}  H02 
{109}  H02 
{110}  H02 
{111}  H02 
{112}  H02 
{113}  H02 
{114}  H02 
{115}  H02 
{116}  H02 
{117}  H02 
{118}  H02 
{119}  H02 
{120}  H02 
{121}  M02 
{122}  M02 
{123}  M02 

+  M02 
+  ETHP 
+  HC3P 
+  HC5P 
+  HC8P 
+  OL2P 
+  OLTP 
+  OLIP 
+  KETP 
+  AC03 
+  TOLP 
+  XYLP 
+  TC03 
+  OLN 
+  M02 
+  ETHP 
+  HC3P 

{124}  M02 

+  HCSP 

{125}  M02 

+  HC8P 

{126}  M02 
{127}  M02 
{128}  M02 

{129}  M02 
{130}  M02 

+  OL2P 
+  OLTP 
+  OLIP 

+  KETP 
+  AC03 

{131}  M02 

+  TOLP 

.  {132}  M02 

+  XYLP 

{133}  M02 

+  TC03 

{134}  M02 

+  OLN 

{135}  ETHP 

+  AC03 

{136}  HC3P 

+  AC03 

{137}  HC5P 

+  AC03 

{138}  HC8P 

+  AC03 

{139}  OL2P 

+  AC03 

{140}  OLTP 

+  AC03 

{141}  OLIP 

+  AC03 

+ 

+ 

+ 

H02 

0.770*HCHO 

+  0.410*ALP 

+  0.750*KET 

H02 

0,800*HCHO 

+  0.460*ALD 

+  1.390*KET 

H02 

+  O.SOO*ALD 
+  0,200*0RA2 
+  0.lOO•HO 
+  0.720*ALD 
+  0.060*0RA1 
+  0.140*HO 

+  0,500*ALD 
+  0.200*0RA2 
+  O.lOO*HO 

+  0.330*CO 
+  0,230*H02 

+  O.lOO*KET 
+  0.290*0RA2 
+  0.310*M02 

+  0.330*CO 
+  0.230*H02 

H02 
+ 
H02 
+ 
+  0,770*ALD 

+  0.750*ALD 
+  0.260*KET 

+  0,350*ALD 
+  0.7SO*ALP 
+  0.725*ALD 

+ 
+ 
+ 

H02 
H02 
H02 

+  0.7SO*MGLY 
+  O.SOO*HO~ 

H02 
+ 
+  O.SOO*M02 

+  O.l,70*MGLY 
+  2.000•H02 
+  0,450*MGJ;,Y 

+  0.160*GLY 

+  0.806*PCB 

+  0.445*GLY 
+  0.025*ACQ3 
X02 
+ 
+  O.SOO*H02 

+  0. 055*MG;i:..Y 
+  0.460*H02 

+ 

ALD 

+  Q.SOO*H02 

+.  0. SOO*M02 

+  0,260*KET 
+  O.SOO*ORA2 
+  0 .. 750*KJ;lT 
+  O,SOO*ORA2 
+  1,390*KET 
+  0,500*0RA2 
+  0,600*ALD 
+  0.500*0RA2 
+  O.SOO*HCHO 
+  0,SOO*ORA2 
+  O.SSO*I<ET 

+  0,SOO*H02 

+  0.500*H02 

+  O.SOO*H02 

+  O.SOO*H02 

+  O,SOO*H02 

+  0.140*HCHO 

--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 

--> 

--> 
--> 
--> 

--> 
--> 

--> 

--> 

--> 

--> 

0.530*HCHO 
+  0.200*0RA1 
.+  0.220*M02 

--> 

0,180*HCHO 

+  o.23o•co 
+  0,260*H02 
+ 

OLIAi;:R 

0.530*HCHO 
+  0.200*0RA1 
+  0.220*M02 
OP1 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
PAA 
OP2 
OP2 
OP2 
ONIT 
1.500*HCHO 
0,750*HCHO 
O. 840*HCHO. 

1. 550*HCHO 
1.250*HCHO 
0,890*HCHO 

+  0.550*KET 

0.7SO*HCHO 
HCHO 
+  O.SOO*ORA2 
HCHO 
+  0.700*PCB 
.HCHO 

+  2,000*H02 

0.500*HCHO 
+  O.SOO*ORA2 
+  0.475-l<CO 

--> 

1.7SO*HCHO 

--> 

--> 

--> 

--> 

+ 

N02 
ALP 

+  0.500*0RA2 

0.770*ALD 
+  0,500*M02 
0.4iO*ALD 
+  0.500*M02 
0.460*ALD 
+  o.::.oo•Mo2 

--> 

0.800*HCHO 

+  0.500*M02 
ALP 
+  0.500*M02 
0.725*ALD 

--> 

--> 

Table 8A-8.  RADM2_AE and RADM2_AE_AQ Mechanisms 

I 

{142}  KETP 

' 

+  AC03 

(143}  AC03 
{144}  AC03 

' 

+  ~C03 
+  °l'.OLP 

{145}  AC03 

' 

+  ~p 

{146}  AC03 

' 

+  TC03 

(147}  AC<;l3 

1 

+  qLN 

--> 

--> 
--> 

--> 

--> 

+  0.500*H02 

MGLY 
+  0.500*0RA2 

2.000*M02 
M02 
+  0.700*DCB 
M02 
H02 
M02 

+ 

+  O.l:J.O*MGLY 
+  2.000*X02 

--> 

+ 

HCHO 
N02 

+  0.500*M02 
+  0.500*H02 

+  0.500*0RA2 
+  O.SOO*M02 

+  0.170*MGLY 
+ 
+  0.450*MGLY 

H02 

+  0.160*GLY 

+  0.806*DCB 

+  0.920*H02 
+  0.050*AC03 

+  0.890*GLY 
+  o.9~~·c9 

+ 
ALD 
+  0.500*M02 
+  2.000*ALD 

+  O.SOO*ORA2 

+  2.000*N02 

'" 

2.000*HCHO 

.. , 

I 

+ 

H02 

{14e}  o~ 
(149}  X02 
{150}  X02 
{151}  X02 
{152}  X02 
{153}  X02 
{154}  XN02 
{155}  XN02 
(156}  XN02 
(157}  XN02 
{158}  xN02 
(159}  TERP 
(160}  TERP 
{161}  TERP 
>--------~----------------~--------------------------------------------------------< 

+q~ 
+  J102 
.+  Mo2 
+  AC03 
+  ~02 
+  1'f0 
+  N02 
+  1102 
+  1102 
+  AC03 
+  *o2 
+  HO 
+  1'l'03 
+  03 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

TERPAER  + 
TERPAER  + 
TERPAER  + 

OP2 
HCHO 
M02 

N02 
ONIT 
OP2 
HCHO 
M02 

+ 

H02 

HO 
N03 
03 

Rate  Constant 
Rate  Expression 
mmmm•=======•===~====r=======m;:a;:a;::;::;::;::=ci=;:z;:;:;::=====·:;:========-====o::a===-ss••m••cs•••lill:lm•• 
,  scaled  by  l.OOOOOE+OO 
{ 0. OOOOOE.+00} ,··, 
k(  1)  uses  photo  table  N02_RADM88 
{ 0. OOOOOE+OO}  , ,, 
,  scaled  by  l.OOOOOE+OO 
k( 
2)  uses  photo  table  0301D_RADMBB 
,  scaled  by  1.00000E+OO 
{O.OOOOOE+OO} 
k(  3)  uses  photo  table  0303P_RADM8B 
,  scaled  by  1.00000E+OO 
{o.oooooE+oo} 
k(  4)  uses  photo  table  HONO_RADM88 
,  scaled  by  l.OOOOOE+OO 
{ 0. OOOOOE+OO}'' 
5)  uses  photo  table  HN03_RADMBB 
k( 
,  scaled  by  l.OOOOOE+OO 
{o.oooooE+oo} 
6)  uses  photo  table  HN04_RADMBB 
k( 
,  scaled  by  l.OOOOOE+OO 
k( 
7)  uses  photo  table  N03NO_RADM88 
{O.OOOOOE+oo} 
,  scaled  by  l.OOOOOE+OO 
{o.oooooE+oo} 
8)  uses  photo  table  N03N02_RADM88 
k( 
,  scaled  by  l.OOOOOE+OO 
{o.ooooo:s:+oo} 
9)  uses  photo  table  H202_RADM88 
k( 
,  scaled  by  l.OOOOOE+OO 
{O.OOOOOE+OO} 
k(  10)  uses  photo  table  HCHOmol_RADMBB 
{ 0. OOOOOE+OO}  ··· 
k(  11)  uses  photo  table  H~Orad_RADMBB  ,  scaled  by  1.00000E+OO 
,  scaled  by  1.00000E+OO 
k(  12)  uses  photo  table  ALD_RADMBB 
{ 0. OOOOOE+OO} "' 
(O.OOOOOE+OO} 
,  scaled  by  1.00000E+OO 
k(  13)  uses  photo  table  MHP_RADMBB 
{0.00000E+OO} 
,  scaled  by  1.00000E+OO 
k(  14)  uses  photo  table  HOP_RADMBB 
(O.OOOOOE+oo} 1 
,  scaled  by  1.00000E+OO 
k(  15)  uses  photo  table  PAA_RADM88 
,  scaled  by  l.OOOOOE+OO 
{O.OOOOOE+OO} 
k(  16)  uses  photo  table  KETONE_RADMBB 
{O.OOOOOB+oo} 
,  scaled  by  1.00000E+OO 
k(  17)  uses  photo  table  GLYform_RADMBB 
,  scaled  by  1.00000E+OO 
(O.OOOOOB+OO} 
k(  18)  uses  photo  table  GLYmol_RADMBB 
{O.OOOOOE+OO} 
,  scaled  by  1.00000E+OO 
k(  19)  uses  photo  table  MGLY_RADMBB 
{o.ooooo:s:+oo} 
,  scaled  by  1.00000E+OO 
k(  20)  uses  photo  table  UDC_RADMBB 
{ 0. OOOOOB+OO}  ' 
,  scaled  by  1.00000E+OO 
k(  21)  uses  photo  table  ORGNIT_RADMB8 
k(  22)  =  6.0000E-34  *  (T/300)**(-2.30) 
{6.09302B-34} 
120.0/T) 
k(  23)  =  6.5000E-12  *  exp( 
{9.72293E-12} 
{2.60365E-11} 
110.0/T) 
.. 
k(  24) 
1.BOOOE-11  *  exp( 
{4.04730B·l.1} 
70.0/T) 
k(  25) 
..  3.2000E-11  *  exp( 
{2.20000B-10} 
k(  26)  =  2.2000E-10 
{1.82272E-14} 
k(  27)  =  2.0000E-12  *  exp(  -1400.0/T) 
k(  28)  =  1.6000E-12  *  exp( 
(6.82650B-14} 
-940.0/T) 
k(  29)  =  1.lOOOE-14  *  exp( 
{2.05452E·l5} 
-500.0/T) 

••• 

i 

! 

1,"' 
I• 

Table 8A-8.  RADM2_AE and RADM2_AE_AQ Mechanisms 

k(  30) 
k(  31.)  is 

240.0/T) 
3.7000E-12  *exp( 
a  falloff  expression  using: 

{8.27883E-l.2} 
{l..39058E-12} 

EPA/600/R.-99/030 

kO 
kinf  = 
F  =  0.60, 

1.8000E-31  *  CTi300)**(-3.20) 
4.7000E-12  *  (T/300)**(-1.40) 

n  =  1.00 

k(  32)  =  k(  31)  I  Keq,  where  Keq  = 
k(  33)  is  a  special  rate  expression 

2.lOOE-27  *  exp(  10900.0/T) 

of  the  form: 

{8.62399E-02} 
{3.01634E-12} 

k  =kl+  k2(M],  where 
kl.  =  2.2000E-13  *  exp( 
k2  =  1.9000E-33  *  exp( 

620.0/T) 
980.0/T) 

k(  34)  is  a  special  rate  expression  of  the  form: 

{6.7890SE-30} 

k  =kl+  k2(MJ,  where 
kl=  3.0800E-34  *exp(  2820.0/T) 
k2  =  2.6600E-54  *exp(  3180.0/T) 

k(  35)  =  3.3000E-12  *  exp( 
-200.0/T) 
k(  36)  is  a  falloff  expression  using: 

=  7.0000E-31.  *  (T/300)**(-2.60) 
1.SOOOE-11  *  (T/300)**(-0.50) 

kO 
kinf  = 
F  =  0.60, 

n  =  1.00 

k(  37)  =  3.3000E-39  *  exp( 
k(  38)  =  1.4000E-13  *  exp( 
k(  39)  =  1.7000E-11  *  exp( 
k(  40)  =  2.SOOOE-14  *  exp( 
k(  41)  = 
k(  42)  is  a  falloff  expression  using:· 

530.0/T) 
-2500.0/T) 
150.0/T) 
-1230.0/T) 

2.SOOOE-12 

kO 
=  2.2000E-30  *  (T/300)**(-4.30) 
kinf  =  1.SOOOE-12  *  (T/300)**(-0.50) 
F  =  0.60, 

n  =  1.00 

k(  43)  =  k(  42) 
k(  44)  =  2.0000E-21 
k(  45)  is  a  falloff  expression  using: 

/  Keq,  where  Keq  = 

l.lOOE-27  *  exp(  l.l.200.0/T) 

kO 
=  2.6000E-30  *  (T/300)**(-3.20) 
kinf  =  2.4000E-11  *  (T/300)**(-1.30) 
F  =  0.60, 

n  =  1.00 

k(  46)  is  a  special  rate  expression  of  the  form: 

(1  +  k3[M]/k2J},  where 

k  =  kO  +  {k3[M]  I 
kO  =  7.2000E-15  *  exp( 
785.0/T) 
k2  =  4.lOOOE-16  *  exp(  1440.0/T) 
k3  =  1.9000E-33  *  exp( 
725.0/Tl 
380.0/T) 
k(  47)  =  1.3000E-12  *.exp( 
k(  48)  =  4.6000E-11  *  exp( 
230.0/T) 
k(  49)  is  a  falloff  expression  using: 

=  3.0000E~31  *  (T/300)**(-3.30) 
1.SOOOE-12  *  (T/300)**(  0.00) 

kO 
kinf  = 
F  =  0.60, 

n  =  1.00 

k(  SO)  =  1.SOOOE-13  *  (1.0  +  0.6*Pressure) 
k(  51)  =  2.8300E+Ol  *  (T/300)**(  2.00)  *exp(  -1280.0/T) 
k(  52)  =  1.2330E-12  *  (T/300)**(  2.00)  *exp( 
-444.0/T) 
k(  53)  =  1.5900E-11  *  exp(  .  -540.0/T) 
k(  54)  =  1.7300E-ll  *exp( 
-380.0/T) 
-380.0/T) 
k(  55)  =  3.6400E-11  *exp( 
411.0/T) 
k(  56)  =  2.lSOOE-12  *exp( 
504.0/T) 
k(  57)  =  5.3200E-12  *exp( 
549.0/T) 
k(  58)  =  1.0700E-11  *exp( 
k(  59)  =  2.lOOOE-12  *exp( 
322.0/T) 
k(  60)  =  1.8900E-11  *  exp( 
116.0/T) 
k(  61)  =  4.0000E-11 
k(  62)  =  9.0000E-01  *  k(  61) 
k(  63)  =  9.0000E-12 
k(  64)  =  6.8700E-12  *  exp( 
l.2000E-11  *  exp( 
k(  65)  = 

256.0/T) 
-745.0/T) 

8-63 

{1. 68671.E-12} 
{4.87l.44E-12} 

{1.95397E-38} 
{3.18213E-17} 
{2.81225E-11} 
{4.03072E-16} 
{2.SOOOOE-12} 
{1.26440E-12} 

{S.47034E-02} 
{2.00000E-21} 
{1.1488SE-11} 

{1.47236E-13} 

{4.65309E-12} 
{9.95294E-11} 
{8.88848E-l.3} 

{2.40000E-13} 
{3.80672E-Ol} 
{2.74210E-13} 
{2.59669E-12} 
{4.83334E-12} 
{1.01696E-11} 
{8.539l.6E-l.2} 
{2.88684E-11} 
{6.75269E-11} 
{6.1871.SE-12} 
{2.78943E-11} 
{4.00000E-11} 
{3.60000E-11} 
{9.00000E-12} 
{1.62197E-11} 
{9.85020E-13} 
-444.0/T) 

Table 8A-8.  RADM2_AE and RADM2_AE_AQ Mechanisms 
k(  66)  =  l.1500E-ll 
k(  67)  =  1.7000E-ll 
k(  68)  =  2.8000E-ll 
k(  69)  =  1.0000E-11 
k(  70)  =  1.0000E-11 
k(  71)  =  1.0000E-11 
k(  72)  =  6.1650E-13  *  (T/300) ** (  2. 00)  *  exp( 
k(  73)  =  1.5500E-ll  *  exp( 
-540.0/T) 
k(  74)  =  2.5500E-ll  *  exp( 
409.0/T) 
k(  75)  =  2.8000E-12  *  exp( 
181. O/T) 
k(  76)  =  1.9500E+l6  *  exp(-13543.0/T) 
k(  77)  =  4.7000E-12 
k(  78)  =  1.9500E+l6  *  exp(-13543.0/T) 
k(  79)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  80)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  81)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  82)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  83)  =  4.2000E-12  *  exp( 
180. O/T) 
k(  84)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  85)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  86)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  87)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  88)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  89)  =  4.2000E-12  *  exp( 
180,0/T) 
k(  90)  =  4.2000E-12  *  exp( 
180. O/T) 
k(  91)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  92)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  93)  =  6.0000E-13  *  exp(  -2058.0/T) 
k(  94)  =  1.4000E-12  *  exp(  -1900.0/T) 
k(  95)  =  6.0000E-13  *exp(  -2058.0/T) 
k(  96)  =  l.4000E-12  *  exp(  -1900.0/T) 
l.4000E-12  *  exp(  -1900.0/T) 
k(  97) 
k(  98) 
2.2000E-ll 
k(  99)  =  2.0000E-12  *  exp(  -2923.0/T) 
k(lOO)  =  1.0000E-11  *  exp(  -1895.0/T) 
k(l01)  =  3.2300E-ll  *  exp( 
-975.0/T) 
k(102)  =  5.8100E-13 
k(103)  =  1.2000E-14  *  exp(  -2633.0/T) 
k(l04)  =  1.3200E-l.4  *  exp(  -2105.0/T) 
k(lOS)  =  7.2900E-15  *  exp(  -1136.0/T) 
k(106)  =  1.2300E-14  *exp(  -2013.0/T) 
k(l.07)  =  7.?000E-14  *  exp(  1300. O/T) 
k(108)  =  7.7000E-14  *  exp(  1300.0/T) 
k(109)  =  7.7000E-14  *  exp(  1300. O/T) 
k(llO)  =  7.7000E-14  *  exp(  1300.0/T) 
k(ll.l.)  =  7.7000E-14  *  exp(  1300. O/T) 
k(112)  =  7.7000E-14  *  exp(  1300.0/T) 
k(l.13)  =  7.7000E-14  *  exp(  1300. O/T) 
k(114)  =  7.7000E-14  *  exp(  1300.0/T) 
k(115)  =  7.7000E-14  *  exp(  1300. O/T) 
k(116)  =  7.7000E-14  *  exp(  1300.0/T) 
k(117)  =  7.7000E-14  *  exp(  1300.0/T) 
k(llB)  =  7.7000E-14  *  exp(  1300.0/T) 
k (l.19)  =  7.7000E-14  *  exp(  1300.0/T) 
k(120)  =  7.7000E-14  *  exp(  1300. O/T) 
k(l21)  =  1.9000E-13  *  exp( 
220. O/T) 
220.0/T) 
k(122)  =  1.4000E-13  *  exp( 
220.0/T) 
k(123)  =  4.2000E-14  *  exp( 
k(124)  =  3.4000E-14  *exp( 
220.0/T) 
220.0/T) 
k(125)  =  2.9000E-14  *  exp( 
k(126)  =  1.4000E-13  *  exp( 
220.0/T) 

I 
I 
I 

c 

c 

' 

{l.15000E-ll} 
{1.70000E-11} 
{2.80000E-ll} 
{1.00000E-11} 
{1.00000E-11} 
{1.00000E-11} 
{l.37105E-13} 
{2.53137E-12} 
{1.00601E-10} 
{5.13974E-12} 
{3.57235E-04} 
{4.70000E-12} 
{3.57235E-04} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7 .68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.6837BE-12} 
{6.0l.030E-l.6} 
{2.38307E-l.5} 
{6.01030E-16}, 
{2.38307E-15} 
{2.38307E-15} 
{2.20000E-11} 
{l. 09940E-16} 
{1.73099E-14} 
{1.22539E-12} 
{5.8l.OOOE-l.3} 
{1.74559E-1B} 
{l.12933E-17} 
{1.6l.l.25E-16} 
{1.43295E-17} 
{6.04038E-12} 
{6.0403BE-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-l.2} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.0403BE-12} 
{3.97533E-13} 
{2.92919E-13} 
{B.78758E-14} 
{7.11376E-14} 
{6.06762E-14} 
{2.92919E-13} 

Table 8A-8.  RADM2_AE and RADM2_AE_AQ Mechanisms 
k(127)  =  1.4000E-13  *  exp( 
220.0/T) 
k(128)  =  1. 7000.E-14  *  exp ( 
220.0/T) 
k(129)  =  1.7000E-14  *exp( 
220.0/T) 
k(130)  =  9.6000E-13  *  exp( 
220.0/T) 
k(131)  =  1.7000E-14  *  exp( 
220.0/T) 
k(132)  =  1.7000E-14  *  exp( 
220.0/T) 
k(l33)  =  9.6000E-13  *  exp( 
220.0/T) 
k.(134)  =  1.7000E-14  *  exp( 
220. O/T) 
k(135)  =  3.4000E-13  *exp( 
220.0/T) 
k(136)  =  1.0000E-13  *  exp( 
220.0/T) 
k  (137)  =  8.4000E-14  *  exp( 
220.0/T) 
k(138)  =  7.2000E-14  *  exp( 
220.0/T) 
k(139)  =  3.4000E-13  *  exp( 
220.0/T) 
k(140)  = '3.4000E-13  *exp( 
220.0/T) 
k  (141)  =  4.2000E-14  *  exp( 
220.0/T) 
k(l42)  =  4.2000E-14  *  exp( 
220.0/T) 
k(l43)  = 
l.1900E-12  *  exp( 
220.0/T) 
k(144)  =  4.2000E-14  *  exp( 
220.0/T) 
k(145)  =  4.2000E-14  *  exp( 
220.0/T) 
k(146)  = 
l.1900E-12  *  exp( 
220.0/T) 
k(147)  =  4.2000E-14  *  exp( 
220. O/T) 
k.(148)  =  3.GOOOE-16  *  exp( 
220.0/T) 
k(149)  =  7.7000E-14  *  exp(  1300.0/T) 
k(150)  =  1.7000E-14  *exp( 
220.0/T) 
k.(151)  =  4.2000E-14  *  exp( 
220.0/T) 
k(l52)  =  3.6000E-16  *exp( 
220.0/T) 
k(153)  =  4.2000E-12  *  exp( 
180.0/T) 
k(154)  =  4.2000E-12  *  exp( 
180.0/T) 
k(lSS)  =  7.7000E-14  *  exp(  1300.0/T) 
k(156)  =  1.7000E-14  *  exp( 
220.0/T) 
k(157)  =  4.2000E-14  *  exp( 
220.0/T) 
k(l58)  =  3.6000E-16  *  exp( 
220.0/T) 
k(159)  =  1.0000E+OO  *  k(  58) 
k(160)  =  1.0000E+OO  *  k.(101) 
k(161)  =  1.0000E+OO  *  k(lOS) 

{2.92919E-13} 
{3.55688E-14} 
{3.55688E-14} 
{2.00859E-12} 
{3.55688E-14} 
{3.55688E-14} 
{2.00859E-12} 
{3.55688E-14} 
{7.11376E-13} 
{2.09228E-13} 
{l.75752E-13} 
{ 1. 50644E-13} 
{7.11376E-13} 
{7 .. 11376E-13} 
{8.78758E-14} 
{8.78758E-14} 
{2.48981E-12} 
{8.78758E-14} 
{8.78758E-14} 
{2.48981E-12} 
{8.78758E-14} 
{7.53221E-16} 
{6.04038E-12} 
{3.55688E-14} 
{8.78758E-14} 
{7.53221E-16} 
{7.68378E-12} 
{7.68378E-12} 
{6.04038E-12} 
{3.55688E-14} 
{8.78758E-14} 
{7.53221E-16} 
{6.75269E-11} 
{1.22539E-12} 
{1.61125E-16} 

======================================================a=============cm=====~=== 

Table 8A-9.  RADM2_CIS1  and RADM2_CISl_AQ Mechanisms 

II 

l}  N02 

+ 

+ 
+ 
+ 

+ 

Reaction  List 
>- - - - - - - - - - - --- - - -1- - - - - - -.;:- - - -·- - - -- ------- - - - - - -.---- - - ---- - - - - - -- ------ -----:"1~--~,,,;.-:--------<  '·II 
{ 
{  2}  03 
{  3}  03 
{  4}  HONO 
{  s}  HN03 
{  6}  HN04 
7}  N03 
{ 
8}  N03 
{ 
{ 
9}  H202 
{  10}  HCHO 
{  11}  HCHO 
{  12}  ALD 
{  13}  OPl 
{  14}  OP2 
{  15}  PAA 
{  16}  KET 
{  17}  GLY 
{  18}  GLY 
{  19}  MGLY 
{  20}  DCB 
{  21}  ONIT 

,  03P 
OlD 
03P 
HO 
HO 
H02 
NO 
N02 
2.000*HO 
CO 
H02 
M02 
HCHO 
ALD 
M02 
AC03 
O.l30*HCHO 
0.4SO*HCHO 
~CQ3 

+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  ~v 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  ~v 

H02 
H02 
H02 
H02 
HO 
ETHP 

+ 
+ 
+ 
+ 
+ 
+ 
+  l.870*CO 
+  l.ssp•co 
+ 
HQf 
+  0.02!l~AC03 
+  O.BOO*KET 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
-->· 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

+  O.BOO*H02 
co 
;j; 
T<;:03 
:I: 
H02 
+ 

NO 

NO 
N02 
N02 

03P 

+ 
+ 
t, 
+ 

co 
co 
HO 
HO 

''11,, 

,,,, 

d·il' 

+  0.009*HCHO 

+ 

+ 

+ 

+ 

HO 

N02 

N02 

N03 

+ 

H02 

+  O.l70*H02 
+  0.025*KBT 
+  o.2so•xo2 
+  0.7SO*X02 

03P 
{  22} 
03P 
{  23} 
OlD 
{  24} 
OlD 
{  25} 
OlD 
{  26} 
03 
{  27} 
{  28}  03 
{  29}  03 
{  30}  H02 
H02 
{  31} 
HN04 
{  32} 
H02 
{  33} 
H02 
{  34} 
H202 
{  35} 
NO 
{  36} 
{  37}  NO 
{  38}  03 
{  39}  N03 
N03 
{  40} 
N03 
{  41} 
N03 
{  42} 
N205 
{  43} 
N205 
{  44} 
{  45}  HO 
{  46}  HO 
{  47}  HO 
{  48}  HO 
{  49}  HO 
{  so}  co 
{  51}  HO 
{  52}  ETH 
{  53}  HC3 

54}  HCS 
ss}  HC8 
56}  OL2 
57}  OLT 

I 

+  [M] 
+  ~O:;? 
+  (N2] 
+  [02] 
+  [H20] 
+  NO 
+  HO 
+  H02 
+  NO 
+  N02 

+  H02 
·+  H02 
+  HO 
+  HO 
+  ~o 
+  ~02 
+  NO 
+  ~02 
+  H02 
+  N02 

[H20] 

+ 
+  ,N02 
+  .HN03 
+  HN04 
+  H02 
+  ,S02 
+  HO 

+  HO 
+  HO 

+  HO 
+,HO 
+,HO 
+  HO 

I 

+ 

0.980*H02 
0.200*ALD 
N02 
03 
NO 
03P 
03P 

2.000*HO 
N02 
H02 
HO 
N02 
HN04 
H02 
H202 
H202 
H02 
HONO 
2.000*N02 
N03 
2.000*N02 

NO 
HN03 
N205 
N02 

2.000*HN03 
HN03 
N03 
N02 

SULF 
H02 
M02 
ETHP 
O.B30*HC3P 

+  [02] 

+  [H20] 

t 

[02] 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

+  0.075*ALD 
HCSP 
HC8P 
OL2P 
OLTP 

--> 
--> 
--> 
--> 

Table SA-9.  RADM2_CIS1  and RADM2_CIS1_AQ Mechanisms 

EP A/600/R-99/030 

{  58}  OLI 
{  59}  TOL 
{  60}  XYL 
{  61}  CSL 
{  62}  CSL 
{  63}  HCHO 
{  64}  ALD 
{  65}  KET 
{  66}  GLY 
{  67}  MGLY 
{  68}  DCB 
{  69}  OP1 
{  70}  OP2 
{  71}  PAA 
{  72}  PAN 
{  73}  ONIT 
{  74}  AC03 
{  75}  PAN 
{  76}  TC03 
{  77}  TPAN 
{  78}  M02 
{  79}  HC3P 

so}  HC5P 

81}  HCBP 

82}  OL2P 

83}  OLTP 

84}  OLIP 

85}  AC03 
86}  TC03 

87}  TOLP 

88}  XYLP 

{  89}  ETHP 
{  90}  KETP 
{  91}  OLN 
{  92}  HCHO 
{  93}  ALD 
{  94}  GLY 
{  95}  MGLY 
{  96}  DCB 
{  97}  CSL 
{  98}  OL2 
{  99}  OLT 
{100}  OLI 
{101}  OL2 

{102}  OLT 

+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  N02 

+  N02 

+  NO 
+  NO 

+  NO 

+  NO 

+  NO 

+  NO 

+  NO 

+  NO 
+  NO 

+  NO 

+  NO 

+  NO 
+  NO 
+  NO 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  03 

+  03 

{103}  OLI 

+  03 

OLIP 
0.750*TOLP 
0.830*XYLP 
0.100*H02 
CSL 
H02 
AC03 
KETP 
H02 
AC03 
.TC03 
0.500*M02 
0.500*HC3P 
AC03 
HCHO 
HC3P 
PAN 
AC03 
TPAN 
TC03 
HCHO 
0.750*ALD 

+  0.036*0NIT 

0.3BO*ALD 
+  0. 920*N02 
0.350*ALD 

+  0.240*0NIT 
1.600*HCHO 

+ 

+  0,200*ALD 
ALD 
N02 
H02 
+  0.100*KET 
M02 
N02 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 

--> 

--> 

--> 

~-> 

-->. 
--> 

--> 

--> 

+  0.110*MGLY 
+  2,000*X02 
N02 
+  0.160*GLY 
N02 
+  0.806*DCB 
ALD 
MGLY 
HCHO 
H02 
AC03 
HN03 
HN03 
HN03 
HN03 
OLN 
OLN 
OLN 
HCHO 
+  0.120*H02 

--> 
--> 
-->. 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 

0. 530*.HCHO 
+  0,200*0RA1 
+  0.220*M02 

--> 

0.180*HCHO 

.+  0.230*CO 
+  0.260*H02 

8-67 

+  0.250*H02 
+  0.170*H02 
+  0.900*TC03 

+.  0.250*CSL 
+  0.170*CSL 
+  0:900*X02 

+ 

co 

+  2.000*CO 
co 
+ 

+  0.500*HCHO 
+  0.500*ALD 

+  0.500*HO 
+  0.500*HO 

+ 
+ 

+ 

N03 
N02 

N02 

N02 
+ 
+ 
H02 
+  0.250*KET 
+  0.964*N02 
+  0.690*KET 
+  0.920*H02 
+  1.060*KET 
+  0.760*N02 
+ 
H02 

+ 

X02 

N02 

+ 
.+  0. 090*HCHO 
+  0.964*HQ2 
+  0.080*0NIT 

+  0.040*HCHO 
+  0.760*H02 
+ 
N02 

+ 

HCHO 

+ 

H02 

+  1. 450*ALD. 
N02 
+ 
+ 
N02 
+  0.920*H02 
+  0.050*AC03 

H02 
+ 
+  0.700*DCB 
H02 
+ 

+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 

H02 
N02 
ALD 
HN03 
HN03 
H02 
AC03 
TC03 
XN02 

+  0,280*HCHO 

+  0.890*GLY 
+  0.950*CO 

+  0.170*MGLY 

+  0 .450*MGLY 

+ 
N02 
+ 
H02 
+  2.000*N02 
co 
+ 

+  2.000*CO 
co 
+ 

+  0.500*CSL 

+  0.400*0RA1 

+  0.420*CO 

+  0.500*ALD 
+  0.200*0RA2 
+  0.100*HO 
+  0.720*ALD 
+  0,060*0RA1 
+  0.140*HO 

+  .0.330*CO 
+  0.230*H02 

+  0.100*KET 
. +  0. 290*0RA2 
+  0.310*M02 

Table 8A-9.  RADM2_CISI  and RADM2_CISI_AQ Mechanisms 

I. 

OPl 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
PAA 
OP2 
OP2 
OP2 
ONIT 
1.500~HCH0 
0.750*HCHO 
0. 840*JiC.HO 

H02 

+ 
H02 
+ 
H02 
+  0,770*ALD 

0.770*HCHO 

+  0.410*ALD 

H02 

1111 

,,

1111"' 

+  a . 7!fo•ALI> 
+  0.260*KET 
:,,,,,;,, 
+  0,750*KET· 

:::11;: 

',1 

'! 

,,. 

'11'1 

o.0oo•HCHO 

+  0.460*11,LD 

+  1.390*KET 

H02 

:,11111:: 

111: 

11111'111 

+  0.3SO*ALD 
+  0.750*ALD 
+  0.725*ALD 

+ 
+ 
+ 

'ii' 

H02 
H02 
H02 

+  0.7SO*MGLY 
+  o.soo•Ho2 

+ 
H02 
+  o.soo•Mo2 

+  0.170*MGLY 
+  2.000*H02 
+  0 ,450*MGLY 

+  0.44S*GLY 
+  0.02S*AC03 
+ 
X02 
+  o.soo•Ho2 

+  o,soo•Ho2 

+  0.2~0*KET 
+  o.soo•oRA2 
+  0.750*KET 
+  O.SOO*ORA2 
+  l.390*KET 
+  O.!;iOO*ORA2 
+  0.600*ALD 
+  O.SOO*ORA2 
+  o.soo•HCHO 
+  o.soo•ORA2 
+  o.sso•KET 
+  O.SOO*M02 
+  Q.SOO*H02 

1
1';,":'iWI' 
'1

+  0.160*GLY 

,111 

+  0.806*DCB 

,,,1·:1::·:1 

+  o.oss•MGLY 
+  0.460*H02 

1:111'1111• 

'II 

+ 

ALD 

t.  o. s~.o•Mo2 

"':  o.soo•Ho2 

+  o.soo•Ho2 

111',' 

11 

+  O.SOO*H02 

+  o.soo•Ho2 

+  o.soo•Ho2 

+  0.140*HCHO 
+  o.soo•oRA2 
,+  0, §OO*M02 

+  0.170*MGLY 
+ 
+  0.4SO*MGLY 

HC;?2 

+  0.160*GLY 

+  O.B06*DCB 

+  0.920*H02 
+  o.oso•AC03 

+  0.890*GLY 
+  0.9SO*CO 

{104}  H02 
{105}  H02 
{106}  H02 
{107}  H02 
{108}  H02 
{109}  H02 
{110}  H02 
{111}  H02 
{112}  H02 
{113}  H02 
{114}  H02 
{115}  H02 
{116}  H02 
{117}  H02 
{118}  M02 
{119}  M02 
{120}  M02 

+  M02 
+  ETHP 
+  HC3P 
+  HC5P 
+  HC8P 
+  OL2P 
+  OLTP 
+  OLIP 
+  KETP 
+  AC03 
+  TOLP 
+  XYLP 
+  TC03 
+  OLN 
+  M02 
+  ETHP 
+  HC3P 

{121}  M02 

+  HC5P 

{122}  M02 

+  HC8P 

{123}  M02 
{124}  M02 
{125}  M02 

{126}  M02 
{127}  M02 

+  OL2P 
+  OLTP 
+  O;LIP 

+  KETP 
+  AC03 

{128}  M02 

+  TOLP 

{129}  M02 

+  XYLP 

{130}  M02 

+  TC03 

-;--> 
--> 
--> 

-~> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 

--> 

--> 
--> 
--> 

--> 
--> 

--> 

--> 

--> 

+ 

+ 

+ 

1.550*H.CHO 
1.250*HCHO 
0.890*HCHO 

+  o.5so•KET 
0.750*HCHO 
HCHO 
+  0.500*0RA2 
HCHO 
+  0.700*DCB 
HCHO 

+  2.000*H02 

o.500•HCHO 
+  o.5oo•oRA2 
+  0.47S*CO 

{131}  M02 

+  OLN 

- - > 

1. 7SO*HCHO 

{132}  ETHP 

+  }\.C03 

{133}  HC3P 

+  AC03 

{134}  HC5P 

+  AC03 

{135}  HC8P 

+  AC03 

{136}  OL2P 

+  AC03 

{137}  OLTP 

+  AC03 

{138}  OLIP 

+  AC03 

{139}  KETP 

+  AC03 

{140}  AC03 
{141}  AC03 

+  AC03 
+  TOLP 

{142}  AC03 

+  XYI,,P 

{143}  AC03 

+  TC03 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 
--> 

--> 

--> 

+ 

N02 
ALD 

+  Q,500*0RA2 

0,770*ALD 
+  O.SOO*M02 
0,410*ALD 
+  O.SOO*M02 
0.469*ALD 
+  0,SOO*M02 

o.0oo•HCH0 

+  0,500*M02 
ALD 
+  O.SOO*M02 
0.725*ALD 
+  o.soo•Ho2 

MGLY 
+  o.500•0RA2 

2.000*M02 
M02 
+  0.700*DCB 
M02 
H02 
M02 

+ 

+  o.11o•MGLY 

8-68 

Table 8A-9.  RADM2_CIS1  and RADM2_CISl_AQ Mechanisms 

{144}  AC03 

+  OLN 

{145}  OLN 
{146}  X02 
{147}  X02 
{148}  X02 
{149}  X02 
{150}  X02 
{151}  XN02 
{152}  XN02 
{153}  XN02 
{154}  XN02 
{155}  XN02 
{156}  ISO 
{157}  ISO..:_R02 

+  OLN 
+  H02 
+  M02 
+  AC03 
+  X02 
+  NO 
+  N02 
+  H02 
+  M02 
+  AC03 
+  XN02 
+  HO 
+  NO 

{158}  ISO_R02  +  H02 
{159}  ISO_R02  +  AC03 

{160}  ISO_R02  +  M02 
+  03 
{161}  ISO 

{162}  ISO 

+  03P 

{163}  ISO 
{164}  ISON_R02  +  NO 

+  N03 

{165}  ISON_R02  +  H02 
{166}  ISON_R02  +  AC03. 

{167}  ISON_R02  +  M02 

{168}  ISOPROD  +  HO 
+  NO 
{169}  IP_R02 

{170}  IP_R02 
{171}  IP_R02 

+  H02 
+  AC03 

{172}  IP_R02 

+  M02 

{173}  ISOPROD  +  03 

{174}  ISOPROD  +  hv 

{175}  ISOPROD  +·N03 

+  2.000*X02 
HCHO 
N02 

+ 

2.000*HCHO 

OP2 
HCHO 
M02 

N02 
ONIT 
OP2 
HCHO 
M02 

+ 
ALD 
+  0.500*M02 
+  2.000*ALD 

+ 

H02 

+  0.500*0RA2 

+  2.000*N02 

+ 

H02 

0;088*0NIT 

ISO_R02  +  0.079*X02 
+  0.912*N02 

+  0.912*ISOPROD  +  0.629*HCHO 

+  0.912*H02 

OP2 
0.500*H02 

+ 

ISO PROD 

+  0.500*M02 

+  0.500*0RA2 

0.500*HCHO 
0.600*HCHO 

+  0.500*H02 
+  0.650*ISOPROD  +  0.390*0RA1 
+  0.070*H02 
+  0.200*AC03 
0.750*ISOPROD  +  0.250*AC03 

+  0.070*CO 
+  O.l50*ALD 
+  0.250*HCHO 

+  0.270*HO 
+  0.200*X02 

+ 

ISOPROD 

--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 
--> 

--> 
--> 

--> 

--> 
--> 

--> 
--> 

+  0.250*M02 

ISON_R02 
N02 
+  0.800*H02 

ONIT 

0.500*H02 
ALD 

+ 

+ 

--> 

0.500*HCHO 
ONIT 
-->·  0.500*AC03 
--> 

N02 
+  0;550*ALD 
+  0. 340*MGLY 

--> 
--> 

--> 

--> 

--> 

--> 

OP2 
0.500*H02 
+  0.500*ALD 

0.500*HCHO 

+  O.SOO*KET 

0.268*HO 
+  0.054*M02 
+  0.146*HCHO 
+  0.850*MGLY 
0.970*AC03 
+  0.200*HCHO 
+  0.033*KET 
0.075*AC03 
+  o·.282*HCHO 
+  0.925*H02 

+  0.800*ALD 
+  0.200*ISOPROD  +  0.200*N02 

+  0.800*0NIT 

+  0.500*M02 
+ 
+  0.500*H02 

ONIT 

+  0.500*IP_R02 
+ 
+  0.250*HCHO 
+  0.630*KET 

H02 

+  0.500*M02 
+  0.500*KET 
+  0.500*H02 

+  0.100*H02 
+  0.070*X02 
+  0.020*ALD 
+  0.090*KET 
+  0.333*H02 
+  0.333*CO 

+  0.075*HN03 
+  0.925*0NIT 
+  0.925*X02 

+  0.500*0RA2 

+ 

ALD 

+  0.200*X02 
+  o'.590*CO 
+  0.080*GLY 

+  0.500*0RA2 

+  0.500*ALD 

+  0.114*AC03 
+  0.155*CO 
+  O.OlO*GLY 
+  0.462*0RA1 
+  0.700*M02 
+  0.067*ALD 

+  0.643*CO 
+  0.282*ALD 

>----------------------------------------~-----------------------------------------------< 

Rate  Expression 

k(  1)  uses  photo  table  N02_RADM88 
k(  2)  uses  photo  table  0301D~RADM88 
k(  3)  uses  photo  table  0303P_RADM88 

'  scaled  by  l.OOOOOE+OO 
'  scaled  by  l.OOOOOE+OO 
'  scaled  by  l.OOOOOE+OO 

Rate  Constant 

{o.oooooE+oo} 
{O.OOOOOE+oo} 
{O.OOOOOE+OO} 

Table 8A-9.  RADM2  CISl and RADM2  CISl _AQ Mechruiisnis 

-

k(  4)  uses  photo  table  HONO_RADM88 
5)  uses  photo  table  HN03_RADM88 
k( 
6)  uses  photo  table  HN04_RADM88 
k( 
7)  uses  photo  table  N03NO_RADM88 
k( 
8)  uses  photo  table  N03N02_RADM88 
k( 
9)  uses  photo  table  H202_RADM88 
k( 

-
'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
. scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
. scaled  by  1.00000E+OO 
k(  10)  uses  photo  table  HCHOmol~RADM88  . scaled  by  1.00000E+OO 
k(  11)  uses  photo  table  HCHOrad_RADMBB  . scaled  by  1.00000E+OO 
. scaled  by  1.00000E+OO 
. scaled  by  1.00000E+OO 
. scaled  by  1.00000E+OO 
. scaled  by  l.OOOOOE+OO 
. scaled  by  l.OOOOOE+OO 
k(  17)  uses  photo  table  GLYform_RADMB8  . scaled  by  1.00000E+OO 
. scaled  by  l.OOOOOE+OO 
. scaled  by  l.OOOOOE+OO 
. scaled  by  l.OOOOOE+OO 
. scaled  by  1.00000E+OO 

k(  12)  uses  photo  table  ALD_RADM88 
k(  13)  uses  photo  table  MHP_RADMBB 
k(  14)  uses  photo  table  HOP_RADM88 
k(  15)  uses  photo  table  PAA_RADM88 
k(  16)  uses  photo  table  KETONE_RADMBB 

k(  18)  uses  photo  table  GLYmol_RADMBB 
k(  19)  uses  photo  table  MGLY_RADMB8 
k(  20)  uses  photo  table  UDC_RADMBB 
k(  21)  uses  photo  table  ORGNIT_RADMBB 
k(  22)  =  6.0000E-34  *  (T/300)**(-2.30) 
k(  23)  = 
6.SOOOE-12  *  exp( 
k(  24)  =  1.8000E-11  *  exp( 
k(  25)  =  3.2000E-ll  *  exp( 
k(  26)  =  2.2000E-10 
k(  27)  =  2.0000E-12  *  exp(  -1400.0/T) 
k(  28)  =  1.6000E-12  *  exp( 
-940.0/T) 
k(  29)  =  l.lOOOE-14  *  exp( 
-500.0/T) 
k(  30)  =  3.7000E-12  *  exp( 
240. O/T) 
k(  31)  is  a  fallof~  expression  using: 

120. O/T) 
110.0/T) 
70.0/T) 

=  1. 8000El31  *  (T/300) ** (-3. 20) 
kO 
kinf  =  4.7000E-12  *  (T/300) ** (-1.40) 
F  =  0.60, 

n  =  1.00 

I. 

{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{o.oooooE+oo} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{O.OOOOOE+oo} 
{0.00000E+OO} 
{O.OOOOOE+oo} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{6.093028-34~ 
{9.72293E-12 
{2.6036SE-11} 
{4.04730E-11} 
{2.20000E-10} 
{l.82272E-14} 
{6.826SOE-14} 
{2.05452E-15} 
{B.27883E-12} 
{1.39058E-12} 

{8.62399E-02} 
{3.01634E-12} 

{6.78905E-30} 

" 

{1.68671E-:J..2} 
{4.87144E-12} 

{1.95397E-38} 
{3.18213E-17} 
{2.8122SE-11} 
{4.03072E-16} 
{2.SOOOOE-12} 
{l.26440E-12} 

{S.47034E-02} 
{2.00000E-21} 
{1.1488SB-11} 

{l.47236E-13} 

k(  32)  = k(  31)  I  Keq,  where  Keq  =  2.lOOE-27  *  exp(  10900.0/T) 
k(  33)  is  a  specia~  rate  expression  of  the  form: 

k  =kl+  k2[M],  ,where 
kl  =  2.2000E-13  *  exp( 
k2  =  i.9000E:-33  *"exp( 

620.0/T) 
980.0/T) 

k(  34)  is  a  special  rate  expression  of  the  form: 

k  =kl+  k2[M],:where 
kl  =  3.0800E-3~  *  exp(  2820. O/T) 
k2  =  2.6600E-54  *  exp(  3180. O/T) 
k(  35)  =  3.3000E-12  *exp( 
-200.0/T) 
k(  36)  is  a  falloff  expression  using: 

=  7.0000E~31  *  (T/300)**(-2.60) 
kO 
kinf  =  1~SOOOE".:11  *  CT/300) ** (-0. SO) 
F  =  0.60, 

n  =  1.00 

k(  37)  =  3.3000E-39  *  exp( 
530. O/Ti 
k(  38)  =  1.4000E-13  *  exp(  -2500.0/Tl 
150.0/T) 
l.7000E-ll  *  exp( 
k(  39)  = 
2.SOOOE-14  *  exp(  -1230.0/T) 
k(  40)  = 
k(  41)  = 
2.SOOOE-12 
k(  42)  is  a  falloff  expression  using: 

=  2.2opo~-3o  *  (T/300)**(-4.30) 
kO 
kinf  =  1. SOOOE-12  *  cl'/300) ** c-o.soJ 
F  =  0.60, 

n  =  1. 00 

' 

k(  43)  =  k(  42)  I  Keq,  where  Keq  =  l.lOOE-27  *  exp(  11200.0/T) 
k(  44)  =  2.0000E-21 
k(  45)  is  a  falloff  expres~ion using: 

=  2.6000Ej~30  *  (T/300)**(-3.20) 
kO 
kinf  =  2.4000E-ll  *  CT/JOO)** (-1.30) 
F  =  0.60, 

n  =  1.00 

I 

k(  46)  i•  •  speciil  rate  expreooion  of  the  fo~' 

Table SA-9.  RADM2_CISI  and RADM2_CISI_AQ Mechanisms 

EPA/600/R-99/030 

(1  +  k3[M]/k2)},  where 

k  = kO  +  {k3[M]  I 
kO  =  7.2000E-15  *exp( 
785.0/T). 
k2  =  4.1000E-16  *  exp(  1440.0/T) 
k3  =  1.9000E-33  *  exp( 
725.0/T) 
k(  47)  =  1.3000E-12  *exp( 
3.80.0/T) 
k(  48)  =  4.6000E-1i  *  exp( 
230.0/T) 
k(  49)  is  a  falloff  expression  using: 

=  3.0000E-31  *  (T/300)**(-3.lO) 
kO 
kinf  =  1.SOOOE-12  *  (T/300)**(  0.00) 
F  =  0.60, 

n  =  1.00 

· 

256.0/T) 
-745.0/T) 

-S40.0/T) 
-380 ..  0/T) 
-380.0/T) 
411.0/T) 
504.0/T) 
549.0/T) 
322.0/T) 
116.0/T) 

k(  SO)  =  1.SOOOE-13  *  (1.0  +  0.6*Pressure) 
k(  S1)  =  2.B300E+01  *  (T/300)**(  2.00)  *exp( 
k(  S2)  =  1.2330E-12  *  (T/300)**(  2.00)  *exp( 
k(  S3)  =  1.S900E-11  *  exp( 
k(  S4)  =  1.7300E-11  *exp( 
k(  SS)  =  3.6400E-11  *  exp( 
k(  S6)  =  2.1SOOE-12  *  exp( 
k(  57)  =  5.3200E-12  *  exp( 
k(  58)  =  1.0700E-l1  *exp( 
k(  S9)  =  2.1000E-12  *exp( 
k(  60)  =  1.8900E-11  *  exp( 
k(  61)  =  4.0000E-11 
k(  62)  =  9.0000E-01  *  k(  61) 
k(  63)  =  9.0000E-12 
k(  64)  =  6.8700E-12  *  exp( 
k(  6S)  =  1.2000E-11  *  exp( 
k(  66)  =  1.1500E-11 
k(  67)  =  1.7000E-11 
k(  68)  =  2.800QE-11 
k(  69)  =  1.0000E-11 
k(  70)  =  1.0000E-11 
k(  71)  =  1.0000E-11 
k(  72)  =  6.16SOE-13  *  (T/300)**(  2.00)  *exp( 
-540.0/T) 
1.SSOOE-11  *  exp( 
k(  73)  = 
k(  74)  =  2.BOOOE-12  *  exp( 
181.0/T) 
k(  75)  =  1.9SOOE+16  *  exp(-13543.0/T) 
k(  76)  =  4.7000E-12 
k(  77)  =  1.9500E+16 
k(  78)  =  4.2000E-12 
k(  79)  =  4.2000E-12 
k(  80)  =  4.2000E-12 
k(  81)  =  4.2000E-12 
k(  82)  =  4.2000E-12 
k(  83)  =  4.2000E-12 
k(  84)  =  4.2000E-12 
k(  85)  =  4.2000E-12 
k(  86)  =  4.2000E-12 
k(  87)  =  4.2000E-12 
k(  88)  =  4.2000E-12 
k(  89)  =  4.2000E-12 
k(  90)  =  4.2000E-12 
k(  91)  =  4.2000E-12 
k(  92)  =  6.0000E-13 
k(  93)  =  1.4000E-12 
k(  94)  =  6.0000E-13 
k(  95)  =  1.4000E-12 
k(  96)  =  1.4000E-12 
k(  97)  =  2.2000E-11 
k(  98)  =  2.0000E-12  *  exp( 
k(  99)  =  1.0000E-11  *  exp( 
k(100)  =  3.2300E-11  *exp( 

*  exp(-13543.0/T) 
180. O/T) 
*  exp ( 
*  exp( 
180.0/T) 
180.0/T) 
*exp( 
180. O/T) 
*  exp ( 
180. O/Tl 
*  exp ( 
180.0/T) 
*  exp( 
*exp( 
180.0/T) 
180.0/T) 
*  exp( 
180.0/T) 
*  exp( 
180.0/T) 
*  exp( 
180.0/T) 
*  exp( 
*exp( 
180.0/T) 
180.0/T) 
*exp( 
*exp( 
180.0/T) 
*  exp(  -2058.0/T) 
*  exp(  -1900.0/T) 
*  exp(  -2058.0/T) 
*  exp(  -1900.0/T) 
*  exp (  -1900. O/Tl 

-2923.0/T) 
-1895.0/T) 
-975.0/T) 

-1280.0/T) 
-444.0/T) 

-444 .O/T) 

{4.6S309E-12} 
{9.95294E-11} 
{8.8884BE-13} 

{2.40000E-13} 
{3.80672E-01} 
{2. 74210E-13} 
{2.S9669E-12} 
{4.83334E-12} 
{1.01696E-11} 
{B.53916E-12} 
{2.88684E-11} 
{6.75269E-11} 
{6.18715E-12} 
{2.78943E-11} 
{4.00000E-11} 
{3.60000E-11} 
{9.00000E-12} 
{1.62197E-11} 
{9.85020E-13} 
{1.15000E-11} 
{1. 70000E-11} 
{2.80000E-11} 
{1.00000E-11} 
{1.00000E-11} 
{1.00000E-11} 
{1.3710SE-13} 
{2. S313 7E-12} 
{5.13974E-12} 
{3.57235E-04} 
{4.70000E-12} 
{3.S7235E-04} 
{7.68378E-12} 
{7.6837BE-12} 
{7.6837BE-12} 
{7.68378E-12} 
{7.6837BE-12} 
{7.6837BE-12} 
{7.6837BE-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.6837BE-12} 
{7.6837BE-12} 
{7.6837BE-12} 
{7.6837BE-12} 
{6.01030E-16} 
{2.38307E-15} 
{6.01030E-16} 
{2.38307E-15} 
{2.38307E-15} 
{2.20000E-11} 
{1.09940E-16} 
{1. 73099E-14} 
{1.22539E-12} 

Table 8A-9.  RADM2_CIS1  and RADM2_CIS1_AQ Mechanisms 

k(lOl)  =  1.2000E-14  *exp(  -2633.0/T) 
k(102)  =  1.3200E-14  *  exp(  -2105.0/T) 
k(l03)  =  7.2900E-15  *  exp(  -1136.0/T) 
k(104)  =  7.7000E-14  *  exp(  1300. O/T) 
k(lOS)  =  7. ?OOOE-14  *  exp<.  1300.0/T) 
k(106)  =  7.?000E-14  *exp(  1300. O/T) 
k(107)  =  7.7000E-14  *  exp(  1300. O/T) 
k(108)  =  7.7000E-14  *  exp(  1300.0/T) 
k(109)  =  7.?000E-14  *  exp(  1300.0/T) 
k(110)  =  7.7000E-l4  *  exp(  1300. O/T) 
k(111)  =  7.7000E-14  *  exp(  1300. O/T) 
k(112)  =  7.7000E-14  *  exp(  1300. O/T) 
k(113)  =  7.?000E-14  *  exp(  1300 .O/T) 
k(l14)  =  7.?000E-14  *  exp(  1300.0/T) 
7.7000E-14  *  exp(  1300. O/T) 
k(115)  = 
k(116)  =  7.7000E-l4  *  exp(  1300. O/T) 
k(117)  =  7.7000E-l4  *  exp(  1300. O/T) 
k (118)  =  1.9000E-13  *  exp( 
220.0/T) 
k(ll9)  =  1.4000E-13  *  exp( 
220.0/T) 
k(120)  =  4.2000E-14  *  exp( 
220.0/T) 
k(121)  =  3.4000E-14  *exp( 
220.0/T) 
k(122)  =  2.9000E-14  *  exp( 
220.0/T) 
k(123)  =  1.4000E-13  *  exp( 
220.0/T) 
k(l24)  =  1.4000E-13  *exp( 
220.0/T) 
k(125)  =  1.7000E-14  *  exp( 
220.0/T) 
k(126)  =  1.7000E-14  *  exp( 
220.0/T) 
k(127)  =  9.GOOOE-13  *  exp( 
220.0/T) 
k(128)  =  1.7000E-14  *  exp( 
220.0/T) 
k(129)  =  1.7000E-14  *  exp( 
220.0/T) 
k (130)  =  9.GOOOE-13  *  exp( 
220.0/T) 
k(131)  =  1.?000E-14  *  exp( 
220.0/T) 
k(132)  =  3.4000E-13  *exp( 
220. O/T) 
k(133)  =  1.0000E-13  *exp( 
220.0/T) 
k(134)  =  8.4000E-14  *  exp( 
220.0/T) 
k (135)  =  7.2000E-14  *  exp( 
220. O/T) 
k(136)  =  3.4000E-13  *  exp( 
220.0/T) 
k(137)  =  3.4000E-13  *.exp( 
220.0/T) 
k(138)  =  4.2000E-14  *  exp( 
220.0/T) 
k(139)  =  4.2000E-14  *  exp( 
220.0/T) 
k(140)  =  1.1900E-12  *  exp( 
220.0/T) 
k(141)  =  4.2000E-14  *  exp( 
220.0/T) 
k(142)  =  4.2000E-14  *  exp( 
220. O/T) 
k(l43)  =  1.1900E-12  *  exp( 
220.0/T) 
k(144)  =  4.2000E-14  *  exp( 
220.0/T) 
k(145)  =  3.GOOOE-16  *  exp( 
220. O/T) 
k(146)  =  7.?000E-14  *  exp(  1300.0/T) 
k(147)  =  1.?000E-14  *  exp( 
220.0/T) 
k(148)  =  4.2000E-14  *  exp( 
220.0/T) 
k(149)  =  3.GOOOE-16  *exp( 
220.0/T) 
k(lSO)  =  4.2000E-12  *  exp( 
180.0/T) 
k(lSl)  =  4.2000E-12  *  exp( 
180.0/T) 
k(152)  =  7.7000E-14  *  exp(  1300. O/T) 
k(153)  =  1.7000E-14  *  exp( 
220. O/T) 
k(154)  =  4.2000E~14  *  exp( 
220.0/T) 
k(lSS)  =  3.6000E-16  *  exp( 
220. O/T) 
k(156)  =  2.5400E-11  *  (T/300)**(  1.00)  *  exp( 
407.6/T) 
k(157)  =  4.2000B-12  *  (T/300)**(  1.00)  *  exp( 
181.2/T) 
k(158)  =  7.7000E-14  *  (T/300) ** (  1. 00)  *  exp(  1298.3/T) 
k(159)  =  8.4000E-14  *  (T/300)**(  1.00)  *  exp( 
221.4/T) 
k(160)  =  3.4000E-14  *  (T/300) ** (  1. 00)  *  exp( 
221.4/T) 
k(161)  =  7.8600E-15  *  (T/300)**(  1.00)  *  exp(  -1912.2/T) 

I 

I 
I 

8-72 

{1.74559B-18} 
{1.12933E-17} 
{1.6112SE-16} 
{6.04038E-12} 
{6.04038E-12} 
{6 .0403BE-J.2} '""I"" 
{6.04038E-12}  . 
{6.0403BE-12} 
{6.0403BE-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.0403BE-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.0403BE-12} 
{3.97533E-13}  I .. 
{2.92919E-13} 
{B.7875BE-14} 
{7.ll376E-14) 
{6.06762E-14} 
{2.92919E-13} 
{2.92919E-13} 
{3 .SS68BE-14}  I 
{3.55688E-14} 
{2.00859E-12} 
{3.55688E-14} 
{3.55688E-14} 
{2.00859E-12} 
{3.556BBE-14} 
{7.11376E-13} 
{2.09228E-13} 
{1.75752E-13} 
{1. 50644E-13} 
{7.11376E-13} 
{7.11376E-13} 
{B.78758E-14} 
{B.78758E-14} 
{2.48981E-12} 
{8.78758E-14} 
{B.7875BE-14} 
{2.48981E-12} 
{8.7875BE-14} 
{7.53221E-16}  J 
{6.0403BE-12} 
{3.55688E-14} 
{B.7875BE-14} 
{7.53221E-16} 
{7.6837BE-12} 
{7.6837BE-12} 
{6.0403BE-12} 
{3.55688E-14} 
{B.7875BE-14} 
{7.53221E-16} 
{9.90719E-11} 
{7.66335E·12} 
{S.9659BE-12}' 
{ 1. 75402E-13} 
{7.09961E-14} 
{1.27569E-17} 

I h ' ~ ! 11 ' 

Table 8A-9.  RADM2_CIS1  and RADM2_CISl_AQ Mechanisms 
k(l62)  =  3.6000E-11 
k(163)  =  3.0300E-12  *  (T/300)**(  1.00)  *  exp( 
-447.9/T) 
k(164)  =  4.2000E-12  *  (T/300)**(  1.00)  *  exp( 
181.2/T) 
k(l65)  =  7.7000E-14  *  (T/300)**(  1.00)  *  exp(  1298.3/T) 
k(l66)  =  8.4000E-14  *  (T/300)**(  1.00)  *  exp( 
221. 4/'l') 
k(l67)  =  3.4000E-14  *  (T/300)**(  1.00)  *  exp( 
221.4/T) 
k(l68)  =  3.3600E-ll 
k(l69)  =  4.2000E-12  *  (T/300)**(  1.00)  *  exp( 
181.2/T) 
k(170)  =  7.7000E-14  *  (T/300)**(  1.00)  *  exp(  1298.3/T) 
k(l7l)  =  8.4000E-14  *  (T/300) ** (  1. 00)  *  exp ( 
221.4/Tl 
k(l72)  =  3.4000E-14  *  (T/300)**(  1.00)  *exp( 
221.4/T) 
k(l73)  =  7.llOOE-18 
k(l74)  uses  photo  table  ACROLEIN 
k(l75)  =  l.OOOOE-15 

{3.60000E-11} 
{6.69552E-13} 
{7.66335E-12} 
{5.96598E-12} 
{1.75402E-13} 
{7.09961E-14} 
{3.36000E-ll} 
{7.66335E-12} 
{S.96598E-12} 
{1.75402E-13} 
{7.09961E-14} 
{7.llOOOE-18} 
{o.oooooE+oo} 
{ 1. OOOOOE-15} 
======c=====~=c==================================c======ne====a===========c==== 

,. 

,  scaled  by  3.60000E-03 


:1111111' 

1:1;, 

'l·,1l1:il:.1111.1:, 
II' 

l'::i• 

:'11' 

1
111,

1
!!:11 

1·

':::1111:111 

EP A/600/R-99/030 

Table 8A-10.  RADM2_CIS1~AE and RADM2_CIS1_AE_AQ Mechanisms 

I 

Reaction  List 

I' 

,,I 

· 

' 

,,,,', 

1i·: 

' 

+ 

NO 

>------------------,--------.-------------.-------------------------------------------------< 
l.}  N02 
{ 
2}  03 
{ 
3}  03 
{ 
{ 
4}  HONO 
{  S}  HN03 
{  6}  HN04 
7}  N03 
{ 
{ 
8}  N03 
{ 
9}  H202 
{  l.O}  HCHO 
{  l.l.}  HCHO 
{  l.2}  ALD 
{  l.3}  OPl. 
{  l.4}  OP2 
{  l.S}  PAA 
{  l.6}  KET 
{  l.7}  GLY 
{  l.8}  GLY 
{  l.9}  MGLY 
{  20}  DCB 
{  2l.}  ONIT 

+ 
+ 
+ 
+ 
+ 
+ 
+  l. 870*CO 
+  l.SSO*CO 
+ 
H02 
+  0.020*AC03 
+  0.800*KET 

+  hv 
+  hv; 
+  hv: 
+  hv 
+  hv: 
+  hv 
+  hv 
+  hv 
+  h'{ 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
·+  hy 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

H02 
H02 
H02 
H02 
HO 
ETHP 

co 
co 
HO 
HO 

+  0.800*H02 
co 
+ 
+ ,,, 
+ 

H02 

I  T,'70~ 

,,, 

,, 

NO 
N02 
N02 

+ 
+ 
+ 
+ 

-~> 
--> 

+ 
+ 
+ 

+ 

03P 

'11111111 

',ii, 

''I,, 

''1111 

+ 

+ 

+ 

+ 

HO 

N02 

N02 

N03 

+ 

H02 

',''11 

+ 

;1,·, 

,,,,11 

SULAER 

+  O.l.70*H02 
+  0.02S*KET 
+  0.2SO*X02 
+  0.750*X02 

t  O.Qp9*~CHO 

+ 

HC8ABR 

03P 
OlD 
03P 
HO 
HO 
H02 
NO 
N02 
2.000*HO 
co 
H02 
,.M02 
HCHO 
ALD 
M02 
AC03 
O.l30*HCHO 
0.4SO*HCHO 
AC03 
0.980*H02 
0.200*ALD 
N02 
03 
NO 
03P 
03P 

2.000*HO 
N02 
H02 
HO 
N02 
HN04 
H02 
H202 
H202 
H02 
HONO 

2.000*N02 
N03 
2.000*N02 

NO 
HN03 
Nios 
N02 

2.000*HN03 
HN03 
N03 
N02 

SULF 
H02 
M02 
ETHP 
0.830*HC3P 
+  0. 07S*ALD' 
HCSP 
HC8P 
OL2P 
OLTP 

--> 
--> 
-->' 
--> 

8-74 

+ 

+  [02] 

+  [H20] 

+  (02] 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

{  22}  03P 
{  23}  03P 
{  24}  Ol.D 
{  2S}  Ol.D 
{  26}  Ol.D 
{  27}  03 
{  28}  03 
{  29}  03 
{  30}  H02 
{  3l.}  H02 
{  32}  HN04 
{  33}  H02 
{  34}  H02 
{  3S}  H202 
{  36}  NO 
{  37}  NO 
{  38}  03 
{  39}  N03 
{  40}  N03 
{  4l.}  N03 
{  42}  N03 
{  43}  N20S 
{  44}  N20S 
{  4S}  HO 
{  46}  HO 
{  47}  HO 
{  48}  HO 
{  49}  HO 
{  so}  co  . 
{  Sl.}  HO 
{  S2}  ETH 
{  S3}  HC3 

{  S4}  HCS 
{  SS}  HC8 
{  S6}  OL2 
{  S7}  OLT 

I 

+ 
[M] 
+  N02 
+  [N2] 
+  [02] 
+  [H20] 
+  NP 
+  Hp 
+  H02 
+  NO 
+  N02 

+  H02 
+  Hp2 
+  HP 
+  Hp 
+  Np 
+  Np2 
+  NO 
+  N,02 
+  H02 
+  N02 

+  [H20] 
+  ~02 
+  HN03 
+  HN04 
I.,  I 
+  H02 
+  S02 
+  HO 

I 

+  HO 
+  HO 

+  HO 
+  HO 
+  HO 
+  ¥0 

Table 8A-10.  RADM2  CISl  AB.and RADM2  CISl  AE_AQ Mechanisms 

EPN600/R-99/030 

+ 

+ 

+ 

OLIP 
0.7SO*TOLP 

+ 
+  0.250*CSL 

OLIAER 

+  0.2SO*H02 

TOLAER 

O.B30*XYLP 

+  0.170*CSL 

+  0.170*H02 

XYLAER 

0.100*H02 

+  0.900*X02 

+  0.900*TC03 

CSLAER 
CSL 
H02 
AC03 
KETP 
H02 
AC03 
TC03 

+ 

co 

+  2.000*CO 
co 
+ 

--> 
--> 

--> 

--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 

--> 

--> 

--> 

--> 

--> 
--> 

--> 

--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
·-> . 
--> 

O.SOO*M02 
O.SOO*HC3P 
AC03 
HCHO 
HC3P 
PAN 
AC03 
TPAN 
TC03 
HCHO 
0.7SO*ALD 

+  0.036*0NIT 

0.3BO*ALD 
+  0.920*N02 
0.350*ALD 

+  0.240*0NIT 
1.600*HCHO 

+ 

+  0.200*ALD 
ALD 
N02 
H02 
+  0.100*KET 
M02 
N02 

+  0.110*MGLY 
+  2.000*X02 
N02 
+  O.J.60*GLY 
N02 
+  O,B06*DCB 
ALD 
MGLY 
HCHO 
H02 
AC03 
HN03 
HN03 
HN03 
HN03 

+  O.SOO*CSLAER 

--> 
--> 
--> 
--> 

--> 

OLN 
OLN 
OLN 
HCHO 
+  0.120*H02 

0.530*HCHO 
+  0.200*0RA1 

8-75 

{  SB}  OLI 
{  59}  TOL 

60}  XYL 

61}  CSL 

62}  CSL 
63}  HCHO 
64}  ALD 
65}  KET 
66}  GLY 
67}  MGLY 
6B}  DCB 
69}  OP1 
70}  OP2 
71}  PAA 
72}  PAN 
73}  ONIT 
74}  ACOJ 
75}  PAN 
76}  TC03 
77}  TPAN 
7B}  M02 
79}  HC3P 

BO}  HCSP 

B1}  HCBP 

B2}  OL2P 

B3}  OLTP 

+  HO 
+HO 

+  HO 

+  HO 

+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  N02 

+  N02 

+  NO 
+  NO 

+  NO 

+  NO 

+  NO 

+  NO 

B4}  CLIP 

+  NO 

BS}  AC03 
B6}  TC03 

+  NO 
+  NO 

B7}  TOLP 

BB}  XYLP 

B9}  ETHP 
90}  KETP 
91}  OLN 
92}  HCHO 
93}  ALD 
94}  GLY 
95}  MGLY 
96}  DCB 
97}  CSL 

{  9B}  OL2 
{  99}  OLT 
{100}  OLI 
{101}  OL2 

+  NO 

+  NO 

+  NO 
+  NO 
+  NO 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 

+  N03 
+  N03 
+  N03 
+  03 

{102}  OLT 

+  03 

+  O.SOO*HCHO 
+  O.SOO*ALD 

+  O.SOO*HO 
+  O.SOO*HO 

+ 
+ 

+ 

N03 
N02 

N02 

N02 
+ 
+ 
H02 
+  0.250*KET 
+  0.964*N02 
+  0.690*KET 
+  0.920*H02 
+  1.060*KET 
+  0.760*N02 
+ 
H02 

+ 

X02 

N02 

+ 
+  0.090*HCHO 
+  0.964*H02 
+  O.OBO*ONIT 

+  0.040*HCHO 
+  0.760*H02 
+ 
N02 

+ 

HCHO 

+ 

H02 

+  1.450*ALD 
N02 
+ 
+ 
N02 
+  0.920*H02 
+  O.OSO*AC03 

H02 
+ 
+  0.700*DCB 
+ 
H02 

+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 

H02 
N02 
ALD 
HN03 
HN03 
H02 
AC03 
TC03 
XN02 

+  0.2BO*HCHO 

+  O.B90*GLY 
+  0.9SO*CO 

+  0.170*MGLY 

+  0.450*MGLY 

N02 
+ 
+ 
H02 
+  2.000*N02 
co 
+ 

+  2.000*CO 
co 
+ 

+  O.SOO*CSL 

+ 
+  0.400*0RA1 

OLIAER 

+  0.420*CO 

+  O.SOO*ALD 
+  0.200*0RA2 

+  0.330*CO 
+  0.230*H02 

I 

I 

,I 

Table SA-10.  RADM2  CISl_AE and RADM2_CIS1_AE_AQ Mechanisms 

, 

,,, 

,, 

,, 

{103}  OLI 

I 

+  O~ 

{104}  H02 
{105}  H02 
{106}  H02 
{107}  H02 
{108}  H02 
{109}  H02 
{110}  H02 
{111}  H02 
{ 112}  H02 
{113}  HO~ 
{114}  H02 
{115}  H02 
{116}  H02 
{117}  H02 
{118}  M02 
{119}  M02 
{120}  M02 

{121}  M02 

{122}  M02 

{123}  M02 
{124}  M02 
{125}  M02 

{126}  M02 
{127}  M02 

I 

+  Mp2 
+  EtHP 
+  HC3P 
+  H~5P 
+  H~8P 
+  OL2P 
+  OLTP 
+  O~IP 
+  iq;:TP 
+  11f03 
+  ~pLP 
+  x,yLP 
+  T~03 
+  O;r,.N 
+  Mp2 
+  E,:rHP 
+  H~3P 

I 

+  H~5P 

I 

+  H,C8P 

+  0~2P 
+  OLTP 
+  o~IP 

+  iq;:TP 
+  AC03 

{128}  M02 

+  TOLP 

{129}  M02 

{130}  M02 

+  XYLP 

+  TC03 

{131}  M02 

+  ~LN 

{132}  ETHP 

+  AC03 

{133}  HC3P 

+  AC03 

{134}  HCSP 

{135}  HC8P 

{136}  OL2P 

+  ~C03 

I 

+  AC03 

: 

+  ~C03 

{137}  OLTP 

+  AC03 

{138}  OLIP 

+  AC03 

: 

, 

{139}  KETP 

+  ~C03 

{140}  AC03 
{141}  AC03 

, 

+  AC03 
+  IOL!? 

I 
I 

--> 

--> 
--> 
--> 
~-> 
--> 
--> 
--> 
--> 
- - > 
--> 
--> 
•-> 
--> 
--> 
--> 
--> 
--> 

--> 

--> 

--> 
--> 
--> 

--> 
--> 

--> 

--> 

--> 

+  0.220*M02 

0.180*HCHO 

+  0.230*CO 
+  0.260*H02 
+ 

OLIAER 
OP1 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
0,!?2 
P~ 
OP2 
OP2 
OP2 
ONIT 
1.500*?CHO 
0.750*HCHO 
0.840*HCHO 

+ 

+ 

+ 

H02 

0.770*HCHO 

H02 

0.800*HCHO 

H02 

1.550*HCHO 
1.250*HCHO 
0.890*HCHO 

+  0.550*KET 

0.750*HCHO 
HCHO 
+  0.500*0RA2 
HCHO 
+  0.700*DCB 
HCHO 
+  2.000*H02 

0.500*HCHO 
+  0.500*0RA2 
+  0.475*CO 

--> 

--> 

+ 

1.750*HCHO 

N02 
ALD 

+  0.500*0RA2 

--> 

--> 

--> 

0.770*ALD 
+  0.500*M02 
0.410*ALD 
+  0. SOO*M02 
0.460*ALD 
+  O.SOO*M02 

--> 

0.800*HCHO 

+  O.SOO*M02 
ALD 
+  0. SOO*M02 
0.725*ALD 
+  0.500*H02 
MGLY 
+  O.SOO*ORA2 
2.000*M02 
M02 

--> 

--> 

--> 

--> 
--> 

+  0.100*HO 
+  0.720*ALD 
+  0.060*0RA;t 
+  0.140*HO 

+  0.100*KET 
+  0.2,!l()*ORA2 
+  0.310*M02 

+ 
H02 
+ 
H02 
+  0.770*ALD 

"' 

+  0.750*ALD 
+  0.260*KBT 

+  0.410*ALD 

+  0.750*KBT 

+  0.460*ALD 

+  1.390*KET 

+  0.350*ALD 
+  0.750*ALD 
+  0.72,,5*ALD 

+ 
+ 
~ 

H02 
H02 
H02 

+  0.750*MGLY 
+  0.500*H02 

+ 
H02 
+,  O.SQP*M<!>2 

+  0.170*MGLY 
+  2.000*H02 
+  0.450*MGLY 

+  0.160*GLY 

+  0.806*DCB 

+  0.445*GLY 
+  0.025*AC03 
+ 
X02 
+  O.SOO*H02 

+  O.OSS*MGLY 
+  0.460*H02 

+ 

ALO 

+  O.SOO*H02 

+  O.SOO*M02 

+  0.260*KET 
+  O.SOO*ORA2 
+  0.7SO*KET 
+  0. SOO*ORA2 
+  1.390*KET 
+  O.SOO*OW 
+  0.600*ALD 
+  0.500*0RA2 
+  O.SOO*HCHO 
+  0. s6o•oRA2 
+  0.550*KET 
+  0.500*M02 
+  O.SOO*H02 

+  0.500*H02 

+  O.SOO*H02 

+  O.SOO*H02 

+  O.SOO*H02 

+  O.SOO*H02 

+  O.l40*HCHO 
+  O.SOO*ORA2 
+  O.SOO*M02 

+  O.l70*MGLY 

+  O.l60*GLY 

Table 8A-10.  RADM2  CISl  AE and RADM2  CIS 1 _AE_AQ Mechanisms 

{J.42}  AC03 

+  XYLP 

{J.43}  AC03 

+  TC03 

{J.44}  AC03 

+  OLN 

+  OLN 
{ J.45}  OLN 
+  H02 
{ J.46}  X02 
+  M02 
{J.47}  X02 
+  AC03 
{ J.48}  X02 
+  X02 
{J.49}  X02 
+  NO 
{J.50}  X02 
+  N02 
{J.51}  XN02 
+  H02 
{J.52}  XN02 
+  M02 
{J.53}  XN02 
+  AC03 
{J.54}  XN02 
+  XN02 
{155}  XN02 
+  HO 
{J.56}  TERP 
+  N03 
{J.57}  TERP 
+  03 
{J.58}  TERP 
+  HO 
{J.59}  ISO 
{J.60}  ISO_R02  +  NO 

{J.61}  ISO_R02  +  H02 
{J.62}  ISO_R02  +  AC03 

{J.63}  ISO_R02  +  M02 
{J.64}  ISO 

+  03 

{J.65}  ISO 

+  03P 

{J.66}  ISO 
{J.67}  ISON_R02  +  NO 

+  N03 

{J.68}  ISON_R02  +  H02 
{J.69}  ISON_R02  +  AC03 

{J.70}  ISON_R02  +  M02 

{J.71}  ISOPROD  +  HO 
+  NO 
{J.72}  IP_R02 

{J.73}  IP_R02 
{J.74}  IP_R02 

+  H02 
+  AC03 

{J.75}  .IP_R02 

+  M02 

{J.76}  ISOPROD  +  03 

{J.77}  ISOPROD  +  hv 

{J.78}  ISOPROD  +  N03 

+  0.700*DCB 
M02 
H02 
M02 

+ 

+  O.J.J.O*MGLY 
+  2.000*X02 

+ 

HCHO 
N02 

2.000*HCHO 

OP2 
HCHO 
M02 

N02 
ONIT 
OP2 
HCHO 
M02 

+ 
+  0.450*MGLY 

H02 

+  0.806*DCB 

+  0.920*H02 
+  0.050*AC03 

+  0.890*GLY 
+  0.950*CO 

+ 
ALD 
+  0.500*M02 
+  2.000*ALD 

+ 

H02 

+  0.500*0RA2 

+  2.000*N02 

+ 

H02 

TERPAER  + 
TERPAER  + 
TERPAER  + 
ISO  R02  +  0.079*X02 
+  0'. 9J.2*N02 

HO 
N03 
03 

0.088*0NIT 

+  0.9l.2*H02 

+  0.9J.2*ISOPROD  +  0.629*HCHO 

OP2 
0.500*H02 

+ 

ISOPROD 

+  0.SOO*M02 

+  0.500*0RA2 

0.500*HCHO 
0.600*HCHO 

+  O.SOO*H02 
+  0.650*ISOPROD  +  0.390*0RAJ. 
+  0. 070*H02 
+  0.200*AC03 
0.750*ISOPROD  +  0.250*AC03 

+  0.070*CO 
+  O.J.50*ALD 
+  0.250*HCHO 

+  0.270*HO 
+  0.200*X02 

+ 

ISOPROD 

+  0.800*ALD 
+  0.200*ISOPROD  +  0.200*N02 

+  0.800*0NIT 

+  0.500*M02 
+ 
+  0.500*H02 

ONIT 

+  0.500*IP  R02 
+ 
+  0.250*HCHO 
+  0.630*KET 

H02 

+  0.500*M02 
+  0.500*KET 
+  0.500*H02 

+  O.J.OO*H02 
+  0.070*X02 
+  0.020*ALD 
+  0.090*KET 
+  0.333*H02 
+  0.333*CO 

+  0.075*HN03 
+  0.925*0NIT 
+  0.925*X02 

+  0.500*0RA2 

+ 

ALD 

+  0.200*X02 
+  0.590*CO 
+  0.080*GLY 

+  0.500*0RA2 

+  O.SOO*ALD 

+  O.J.J.4*AC03 
+  O.J.55*CO 
+  0.010*GLY 
+  0.462*0RAJ. 
+  0.700*M02 
+  0.067*ALD 

+  0.643*CO 
+  0.282*ALD 

--> 

--> 

--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 
--> 

--> 
--> 

--> 

--> 
--> 

--> 
--> 

--> 

--> 
--> 

--> 
--> 

+  0.250*M02 

ISON_R02 
N02 
+  0.800*H02 

ONIT 

0.500*H02 
ALD 

0.500*HCHO 
ONIT 
0.500*AC03 

+ 

+ 

N02 
+  0.550*ALD 
+  0.340*MGLY 

OP2 
0.500*H02 
+  0.500*ALD 

--> 

0.500*HCHO 

--> 

--> 

--> 

+  O.SOO*KET 
0.268*HO 
+  0.054*M02 
+  O.J.46*HCHO 
+  0.850*MGLY 
0.970*AC03 
+  0.200*HCHO 
+  0.033*KET 
0.075*AC03 
+  0,282*HCHO 
+  0.925*H02 

8-77 

1,,,, 

EPA/600/R-99/030 

Table 8A-10.  RADM2_CIS1_AE and RADM2_CISl_AE_AQ Mechanisms 
>----------------------------------------------------------------------------------------< 

Rate  Expression 

Rate  Constant 

========~====~c====c==========================c===================c=====m====== 

,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 

l.OOOOOE+OO 
l.OOOOOE+OO 
l.OOOOOE+OO 
l.OOOOOE+OO 
l.OOOOOE+OO 
l.OOOOOE+OO 
l.OOOOOE+OO 
l.OOOOOE+OO 
1.00000E+OO 
1.00000E+OO 
1.oooooE+oo 
1. OOOOOE+OO 
l.OOOOOE+OO 
l.OOOOOE+OO 
l.OOOOOE+OO 
l.OOOOOE+OO 
l. OOOOOE+OO 
l.OOOOOE+OO 
1.00000E+OO 
1.00000E+OO 
1.00000E+OO 

k( 
l)  uses  photo  table  N02_RADM88 
k( 
2)  uses  photo  table  03010  RADM88 
k(  3)  uses  photo  table  0303P-RADM88 
k(  4)  uses  photo  table  HONO_RAoM88 
5)  uses  photo  table  HN03_RADM88 
k( 
6)  uses  photo  table  HN04_RADM88 
k( 
k( 
7)  uses  photo  table  N03NO  RADM88 
8)  uses  photo  table  N03N02_RADM88 
k( 
k( 
9)  uses  photo  table  H202_RADM88 
k(  10)  uses  photo  table  HCHOmol_RADM88 
k(  11)  uses  photo  table  HCHOrad_RADM88 
k(  12)  uses  photo  table  ALD_RADM88 
k(  13)  uses  photo  table  MHP_RADM88 
k(  14)  uses  photo  table  HOP_RADM88 
k(  15)  uses  photo  table  PAA_RADM88 
k(  16)  uses  photo  table  KETONE_RADM88 
k(  17)  uses  photo  table  GLYform_RADM88 
k(  18)  uses  photo  table  GLYmol_RADM88 
k(  19)  uses  photo  table  MGLY_RADM88 
k(  20)  uses  photo  table  UDC_RADM88 
k(  21)  uses  photo  table  ORGNIT  RADM88 
k(  22)  =  6.0000E-34  *  (T/300);*(-2.30) 
k(  23)  =  6.5000E-l2  *exp( 
120.0/T) 
k(  24)  = 
l.8000E-11  *  exp( 
110.0/T) 
k(  25)  =  3.2000E-11  *exp( 
70.0/T) 
k(  26)  =  2.2000E-10 
k(  27)  =  2.0000E-12  *  exp(  -1400.0/T) 
k(  28)  =  1.6000E-12  *  exp( 
-940.0/T) 
k(  29)  =  1.lOOOE-14  *  exp( 
-500.0/T) 
k(  30)  =  3.?000E-12  *exp( 
240.0/T) 
k(  31)  is  a  falloff  expression  using: 

=  l.0oooi-31  *  (T/300)**(-3.20) 
kO 
kinf  =  4.7000E~12  *  (T/300)**(-1.40) 
F  =  0.60, 

n  =  1:00 

•• 

I 

, 

k(  32)  =  k(  31)  I  Keq,  where  Keq  =  2.100E-27  *  exp(  10900.0/T) 
k(  33)  is  a  special  ·rate  expression  of  the  form:. 

k  = kl  +  k2 [MJ,'  where 
··· 
kl  =  2.2000E-13  *  exp( 
k2  = 
l.9000E-33  *  exp( 

·· 

620.0/T) 
980.0/T) 

k(  34)  is  a  special  rate  expression  of  the  form: 

k  =kl+  k2[M],'  where 
kl  =  3.0800E-34  *  exp(  2820.0/T) 
k2  =  2.6600E-54  *  exp(  3180.0/T) 
k(  35)  =  3.3000E-12  *exp( 
-200.0/T) 
k(  36)  is  a  falloff  expression  using: 

0.60, 

n  =  1.00 

• .  

ko  =  7.oopoE-31  *  (~/300)**(-2.60) 
kinf  ~  1.5000~-11 *  (~/300)**(-0.50) 
F  = 
k(  37) 
k(  38) 
k(  39) 
k(  40) 
k(  41) 
k(  42) 
2.2000E-3o  *  (T/300)**(-4.30) 
kO 
kinf  =  1.5000E-12  *  (T/300)**(-0.50) 

3.3000E-39  *  exp( 
l.4000E-13  *  exp( 
l.?OOOE-11  *  exp( 
2.5000E-14  *exp( 
2.5000E-12 

530.0/T) 
-2500.0/T) 
150.0/T) 
-1230.0/T) 

is  a  falloff  expression  using: 

I 

8-78 

!11! 

I 

{O.OOOOOE+OO} 
{o.oooooE+oo} 
{o.oooooE+oo} 
{o.oooooE+oo} 
{o.oooooE+oo} 
{o.oooooE+oo} 
{0.00000E+OO} 
{0.00000E+OO} 
{O.OOOOOE+OO} 
{O.OOOOOE+oo} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{ 0. OOOOOE+OO} 
{O.OOOOOE+oo} 
{O.OOOOOE+OO} 
{o.oooooE+oo} 
{O.OOOOOE+OO} 
{0.00000E+OO} 
{O.OOOOOE+OO} 
{o.oooooE+oo} 
{O.OOOOOE+oo} 
{6.09302E-34} 
{9.72293E-12}' 
{2.60365E-11} 
{4.04730E-11} 
{2.20000E-10} 
{l. 82272E-14} 
{6.82650E-14} 
{2.05452E-15} 
{8.27883E-12} 
{l.39058E-12} 

{8.62399E-02} 
{3.01634B-l2} 

{6.78905E-30} 

{l.6867lE-12} 
{4.87144E-l2} 

{l.95397E-38} 
{3.18213E-l7} 
{2.81225E-1l} 
{4.03072E-l6} 
{2.50000E-l2} 
{l.26440E-l2} 

Table 8A-10.  RADM2  CISl_AE and RADM2_CIS1_AE_AQ Mechanisms 

EP A/600/R-99/030 

F  =  0.60, 

n  =  1.00 

k(  43)  = k(  42)  I  Keq,  where  Keg  =  l.lOOE-27  *  exp(  11200.0/T) 
k(  44)  =  2.0000E-21 
k(  4S)  is  a  falloff  expression  using: 

k(  46)  is  a  special  rate  expression  of  the  form: 

n  =  1.00 

=  2.6000E-30  *  (T/300)**(-3.20) 
kO 
kinf  =  2.4000E-ll  *  (T/300)**(-1.30) 
F  =  0.60, 
k  = kO  +  {k3[M) 
kO  =  7.2000E-1S  *  exp( 
78S.O/T) 
k2  =  4.lOOOE-16  *  exp(  1440.0/T) 
k3  =  1.9000E-33  *  exp( 
72S.O/Tl 
k(  47)  =  l.3000E-12  *  exp( 
380.0/T) 
k(  48)  =  4.6000E-11  *  exp( 
230.0/T) 
k(  49)  is  a  falloff  expression  using: 

/ 

(1  +  k3[MJ/k2J},  where 

0.60, 

n  =  1.00 

256.0/T) 
-74S.O/T) 

-S40.0/T) 
-380.0/T) 
-380.0/T) 
411.0/T) 
S04.0/T) 
S49.0/T) 
322.0/T) 
116.0/T) 

kO  =  3.0000E-31  *  (T/300)**(-3.30) 
kinf  =  1.SOOOE-12  *  (T/300)**(  0.00) 
F  = 
k(  SO) 
k(  Sl) 
k(  S2) 
k(  S3) 
k(  S4) 
k(  SS) 
k(  S6) 
k(  S7) 
k(  S8) 
k(  S9) 
k(  60) 
k(  61) 
k("  62) 
k(  63) 
k(  64)  = 
k(  6S) 
k(  66) 
k(  67) 
k(  68) 
k(  69) 
k(  70) 
k(  71) 
k(  72) 
k(  73) 
k(  74) 
k(  7S) 
k(  76) 
k(  77) 
k(  78) 
k(  79) 
k(  80) 
k(  81) 
k(  82) 
k(  83)  = 
k(  84) 
k(  8S) 
k(  86) 
k(  87) 
k(  88) 
k(  89)  = 
k(  90)  = 
k(  91) 
k(  92) 

l.SOOOE-13  *  (1.0  +  0.6*Pressure) 
2.8300E+Ol  *  (T/300)**(  2.00)  *  exp(  -1280.0/T) 
1.2330E-12  *  (T/300)**(  2.00)  *exp( 
-444.0/T) 
1.S900E-11  *  exp( 
1.7300E-11  *  exp( 
3.6400E-11  *exp( 
2.lSOOE-12  *  exp( 
S.3200E-12  *  exp( 
l.0700E-ll  *  exp( 
2.lOOOE-12  *  exp( 
1.8900E-ll  *  exp( 
4.0000E-11 
9.0000E-01  *  k(  61) 
9.0000E-12 
6.8700E-12  *  exp( 
l.2000E-11  *  exp( 
l.lSOOE-11 
l.7000E-11 
2.8000E-11 
1.0000E-11 
1. OOOOE-11 
1. OOOOE-11 
6.16SOE-13  *  (T/300)**(  2.00)  *exp( 
-540.0/Tl 
l.5500E-ll  *  exp( 
2.8000E-12  *  exp( 
181.0/T) 
l.9500E+l6  *  exp(-13S43.0/Tl 
4.7000E-12 
l.9500E+16  *  exp(-13S43.0/T) 
180.0/T) 
4.2000E-12  *  exp( 
4.2000E-12  *  exp( 
180.0/T) 
180.0/T) 
4.2000E-12  *  exp( 
180.0/T) 
4.2000E-12  *  exp( 
4.2000E-12  *  exp( 
180.0/T) 
180.0/T) 
4.2000E-12  *  exp( 
4.2000E-12  *  exp( 
180.0/T) 
180.0/Tl 
4.2000E-12  *  exp( 
180.0/Tl 
4.2000E-12  *  exp( 
4.2000E-12  *  exp( 
180.0/T) 
4.2000E-12  *  exp( 
180.0/T) 
180.0/T) 
4.2000E-12  *  exp( 
180.0/T) 
4.2000E-12  *  exp( 
4.2000E-12  *  exp( 
180.0/T) 
6.0000E-13  *  exp(  -2058.0/T) 

-444.0/T) 

{S.47034E-02} 
{2.00000E-21} 
{1.14885E-11} 

{1.47236E-13} 

{4.65309E-12} 
{9.95294E-ll} 
{8.88848E-13} 

{2.40000E-13} 
{3.80672E-01} 
{2.74210E-13} 
{2.59669E-12} 
{4.83334E-12} 
{1.01696E-11} 
{8.S3916E-12} 
{2.88684E-ll} 
{6.75269E-11} 
{6.18715E-12} 
{2.78943E-11} 
{4.00000E-11} 
{3.60000E-11} 
{9.00000E-12} 
{l.62197E-11} 
{9.8S020E-13} 
{l.15000E-11} 
{1. 70000E-11} 
{2.80000E-11} 
{1.00000E-11} 
{l.OOOOOE-11} 
{1.00000E-11} 
{1.3710SE-13} 
{2.53137E-12} 
{S.13974E-12} 
{3.57235E-04} 
{4.70000E-12} 
{3.57235E-04} 
{7.68378E-:12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7,68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{6.01030E-16} 

Table 8A-10.  RADM2_CIS1_AE and RADM2_CISI_AE_AQ Mechanisms 
k(  93)  =  1.4000E-12  *  exp(  -1900.0/T) 
k(  94)  =  6.0000E-13  *  exp(  -2058.0/T) 
k(  95)  =  1.4000E-12  *  exp(  -1900.0/T) 
k(  96)  =  1.4000E-12  *  exp(  -1900.0/T) 
k(  97)  =  2.2000E-11 
k(  98)  =  2.0000E~12  *  exp(  -2923.0/T) 
k(  99)  =  1.0000E-11  *  exp(  -1895.0/T) 
k(100)  =  3.2300E-11  *  exp( 
-975.0/T) 
k(101)  =  1.2000E~14  *  exp(  -2633.0/T) 
k(102)  =  l.3200E-14  *  exp(  -2105.0/T) 
k(103)  =  7.2900E-15  *  exp(  -1136.0/T) 
k(104)  =  7.7000E-14  *  exp(  1300. O/T) 
k(105)  =  7.7000E-14  *  exp(  1300. O/T) 
k(l06)  =  7.7000E-14  *  exp(  1300. O/T) 
k(l07)  =  7. 7000E-14  *  exp(·  1300.0/T) 
k(108)  =  7.7000E-14  *  exp(  1300. O/T) 
k(109)  =  7.7000E~l4  *  exp(  1300. O/T) 
k(llO)  =  7.7000E-14  *  exp(  1300.0/T) 
k(111)  =  7.7000E-14  *  exp(  1300 .O/T) 
k(l12)  =  7.7000E~14  *  exp(  1300. O/T) 
k(l13)  =  7.7000E-14  *  exp(  1300. O/T) 
k(l14)  =  7.7000E-14  *  exp(  1300. O/T) 
k(l15)  =  7.7000E-14  *  exp(  1300.0/T) 
k(l16)  =  7.7000E-14  *  exp(  1300. O/T) 
k(l17)  =  7.7000E-14  *  exp(  1300.0/T) 
k(l18)  = 
220.0/T) 
l.9000E-13  *  exp( 
k (l.19)  =  1.4000E-13  *  exp( 
220. O/T) 
k(120)  =  4.2000E-14  *  exp( 
220.0/T) 
k(121)  =  3.4000E-14  *  exp( 
220.0/T) 
k(122)  =  2.9000E-14  *  exp( 
220.0/T) 
k(l23)  = 
220,0/T) 
l.4000E-13  *  exp( 
k(124)  =  1.4000E-13  *  exp( 
220.0/T) 
220.0/T) 
k(125)  =  1.7000E-14  *  exp( 
k(l26)  =  1.7000E-14  *  exp( 
220.0/T) 
k(l27)  =  9.GOOOE-13  *  exp( 
220. O/T) 
k(128)  =  1.7000E-14  *exp( 
220.0/T) 
k(129)  =  1.7000E-14  *  exp( 
220.0/T) 
k(130)  =  9.6000E-13  *  exp( 
220.0/T) 
220.0/T) 
k (131)  =  1.7000E-14  *  exp( 
k(132)  =  3.4000E-13  *exp( 
220.0/T) 
k (133)  =  1.0000E-13  *  exp( 
220.0/T) 
k(l34)  =  8.4000E-14  *  exp( 
220.0/T) 
k(135)  =  7.2000E-14  *  exp( 
220.0/T) 
k(136)  =  3.4000E-13  *  exp( 
220.0/T) 
k(137)  =  3.4000E-13  *  exp( 
220.0/T) 
k(l38)  =  4.2000E-14  *  exp( 
220.0/T) 
k(139)  =  4.2000E-14  *  exp( 
220.0/T) 
k(l40)  =  1.1900E-12  *  exp( 
220 .O/T) 
k(141)  =  4.2000E-14  *  exp( 
220. O/T) 
k(l42)  =  4.2000E-14  *  exp( 
220 .O/T) 
k(143)  =  1.1900E-12  *  exp( 
220. O/T) 
k(l44)  =  4.2000E-14  *  exp( 
220 .O/T) 
k(145)  =  3.6000E~16  *exp( 
220.0/T) 
k(146)  =  7.7000E-14  *  exp(  1300.0/T) 
k(147)  =  1.7000E-14  *  exp( 
220.0/T) 
k(148)  =  4.2000E~14  *  exp( 
220.0/T) 
220.0/T) 
3.GOOOE-16  *exp( 
k(149)  = 
k(150)  =  4.2000E-12  *  exp( 
180.0/T) 
k(151)  =  4.2000E-12  *  exp( 
180.0/T) 
k(152)  =  7.7000E-14  *  exp(  1300.0/T) 
k(153)  =  l.7000E-14  *  exp( 
220.0/T) 

{2.38307E-15} 
{6.01030E-16} 
{2.38307E-15} 
{2.38307E::,15} 
{2.20000E-11},, 
{1.09940E-16} 
{1.73099E-14} 
{1.22539E-12} ,, 
{1.74559E-18} 
{1.12933E-17} 
{1.61125E-16} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12}"'' 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.040J8E-12} 
{3.97533E-13} 
{2.92919E-13}, 
{ 8. 78758E-14} ·• 
{7.l.1376E-14} "' 
{ 6. 06762E-14}  ' 
{2.92919E-13} 
{2.92919E-13} 
{3. 55688E-14}  1
{3.55688E-14} 
{2.00859E-12} ··' 
{3 .55688E-14}  !" 
{3.55688E-14} 
{2.00859E-12} 
{3.55688E-14} 
{7.11376E-13} 
{2.09228E-13}  ,. 
{1.75752E-13} 
{1.50644B-13} 
{7.l.1376E-13} 
{7.l.1376E-13} 
{8.78758E-14} 
{8.78758E-14} 
{2.48981E-12} 
{8.78758E-14}' 
{8.78758E-14} 
{2.48981E-12} 
{8.78758E-14} 
{7.53221E-16} 
{6.04038E-12}, 
{3 .55688E-14}  · 
{8.78758E-14} 
{7.53221E-16} 
{7.68378E·12} 
{7.68378E-12} 
{6.04038E-12} 
{3.55688E-14} 

" 

Table 8A-10.  RADM2  CISl~AE and RADM2~CISl_AE_AQ Mechanisms 

EP A/600/R-99/030 

{8.78758E-14} 
{7.53221E-16} 
{6.75269E-11} 
{1.22539E-12} 
{1.61125E-16} 
{9.90719E-11} 
{7.66335E-12} 
{5.96598E-12} 
{1.75402E-13} 
{7.09961E-14} 
{1.27569E-17} 
{3.60000E-11} 
{6.69552E-13} 
{7.66335E-12} 
{5.96598E-12} 
{1. 75402E-13} 
{7.09961E-14} 
{3 .36000E-11} 
{7.66335E-12} 
{5.96598E-12} 
{1. 75402E-13} 
{7.09961E-14} 
{7.llOOOE-18} 
{O.OOOOOE+OO} 
{1.00000E-15} 

220.0/T) 
220.0/T) 

4.2000E-14  *  exp( 
3.6000E-16  *exp( 
l.OOOOE+OO  *  k(  58) 
l.OOOOE+OO  *  k(lOO) 
l.OOOOE+OO  *  k(l03) 
2.5400E-ll  *  (T/300)**( 
4.2000E-12  *  (T/300)**( 
7.7000E-14  *  (T/300)**( 
8.4000E-14  *  (T/300)**( 
3.4000E-14  *  (T/300)**( 
7.8600E-15  *  (T/300)**( 
3.6000E-11 

k(l54) 
k(l55) 
k(l56) 
k(l57) 
k(l58) 
k(l59) 
k(l60) 
k(l61) 
k(l62) 
k(l63) 
k(l64) 
k(l65) 
k(l66)  =  3.0300E-12  * 
k(l67) 
4.2000E-12  * 
k(l68)  =  7.7000E-14  * 
k(l69)  =  8.4000E-14  * 
k(l70) 
3.4000E-14  * 
k(l71) 
3.3600E-11 
k(l72) 
4.2000E-12  * 
k(l73) 
7.7000E-14  * 
k(l74) 
8.4000E-14  * 
k(l75) 
3.4000E-14  * 
k(l76) 
7.llOOE-18 
k(l77)  uses  photo  table  ACROLEIN 
k(l78)  =  l.OOOOE-15 

(T/300) ** ( 
(T/300) ** ( 
(T/300) ** ( 
(T/300) ** ( 
(T/300) ** ( 

(T/300) ** ( 
(T/300) ** ( 
(T/300) ** ( 
(T/300) ** ( 

1. 00) 
1. 00) 
1. 00) 
1. 00) 
1. 00) 
1.00) 

*  exp( 
*  exp( 
*  exp( 
*  exp( 
*  exp( 
*  exp( 

407.6/T) 
181.2/T) 
1298.3/T) 
221.4/T) 
221.4/Tl 
-1912.2/Tl 

1. 00) 
1. 00) 
1. 00) 
1. 00) 
1. 00) 

*  exp( 
*  exp( 
*  exp( 
*  exp( 
*  exp( 

-447.9/Tl 
181.2/T) 
1298.3/T) 
221.4/T) 
221.4/T) 

1.00) 
1.00) 
1.00) 
1.00) 

*  exp( 
*  exp( 
*  exp( 
*  exp( 

181.2/T) 
1298.3/T) 
221.4/T) 
221.4/T) 

,  scaled  by  3.60000E-03 

Table 8A-1 l.  RADM2_CIS4 and RADM2_CIS4_AQ Mech~~ms 

I 

R~action List 

I 

..  .  .. 

. 

. 

. 

. 

., 

. 

., 

+ 

NO 

+ 
+ 
+ 

. 
.. ,., 

>------------~---~--------------------------·-~-----------------~------------------------< 
l}  N02 
{ 
2 }  03 
{ 
3}  03 
{ 
4}  HONO 
{ 
s}  HN03 
{ 
6}  HN04 
{ 
{ 
7}  N03 
{  a}  N03 
{ 
9}  H202 
{  10}  HCHO 
{  11}  HCHO. 
{  12}  ALD 
{  13}  OPl 
{  14}  OP2 
{  lS}  PAA 
{  16}  KET 
{  17}  GLY 
{  18}  GLY 
{  19}  MGLY 
{  20}  DCB 
{  21}  ONIT 

+ 
+ 
+ 
+ 
+ 
+ 
+  1.s1o•co 
+  i.sso•co 
+ 
H02 
+  0.020*AC03 
+  O.SOO*KET 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
-~> 
--> 
--> 
--> 
--> 
--> 
--> 

+  0.80D*H02 
co 
+ 
+ 
TC03 
+ 
H02 

H02 
H02 
H02 
H02 
HO 
BTHP 

co 
co 
HO 
HO 

NO 
N02 
N02 

+ 
+ 
+ 
+ 

03P 

+ 

1 1':11, 

03P 
010 
03P 
HO 
HO 
H02 
NO 
N02 
2.ooo•Ho 
CO 
H02 
M02 
HCHO 
ALO 
~02 
AC03 
0.130*HCHO 
0.4SO•HCHO 
AC03 
0.980*H02 
0.200*ALD 
N02 
03 
NO 
,03P 
03P 

+ 

+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  ~v 
+  hv 
+  hv 
+  ~v 
+  ~v 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
' 
+ 
[ 1
M] 
I 
+  N02 
+  cl:.i21 
+  c'o2J 
+  cli201 
+  N,O 
+  HO 
+  Jio2 
I 
+  NO 
I 
+  N02 

I 

I 

I 

I 

I 

+  H02 
+  H02 
+  HO 
+  HO 
+  NO 
+  N02 
+  NO 
+  N02 
+  H02 
+  N02 

I 

I 

I 

I 

+  [H20] 
+  N02 
+  HN03 
+~Oji 
+  H02 
+  S02 
+  HO 
I 
+  Hp 
+  Hp 
+  Hp 
+  HO 
I 
+  HO 
+  llf 
I 

I 

{  22}  03P 
{  23}  03P 
{  24}  010 
{  2S}  OlD 
{  26}  OlD 
{  27}  03 
{  28}  03 
{  29}  03 
{  30}  H02 
{  31}  H02 
{  32}  HN04 
{  33}  H02 
{  34}  H02 
{  3S}  H202 
{  36}  NO 
{  37}  NO 
{  38}  03 
{  39}  N03 
{  40}  N03,, 
{  41}  N03 
{  42}  N03 
{  43}  N20S 
{  44}  N20S 
{  4S}  HO 
{  46}  HO 
{  47}  HO 
{  48}  HO 
{  49}  HO 
{  so}  co 
{  Sl}  HO 
{  S2}  ETH 
{  S3}  HC3 

{  S4}  HCS 
{  SS}  HC8 
{  S6}  OL2 
{  S7}  OLT 

+  [02] 

+  [H20] 

+  [02] 

--> 
--> 
--> 
--> 
--> 
-•> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
-·> 
--> 
--> 
·-> 
--> 
--> 

--> 
--> 
--> 
--> 

2 •. 000*HO 

N02 
H02 
HO 
N02 
HN04 
H02 
H202 
H202 
H02 
HONO 

2.000*N02 
N03 
2.000*N02 
~o 
HN03 
N20S 
N02 

2.000*HN03 
HN03 
N03 
N02 

SULF 
802 
M02 
ET.l:lP 
0.830•HC3P 
+  0.075*ALD 
,.  HcSP 
HC8P 
OL2P 
OLTP 

8-.82 

+ 

+ 

+ 

+ 

HO 

N02 

N02 

N03 

'11111 

+ 

H02 

+  O.l.70*H02 
+  0.02S*KET 
+  o.2so•xo2 
+  <i.7SO*X02 

+  0.009*HCHO 

11111":1 

+  0.250*CSL 
+  0.170*CSL 
+  0.900*X02 

+ 

co 

--> 
--> 
--> 
--> 
--> 
--> 

Table 8A-1 l.  RAD1:f2_CIS4. and RADM2_CIS4_AQ Mechanisms: 
{  58}  OLI 
{  59}  TOL 
{  60}  XYL 
{  61}  CSL 
{  62}  CSL 
{  63}  HCHO 
{  64}  ALD 
{  65}  KET 
{  66}  GLY 
{  67}  MGLY 
{  68}  DCB 
{  69}  OP1 
{  70}  OP2 
{  71}  PAA 
{  72}  PAN 
{  73}  ONIT 
{  74}  AC03 
{  75}  PAN 
{  76}  TC03 
{  77}  TPAN 
{  78}  M02 
{  79}  HC3P 

OLIP 
0.750*TOLP 
0.830*XYLP 
0.100*li02 
CSL 
H02 
AC03 
I<ETP 
H02 
AC03 
TC03 
0.500*M02 
0.500*HC3P 
AC03 
HCHO 
HC3P 
PAN 
AC03 
TPAN 
TC03 
HCHO 
0.750*ALD 

+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  HO 
+  N02 

--:~ 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--::· 
--> 
--::-

+  2.000*CO 
co 
+ 

+  N02 

+  NO 
+  NO 

+ 
+ 

+ 

+  0.500*HCHO 
+  0.500*ALD 

N03 
N02 

N02 

N02 
+ 
+ 
H02 
+  0.250*KET 
+  0.964*N02 
+  0.690*I<ET 
+  0.920*H02 
+  1.060*I<ET 
+  0.760*N02 
+ 
H02 

EP A/600/R·99/030 

+  0.250*H02 
+  0.170*H02 
+  0.900*TC03 

+  0.500*HO 
+  0.500*HO 

+ 

X02 

N02 

+ 
+  0.090*HCHO 
+  0.964*H02 
+  O.OSO*ONIT 

+  0.040*HCHO 
+  0.760*H02 
+ 
N02 

so}  HC5P 

81}  HCSP 

82}  OL2P 

83}  OLTP 

84}  OLIP 

85}  AC03 
86}  TC03 

87}  TOLP 

88}  XYLP 

{  89}  ETHP 
{  90}  I<ETP 
{  91}  OLN 
{  92}  HCHO 
{  93}  ALD 
{  94}  GLY 
{  95}  MGLY 
{  96}  DCB 
{  97}  CSL 
{  98}  OL2 
{  99}  OLT 
{100}  OLI 
{101}  OL2 

+  NO 

+  NO 

+  NO 

+  NO 

+  NO 

+  NO 
+  NO 

+  NO 

+  NO 

+  NO 
+  NO 
+  NO 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  N03 
+  03 

{102}  OLT 

+  03 

{103}  OLI 

+  03 

--> 

--> 

- -> 

--> 

--> 

--> 
--> 

--:-

--:-

--:-
--:-
--> 
--:-
--:-
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 

--> 

+  0.036*0NIT 

·0.380*ALD 
+  0.920*N02 
0.350*ALD 

+ 

+  0.240*0NIT 
1. 600*HCHO 
+  0.200*ALD 
ALD 
N02 
H02 
+  0.100*KET 
M02 
N02 

+  0.110*MGLY 
+  2.000*X02 
N02 
+  0.160*GLY 
N02 
+  ·o.S06*DCB 
ALD 
MGLY 
HCHO 
H02 
AC03 
HN03 
HN03 
HN03 
HN03 
OLN 
OLN 
OLN 
HCHO 
+  0.120*H02 

0.530*HCHO 
+  0.200*0RA1 
+  0.220*M02 

0.180*HCHO 

+·  ·0.230*CO 
+  ·a. 260*H02 

8~83 

+ 

HCHO 

+ 

H02 

+  1.450*ALD 
+ 
N02 
+ 
N02 
+  0.920*H02 
+  0.050*AC03 

+ 
H02 
+  0.700*DCB 
+ 
H02 

+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 

H02 
N02 
ALD 
HN03 
HN03 
H02 
AC03 
TC03 
XN02 

+  0.280*HCHO 

+  0.890*GLY 
+  0.950*CO 

+  0.170*MGLY 

+  0.450*MGLY 

+ 
N02 
+ 
H02 
+  2.000*N02 
co 
+ 

+  2.000*CO 
co 
+ 

+  0.500*CSL 

+  0.400*0RA1 

+  0.420*CO 

+  0.500*ALD 
+  0.200*0RA2 
+  0.100*HO 
+  0.720*ALD 
+  0.060*0RA1 
+  0.140*HO 

+  0.330*CO 
+  0.230*H02 

+  0.100*I<ET 
+  0.290*0RA2 
+  0. 310*M02 

Table 8A-1 l.  RADM2  CIS4 and RADM2  CIS4  AQ Mecl!am,~ros , 

I 

Ii' 

1

11,:1

: 

+ 
H02 
+ 
H02 
+  0.770*ALD 

+  0.750*ALD 
+  0.260*KET 

+  0.410*ALD 

+  0.750*KET 

O.BOO*HCHO 

+  0.460*ALD 

{104}  H02 
{105}  H02 
{106}  H02 
{107}  H02 
{108}  H02 
{109}  H02 
{110}  H02 
{111}  H02 
{112}  H02 
{113}  H02 
{114}  H02 
{115}  H02 
{116}  H02 
{117}  H02 
{118}  M02 
{119}  M02 
{120}  M02 

{121}  M02 

{122}  M02 

{123}  M02 
{124}  M02 
{125}  M02 

: {126}  M02 
.{127}  M02 

{128}  M02 

{129}  M02 

{130}  M02 

,, 

I 

I 

J 
,11, 

' 
+  M02 
+  ETHP 
+  ¥c3P 
+  HC5P 
+  HCBP 
+  OL2P 
+  ?LTP 
+  OLIP 
+  KETP 
+  AC03 
+  TOLP 
+  XYLP 
+  TC03 
+  OLN 
+  ~02 
+  ~THP 
+  HC3P 

'I''' 

I 

I 

., 

,, 

:1 

I 

I 

I 

+  HCSP 

I 

+  ¥CSP 

I 

+  ?L2P 
+  ?LTP 
+  ?LIP 

+  ~TP 
+  AC03 

I 

I 

I 

+  'f'OLP 

I 

+  XYLP 

I 

+  'f'C03 

{131}  M02 

+  OLN 

{132}  ETHP 

+  AC03 

{133}  HC3P 

+  AC03 

{134}  HC5P 

+  AC03 

{135}  HCBP 

{136}  OL2P 

{137}  OLTP 

{138}  OLIP 

{139}  KETP 

{140}  AC03 
{141}  AC03 

{142}  AC03 

{143}  AC03 

+  AC03 

, 

+  ~C03 

I 

+  ~C03 

I 

+  ~C03 

+  AC03 

I 

I 

I 

+  ~C03 
+  'f OLP 

I 

+  ¥-YLP 

I 

+  ':1'C03 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 

--> 

--> 
--> 
--> 

--> 
--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 

--> 
--> 

--> 

--> 

OPl 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
PAA 
OP2 
OP2 
OP2 
ONIT 
l.500*HCHO 
0.750*HCHO 
0.840*HCHO 

H02 

0.770*HCHO 

H02 

+ 

+ 

+ 

H02 

l.550*HCHO 
l.250*HCHO 
0.890*HCHO 

+  0.550*KET 
0.750*HCHO 
HCHO 
+  0.500*0RA2 
HCHO 
+  0.700*DCB 
HCHO 

+  2.000*H02 

0.500*HCHO 
+  0.500*0RA2 
+  0.475*CO 

+ 

l.750*HCHO 

N02 
ALD 

+  0.500*0RA2 
0.770*ALD 
+  0.500*M02 
0.410*ALD 
+  0.500*M02 
0.460*ALD 
+  0.500*M02 

O.SOO*HCHO 

+  0.500*M02 
ALD 
+  0.50~*M02 
0.725*ALD 
+  0.500*H02 

MGLY 
+  o.soo*oRA2 

2.000*M02 
M02 
+  0.700*DCB 
M02 
H02 
M02 

+ 

+  O.llO*MGLY 

8-84 

+  0.350*ALD 
+  0.750*ALD 
+  0.725*ALD 

+  0.750*MGLY 
+  0.500*~02 

+  0.170*MGLY 
+  2.000*H02 
+  0 .450*MGLY 

+  0.44S*GLY 
+  0.025*AC03 
X02 
+ 
+  0.500*H02 

+  0.500*H02 

+  0. 26.0*KET 
+  0,500*0RA2 
+  0.750*KET 
+  0. 500*0RA2 
+  1.390*KET 
+  0.500*0RA2 
+  0.600*ALD 
+  0.500*0RA2 
+  0. 500*HC,i0 
+  0.500*0RA2 
+  0.550*KET 
+  o·.500*M02 
+  0.500*H02 

+  1. 390*KET 

+ 
+ 
+ 

+ 

H02 
H02 
H02 

H02 
O.SOO*M02 

1:1111

": 

Ill", 

+  0. i'6o•GLY 

+  0.806*DCB 

+  O.OSS*MGLY 
+  0.460*H02 

+ 

ALO 

''::::ii; 

+  O.SOO*M02 

+  o. s .. !l0*H02 

+  O.SOO*H02 

+  O.SOO*H02 

+  O.SOO*H02 

t  .. o. !:jOO*HQ2 

+ o. i'.•o•Hca'o 
+  o. scfo•oRA2 
+  O.SOO*M02 

+  0.170*MGLY 
+ 
+  0.4SO*MGLY 

H02 

+  0.160*GLY 

+  0.806*DCB 

+  0.920*H02 
+  O.OSO*AC03 

+  0.890*GLY 
+  0 •.. 9 .. SO*CO 

: i 11 ~ ! i " 

1'11' 

Table 8A-l 1.  RADM2_CIS4 and RADM2_CIS4_AQ Mechanisms 

{144}  AC03 

+  OLN 

+  OLN 
{145}  OLN 
+  H02 
{146}  X02 
+  M02 
{147}  X02 
+  AC03 
{148}  X02 
+  X02 
{149}  X02 
+  NO 
{150}  X02 
+  N02 
{151}  XN02 
+  H02 
{152}  XN02 
+  M02 
{153}  XN02 
+  AC03 
{154}  XN02 
+  XN02 
{155}  XN02 
+  HO 
{156}  ISO 
{157}  ISO_R02  +  NO 

{158}  ISO_R02  +  H02 
{159}  ISO_R02  +  AC03 

{160}  ISO_R02  +  M02 

{161}  ISO 

+  03 

+  2.000*X02 

+ 

HCHO 
N02 

2.000*HCHO 

OP2 
HCHO 
M02 

N02 
ONIT 
OP2 
HCHO 
M02 

ALO 
+ 
+  0.500*M02 
+  2.000*ALD 

+ 

H02 

+  0.500*0RA2 

+  2.000*N02 

+ 

H02 

--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

ISO  R02 

0.088*0NIT 

+  0.079*X02 
+  0.912*N02 

+  0.362*ISOPROO  +  0.230*MACR 
+  0.629*HCHO 

+  0.079*X02 

--> 
--> 

OP2 
0.500*H02 

+  0.500*M02 

--> 

--> 

+  0.362*ISOPROO  +  0.230*MACR 
+  0.629*HCHO 
0.500*HCHO 
+  0.230*MACR 
0.600*HCHO 
+  0.390*0RA1 
+  0.070*CO 
+  0.150*ALO 

+  0.500*H02 
+  0.320*MVK 
+  0.390*MACR 
+  0.270*HO 
+  0.200*X02 
+  0.100*ISOPROO 

+  0.912*H02 
+  0.320*MVK 

+  0.500*0RA2 
+  0.320*MVK 

+  0.362*ISOPROO 
+  0.629*HCHO 
+  0.160*MVK 
+  0.070*H02 
+  0.200*MC03 

{162}  ISO 

+  03P 

--> 

0.750*ISOPROO  +  0.250*MC03 

+  0.250*HCHO 

{163}  ISO 
{164}  ISON_R02  +  NO 

+  N03 

{165}  ISON_R02  +  H02 
{166}  ISON_R02  +  AC03 

{167}  ISON_R02  +  M02 

+  HO 
{168}  MACR 
{169}  MACR_R02  +  NO 

{170}  MACR_R02  +  H02 
{171}  MACR_R02  +  AC03 

{172}  MACR_R02  +  M02 

{173}  MACR 

+  03 

{174}  MACR 

+  hv 

{175}  MACR 

+  N03 

{176}  MVK 
{177}  MVK_R02 

+  HO 
+  NO 

{178}  MVK_R02 
{179}  MVK  R02 

+  H02 
+  AC03 

--> 
--> 

--> 
--> 

--> 

--> 
--> 

--> 
--> 

--> 

+  0.250*M02 

ISON  R02 
N02 
+  0.800*H02 

ONIT 

0.500*H02 
ALO 

+ 

0.500*HCHO 
+  0.500*0NIT 
0.500*MC03 

N02 
+  0.840*CO 
OP2 
0.500*H02 
+  0.840*KET 
+  0.150*MGLY 
0.650*HCHO 

+  0.840*CO 

--> 

0.630*0RA1 

--> 

+  0.110*CO 
+  0.100*AC03 
0.660*H02 

+  0.670*HCHO 
+  0.340*X02 

+  0.800*ALO 
+  0.200*ISOPROO  +  0.200*N02 

+  0.800*0NIT 

+  0.500*M02 
+ 
+  0.500*H02 

ONIT 

+  0.500*0RA2 

+  0.500*ALO 

+  0.500*MACR_R02 
+ 
+  0.150*HCHO 

H02 

+  0.840*KET 
+  0.150*MGLY 

+  0.500*M02 
+  0.840*CO 

+  0.500*0RA2 
+  0.150*HCHO 

+  0.500*H02 
+  0.150*MGLY 
+  0.210*HO 
+  0.200*HCHO 

+  0.840*KET 

+  0.110*H02 
+  0.100*X02 

+  0.330*MC03 
+  0.670*AC03 

+  0.670*CO 
+  0.340*HO 

--> 

0.500*MC03 

+  0.500*H02 

+  0.500*HN03 
+  0.500*0NIT 

+  O.SOO*CO 
+  0.500*X02 

--> 
--> 

--> 
--> 

MVK_R02 
N02 

+  0.700*AC03 
+  0.300*H02 
OP2 
0.500*H02 

+  0.700*ALO 
.+  0.300*HCHO 

+  0.700*X02 
+  0. 300*MGLY 

+  1.200*M02 

+  0.500*0RA2 

Table 8A-11.  RADM2_CIS4 and RADM2_CIS4_AQ Mechanisms 

I 
' 

"'' 

,,; 

"", 

' 

' 

' 
I" 

1

' 

', 

'I 

{180}  MVK_R02 

I +  M02 

I 

{181}  MVK 

+  03 

I 

I 

+  0.700*ALD 

--> 

0.800*HCHO 

+  0.700*M02 

--> 

0.670*0RA1 

+  O.llO*CO 
+  0.050*X02 

+  0.300*HCHO 
+  0.500*H02 
+  0.300*MGLY 
+  0.160*HO 
+  0. 950*MGLY 
+  0. 050*AC03  ' 

+  0.300*MGLY 
+  0.700*ALD 

+  O.llO*H02 
+  O.lOO*HCHO 

{182}  MVK 

+  hv 

--> 

0.700*ISOPROD  +  0.700*CO 

+  0.300*M02 

{183}  MPAN 
{184}  MC03 
{185}  MC03 
{186}  MC03 
{187}  MC03 
{188}  MC03 
{189}  MC03 
{190}  ISOPROD 
{191}  IP_R02 

{192}  IP_R02 
{193}  IP_R02 

{194}  IP_R02 

{195}  ISOPROD 

I 

I +  NO 
I +  N02 
+  ho2 
+  M02 
+  AC03 
+  MC03 
I +  HO 
+  NO 

I 

I 

I +  H02 
+  AC03 

,, 

I 

+  M02 

+  03 

{196}  ISOPROD 

I +  hv 

I 

,, 

I 

I 

I 

I 

I 

I 

I 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 
--> 

+  0.300*MC03 
MC03 
N02 
MPAN 
PAA 

2.250*HCHO 
2.000*M02 
2.000*M02 
0. 313*AC03 

N02 
+  0.270*ALD 
+  0.210*MGLY 

OP2 
O.SOO*H02 
+  0.500*ALD 

--> 

0.500*HCHO 

+  O.SOO*KET 

--> 

0.476*HO 

+  0.237*AC03 
+  0.218*HCHO 
+  0.03l*GLY 

--> 

1. 216*CO 

+  0.216*KET 

+ 
+ 

N02 
HCHO 

+ 

AC03 

+  0.500*H02 
HCHO 
+ 
+  2.000*HCHO 
+  0.687*IP_R02 
+ 
+  0.030*HCHO 
+  0.700*KET 

H02 

+  0.500*M02 

+  0.610*CO 
+  0.180*GLY 

+  0.500*M02 
+  0.500*KET 
+  0.500*H02 

1 '  

,, 

' 

'""'I 

+  O.SOO*ORA2 
,,p·, 
+  0.500*ALD 

,,, 

+  0.072*H02 
+  0.100*X02 
+  0.062*ALD 
+  0.653*MGLY 
+  0.434*ALD 
+  l.216*H02 
+  0.332*HCHO 
+ 

H02 

+  0.168*M02 
+  0.243*CO 
+  0.278*KBT 
+  0.044*0RA1 
+  0.350*HCHO 
+  0.784*AC03 
+  0.332*ALD 
X02 
+ 

'""' 

{197}  ISOPROp 
>----------------------------------------------------------------------------------------< 

0.668*CO 

ONIT. 

+  N03 

--> 

+ 

I 

I 

==c==ccc==•=m====7==========~~========~======c==========a=========~•=•==~x=•••• 

Rate  Expression 

k(  1)  uses  photo  table  N02_RADM88 
k(  2)  uses  photo  table  0301D_RADM88 
k(  3)  uses  photo  table  0303P_RADM88 
k(  4)  uses  photo  table  HONO_RADM88 
5)  uses  photo  table  HN03_RADM88 
k( 
k( 
6)  uses  photo  table  HN04_RADM88 
k( 
7)  uses  photo  table  N03NO_RADM88 
k( 
8)  uses  photo  table  N03N02_RADM88 
k( 
9)  uses  photo  table  H202_RADM88 
k(  10)  uses  photo  table  HCHOmol_RADM88 
k(  11)  uses  photo  table  HCHOrad_RADM88 
k(  12)  uses  photo  table  ALD_RADM88 
k(  13)  uses  photo  table  MHP_RADM88 
k(  14)  uses  photo,table  HOP_RADM88 
k(  15)  uses  photo  table  PAA  RADM88 
k(  16)  uses  photo.table  KETONE_RADM88 
k(  17)  uses  photo  table  GLYform_RADM88 
k(  18)  uses  photo  table  GLYmol_RADM88 
k(  19)  uses  photo  t~ble MGLY_RADM88 
k(  20)  uses  photo  table  UDC_RADM88 
k(  21)  uses  photo  table  ORGNIT_RADM88 
k(  22) 
k(  23)  =  6.5000E-12  *  exp( 
k(  24)  = 
l.8000E-ll  *  exp( 

120. O/T) 
110. O/T) 

6.0000E-34  *  (T/300)**(-2.30) 

c 

,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 
,  scaled  by  l.OOOOOE+OO 

Rate  Constant 

1
11111111 

{o.oooooE+oo} 
{O.OOOOOE+OO} 
{ 0. OOOOOE+OO}  I 1111',, 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{O.OOOOOE:f-00} 
{ 0. OOOOOE.\.oo}  : 
{O.OOOOOE+OO} 
{o.OOOOOE+oo} 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{o.ooooos+oo} 
{o.oooooE+oo} 
{O.OOOOOE+OO}  ,, 
{O.OOOOOE+OO} 
{O.OOOOOE+OO} 
{o.oooooE+oo}' 
{ 0. OOOOOE+OO}  , 
{O.OOOOOE+OO} 
{6.09302E-34} 
{9.72293E-12} 
{2. 60365E-11} 

8-86 

'! 

Table 8A-l l.  RADM2_CIS4 and RADM2_CIS4_AQ Mechanisms 

70.0/T) 

k(  25)  =  3.2000E-11  *exp( 
k(  26)  =  2.2000E-10 
k(  27)  =  2.0000E-12  *  exp(  -1400.0/T) 
-940. O/T) 
k(  28)  =  1.6000E-12  *  exp( 
-500.0/T) 
k(  29)  =  1.lOOOE-14  *  exp( 
k  (  30)  =  3.7000E-12  *  exp( 
240. O/T) 
k(  31)  is  a  falloff  expression  using: 

=  1.8000E-31  *  (T/300)**(-3.20) 
kO 
kinf  =  4.7000E-12  *  (T/300)**(-1.40) 
F  =  0.60, 

n  =  1. 00 

k(  32)  =  k(  31)  I  Keq,  where  Keq  =  2.100E-27  *  exp(  10900.0/T) 
k(  33)  is  a  special  rate  expression  of  the  form: 

k  =kl+  k2[M],  where 
kl  =  2.2000E-13  *  exp( 
k2  =  1.9000E-33  *  exp( 

620. O/T) 
980.0/T) 

k(  34)  is  a  special  rate  expression•of  the  form: 

k  =kl+  k2[M],  where 
kl  =  3.0800E-34  *  exp(  2820.0/T) 
k2  =  2.6600E-54  *  exp(  3180. O/T) 

k(  35)  =  3.3000E-12  *  exp( 
k(  36)  is  a  falloff  expression  using: 

-200.0/T) 

=  7.0000E-31  *  (T/300)**(-2.60) 
kO 
kinf  =  1. 5000E-ll  *  (T/300) ** (-0. 50) 
F  =  0.60, 

n  =  1. 00 

k(  37)  =  3.3000E-39  *  exp( 
530.0/T) 
k  (  38)  =  1.4000E-13  *  exp(  -2500.0/T) 
150.0/T) 
k(  39)  =  1.7000E-11  *  exp( 
k(  40)  =  2.5000E-14  *exp(  -1230.0/T) 
k(  41)  =  2.5000E-12 
k(  42)  is  a  falloff  expression  using: 

=  2.2000E-30  *  (T/300)**(-4.30) 
kO 
kinf  =  1. 5000E-12  *  (T/300)**(-0.50) 
F  =  0.60, 

n  =  1.00 

k(  43)  =  k(  42)  I  Keq,  where  Keq  =  1.100E-27  *  exp(  11200.0/T) 
k(  44)  =  2.0000E-21 
k(  45)  is  a  falloff  expression  using: 

kO 
=  2.6000E-30  *  (T/300)**(-3.20) 
kinf  =  2.4000E-11  *  (T/300)**(-1.JO) 
F  =  0.60, 

n  =  1.00 

k(  46)  is  a  special  rate  expression  of  the  form: 

I 

k  =  kO  +  {k3[MJ 
kO  =  7.2000E-15  *  exp( 
785.0/T) 
k2  =  4.lOOOE-16  *  exp(  1440.0/T) 
k3  =  1. 9000E-33  *  exp ( 
725 .. O/T) 

(1  +  k3[MJ/k2)},  where 

380.0/T) 
k(  47)  =  1.3000E-12  *exp( 
k(  48)  =  4.6000E-11  *  exp( 
230.0/TJ 
k(  49)  is  a  falloff  expression  using: 

kO 
=  3.0000E-31  *  (T/300)**(-3.30) 
kinf  =  1.5000E-12  *  (T/300)**(  0.00) 
F  =  0.60, 

n  =  1.00 

k(  50) 
k(  51) 
k(  52) 
k(  53) 
k(  54) 
k(  55) 
k(  56) 
k(  57) 
k(  58) 
k(  59) 
k(  60) 

1.5000E-13  *  (1.0  +  0.6*Pressure) 
2.8300E+Ol  *  (T/300)**(  2.00)  *exp( 
1.2330E-12  *  (T/300)**(  2.00)  *exp( 
1.5900E-11  *  exp( 
1.7300E-11  *  exp( 
3.6400E-11  *exp( 
2.1500E-12  *  exp( 
5.3200E-12  *  exp( 
1.0700E-11  *  exp( 
2.lOOOE-12  *exp( 
1.8900E-11  *  exp( 

-540.0/TJ 
-380.0/T) 
-380.0/T) 
411.0/T) 
504.0/T) 
549.0/T) 
322.0/T) 
116.0/TJ 

-1280.0/T) 
-444.0/T) 

{4.04730E-11} 
{2.20000E-10} 
{1.82272E-14} 
{6.82650E-14} 
{2.05452E-15} 
{8.27883E-12} 
{1.39058E-12} 

{8.62399E-02} 
{3.01634E-12} 

{6.78905E-30} 

{ 1. 68671E-12} 
{4.87144E-12} 

{1.95397E-38} 
{3.18213E-17} 
{2.81225E-11} 
{4.03072E-16} 
{2.50000E-12} 
{1.26440E-12} 

{5.47034E-02} 
{2.00000E-21} 
{1.14885E-U} 

{1.47236E-13}, 

{4.65309E-12} 
{9.95294E-11} 
{8.88848E-13} 

{2.40000E-13} 
{3.80672E-01} 
{2.74210E-13} 
{2.59669E-12} 
{4.83334E-12} 
{1.01696E-11} 
{8.53916E-12} 
{2.88684E-11} 
{6.75269E-11} 
{6.18715E-12} 
{2.78943E-11} 

8-87 


I 

I 

,111:' 

-444 .O/T) 

256.0/T) 
-745.0/T) 

Table 8A-1 l.  RADM2_CIS4 and RADM2_CIS4_AQ Mech,~sms 
k(  61)  =  4.0000E-11 
k(  62)  =  9.0000E-01  *  k(  61) 
k(  63)  =  9.0000E-12 
k(  64)  =  6.8700E-12  *  exp( 
k(  65)  =  1.2000E-11  *  exp( 
k(  66)  =  1.lSOOE-11 
k(  67)  =  l. 7000E-11 
k(  68)  =  2. 8000E.-11 
k(  69)  = 
l.OOOOE,-11 
k(  70)  =  l. OOOOE-11 
k(  71)  =  l. OOOOE-11 
k(  72)  =  6.1650E-13  *  (T/300)**(  2.00)  *  exp( 
k(  73)  =  1.5500E-11  *  exp( 
-540.0/T) 
181. 0/T) 
k(  74)  =  2.8000E-12  *  exp( 
k(  75)  =  1.9500E+16  *  exp(-13543.0/T) 
k(  76)  =  4.7000E-12 
k(  77)  =  1.9500E+l6  *  exp(-13543.0/T) 
k(  78)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  79)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  80)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  81)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  82)  =  4.2000E-12  *  exp( 
180.0/T) 
180.0/T) 
k(  83)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  84)  =  4.2000E-12  *  exp( 
k(  85)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  86)  =  4.2000E-12  *  exp( 
180.0/T) 
180.0/T) 
k(  87)  =  4.2000E-12  *  exp( 
k(  88)  =  4.2000E-12  *  exp( 
180.0/T) 
180.0/T) 
k(  89)  =  4.2000E-12  *  exp( 
k(  90)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  91)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  92)  =  6.0000E-13  *  exp(  -2058.0/T) 
k(  93)  =  1.4000E-12  *  exp(  -1900.0/T) 
k(  94)  =  6.0000E-13  *  exp(  -2058.0/T) 
k(  95)  =  1.4000E-12  *  exp(  -1900.0/T) 
k(  96)  =  1.4000E-12  *  exp(  -1900.0/T) 
k(  97)  =  2.2000E-11 
k(  98)  =  2. OOOOE,-12  *  exp (  -2923. O/T) 
k(  99)  =  1.0000E-11  *  exp(  -1895.0/T) 
k(100)  =  3.2300E-ll  *  exp( 
-975. O/T) 
k(l01)  = 
l.2000E-14  *  exp(  -2633.0/T) 
k(102)  =  1.3200E-14  *  exp(  -2105.0/T) 
k(103)  =  7.2900E-15  *  exp(  -1136.0/T) 
k(104)  =  7.7000E-14  *  exp(  1300. O/T) 
k(l05)  =  7.7000E-14  *  exp(  1300. O/T) 
k(106)  =  7.7000E-14  *  exp(  1300. O/T) 
k(l07)  =  7.7000E-14  *  exp(  1300. O/T) 
k(l08)  =  7.7000E-14  *exp(  1300.0/T) 
k(l09)  =  7.7000E-14  *  exp(  1300. O/T) 
k(llO)  =  7.7000E-14  *  exp(  1300. O/T) 
lJOO. O/T) 
k(111)  =  7.7000E-14  *  exp( 
k(l12)  =  7.7000E-14  *  exp(  1300 .O/T) 
k(113)  =  7.7000E-14  *  exp(  1300.0/T) 
k(114)  =  7.7000E-14  *  exp(  1300.0/T) 
k(l15)  =  7.7000E-14  *  exp(  1300.0/T) 
k(l16)  =  7.7000E-l4  *  exp(  1300. O/T) 
k(117)  =  7.7000E-14  *  exp( 
· 1300. O/T) 
k (118)  =  1.9000E-13  *  exp( 
220.0/T) 
k(ll9)  =  1.4000E-13  *  exp( 
220.0/T) 
k(120)  =  4.2000E-14  *  exp( 
220.0/T) 
220.0/T) 
k(121)  =  3.4000E-14  *exp( 

{4.00000E~11} "' 
{3.60000E-11}, 
{9.00000E-12} 
{1.62197E-11} 
{9.85020E-13} 
{1.15000E-11}' 
{1.70000E-11} 
{2.80000E-11} 
{1.00000E-11} 
{1. OOOOOE-11} 
{1.00000E-11} ,,, 
{1.37105E-13}  '' 
{2.53137E-12} 
{5.13974E-12} :: 
{3.57235E-04} 
{4.70000E-12)" 
{3.57235E-04} 
{7.68378E-12} 
{7.68378E-12} 
{7 .68378E-12} ii;, 
{7.68378E:-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12}" 
{7.6837BE-12} 
{7.6837BE-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12}' 
{ 6. 01030E-16} .,• 
{2.38307E-15} 
{6.01030E-16} 
{2.38307E-15} 
{2.38307E-15} 
{2 .20000B-11} "" 
{1.09940J;:;-16} 
{1.73099E-14} 
{1.22539E-12} .,, 
{1. 74559E-18} 
{1.12933E-17} 
{1.61125E-16} 
{6.04038E-12} .. 
{6.04038B-12},, 
{6.04038E-12} 
{ 6. 04 03 8E-12} !!11::1 
{6.04038E-12} 
{6.04038E-12} 
{6.04038B-12} 
{6.04038E-12} 
{ 6. 04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.0403BE-12} ,, 
{6.04038E-12} 
{3.97533E-13} 
{2. 92919E-13} ,,, 
{8.78758E-14} 
{7.11376E-14} 

8-88 


Table 8A-l 1.  RADM2_CIS4 and RADM2_CIS4_AQ Mechanisms 
k(122)  =  2.9000E-14  *  exp( 
220.0/T) 
k(123)  =  1.4000E-13  *  exp( 
220.0/T) 
k(124)  =  1.4000E-13  *  exp ( 
220. O/T) 
k(125)  =  1.7000E-14  *  exp( 
220.0/T) 
k(126)  =  1.7000E-14  *  exp( 
220.0/T) 
k(127)  =  9.6000E-13  *  exp( 
220.0/T) 
k(128)  =  1.7000E-14  *  exp( 
220.0/T) 
k(129)  =  1.7000E-14  *  exp( 
220.0/T) 
k(130)  =  9.6000E-13  *  exp( 
220.0/T) 
k(131)  =  1.7000E-14  *  exp( 
220.0/T) 
k(132)  =  3.4000E-13  *exp( 
220.0/T) 
k(133)  =  1.0000E-13  *  exp( 
220.0/T) 
k(134)  =  8.4000E-14  *  exp( 
220.0/T) 
k(135)  =  7.2000E-14  *  exp( 
220.0/T) 
k(136)  =  3.4000E-13  *exp( 
220.0/T) 
k(137)  =  3.4000E-13  *exp( 
220.0/'r) 
k(138)  =  4.2000E-14  *  exp( 
220.0/T) 
k(139)  =  4.2000E-14  *  exp( 
220.0/T) 
k(140)  =  1.1900E-12  *  exp( 
220.0/T) 
k(141)  =  4.2000E-14  *  exp( 
220. O/T) 
k(142)  =  4.2000E-14  *  exp( 
220.0/T) 
k(143)  =  1.1900E-12  *  exp( 
220. O/T) 
k(144)  =  4.2000E-14  *  exp( 
220.0/T) 
k(145)  =  3.6000E-16  *  exp( 
220.0/T) 
k(146)  =  7.7000E-14  *  exp(  1300. O/T) 
k(147)  =  1.7000E-14  *  exp( 
220.0/T) 
k(148)  =  4.2000E-14  *  exp( 
220.0/T) 
k(149)  =  3.6000E-16  *  exp( 
220. O/T) 
k(150)  =  4.2000E-12  *  exp( 
180.0/T) 
k(151)  =  4.2000E-12  *  exp( 
180.0/T) 
k(152)  =  7.7000E-14  *  exp(  1300.0/T} 
k(153)  =  1.7000E-14  *  exp( 
220.0/T} 
k(154)  =  4.2000E-14  *  exp( 
220.0/T) 
220. O/T) 
k(155)  =  3.6000E-16  *exp( 
k(156)  =  2.5400E-11  *  (T/300)**(  1.00)  *exp( 
407.6/T) 
k0.57)  =  4.2000E-12  *  (T/300) ** (  1.00)  *  exp( 
181,2/T) 
k(158)  =  7.7000E-14  *  (T/300)**(  1.00)  *exp(  1298.3/T) 
k(159)  =  8.4000E-14  *  (T/300)**(  1.00)  *  exp( 
221.4/T) 
k(160)  =  3.4000E-14  *  (T/300)**(  1.00)  *exp( 
221.4/T) 
k(161)  =  7.8600E-15  *  (T/300)**(  1.00)  *  exp(  -1912.2/T) 
k(162)  =  3.6000E-11 
k(163)  =  3.0300E-12  *  (T/300)**(  1.00)  *  exp( 
-447.9/T) 
k(164)  =  4.2000E-12  *  (T/300) ** (  1. 00)  *  exp ( 
181.2/T) 
k(165)  =  7.7000E-14  *  (T/300)**(  1.00)  *  exp(  1298.3/T) 
k(166)  =  8.4000E-14  *  (T/300)**(  1.00)  *  exp( 
221.4/T) 
k(167)  =  3.4000E-14  *  (T/300)**(  1.00)  *  exp( 
221.4/T) 
k(168)  =  1.8600E-11  *  (T/300)**(  1.00)  *exp( 
176 .1/T) 
k(169)  =  4.2000E-12  *  (T/300)**(  1.00)  *  exp( 
181.2/T) 
k(170)  =  7.7000E-14  *  (T/300)**(  1.00)  *  exp(  1298.3/T) 
k(171)  =  8.4000E-14  *  (T/300)**(  1.00)  *  exp( 
221.4/T) 
221.4/T) 
k(172)  =  3.4000E-14  *  (T/300)**(  1.00)  *  exp( 
k(173)  =  1.3600E-15  *  (T/300)**(  1.00)  *exp(  -2113.5/T) 
k(174)  uses  photo  table  ACROLEIN 
k(175)  =  1.SOOOE-12  *  (T/300)**(  1.00)  *  exp(  -1726.0/T) 
k(176)  =  4.1400E-12  *  (T/300)**(  1.00)  *  exp( 
452.9/T) 
k(177)  =  4.2000E-12  *  (T/300)**(  1.00)  *  exp( 
181.2/'r) 
k(178)  =  7.7000E-14  *  (T/300)**(  1.00)  *  exp(  1298.3/T) 
k(179)  =  8.4000E-14  *  (T/300)**(  1.00)  *  exp( 
221.4/T) 
k(180)  =  3.4000E-14  *  (T/300)**(  1.00)  *  exp( 
221.4/T) 
k(181)  =  7.5100E-16  *  (T/300)**(  1.00)  *  exp(  -1519.7/T) 
k(182)  uses  photo  table  ACROLEIN 

{6.06762E-14} 
{2.92919E-13} 
{2.92919E-13} 
{3.55688E-14} 
{3.55688E-14} 
{2.00859E-12} 
{3.55688E-14} 
{3.SS688E-14} 
{2.00859E-12} 
{3.55688E-14} 
{7.11376E-13} 
{2.09228E-13} 
{1. 75752E-13} 
{1.S0644E-13} 
{7.11376E-13} 
{7.11376E-13} 
{8.78758E-14} 
{8.78758E-14} 
{2.48981E-12} 
{8.78758E-14} 
{8.78758E-14} 
{2.48981E-12} 
{8.78758E-14} 
{7.53221E-16} 
{6.04038E-12} 
{3.55688E-14} 
{8.78758E-14} 
{7.53221E-16} 
{7.68378E-12} 
{7.68378E-12} 
{6.04038E-12} 
{3.55688E-14} 
{8.78758E-14} 
{7.53221E-16} 
{9.90719E-11} 
{7.66335E-12} 
{S.96598E-12} 
{1.75402E-13} 
{7.09961E-14} 
{1.27569E-17} 
{3.60000E-11} 
{6.69552E-13} 
{7.66335E-12} 
{5.96598E-12} 
{1.75402E-13} 
{7.09961E-14} 
{3.33618E-11} 
{7.66335E-12} 
{5.96598E-12} 
{1. 75402E-13} 
{7.09961E-14} 
{1.12330E-18} 
{O.OOOOOE+OO} 
{4.54753E-15} 
{1.87990E-11} 
{7.66335E-12} 
{5.96598E-12} 
{1. 75402E-13} 
{7.09961E-14} 
{4 .54966E-18}. 
{O.OOOOOE+OO} 

,  scaled  by  3.60000E-03 

,  scaled  by  1.11000E-02 

8-89 


i 

"'  ,1,, 

'11111'', 

'•,:;;,, 

Table 8A-l 1.  RADM2_CIS4 and RADM2_CIS4_AQ Mechanisms 
k(183)  =  1.6000E+16  *  (T/300)**(  1.00)  *  exp(-13486.0/Tl 
k(184)  =  4.2000E-12  *  (T/300)**(  1.00)  *  exp( 
181.2/T) 
k(l85)  "  2.8000E-12  *  (T/300)**(  1.00)  *  exp( 
181.2/T) 
k(l86)  ..  7.7000E-14  *  (T/300) ** (  1. 00)  *  exp(  1298.3/T) 
k(187)  =  9.6000E-13  *  (T/300)**(  1.00)  *  exp( 
221.4/T) 
k(188)  =  1.1900E-12  *  (T/300)**(  1.00)  *  exp( 
221.4/T) 
k(l89)  =  l.1900E-12  *  (T/300)**(  1.00)  *  exp( 
221.4/T) 
k(190)  =  3.3600E-11 
k(l,91) 
c 
k(l92)  • 
k(l93)  c 
k(194) 
c 
k(195)  =  7.1100E-18 
k(196)  uses  photo  table  ACROLEIN 
k(197)  =  1.0000E-13 

{3. s2s36E,:04 > "· 
{7.66335E-12} 
{S.10890E-12} 
{S.96598E-12} 
{2.00460E-12}  ..... 
{2. 484 86E-12} 
{2 .48486E-12} ,,,;;, 
{3. 36000E-11} '1111
{7.6633SE-12} 
{5.96598E-12}  · 
{1.75402B-13} 
{7.099618-14} 
{7.11000E::-18} 
{O.OOOOOE+oo} 
{1.00000E::-13} 
amam•====mcm==a~=========~======c~================•====•===~==z=•====aZXs•~•••• 

4.2000E-l.2  *  (T/300) ** (  1. 00)  *  exp( 
181,2/Tl 
7,7000E-14  *  (T/300)**(  1.00)  *  exp(  1298.3/T) 
8.4000E-14  *  (T/300)**(  1.00)  *  exp( 
221,4/Tl 
3.4000E-14  *  (T/300)**(  1.00)  *exp( 
221.4/Tl 

,  scaled  by  3.60000E-03 

I 

I 

m11 

1, 

~, 

' 

111111111 

111 

1ill!:· 

111"' 

:',,,1 

111111,, 

''I,," 

8-90 

Table 8A-12.  RADM2  CIS4  AE and RADM2  CIS4 AE  AQ Mechanisms 
. 

. 

. 

. 

.  . 

-

-

.-

-

EP A/600/R-99/030 

NO 

+ 

+ 
+ 
+ 

+ 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
-··> 
--> 
-··> 
--> 
_ .. > 

Reaction  List 
>---------~----- .... -----~------------------------------------------------------------------< 
{ 
l}  N02 
{  2}  03 
{  3}  03 
{  4}  HONO 
{  5}  HN03 
{  6}  HN04 
{  7}  N03 
{ 
8}  N03 
{  9}  H202 
{  10}  HCHO 
{  11}  HCHO 
{  12}  ALD 
{  13}  OPl 
{  14}  OP2 
{  15}  PAA 
{  16}  KET 
{  17}  GLY 
{  18}  GLY 
{  19}  MGLY 
{  20}  DCB 
{  21}  ONIT 

+ 
+ 
+ 
'+ 
+ 
+ 
+  l.870*CO 
+  l,SSO*CO 
H02 
+ 
+  0.020*AC03 
+  0.800*KET 

+  hv  · 
+  hv· 
+ hv 
+  hv 
+  hv 
+  hv  · 
+  hv 
+  hv· 
+  hv 
+  hv 
+  hv 
+·hv 
+·hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 
+  hv 

+  0.800*H02 
co 
+ 
TC03 
+ 
H02 
+ 

--> 
-·-> 
--> 
--> 
_,,> 
-·-> 
-·r> 
--;> 
--> 

H02 
H02 
H02 
H02 
HO 
ETHP 

co 
- co 
HO 
HO 

NO 
N02 
0 N02 

+ 
+ 
+ 
+ 

03P 

03P 
OlD 
03P 
HO 
HO 
H02 
NO 
N02 
2.000*HO 
co 
H02 
M02 
HCHO 
ALD 
M02 
AC03 
0.130*HCHO 
0.4SO*!iCHO 
AC03 
0.980*H02 
0.200*ALD 
N02 
03 
NO 
03P 
03P 

2.000*HO 
N02 
H02 
HO 
N02 
HN04 
H02 
H202 
H202 
H02 
HONO 
2.000*N02 
N03 
2.000*N02 

+ 

NO 
HN03 
N205 
N02 

2.000*HN03 
HN03 
N03 
N02 

SULF 
H02 
M02 
ETH!? 
0.83Q*HC31? 
+  0.075*ALD 
HCSI? 
HC81? 
OL2P 
OLTI? 

8-91 

+  [02] 

+  [H20] 

+  [02] 

{  22}  03P 
{  23}  03P 
{  24}  OlD 
{  25}  OlD 
{  26}  OlD 
{  27}  03 
{  28}  03 
{  29}  03 
{  30}  H02 
{  31}  H02 
{  32}  HN04 
{  33}  H02 
{  34}  H02 
{  35}  H202 
{  36}  NO 
{  37}  NO 
{  38}  03 
{  39}  N03 
{  40}  N03 
{  41}  N03 
{  42}  N03 
{  43}  N205 
{  44}  N20S 
{  45}  HQ 
{  46}  HO 
{  47}  HO 
{  48}  HO 
{  49}  HO 
{  so}  co 
{  51}  HO 
{  52}  ETH 
{  53}  HC3 

54}  HCS 
SS}  HC8 
S6}  OL2 
57}  OLT 

+ 
[M] 
+  N02 
+  [N2] 
+  [02] 
+  [H20] 
+  NO 
+  HO 
+  H02 
+  NO 
+  N02 

+  H02 
+  H02 
+  HO 
+  HO 
+  NO 
+  N02 
+  NO 
+  N02 
+  H02 
+  N02 

+  [H20] 
+  N02 
+  HN03 
+  HN04 
+  H02 
+  SQ2 
+  HO 

+  HO 
+  HO 

+  HO 
+  HO 
+  HO 
+  HO 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
-·> 
--> 
--> 
--> 
--> 
--> 
-·> 
--> 
--::. 
--> 
--> 
--> 
--::. 
--> 
--> 
-~> 
--> 
--> 
--> 
--> 
--> 
--:? 
--> 
--> 
--> 

--> 
-,-> 
,..,..=!> 
--> 

+ 

+ 

+ 

+ 

+ 

HO 

N02 

N02 

N03 

H02 

+ 

SULAER 

+  0.170*H02 
+  0.025*KET 
+  0.250*X02 
+  0.750*X02 

+  0,009*HCHO 

+ 

HC8AER 

'lh 

Table 8A-12.  RADM2_CIS4_AE and RADM2_CIS4_AE_AQ Mechanisms ... 

OLIP 
0.750*TOLP 

+ 
+  0.250*CSL 

OLIAER 

+  0.2SO*H02 

TOLAER 

0.830*XYLP 

+  0.170*CSL 

+  0.170*H02 

XYLAER 

.0.100*H02 

+  0.900*X02 

{  58}  OLI 
{  59}  TOL 

60}  XYL 

61}  CSL 

{  62}  CSL 
{  63}  HCHO 
{  64}  ALD 
{  65}  KET 
{  66}  GLY 
{  67}  MGLY 
{  68}  DCB 
{  69}  OP1 
{  70}  OP2 
{  71}  PAA 
{  72}  P.IJN 
{  73}  ONIT 
{  74}  AC03 
{  75}  PAN 
{  76}  TC03 
{  77}  '.!;'PAN 
{  78}  M02 
{  79}  HC3P 

so}  .. !iGSP 

81}  HCBP 

82}  OL2P 

83}  OLTP 

84}  OLIP 

as}  AC03 
86}  TC03 

87}  TOLP 

88}  XYLP 

89}  ETHP 
90}  KETP 
91}  OLN 
92}  HCHO 
93}  ALD 
94}  GLY 
95}  MGLY 
96}  DCB 
97}  CSL 

{  98}  OL2 
{  99}  OLT 
{100}  OLI 
{101}  OL2 

+  HO 
+HO 

+  HO 

+  f!O 

+  HO 
+  fiO 
+  HO 
+  fiO 
;HO 
+ 
+  fiO 
+  ;1m 
+  f!O 
+  fiO 
+  f!O 
+  fl() 
+  f!O 
+  ;t'l'02 

I 

+  .:i-m2 

+  ,NO 
+  ,NO 

+  !"0 

+  ,NO 

+  ;tm 
+  ro 
+  ro 
+  ,NO 
+  ro 

I 

+  ,NO 

I 

+  ~m 
+  ro 
+  ;t'!O 
+  .NO 
+  N03 
+  ,N03 
+  .N03 
+  ,N03 
+  N03 
+  ,N03 

1

I 
N03 
+ 
+  ,N03 
+  ,N03 
+  ,03 

{102}  OLT 

+  ,03 

,,11111' 

11111' 

I 

+ 

+ 

+ 

--> 
--> 

--> 

--> 

.--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

CSLAER 
CSL 
H02 
AC03 
KETP 
H02 
AC03 
TC03 

O.SOO*M02 
O.SOO*HC3P 
AC03 
HCHO 
HC3P 
PAN 
AC03 
TPAN. 
TC03 
HCHO 
0.7SO*ALD 

--> 

--> 

--> 

--> 

--> 

--> 
--> 

--> 

--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 
--> 
--> 
--> 

--> 

+  0.036*0NIT 

p.3BO*Aip 
+  0.920*N02 
o.3SO*ALD 

,.•1,,, 

' 

' 

+  0.240*0NIT 
1.600*HCHO 

+ 

+  0.200*ALD 
ALD 
N02 
H02 
+  O.lOO*KET 
M02 
N02 

+  0.110*MGLY 
+  2. OOO* .. X02 
N02 
+  0.160*GLY 
N02 
+  0.806*DCB 
ALD 
MGLY 
HCHO 
H02 
AC03 
HN03 
HN03 
HN03 
HN03 

+  O.SOO*CSLAER 

OLN 
OLN 
OLN 
HCHO 
+  0,120*H02 

0.530*HCHO 
+  0.200*0RA1 

8-92 

1.':: 

+  0.900*TC03 

11111111.11 

+  O.SOO*HO 
+  0.~90*HO 

I I ; 11 I ~ 

' I : " 

+ 

X02 

N02 

+ 
+  0.090*HCHO 
+  0.964*H02 
t  O.OBO*ONI~ 
iii'", 
+  0.040*HCHO 
+  o:760*H02 
N02 
+ 

.:111·'·: 
''II, 

:1 1•1' 

' 

'' 

11

1

1

·

1'1, 

".111 
%'1'• 

11•:::11111 

+ 

co 

+  2.000*CO 
co 
+ 

+  O.SOO*HCHO 
+  O.SOO*ALD 

+ 
+ 

+ 

N03 
N02 

N02 

N02 
+ 
lio2 
+ 
+  0.2SO*KET 
+  0.964*N02 
+  0.690*KET 
+  0.920*H02 
+  1. OGO*KET 
+  0.760*N02 
+ 
H02 

'1'1•' 

' 

+ 

HCHO 

+ 

H02 

+  1.4SO*ALD 
N02 
+ 
+ 
N02 
+  0.920*H02 
+  O.OSO*AC03 

H02 
+ 
+  0.700*DCB 
+ 
H02 

+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 
+ 

H02 
N02 
ALD 
HN03 
HN03 
H02 
AC03 
TC03 
XN02 

+  o.2SO*HCHO 

+  0.890*GLY 
+  0.9SO*CO, 

+  0.170*MGLY 

+  0.450*MGLY 

N02 
+ 
+ 
H02 
+  2.000*N02 
co 
+ 

+  2.000*CO 
co 
+ 

+  O.SOO*CSL 

+ 
+  0.400*0RA1 

OLIAER 

+  0.420*CO 

+  O.SOO*ALD 
+  0.200*0RA2 

+  0.330*CO 
+  0,230*HQ2 

Table SA-12.  RADM2_CIS4_AE and RADM2_CIS4_AE_AQ Mechanisms 

. 

. 

. 

. 

EP N600/R-99/030 

{103}  OLI 

+  03 

+  0.220*M02 
0,180*HCHO 

--> 

+  0.230*CO 
+  0.260*H02 
+ 

OLIAER 
OP1 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
OP2 
PAA 
OP2 
OP2 
OP2 
ONIT 
1.500*HCHO 
0.750*HCHO 
0.840*HCHO 

+  0.100*HO 
+  0. 720*ALD 
+  0.060*0RA1 
+  0.140*HO 

+  0.100*KET 
+  0.290*0RA2 
+  0.310*M02 

H02 
+ 
H02 
+ 
+  0.770*ALD 

+  0. 750*ALD 
+  0.260*KET 

H02 

0.770*HCHO 

+  0.410*ALD 

+  0.750*KET 

H02 

0.800*HCHO 

+  0.460*ALD 

+  1.390*KET 

+  0.350*ALD 
+  0.750*ALD 
+  0.725*ALD 

+ 
+ 
+ 

H02 
H02 
H02 

+  0. 750*MGLY 
+  0.500*H02 

H02 
+ 
+  0.500*M02 

+  0.170*MGLY 
+  2.000*H02 
+  0.450*MGLY 

+  0.160*GLY 

+  0.806*DCB 

+  0.445*GLY 
+  0.025*AC03 
X02 
+ 
+  0.500*H02 

+  0.055*MGLY 
+  0.460*H02 

+ 

ALD 

+  0.500*H02 

+  0.500*M02 

+  0.260*KET 
+  0.500*0RA2 
+  0.750*KET 
+  0.500*0RA2 
+  1.390*KET 
+  0.500*0RA2 
+  0.600*ALD 
+  0.500*0RA2 
+  0.500*HCHO 
+  0.500*0RA2 
+  0.5SO*KET 
+  O.SOO*M02 
+  O.SOO*H02 

+  0.500*H02 

+  0.500*H02 

+  0.500*H02 

+  0.500*H02 

+  0.500*H02 

+  0.140*HCHO 
+  O.SOO*ORA2 
+  O.SOO*M02 

+  0.170*MGLY 

+  0.160*GLY 

{104}  H02 
{105}  H02 
{106}  H02 
{107}  H02 
{108}  H02 
{109}  H02 
{110}  H02 
{111}  H02 
{112}  H02 
{113}  H02 
{114}  H02 
{115}  H02 
{116}  H02 
{117}  H02 
{118}  M02 
{119}  M02 
{120}  M02 

+  M02 
+  ETHP 
+  HC3P 
+  HC5P 
+  HC8P 
+  OL2P 
+  OLTP 
+  OLIP 
+  KETP 
+  AC03 
+  TOLP 
+  XYLP 
+  TC03 
+  OLN 
+  M02 
+  ETHP 
+  HC3P 

{121}  M02 

+  HC5P 

{122}  M02 

+  HC8P 

{123}  M02 
{124}  M02 
{125}  M02 

{126}  M02 
{127}  M02 

+  OL2P 
+  OLTP 
+  OLIP 

+  KETP 
+  AC03 

{128}  M02 

+  TOLP 

{129}  M02 

+  XYLP 

{130}  M02 

+  TC03 

{131}  M02 

+  OLN 

{132}  ETHP 

+  AC03 

{133}  HC3P 

+  AC03 

{134}  HC5P 

+  AC03 

{135}  HC8P 

+  AC03 

{136}  OL2P 

+  AC03 

{137}  OLTP 

+  AC03 

{138}  CLIP 

+  AC03 

{139}  KETP 

+  AC03 

{140}  AC03 
{141}  AC03 

+  AC03 
+  TOLP 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 

--> 

--> 
--> 
--> 

--> 
--> 

--> 

--> 

--> 

+ 

+ 

+ 

H02 

1,550*HCHO 
1,250*HCHO 
0.890*HCHO 

+  0.550*KET 

0.750*HCHO 
HCHO 
+  0.500*0RA2 
HCHO 
+  0.700*DCB 
HCHO 

+  2.000*H02 

0.500*HCHO 
+  0.500*0RA2 
+  0.47S*CO 

--> 

i.750*HCHO 

--> 

--> 

--> 

... -> 

+ 

N02 
ALD 

+  0.500*0RA2 
0.770*1\LD 
+  0.500*M02 
0.410*ALD 
+  O.SOO*M02 
0.460*ALD 
+  O.SOO*M02 

--> 

0.800*HCHO 

+  O.SOO*M02 
ALD 
+  0.500*M02 
0. 725*ALD 
+  O.SOO*H02 

MGLY 
+  O,SOO*ORA2 
2.000*M02 
M02 

--> 

--> 

--::· 

--> 
--> 

8-93 

Table SA-12.  RADM2_CIS4_AE and RADM2_CIS4_AE_AQ Mechanisms 

I 

{142}  AC03 

+  lFYLP 

{143}  AC93 

+  TC03 

{144}  AC03 

+  OLN 

{145}  OLN 
+  9LN 
{146}  X02 
+  JiO~ 
{147}  X02 
+  M02 
+  AC03 
{148}  X02 
+  xo2 
{149}  X02 
+  NO 
{150}  X02 
{151}  XN02 
+  1l702 
{152}  XN02 
+  Ji02 
+  M02 
{153}  XN02 
+  AC03 
{154}  XN02 
+  XN02 
{155}  XN02 
+  HO 
{156}  TERP 
+  l,'l03 
{157}  TERP 
+  03 
{158}  TERP 
+  HO 
{159}  ISO 
{160}  ISO_R02  +  NO 

I' 
{161}  ISO_R02  +  Ji02 
{162}  ISO_R02  +  AC03 

I 

{163}  ISO_R02  +  M02 

' 

{164}  ISO 

+  03 

{165}  ISO 

+  p3P 

{166}  ISO 
{167}  ISON_R02  +  NO 

+  N03 

' 

{168}  ISON_R02  +  H02 
{169}  ISON_R02  +  AC03 

I 

{170}  ISON_R02  +  ~02 

+  HO 
{171}  MACR 
{172}  MACR_R02  +NO 

{173}  MACR_R02  +  H02 
{174}  MACR_R02  +  AC03 

{175}  MACR_R02  +  ~02 

{176}  MACR 

+  03 

{177}  MACR 

+  hv 

+  0.700*DCB 
M02 
H02 
M02 

+ 

+  O.l10*MGLY 
+  2.000*X02 
HCHO 
N02 

+ 

2.000*HCHO 

OP2 
HCHO 
M02 

N02 
ONIT 
OP2 
HCHO 
M02 

+ 
+  0 .450*MGLY 

H02 

+  0.920*H02 
+  O.OSO*AC03 

+ 
ALD 
+  0.500*M02 
+  2.000*ALD 

+ 

H02 

+ 

H02 

TERPAER  + 
TERPAER  + 
TERPAER  + 
ISO_R02  +  0.079*X02 
+  0.9l2*N02 

HO 
N03 
03 

O.OBB*ONIT 

--> 

--> 

--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

+  0.362*ISOPROD  +  0.230*MACR 
+  0.629*HCHO 

+  0.079*XQ2 

' I ' '  

--> 
--> 

OP2 
O.SOO*H02 

+  O.SOO*M02 

--:> 

--> 

+  0.362*ISOPROD  +  0.230*MACR 
+  0.629*HCHO 
O.SOO*HCHO 
+  0.239*MACR 
0.600*HCHO 
+  0.390*0RAl 
+  0.070*CO 
+  O.l50*ALD 

+  0.500*H02 
+  0.320*~ 
+  0.390*MACR 
+  0.270*HO 
+  0.200*X02 
+  O.lOO*ISOPROD 

+  0.806*DCB 

+  0.890*GLY 
+  o.9so•co 

I  lh 

+  0.500*0RA2 

+  2.000*N02 

+  0.912*H02 
+  0.320*MVK 

111::11 

":  O.~~O*ORA2 
+  0.320*MVK 

""1'

111 

'•,i[I 

+  0.362*ISOPROD 
+  0.629*HCHO 
+  0.160*MVK 
+  0.070*H02 
+  0.200*MC03 

I 

111, 

--> 

0.750*ISOPROD  +  0.250*MC03 

+  0.2SO*HCHO 

--> 
--> 

--> 
--> 

--> 

--> 
--> 

--> 
--> 

--> 

--> 

+  0.2SO*M02 

ISON_R02 
N02 
+  O.BOO*H02 

ONIT 

O.SOO*H02 
ALO 

+ 

O.SOO*HCHO 
+  O.SOO*ONIT 
O.SOO*MC03 

N02 
+  0.840*CO 
OP2 
0.500*H02 
+  0.840*KET 
+  0.150*MGLY 
0.6SO*HCHO 

+  0.840*CO 

0.630*0RA1 

+  O.llO*CO 
+  o.lOO*AC03 

--> 

0.660*H02 

+  0.670*HCHO 
+  0.340*X02 

8-94 

+  O.SOO*ALD 
+  0.200*ISOPROD  +  0.200*N02 

+  O.SOO*ONIT 

+  O.SOO*M02 
+ 
+  O.SOO*H02 

ONIT 

+  O.SOO*ORA2 

+  O.SOO*ALD 

+  O.SOO*MACR_R02 
+ 
+  O.lSO*HCHO 

H02 

+  O.SOO*M02 
+  0.840*CO 

+  O.SOO*H02 
+  O.lSO*MGLY 
+  0.2lO*HO 
+  0.200*HCHO 

+  O.B40*KET 
+  O.lSO*MGLY 

111111 

+  O.SOO*ORA2 
+  0.150*HCHO 

+  O.B<lO*KBT 

+  O.ll0*H02 
+  0.100*X02 

+  0.330*MC03 
+  0.670*AC03 

+  0.670*CO 
+  0.340*HO 

Table 8A-12.  RADM2_CIS4_AE and RADM2_CIS4_AE_AQ Mechanisms 

EP A/600/R-99/030 

{178}  MACR 

+  N03 

{179}  MVK 
{180}  MVK_R02 

+  HO 
+  NO 

{181}  MVK_R02 
{182}  MVK_R02 

+  H02 
+  AC03 

{183}  MVK_R02 

+  M02 

{184}  MVK 

+  03 

{185}  MVK 

+  hv 

{186}  MPAN 
{187}  MC03 
{188}  MC03 
{189}  MC03 
{190}  MC03 
{191}  MC03 
{192}  MC03 
{193}  ISO PROD  +  HO 
+  NO 
{194}  IP_R02 

+  NO 
+  N02 
+  H02 
+  M02 
+  AC03 
+  MC03 

{195}  IP  R02 
{196}  IP::::R02 

+  H02 
+  AC03 

{197}  IP_R02 

+  M02 

{198}  ISOPROD  +  03 

{199}  ISOPROD  +  hv 

--> 
--> 
--> 

--> 
--> 
--> 
--> 

--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 
--> 

--> 
--> 
--> 
--> 

--> 
--> 

+ 

+  0.300*MC03 
MC03 
N02 
MPAN 
PAA 

2.250*HCHO 
2.000*M02 
2.000*M02 
0.313*AC03 

N02 
+  0.270*ALD 
+  0.210*MGLY 

OP2 
O.SOO*H02 
+  O.SOO*ALD 

O.SOO*HCHO 

+  0.SOO*KET 
0.476*HO 

+  0.237*AC03 
+  0.218*HCHO 
+  0.03l*GLY 

l.216*CO 

+  0.216*KET 

O.SOO*MC03 

+  O.SOO*H02 

+  O.SOO*HN03 
+  O.SOO*ONIT 

+  O.SOO*CO 
+  0.500*X02 

MVK  R02 
N02 

+  0.700*AC03 
+  0.300*H02 
OP2 
O.SOO*H02 
+  0.700*ALD 
O.SOO*HCHO 

+  0.700*M02 

0.670*0RA1 

+  O ..  llO*CO 
+  0.050*X02 

+  0.700*ALD 
+  0.300*HCHO 

+  0.700*X02 
+  0.300*MGLY 

+  1.200*M02 
+  0.300*HCHO 
+  0.500*H02 
+  0.300*MGLY 
+  0.160*HO 
+  0. 950*MGLY 
+  0.050*AC03 

+  0.500*0RA2 
+  0.300*MGLY 
+  0.700*ALD 

+  0.110*H02 
+  O.lOO*HCHO 

0.700*ISOPROD  +  0.700*CO 

+  0.300*M02 

+ 
+ 

N02 
HCHO 

+ 

AC03 

+  0.500*H02 
HCHO 
+ 
+  2.000*HCHO 
+  0.687*IP_R02 
+ 
+  0.030*HCHO 
+  0.700*KET 

H02 

+  0.500*M02 

+  0.610*CO 
+  0.180*GLY 

+  0.500*M02 
+  O.SOO*KET 
+  0.SOO*H02 

+  O.SOO*ORA2 

+  O.SOO*ALD 

+  0.072*H02 
+  0.100*X02 
+  0.062*ALD 
+  0.653*MGLY 
+  0.434*ALD 
+  1.216*H02 
+  0. 332*HCHO 
+ 

H02 

+  0.168*M02 
+  0.243*CO 
+  0.278*KET 
+  0.044*0RA1 
+  0.3SO*HCHO 
+  0.784*AC03 
+  0.332*ALD 
X02 
+ 

.  {200}  ISOPROD  +  N03 
>----------------------------------------------------------------------------------------< 

0.668*CO 

ONIT 

Rate  Constant 
Rate  Expression 
============================================================================cc= 
{O.OOOOOE+OO} 
k(  1)  uses  photo  table  N02_RADM88 
{O.OOOOOE+OO} 
k(  2)  uses  photo  table  0301D_RADM88 
k( 
3)  uses  photo  table  0303P_RADM88 
{O.OOOOOE+oo} 
{o.OOOOOE+Oo} 
k(  4)  uses  photo  table  HONO_RADM88 
k(  5)  uses  photo  table  HN03_RADM88 
{o.OOOOOE+Oo} 
{O.OOOOOE+OO} 
6)  uses  photo  table  HN04_RADM88 
k( 
{O.OOOOOE+OO} 
7)  uses  photo  table  N03NO_RADM88 
k( 
k( 
{O.OOOOOE+OO} 
8)  uses  photo  table  N03N02_RADM88 
{O.OOOOOE+oo} 
k(  9)  uses  photo  table  H202_RADM88 
k(  10)  uses  photo  table  HCHOmol_RADM88 
{O.OOOOOE+oo} 
{o.oooooE+oo} 
k(  11)  uses  photo  table  HCHOrad_RADM88 
{O.OOOOOE+OO} 
k(  12)  uses  photo  table  ALD_RADM88 
{O.OOOOOE+oo} 
k(  13)  uses  photo  table  MHP_RADM88 
{O.OOOOOE+OO} 
k(  14)  uses  photo  table  HOP_RADM88 
{o.OOOOOE+Oo} 
k(  15)  uses  photo  table  PAA_RADM88 
k(  16)  uses  photo  table  KETONE_RADM88 
{O.OOOOOE+OO} 

'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  l.OOOOOE+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
,  scaled  by  1,00000E+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
,  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 
'  scaled  by  1.00000E+OO 

Table 8A-12.  RADM2_CIS4_AE and RADM2_CIS4_AE_AQ .Mechanism$ 

1 

1
11

:

:11 

,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 
,  scaled  by 

l.OOOOOE+OO 
l.OOOOOE+OO 
l.OOOOOE+OO 
l.OOOOOE+OO 
l.OOOOOE+OO 

uses  photo  table  GLYform_RADM88 
uses  photo  table  GLYmol_RADM88 
uses  photo  table  MGLY_RADM88 
uses  photo  table  UDC_RADM88 
uses  photo  table  ORGNIT_RADM88 

k(  17) 
k(  18) 
k(  19) 
k(  20) 
k(  21) 
k(  22) 
6.0000E-34  *  (T/300)**(-2.30) 
6.5000E-12  *  exp( 
k(  23) 
k(  24) 
l.8000E-ll  *  exp( 
3.2000E-ll  *  exp( 
k(  25)  = 
k(  26) 
2.2000E-10 
k(  27) 
2.0000E-12  *  exp(  -1400.0/Tl 
l.6000E-12  *  exp( 
-940.0/T) 
k(  28)  = 
k(  29) 
-500.0/T) 
l.lOOOE-14  *  exp( 
k(  30) 
3.7000E-12  *exp( 
240.0/Tl 
k(  31)  is  a  falloff  expression  using: 

120.0/T) 
110.0/T) 
70.0/T) 

l.8000E-31  *  (T/300)**(-3.20) 
kO 
kinf  =  4.7000~-12  *  (T/300)**(-1.40) 
F  =  0.60, 

n  •  1.00 

I 

k(  32)  = k(  31) 
k(  33)  is  a  special  rate  express~on of  the  form: 

/  Keq,  where  Keq  =  2.lOOE-27  *  exp(  10900.0/T) 

k  =  kl  +  k2 [Ml  ',  where  '  "' 
kl  =  2.2000E-l3  *  exp( 
k2  = 
l.9000E-33  *  exp( 

620.0/T) 
980.0/T) 

k (  34)  is  a  speci

al  rate  expression  of  the  form: 
1

k  =kl+  k2[M],  where 
kl  =  3.0800E-34  *  exp(  2820.0/T) 
k2  =  2.6600E-S4  *exp(  3180.0/T) 

-200.0/T) 
k(  35)  =  3.3000E-12  *exp( 
k(  36)  is  a  falloff  expres~ion using: 

kO 
=  7.0000E-31  *  (T/300)**(-2.60) 
kinf  =  1.5000~-ll  *  (Tf300)**(-0.50) 
F  =  0.60, 

n  =  1.00 

k(  37)  =  3.3000E-39  *  exp( 
k(  38)  = 
l.4000E-13  *  exp( 
k(  39)  = 
l.7000E-ll  *  exp( 
k(  40)  =  2.5000E-14  *  exp( 
k(  41)  =  2.5000E-12 
k(  42)  is  a  falloff  expression  using: 

530. O/T) 
-2500.0/Tl 
150.0/Tl 
-1230.0/Tl 

-

2.2oodE-30  *  (T/300)**(-4.30) 
kO 
kinf  =  l.500~E-12  *  (T/300)**(-0.50) 
F  =  0.60, 

n  ~  1.00 

k(  43)  = k(  42)  I  Keq,  where  Keq  = 
k(  44)  =  2.0000E-21 
k(  45)  is  a  falloff  expression  using: 

l.lOOE-27  *  exp(  11200.0/T) 

k~  =  2.6000E-30  *  (T/300)**(-3.20) 
kinf  =  2.400~E-ll  *  (T/300)**(-1.30) 
F  =  0.60, 

n  =  1.00 

k(  46)  is  a  special  rate  expression  of  the  form: 

(1  +  k3[M]/k2)),  where 

I 

785.0/T) 
4 .. lOOOE-16  *  exp(  1440. O/T) 
725.0/Tl 

k  =  kO  +  {k3[M] 
kO  =  7.2000E-15  *  exp( 
k2  = 
k3  =  1.9000E~33  *exp( 
380.0/Tl 
k(  47)  =  1.3000E-12  *  exp( 
k(  48)  =  4.6000E-ll  *exp( 
230.0/T) 
k(  49)  is  a  falloff  expression  using: 

=  3.oooriE-31  *  (T/300l**(-3.3ol· 
ko 
kinf  =  l.5000E-12  *  (T/300)**(  o.oo) 
F  =  0.60, 

n  =  1.00 

' 

I 

k(  50) 
k(  51) 
k(  52) 

l.5000E-13  *  (l.O  +  0.6*Pressurel 
2.8300E+Ol  *  (T/300)**(  2.00)  *exp(  -1280.0/T) 
-444.0/T) 
l.2330i-12  * 

(T/300)**(  2.00)  *exp( 

' 

,:11111 

{O.OOOOOE+OO}  ·' 
{o.oooooE+oo} 
{ 0. OOOOOE+OO},  ,, 
{O.OOOOOE+oo} 
{O.OOOOOE+OO} 
{6.09302E-34} 
{9.72293E-12} 
{2.60365E-ll} 
{4.04730E-ll} 
{2.20000E-10}  ::, 
{l.82272E-14} 
{6.82650E-l4} 
{2.05452E-15} 
{8.27883E-12} 
{1.39058E-12} 

{8.62399E-02} 
{3. 01634E-12}  :: 

{6.7890SE-30} 

{1.68671E-l2}  ,,, 
{4.87144E-12} 

{1.95397E-38} 
{3.18213E-17} 
{2.81225E-11} 
{4.03072E-16} 
{'2.SOOOOE-12} 
{l.26440E-12} 

{5.47034E-02}' 
{2.00000E-21} 
{l.14885E-ll} :: 

{l.47236E-l3}' 

1

1 1
'"

1, 

{4.65309E-l.2} 
{9.95294E-ll} 
{ 8. 88848~-l.3} :111 

., 

{2.40000E-l.3} 
{3.80672E-Ol.} 
{2.742l.OE-l.3} 

I 

8-96 

Table 8A-12.  RADM2_CIS4_AE and RADM2_CIS4_AE_AQ Mechanisms 
-S40.0/T) 
-380.0/T) 
-380.0/T) 
411. O/T) 
S04. O/T) 
549. O/T) 
322. 0/T) 
116. 0/T) 

2S6. O/T) 
-74S.O/T) 

-540.0/T) 
l.SSOOE-11  *  exp( 
181. 0/T) 
2.BOOOE-12  *  exp( 
1.9SOOE+16  *  exp(-13S43.0/T) 

k(  S3)  =  1.S900E-11  *  exp( 
k(  S4)  =  1.7300E-ll  *exp( 
k(  SS)  =  3.6400E-11  *  exp( 
2.1SOOE-12  *  exp( 
k(  S6)  = 
k(  S7)  =  S.3200E-12  *  exp( 
k(  58)  =  1.0700E-ll  *  exp( 
k(  S9)  =  2.lOOOE-12  *  exp( 
k(  60)  =  1.8900E-ll  *  exp( 
k(  61)  =  4.0000E-11 
k  (  62)  =  9.0000E-01  *  k(  61) 
k(  63)  =  9.0000E-12 
k  (  64)  =  6.8700E-12  *exp( 
l.2000E-ll  *  exp( 
k(  6S)  = 
l.1500E-ll 
k(  66)  = 
k(  67)  =  1. 7000E-ll 
k  (  68)  =  2.8000E-11 
k(  69)  =  1. OOOOE-11 
k(  70)  =  1.0000E-11 
k(  71)  =  1. OOOOE-11 
k(  72)  =  6.16SOE-13  *  (T/300)**(  2.00)  *exp( 
k(  73)  = 
k(  74)  = 
k(  7S)  = 
k(  76)  =  4.7000E-12 
k(  77)  =  1.9SOOE+l6  *  exp(-13S43.0/T) 
180.0/T) 
k(  78)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  79)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  BO)  =  4.2000E-12  *  exp( 
k  (  81)  =  4.2000E-12  *  exp( 
180.0/T) 
180.0/T) 
k(  82)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  83)  =  4.2000E-12  *exp( 
180.0/T) 
k(  84)  =  4.2000E-12  *  exp( 
180. O/T) 
k(  BS)  =  4.2000E-12  *exp( 
180.0/T) 
k(  B6)  =  4.2000E-12  *  exp( 
k(  87)  =  4.2000E-12  *  exp( 
180.0/T) 
180.0/T) 
k(  BB)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  89)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  90)  =  4.2000E-12  *  exp( 
180.0/T) 
k(  91)  =  4.2000E-12  *  exp( 
k(  92)  =  6.0000E-13  *exp(  -2058.0/T) 
k(  93)  =  1.4000E-12  *  exp(  -1900.0/T) 
k(  94)  =  6.0000E-13  *  exp(  -2058.0/T) 
k(  9S)  =  1.4000E-12  *  exp(  -1900.0/T) 
l.4000E-12  *  exp(  -1900.0/T) 
k(  96)  = 
k(  97)  =  2.2000E-11 
k(  98)  =  2.0000E-12  *exp(  -2'923.0/T) 
k(  99)  =  1.0000E-11  *  exp(  -1895.0/T) 
-975.0/T) 
k(lOO)  =  3.2300E-ll  *exp( 
k(101)  =  1.2000E-14  *exp(  -2633.0/T) 
k(102)  =  1.3200E-14  *  exp(  -2105.0/T) 
k(103)  =  7.2900E-15  *  exp(  -1136.0/T) 
k(104)  =  7.7000E-14  *  exp(  1300.0/T) 
k(10S)  =  7.7000E-14  *  exp(  1300.0/T) 
k(l06)  =  7.7000E-14  *  exp(  1300.0/T) 
k(l07)  =  7.7000E-14  *  exp(  1300.0/T) 
k(l08)  =  7.7000E-14  *  exp(  1300.0/T) 
k(l09)  =  7.7000E-14  *  exp(  1300.0/T) 
k(llO)  =  7.7000E-14  *  exp(  1300.0/T) 
k(lll)  =  7.7000E-14  *  exp(  1300.0/T) 
k(112)  =  7.7000E-14  *  exp(  1300.0/T) 
k(l13)  =  7.7000E-14  *exp(  1300.0/T) 

-444.0/T) 

{2.S9669E-12} 
{4.83334E-12} 
{1. 01696E-11}. 
{8.S3916E-12} 
{2.88684E-ll} 
{6.7S269E-11} 
{6.1871SE-12} 
{2. 78943E-11} 
{ 4 .. OOOOOE-11} 
{3.60000E-11} 
{9.00000E-12} 
{1. 62197E-ll} 
{9.85020E-13} 
{1.lSOOOE-11} 
{1. 70000E-11} 
{2.80000E-11} 
{1.00000E-11} 
{1. OOOOOE-11} 
{1.00000E-11} 
{ 1. 3 710SE-13} 
{2.S3137E-12} 
{S.13974E-12} 
{3.S723SE-04} 
{4.70000E-12} 
{3.S723SE-04} 
{7.68378E-12} 
{7.6837BE-12} 
{7.6B378E-12} 
{7.6837BE-12} 
{7.6B37BE-12} 
{7.6837BE-12} 
{7.6B37BE-12} 
{7.6B37BE-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.68378E-12} 
{7.6837BE-12} 
{7.68378E-12} 
{7.6B378E-12} 
{6.01030E-16} 
{2.38307E-1S} 
{6.01030E-16} 
{2.3B307E-1S} 
{2.38307E-1S} 
{2.20000E-11} 
{1.09940E-16} 
{1. 73099E-14} 
{1.22S39E-12} 
{ 1. 74SS9E-18} 
{1.12933E-17} 
{1.6112SE-16} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 
{6.04038E-12} 

111111:.:r .. :. 

;i:. 

1 

"!,11111:11

Table 8A-12.  RADM2  CIS4_AE and RADM2_CIS4_AE_AQ Mechanisms 
k(114)  =  7.7000E-14  *  exp(  1300. O/T) 
k(115)  =  7.7000E-14  *  exp(  1300. O/T) 
k(ll.6)  =  7.7000E-14  *  exp(  1300. O/T) 
k(ll.7)  =  7.7000E-14  *  exp(  1300. O/T) 
k(118)  =  1.9000E-13  *  exp( 
220.0/T) 
k(119)  =  1.4000E-13  *  exp( 
220.0/T) 
k(l.20)  =  4.2000E-14  *  exp( 
220.0/T) 
k(121)  =  3.4000E-14  *  exp( 
220. O/T) 
k(l.22)  =  2.9000E-14  *  exp( 
220.0/T) 
k(l.23)  =  1.4000E-13.*  exp( 
220. O/T) 
k(l.24)  =  1.4000E-13  *  exp( 
220.0/T) 
k(l.25)  = 
l.7000E-14  *  exp( 
220.0/T) 
k(l.26)  =  1.7000E-14  *  exp( 
220.0/T) 
k(l.27)  =  9.6000E-13  *  exp( 
220.0/T) 
k(l.28)  =  1.7000E-14  *  exp( 
220.0/T) 
k(l.29)  =  l.7000E-14  *  exp( 
220.0/T) 
220.0/T) 
k  (130)  =  9.6000E-13  *  exp( 
k(l.31)  =  1.7000E-14  *  exp( 
220.0/T) 
k(132)  =  3.4000E-13  *  exp( 
220.0/T) 
k(l.33)  =  1.0000E-13  *  exp( 
220. O/T) 
k(l.34)  =  8.4000E-14  *  exp( 
220.0/T) 
k  (135)  =  7.2000E-14  *  exp( 
220.0/T) 
k(l.36)  =  3.4000E-13  *  exp( 
220. O/T) 
k(l.37)  =  3.4000E-13  *  exp( 
220.0/T) 
k  (138)  =  4.2000E-14  *  exp( 
220.0/T) 
k(l.39)  =  4.2000E-14  *  exp( 
220. O/T) 
k(l.40)  =  l.l900E-12  *  exp( 
220.0/T) 
k(l.41)  =  4.2000E-14  *  exp( 
220.0/T) 
k(l.42)  =  4.2000E-14  *  exp( 
220.0/T) 
k(l.43)  = 
220.0/T) 
l.1900E-12  *  exp( 
k(l.44)  =  4.2000E-14  *  exp( 
220.0/T) 
k(l.45)  =  3.6000E-16  *  exp( 
220.0/T) 
k(l.46)  =  7.7000E-14  *  exp(  1300.0/T) 
k(l.47)  =  1.7000E-14  *  exp( 
220.0/T) 
k(l.48)  =  4.2000E-14  *  exp( 
220.0/T) 
k(l.49)  =  3.6000E-16  *exp( 
220.0/T) 
k(l.50)  =  4.2000E-12  *  exp( 
180.0/T) 
k(151)  =  4.2000E-12  *  exp( 
180.0/T) 
k(152)  =  7.7000E-14  *exp(  1300.0/T) 
k(l.53)  = 
220.0/T) 
l.7000E-14  *  exp( 
k(l.54)  =  4.2000E-14  *  exp( 
220.0/T) 
k(l.55)  =  3.6000E-16  *exp( 
220.0/T) 
k(l.56)  = 
k(l.57)  = 
k(lSB)  = 
k(l.59)  =  2.5400E-ll  *  (T/300)**(  1.00)  *  exp( 
407.6/T) 
k(l.60)  =  4.2000E-12  *  (T/300)**(  1.00)  *exp( 
181.2/T) 
k(161)  =  7.7000E-14  *  (T/300)**(  1.00)  *  exp(  1298.3/T) 
k(l.62)  =  B.4000E-14  *  (T/300) ** (  1. 00)  *  exp( 
221.4/T) 
k(163)  =  3.4000E-14  *  (T/300)**(  1.00)  *exp( 
221.4/T) 
k(l.64)  =  7.B600E-15  *  (T/300) ** (  1. 00)  *  exp(  -1912.2/T) 
k(l.65)  =  3.6000E-11 
k(l66)  =  3.0300E-12  *  (T/300)**(  1.00)  *exp( 
-447.9/T) 
k(l.67)  =  4.2000E-12  *  (T/300) ** (  l. 00)  *  exp( 
181.2/T) 
k  (168)  =  7.7000E-14  *  (T/300)**(  1.00)  *exp(  1298.3/T) 
k(l.69)  =  8.4000E-14  *  (T/300)**(  1.00)  *  exp( 
221.4/T) 
k(l.70)  =  3.4000E-14  *  (T/300)**(  1.00)  *exp( 
221.4/T) 
k(l.71)  =  l. B600E-11  *  (T/300) ** (  1.00)  *  exp( 
176.1/T) 
k(l.72)  =  4.2000E-12  *  (T/300)**(  1.00)  *  exp( 
181.2/T) 
k(l73)  =  7.7000E-14  *  (T/300)**(  1.00)  *  exp(  1298.3/T) 
k(l.74)  =  B.4000E-14  * 
221.4/T) 

{6.04038E-12} 
{6.04038E-l.2}, 
{6.04038E-12} 
{6.04038E-12} 
{3.97533E-13} 
{2.92919E-13} 
{8.78758E-14} 
{7.11376E-::14} 
{6.06762E-14} 
{2.92919E-13} 
{2.92919E-::13} 
{3.55688E:::i4}: 
{3.55688E-l.4} 
{2.00859E-12} 
{3.55688E-l4} 
{3.55688E-l.4} 
{2.00859E-l.2} 
{3.55688E-l.4} 
{7.11376E-l.3} 
{2.09228E-13} 
{l. 75752E-13} 
{l.50644E-l3} 
{7.11376E-13} 
{ 7 .113 76E:: .. 13} 
{ 8. 78758E-14}, 
{8.78758E-14} 
{2.48981E-12} 
{ 8. 7.8758E .. : .. l4} 
{ 8. 78758E .. :-14} 
{2.48981E-l2}. 
{8.78758E-l.4}. 
{7.53221E-16} 
{6.04038E-12} 
{3.55688E-l.4} 
{8.78758E-14} 
{7.53221E:::16}  "'I 
{7 .6B378E-12} 
·· 
{7.6B378E-12} 
{6.04038E:-12} 
{3 .5568BE-:l4} :I]'! 
{ B. 78758E:..14}  ' 
{7.53221E-16} 
{6.75269B-11} 
{1,22539E:-l2} 
{l.61125B-16}  ·· 
{9.90719E-11} 
{7.66335E-12} 
{5.96598E-12}  " 
{l. 75402E-13} 
{7.09961E-14} 
{1.27569E-i7} 
{3.60000E-11} 
{6.69552E-13} 
{7.66335E-12} 
{S.9659BE:::12} 
{1.15402E::.13} ,. 
{7.09961E-14} 
{3.33618E-11} 
{7.66335E-12} 
{5.9659BB-12} 
{1. 75402E-l3} 

l.OOOOE+OO  *  k(  SB) 
l.OOOOE+OO  *  k(lOO) 
l.OOOOE+OO  *  k(l03) 

,,,1111 

'II 
":I• 

''1111 

,!::'::;111 
,,,,:Ill 

(T/300)**(  1.00)  *  exp( 

I 

8-98 

·i·1' 

11·1 '' 

Table 8A-12.  RADM2_CIS4_AE and RADM2_CIS4:._AE_AQ Mechanisms 

,  scaled  by  3.60000E-03 

,  scaled  by  1.llOOOE-02 

k(l75)  =  3.4000E-14  *  (T/300)**(  1.00)  *  exp( 
221.4/T) 
{7.09961E-14} 
k(l76)  =  l.3600E-15  *  (T/300)**(  1.00)  *  exp(  -2113.5/T) 
{1.12330E-18} 
k(177)  uses  photo  table  ACROLEIN 
{O.OOOOOE+OO} 
k(l78)  =  l.5000E-12  *  (T/300)**(  1.00)  *  exp(  -1726.0/T) 
{4.54753E-15} 
k(l79)  =  4.1400E-12  *  (T/300)**(  1.00)  *  exp( 
452.9/T) 
{l.87990E-ll} 
k(l80)  =  4.2000E-12  *  (T/300)**(  1.00)  *  exp( 
181.2/T) 
{7.66335E-12} 
k(181)  =  7.7000E-14  *  (T/300) ** (  l. 00)  *  exp{  1298 .3/T) 
{5.96598E-12} 
k(l82)  =  8.4000E-14  *  (T/300)**(  l.00)  *  exp( 
221.4/T) 
{1. 75402E-13} 
k(l83)  =  3 .4000E-14  *  (T/300)**(  l.00)  *  exp( 
221.4/T) 
{7.09961E-14} 
k(l84)  =  7.5100E-16  *  (T/300)**(  1.00)  *exp(  -1519.7/T) 
{4.54966E-18} 
k(185)  uses  photo  table  ACROLEIN 
{O.OOOOOE+oo} 
k(186)  = 
l.6000E+l6  *  (T/300)**(  1.00)  *  exp(-13486.0/T) 
{3.52536E-04} 
k(l87)  =  4.2000E-12  *  (T/300)**(  1.00)  *  exp( 
181.2/T) 
{7.66335E-12} 
k(188)  =  2.8000E-12  *  (T/300)**(  1.00)  *  exp( 
181.2/T) 
{S.10890E-12} 
k(l89)  =  7.7000E-14  *  (T/300)**(  1.00)  *  exp (  1298. 3/T) 
{5.96598E-12} 
k(190)  =  9.6000E-13  *  (T/300)**(  1.00)  *  exp( 
221.4/T) 
{2.00460E-12} 
k(191)  = 
221.4/T) 
l.1900E-12  *  (T/300)**(  1.00)  *  exp{ 
{2.48486E-12} 
k(192)  = 
l.1900E-12  *  (T/300)**(  1.00)  *  exp{ 
{2.48486E-12} 
221.4/T) 
k(l93)  =  3.3600E-ll 
{3.36000E-l.l} 
k(l94)  =  4.2000E-12  *  (T/300)**(  l.00)  *  exp( 
{7.66335E-12} 
{5.96598E-12} 
k(l95)  c 
{1. 75402E-13} 
k(196)  =  8.4000E-14  *  (T/300) ** (  1. 00)  *  exp( 
k(l97)  =  3.4000E-14  *  (T/300)**{  1.00)  *  exp{ 
{7.09961.E-14} 
k(l98)  =  7.llOOE-18 
{7.llOOOE-18} 
{O.OOOOOE+oo} 
k{l99)  uses  photo  table  ACROLEIN 
k(200)  =  1.0000E-13 
{l.OOOOOE-13} 
=============================================================================== 

181.2/T) 
7.7000E-14  *  (T/300)**(  1.00)  *  exp(  1298 .3/T) 
221.4/T) 
221.4/T) 

,  scaled  by  3.60000E-03 


PLUME-IN-GRID TREATMENT OF MAJOR POINT SOURCE EMISSIONS 
