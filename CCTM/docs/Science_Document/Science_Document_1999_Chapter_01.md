Chapter 1
==============

INTRODUCTION TO THE MODELS-3 FRAMEWORK AND

THE COMMUNITY MULTISCALE AIR QUALITY MODEL (CMAQ)

EP A/600/R-99/030

Jason Ching and Daewon Byun

Atmospheric Modeling Division

National Exposure Research ]Laboratory
U.S. Environmental Protection Agency

Research Triangle Park, NC 27711

ABSTRACT

Models-3, a flexible software framework, and its Community Multiscale Air Quality (CMAQ)
modeling system form a powerful third generation air quality modeling and assessment tool
designed to support air quality modeling applications ranging from regulatory issues to science
inquiries on atmospheric science processes.  The CMAQ system can address tropospheric ozone,
acid deposition, visibility, fine particulate and other air pollutant issues in the context of "one"
atmosphere perspective where complex interactions between atmospheric pollutants and regional
and urban scales are confronted.  This CMAQ Science Document contains chapters that address
specific scientific and technical issues involved in the development and application ofModels-
3/CMAQ system; collectively, it provides the scientific basis and point of reference for the state of
the science captured in the June 1998 initial release of the CMAQ. This chapter provides an
overview and context of each contributing chapter to the CMAQ system.

·on assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce.
Corresponding author address: Jason Ching, MD-80, Research Triangle Park, NC 27711.
E-mail: Ching.J ason@epamail.epa.gov

••on assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce.

##1.0 INTRODUCTION TO THE MODELS-3 FRAMEWORK AND THE COMMUNITY MULTISCALE AIR QUALITY MODEL(CMAQ)

Air quality simulation models are important tools for regulatory, policy, and environmental
research communities.  In the United States, the Clean Air Act provides a societal mandate to
assess and manage air pollution levels to protect human health and the environment.  The U.S.
Environmental Protection Agency (USEPA) has established National Ambient Air Quality
Standards (NAAQS), requiring the development of effective emissions control strategies for such
pollutants as ozone, particulate matter, and nitrogen species.  National and regional policies are
needed for reducing and managing the amount and type of emissions that cause acid,·nutrient and
toxic pollutant deposition to ecosystems at risk and for enhancing the visual quality of the
environment.  Air quality models are used to develop emission control strategies to achieve these
objectives.  Optimal control strategies should be both environmentally protective and cost
effective.  Up to now, air quality model paradigms typically addressed individual pollutant issues
separately.  However, it is becoming increasingly evident that when pollutant issues are treated in
isolation, the resulting control strategies may solve one set of problems but may lead to
unexpected aggravation of other related pollutant issues.  Pollutants in the atmosphere are subject
to myriad transport processes and transformation pathways that control their composition and
levels.  Also, pollutant concentration fields are sensitive to the type and history of the atmospheric
mixtures of different chemical compounds.  Thus, modeled abatement strategies of pollutant
precursors, such as volatile organic compounds (VOC) and NOx, to reduce ozone levels may
under a variety of conditions, cause an exacerbation of other air pollutants such as particulate
matter or issues of acidic deposition.

The development of comprehensive air quality models started in the late seventies.  The Urban
Airshed Model (UAM) (Morris and Meyers, 1990) followed by the Regional Oxidant Model
(ROM) (Lamb, 1983a, 1983b) provided Eulerian-based models for ozone, the former for urban
and the latter for regional scale.  Strategies for State Implementation Plans (SIPs) used ROM to
provide boundary conditions for UAM simulations.  Attention to acid deposition issues were
addressed in the eighties with the development and evaluation of regional acid deposition models
such as the Regional Acid Deposition Model (RADM) (Chang et al.,  1987; Chang et al.,  1990),
the Acid Deposition and Oxidant Model (ADOM) (Venkatram et al.,  1988), and the Sulfur
Transport and Emissions Model (STEM) (Carmichael and Peters,  l 984a, 1984b; Carmichael et
al., 1991).  Other major modeling systems included the Regional Lagrangian Modeling of Air
Pollution model (RELMAP) (Eder et al.,  i986), a Lagrangian framework system, and semi(cid:173)
empirical and statistical models.  The genre of models of this period were designed to address
specific air pollution issues such as ozone or acid deposition and to be applied under relatively
prescriptive implementation guidance strategies.  Thus, flexibility to deal with other issues, such
as particulate matter or toxics, was very limited.  Further, With the passage of the Clean Air Act
Amendments of 1990 (CAAA-90), a wide range of additional issues were identified including
visibility, fine and coarse particles, indirect exposure to toxic pollutants such as heavy metals,
semi-volatile organic species, and nutrient deposition to water bodies.  The direct response
approach is to modify, adapt or extend current models to handle more complex implementations
and issues but is both cumbersome and limiting.

Seeking a more strategic approach to handle the increased modeling requirements of the CAAA-
90,  more comprehensive modeling approaches appear to be needed. With projections for the
increasing rapid pace of the development of computational capabilities at the start of the nineties,
the opportunity arose for a strategic review of modeling approaches leading to design of a system
that would both meet and keep pace with the increasing requirements on air quality modeling, of
incorporating advances in state-of-science descriptions of atmospheric processes, as well as
eliminate impediments of the current genre of models. The scope of such a system must be able to
process great and diverse information from complicated emissions mixtures and complex
distributions of sources, to modeling the complexities of atmospheric processes that transport and
transform these mixtures in a dynamic environment that operates on a larg~ range of time scales
~overing minutes to days and weeks.  The corresponding spatial scales are ~ommensurately large,
ranging from local to continental scales.  On these temporal and spatial scales, emissions from
chemical manufacturing and other industrial activities, power generation, transportation, and
waste treatment activities contribute to a variety of air pollution issues including visibility, ozone,
particulate matter (PM), and acid, nutrient and toxic deposition. The residence times of pollutants
in the atmosphere can extend to multiple days, therefore transport must be considered on at least
a regional scale.  NAAQS requirements and other goals for a cleaner environment vary over a
range of time scales, from peak hourly to annual averages. These challenges suggest that more
comprehensive approaches to air quality modeling are needed, and that assessments and pollution
mitigation are achieved mqre successfully when the problems are viewed in a "one atmosphere"
context that considers multiple pollutant issues.  Further discussion of the needs for the third(cid:173)
generation air quality modeling system can be found in Dennis et al. (1996) and Dennis (1998).

To meet both the challenges posed by the CAAA-90 and the need to address the complex
relationships between pollutants, the USEP A embarked upon the Models-3 project and developed
the Community Multiscale Air Quality (CMAQ) system, an advanced air quality modeling system
that addressed air quality from this "one atmosphere" multi-pollutant perspective.  Based on its
conceptual design, the high performance computational Models-3 framework serves to manage
and orchestrate air quality simulations, using the CMAQ modeling system.  The Models-3
framework is an advanced computational platform that provides a sophisticated and powerful
modeling environment for science and regulatory commlll1lities.  The framework provides tools
used to develop and analyze emission control options, integrate related science components into a
state-of-the-art quality modeling system, and apply· graphical and analytical tools for facilitating
model applications and evaluation.  Descriptions of the Models-3 architecture are provided in
Ch~pter 2 and in Novak et al. (1998).  CMAQ is a multi-pollutant, multiscale air quality model
that contains state-of-science techniques for simulating all atmospheric and land processes that
affect the transport, transformation, and deposition of atmospheric pollutants and/or their
precursors on both regional and urban scales.  It is designed as a science-based modeling tool for
handling all the major pollutant issues (including photochemical oxidants, particulate matter,
acidic, and nutrient deposition) holistically.

More than six years of investment and, commitment .from Federal staff and from scientists and
model developers from the environmental and information communities were expended to
develop the Models-3 framework and the CMAQ air quality modeling system.  Models-3 CMAQ
was released to the public in June 1998.  The science and model engineering concepts and
progress of the project have been described in two peer-reviewed A WMA Transaction papers
(Byun et al.,  l 995a; Coats et al.,  1995) and others (Byun et al.,  l 995b, 1996, and 1998a; Ching et
al.,  1995).  This release version of the Model-3  software is supported by the following documents
under the overall heading "Third Generation Air Quality Modeling System":

System Installation and Operations Manual.  EPA/600/R-98/069a, National Exposure
Research Laboratory, EPA, Research Triangle Park, NC

Users Manual.  EPA/600/R-98/069b, National Exposure Research Laboratory, Research
Triangle Park, NC

Tutorial.  EPA/600/R-98/069c, National Exposure Research Laboratory, EPA, Research
Triangle Park, NC

Science Concepts of the Third Generation Air Quality Models: Project Report.  In
preparation.  Edited by D.W. Byun and.A. Hanna. National Exposure Research
Laboratory, EPA, Research Triangle Park, NC

.

•

•

•

•

•

Science Algorithms of the EPA Models-3 Community Multiscale Air Quality (CMAQ)
Modeling System, Edited by D.W. Byun and J.K.S. Ching. National Exposure Research
Laboratory, EPA, Research Triangle Park, NC (this document)

The project report "Science Concepts of the Third Generation Air Quality Models" summarizes
the basic atmospheric science concepts and mathematical principles pertinent to the development
of the third generation air quality modeling.  This document discusses the key scientific features
and options incorporated into the Models-3 CMAQ modeling system.

## 1.1 The Models-3 Emissions, Meteorology, and the CMAQ Modeling Systems

The structure of the Models-3/CMAQ system is shown in Figure 1-1.  Orchestrated through the
Models-3 system framework, the Community Multiscale Air Quality (CMAQ) modeling system
incorporates output fields from emissions and meteorological modeling systems and several other
data source through special interface processors into the CMAQ Chemical Transport Model
(CCTM).  CCTM then performs.chemical transport modeling for multiple pollutants on multiple
scales.  With this structure, CMAQ retains a flexibility to substitute other emissions processing
systems and meteorological models. One of the main objectives of this project was to provide an
air quality modeling system with a "one atmosphere" modeling capability based mainly on the
"first principles" description of the atmospheric system. CMAQ contains state-of-science
parameterizations of atmospheric processes affecting transport, transformation, and deposition of
such pollutants as ozone, particulate matter, airborne toxics, and acidic and nutrient pollutant
species.  With science in a continuing state of advancement and review, the modeling structure of
CMAQ is designed to integrate and to test future formulations in an efficient manner, without
requiring the development of a completely new modeling system.  Contents of the CMAQ in the
June 1998  release version ofModels-3 are summarized in Ching et al ..  (1998) and Byun et al. (1998b).

Currently, the Models-3 Emission Projection and Processing System (MEPPS) produces the
emissions and the Fifth Generation Penn State University/ National Center for Atmospheric
Research Mesoscale Model (MM5) provides the meteorological fields needed for the CCTM.
They are considered to meet the present application needs for diverse air pollution problems in
urban and regional scales.  However, given the CMAQ paradigm, and other considerations, the
emissions processing and meteorological modeling systems can be replaced with alternative
processors.

Each of these three modeling systems are described briefly below, where associated chapters of
this document are highlighted to provide directions to more in-depth discussions of these topics:

The PSU/NCAR MM5 meteorological modeling system (Grell et al.,  1994) generates  the
meteorological fields for CMAQ.  MM5 is a complex, state-of-the-science community
model, which is maintained by NCAR.  MM5 is well-documented by its primary
developers in technical notes and referenced journal articles.  Chapter 3 briefly describes
the scientific aspects ofMM5, including grid definitions, model physics, nesting and four(cid:173)
dimensional data assimilation.

The MEPPS emission modeling system is based on the Geocoded Emission Modeling and
Projection System (GEMAP) (Wilkinson et al., 1994) now known as the Emission
Modeling System-95 (EMS-95).  MEPPS processes emission inventory data, performs
future projections (including control scenarios), and pre-processes data for use in the
CMAQ model (Chapter 4).  It provides speciated emissions consistent with CB-IV or
RADM2 chemistry mechanisms.

The CMAO chemical transport modeling system (CCTM) is then used to perform model
simulations for multiple pollutants and multiple scales with these input data (Chapters 6,
7, 8, 9, 10 and 11).  The fundamental concepts used for the one-atmosphere dynamic
modeling is described in Chapter 5.  The techniques used for the management of
CMAQ's source code are discussed in Chapter 18.

The CMAQ modeling system also includes  interface processors that process input data
for the emission and meteorological modeling systems, and other processors that calculate
photolysis rates, and develop initial and boundary conditions (Chapters 4, 12, 13, and
14).  CMAQ also has an internal program control processor which is disc':lssed in
Chapter 15.

Using the analysis routines provided in Models-3, the CMAQ output can be processed to
provide process analysis information (Chapter 16) and/or analyzed further to provide
aggregated statistical information (Chapter 17).

An important design requirements for CMAQ is that it addresses multiple scales and pollutants,
which requires that governing equati9ns and computational algorithms among the different
systems should be consistent and compatible across the multiple scales.  However, modeling
assumptions used in various modeling systems may not be valid across all scales.  For example,
the atmospheric dynamics description in a meteorological model may have been optimized for
application of certain scale or limited range of scales (e.g., global vs mesoscale vs complex terrain
to urban).  It is incumbent upon the user community to ensure the model component formulations
are applicable to the range of scales upon which CMAQ is applied.  The current version of MM5
and the CCTM is designed for regional to urban scales.  Furthermore, when using nesting
procedures to scale down  from regional to urban scales and for avoiding feedback between the
scales, one way nesting is recommended.  In addition to the challenges of creating a multiscale air
quality model, CMAQ's multi-pollutant capability cannot be achieved ifthe emissions modeling
system does not provide appropriate precursor or pollutant emissions to the chemical transport
model (CTM).  The development of Models-3 and CMAQ overcome these hurdles by providing
the flexibility to modify specific requirements (e.g., chemical mechanisms, model inputs, etc.), a
generic coordinate system that ensures consistency across spatial scales, and user interfaces that
can integrate alternative emissions or meteorological modeling systems.

## 1.2 CMAQ Interface Processors

The CMAQ modeling system includes interface processors to incorporate the outputs of the
meteorology and emissions processors and to prepare the requisite input information for initial
and boundary conditions and photolysis rates to the CCTM.  Figure 1-1  illustrates the relationship
and purpose of each of the CMAQ  processors (and requisite interfaces) and their relation to the
chemical transport modeling system.  The arrows show the flow of data through the modeling
system.  Two additional functional features of the CMAQ system are included, one for process
analysis, which is primarily for model diagnostic analyses, and a second one that is an aggregation
methodology for estimating longer term averaged fields.  Each of these processors is described
briefly below, and the associated chapter numbers are also listed to note where detailed
discussions can be found on the topics.

ECIP

Emissions-Chemistry
· Interface Processor

(Ghapter4)

//

Program Control
Processiilg
(Chapter 1 5)

/
./

1,(

/---------------·

MCIP

Meteorology-Chemistry"

Interface Processor

(Chapter 12)

Fundamentals or"·,,

Dynamics for CMAQ
(Chapter 5)

··,.,

· "·,"

I

l:
/

LUPROC

Land use Processor

(Chapter 1 :i)

ICON and BCON
Initial and Boundary
Condition Processors

Chapter 13)

JPROC

Photolysis Rate

Processor

(Chapter 14)

Figure 1-1.  Emissions and Meteorological modeling systems and the CMAQ
Chemical Transport Model and Interface Processor

•

•

•

The Emission-Chemistry Interface Processor (ECIP) translates data from the MEPPS
emission model for use in the CCTM.  ECIP generates hourly three-dimensional emission
data for CMAQ from the separate source type files produced by MEPPS, which include
mobile, area, and point sources (Chapter 4).  ECIP calculates the plume rise and initial
vertical plume spread of point source emissions to determine the vertical level(s) of
CCTM into which poinj source emissions should be introduced.  Since meteorological
conditions affect both point source plume rise and biogenic emissions, meteorological data
from MCIP is also used in ECIP.

The Meteorology-Chemistry Interface Processor CMCIP) translates and processes model
outputs from the meteorology model for the CCTM (Chapter 12).  MCIP interpolates the
meteorological data if needed, converts between coordinate systems, computes cloud
parameters, and computes surface and planetary boundary layer (PBL) pru_:ameters for the
CCTM.  MCIP uses landuse information from the landuse processor (LUPROC) to
calculate the PBL and surface parameters.

Initial Conditions and Boundazy Conditions QCON and BCON) provide concentration
fields for individual chemical species for the beginning of a simulation and for the grids
surrounding the modeling domain, respectively.  The ICON and BCON processors
(Chapter 13) use data provided from previous three-dimensional model simulations or
from clean-troposphere vertical profiles.  Both the vertical profiles and modeled
concentration fields have a specific chemical mechanisms associated with them, which are
a function of how these files were originally generated.

•

The photolysis processor CJPROC) calculates temporally varying photolysis rates
(Chapter 14).  JPROC requires vertical ozone profiles, temperature profiles, a profile of
the aerosol number density, and the earth's surface albedo to produce the photolysis rates
for the CCTM.  JPROC uses this information in radiative transfer models to calculate the
actinic flux needed for calculating photolysis rates.  JPROC generates a lookup table of
photo-dissociation reaction rates.

Each of these CMAQ interface processors incorporates raw data into CMAQ and performs
functions such as calculating parameters and interpolating or converting data.  Raw input data is
currently specified in the source code for JPROC, LUPROC, ICON, and BCON.  However, the
interface processors in future releases of CMAQ will be modified to handle a more generalized set
ofraw input data, so that alternative data sets with varying resolutions or measurement units can
be used.

## 1.3 The CMAQ Chemical Transport Model (CCTM)

The CCTM simulates the relevant and major atmospheric chemistry, transport and deposition
processes involved throughout the modeling domains.  The fundamental basis for CMAQ's one(cid:173)
atmosphere dynamics modeling is discussed in Chapter 5.  Governing equations and model
structure, including definitions of CMAQ's science process modules, are discussed in Chapter 6.
The science options available to the user include the gas phase chemistry mechanisms, RADM2
and CB-IV, a set of numerical solvers for the mechanisms, options for horizontal and vertical
advection schemes, algorithms for fine and coarse particulate matter predictions, photolysis rates,
and a plume-in-grid approach.  Through the Models-3 :framework, CMAQ simulations can be
developed using these different options without modifying source code.  A general overview of
these science process options is provided below along with a reference to the chapter(s) of this
document where more scientific detail can be found.

Advection and Diffusion (Chapter 7): Several advection methods are implemented in the
CMAQ; these include a scheme by Bott (1989), a piecewise parabolic method (PPM)
(Collela and Woodward, 1984), and the Yamartino-Blackman cubic algorithm.  Options
for computing subgrid vertical transport include eddy diffusion, and the Asymmetric
Convective Model (ACM) (Pleim and Chang, 1992) applicable to convective conditions.
Horizontal diffusion is modeled using a constant eddy diffusion coefficient. Numerical
methods differ in the handling of advection of concentration fields.

Gas Phase Chemistry (Chapter 8):  CMAQ includes both the RADM2 and CB4 gas-phase
chemical mechanisms.  The CMAQ version of CB4 includes the most recent
representation of isoprene chemistry and two additional variants of the RADM2
mechanism also contain the newer isoprene chemistry at two levels of detail.  In addition,
CMAQ provides the capability to edit these mechanisms or to import a completely new
mechanism by means of a generalized chemical mechanism processor.  CMAQ also
accounts for the formation of secondary aerosols and the reactions of pollutants in the
aqueous phase, and aqueous reactions are simulated by means of the aqueous chemical
mechanism incorporated in RADM.  All CMAQ gas-phase mechanisms are linked to these
processes to provide the capability to simulate multi-phase interactions.

Two chemistry solvers are available -- the Sparse Matrix Vectorized Gear (SMVGEAR)
algorithm developed by Jacobson and Turco (1994) and the Quasi-Steady State
Approximation (QSSA) method used in the Regional Oxidant Model.  SMVGEAR is
generally recognized as the more accurate of the two, but it is much slower than QSSA on
non-vector computers.

Plume-in-Grid (PinG) Modeling (Chapter 9):  CMAQ includes algorithms to treat subgrid
scale physical and chemical processes impacting pollutant species in plumes released from
selected Major Elevated Point Source Emitters (TvIEPSEs).  The PinG modules simulate
plume rise and growth, and the relevant dynamic and chemical reaction processes of
subgrid plumes.  PinG can be used for the simulations at 36 km and 12 km resolutions,
PinG is not invoked at 4 km resolutions and the MEPSE emissions are directly released
into the CTM 3-D grid cells.

Particle Modeling and Visibility (Chapter 10): One of the major advancements in CMAQ
is the modeling of fine and coarse mode particles, with the use of the fine particle model
described in Binkowski and Shankar (1995).  CMAQ predicts hourly gridded
concentrations of fine particle mass whose size is equal to or foss that 2.5 microns in
diameter (PM 25), speciated to sulfate, nitrate, ammonium, organics and aerosol water.
Secondary sulfate is produced when hydroxyl radicals react with sulfur dioxide to produce
sulfuric acid that either condenses to existing part1cles or nucleates to form new particles.
CMAQ model output includes number densities for both fine and coarse modes.  The
modeling of aerosols in CMAQ also· provides the capability to handle visibility, which is
another CMAQ output.  In another potential application, CMAQ can provide the basis for
modeling the atmospheric transport and deposition of semi-volatile organic compounds
(SVOC) with parameterizations for their rates of condensation to and/or volatilization
from the modeled particles.

Cloud processes (Chapter 11 and 12): Proper descriptions of clouds are essential in air  ·
quality modeling due to their critical role in atmospheric pollutant transport and chemistry
processes. Clouds have both direct and indirect effects on the pollutant concentrations:
they directly modify concentrations via aqueous chemical reactions, vertical mixing, and
wet deposition removal processes, and they indirectly affect concentrations by altering
radiative transmittances which affect photolysis rates and biogenic fluxes.  CMAQ models
deep convective clouds (Walcek and Taylor, 1986) and shallow clouds using the
algorithms as implemented in RADM (Dennis et al.,  1993) for 36 and 12 km resolutions.
At 4 km resolution,  the clouds are generally resolved, and explicit type cloud dominates.

Photolysis Rates (Chapter 14): The photochemistry of air pollutants is initiated by
photodissociation of smog precursors, which are driven by solar radiation. The amount of
solar radiation is dependent on sun angle (time of day), season, latitude, and land surface
characteristics, and is greatly affected by atmospheric scatterers and absorbers.
Photolytic rates are also wavelength- and temperature-dependent.  Within CCTM,
temporally resolved 3-D gridded photolysis rates are interpolated from a lookup table
generated by JPROC processor and corrected for cloud coverage.

## 1.4 Analysis of CMAQ Output

Air quality modeling simulations arise from modeling of complex atmospheric processes. It is
important to assure and to understand the model results.  Sensitivity tests are needed to detect
problems in model formulations and to determine if the model is credible for assessing emission
control strategies.  A very powerful sensitivity analysis tool called process analysis is provided
with CMAQ.  Also, an aggregation technique is provided with CMAQ.  Aggregation can be used
to estimate seasonal or annual concentrations for pollutants from CMAQ simulations which are
usually performed for shorter time periods due to time and computational limitations.

### 1.4.1  Process Analysis (Chapter 16)

Sensitivity analyses are needed to detect errors and uncertainties introduced into a model by the
parameterization schemes and the input data.  Results must also be analyzed to ensure that
realistic values are obtained for the right reasons rather than through compensating errors among
the science processes.  Process Analysis techniques quantify the contributions of individual
physical and chemical atmospheric processes to the overall change in a pollutant's concentration,
revealing the relative importance of each process.  Process analysis is particularly useful for
understanding the effects from model or input changes.  CMAQ provides the capability to
perform process analyses using two different pieces of information: Integrated Process Rates
(IPRs) and Integrated Reaction Rates (IRRs).

•

•

The IPRs are obtained during a model simulation by computing the change in
concentration of each species caused by physical processes (e.g., advection, diffusion,
emissions), chemical reaction, aerosol production, and aqueous chemistry.  Values provide
only the net effect of each process.  IPRs are particularly useful for identifying
unexpectedly low or high process contributions which could be indicative of model errors.

The IRR analysis involves the details of the chemical transformations.  For gas-phase
chemistry, the CCTM has been designed to compute not only the concentration of each
species, but also the integral of the individual chemical reaction rates.  IRR analyses have
typically been used to understand the reasons for differences in model predictions obtained
with different chemical mechanisms.

### 1.4.2  Aggregation (Chapter 17) ·

In support of sID:dies mandated by the CAAA-90, CMAQ can be used to estimate deposition and
air pollutant concentrations associated with specific levels of emissions. · Asse~sment studies
require estimates of ozone, acidic deposition, particulate matter as well as visibility, on seasonal
and annual time fr~es. A statistical procedure called the "aggregation" has been dev.eloped and
is provided for the CMAQ to derive  the required seasonal and annual est~mates .. This·
methodology is an efficient technique and can be used instead of executing multiple CMAQ model
runs for the intended period of averaging.

A typical CMAQ simulation provides hourly air quality fields for regional to urban scales for
multi-day episodes, typically up to five days in duration.  The new PM2.5  standard includes an
annual average value, so· utilization of CMAQ for PM2.5  will require the use aggregation
techniques in order to estimate annual average PM25 values._ . One such technique, initially
developed for RADM wet deposition applications, was recently modifie~ and successfully applied
to PM2_5  by Eder and LeDuc ( 1996).  The approach utilizes visibility data as a surrogate for PM2_5,
and it will be applied to CMAQ on a.continental scale (i.e., contiguous United States, southern
Canada, and northern Mexico).  Future efforts will be needed to validate this approach when a
network of PM2_5  samplers is deployed; also, aggregation approaches for me~oscale domains will
need to be developed perhaps utilizing the method by Eder and LeDuc (1994).

## 1.5  Management of CMAQ Science Information Objects and Codes in Models-3

The CMAQ source code is managed through the Models .. 3 framework to make the CMAQ
modeling system more efficient and ea·sier to use by applying a program control processor,
management and integration of source code, and implementing a modularity concept.  .Thes~
techniques also help users customize CMAQ for their own modeling applications without so~ce
code modifications.

### 1.5.1  Program Control Processors (Chapter 15)

Certain science information, such as grid and layer definitions and dimensions~ chemical
mechanisms, species list, model configurations, and-episode (case) speCification, is used
repeatedly across the several process components in Models-3 CMAQ modeling system.
Program control processors are a set of programs embedded in the Models-3 framework to
handle these science information components and their codes. ·Program Control Processing (PCP)
refers to setting up internal arrays, mappings of species names in the input processors, defining
global parameters, and establishing linkages among processors in the CMAQ system.  PCP allows
users to define globally shared information on model components, and it uses that information to
generate the global FORTRAN include files required for building a model in CMAQ.

### 1.5.2  CMAQ Code Integration (Chapter 18)

CMAQ's modularity facilitates efficient coordination of development work and management of
the science codes.  Chapter 18 describes the modularity concepts, code management method, and
integration schemes of CMAQ science code with the Models-3 framework.  Integrating the
CMAQ code into the Models-3 framework is achieved by following a set of design, coding, and
implementation standards that include:

•

•

•

A standard subroutine interface at the module level

The restriction of coding practices to avoid practices that can conceal data dependencies,
hinder maintenance and foster hidden bugs

The Models-3 Input/Output Applications Programming Interface (1/0 API)
(http://www.iceis.mcnc.org/EDSS/ioapi/index.html/), which contains standardized file I/O
functions.  The 1/0 API is an interface built on top of self-describing netCDF
(http://www.unidata.ucar.edu/packages/netcdf/) files that are portable across most Unix
platforms.

## 1.6 Post Release Studies and Near-Future Plans

### 1.6.1  CMAQ Evaluation Study

It is important to conduct extensive evaluation of the CMAQ. Subsequent to the initial release of
the Models-3/CMAQ, the development team is engaged in a substantial program of evaluation.
The scope of the effort includes analyses of the  performance and veracity of each individual
process module as well as the integrated air quality system.  Findings from this evaluation can be
incorporated into future releases of the CMAQ modeling system.  The degree and rigor of this
evaluation provides the basis for understanding the strengths or weaknesses of the current state(cid:173)
of-science in CMAQ. The evaluation of the initial release version of CMAQ is underway for three
nested grids with 36, 12, and 4 km grid resolutions.  With these results, CMAQ's performance
can be evaluated on both the regional and urban scales. This model evaluation activity for CMAQ
will be staged with the initial efforts to show relative performance against the RADM model,
which has undergone extensive model evaluation efforts.  Diagnostic evaluation will continue
using databases from different regional studies such as the 1995 Southern Oxidant Study
conducted in the vicinity ofNashville, TN and the 1995 NARSTO-NE study.

### 1.6.2  Testing Operational Configurations

CMAQ can be configured for a wide range of applications from science studies and investigations
to regulatory applications.  While the scientific community can take advantage of the CMAQ open
system and flexibility to create alternative applications of CMAQ for research and development
purposes, regulatory applications depend upon a standardized, evaluated form of CMAQ.  The
CMAQ evaluation program will provide the scientific benchmark needed for this.  As science
advances in CMAQ, future configurations of a more operational nature can also be periodically
re-benched as appropriate.  As understanding of atmospheric processes improves, it is a natural
tendency for models to become more complex and have increased computational demands.
Efforts are underway to improve model computational efficiencies to compensate for this.

### 1.6.3  Extensions and Science Additions

Two major extensions are planned for the CMAQ modeling

A version of the SAPRC-97 gas phase mechanism will be incorporated into CMAQ, in
addition to the current CB-IV and RADM2 mechanisms available.  The initial
configuration of SAPRC will be in a fixed parameter mode, with a preset number of
organic species.  Another possible future implementation of SAPRC will allow the user to
select from about 100 organic surrogate species in the semi-explicit SAPRC mechanism to
construct a user-defined, smaller SAPRC mechanism.  Another approach for representing
gas-phase chemistry in CMAQ is also being developed.  It will use a limited number
reactive entities termed "morphecules" to include in the chemical reactions while using a
much larger number of chemical species (singly or lumped) called allomorphs to provide
extra chemical detail.  This latter approach provides a means for including a much more
detailed representation of atmospheric chemistry than conventional chemical mechanisms
without significantly increasing computational burden, albeit at the expense of additional
computer memory.

A new emissions processor will be implemented which is called the ~Sparse Matrix
Operator Kernel Emissions modeling system (SMOKE)
(http://enypro.ncsc.org/productsD.  The linear operations used in emission processing can
be represented as multiplications by matrices.  Since most entries in these matrices are
zeros, the SMOKE model formulates emissions modeling in terms of sparse matrix
operations that require considerably less time to perform than current systems.  Efficiency
is enhanced even further when considering variations in emissions projections from base
. case scenarios by temporally modeling once per episode, calculating gridding matrices
only· once per grid, and calculating speciation matrices only once per chemical mechanism.

Development and testing of several science options is underway for incorporation into future
releases of the CMAQ.  These include an advanced surface-PBL linked system (Pleim and Xiu,
1995), optional meteorological processors such as the Regional Atmospheric Modeling System,
RAMS, and an advanced 4-D Photolysis Rates Processor.

·

## 1. 7 Opportunities and Encouragement for Long Term Extensions and Science Community Involvement

The Models-3/CMAQ concept is based on an open system design, we encourage the full
participation and involvement of the scientific and modeling communities in the growth and use
ofModels-3 CMAQ.  As described in this document, the Models-3  CMAQ system has flexibility
for incorporating scientific and modeling advances into CMAQ processors, for testing of
alternative modeling techniques for science processes, and for extending its current capability to  .
handle multimedia environmental issues.  Additionally, the community of users should be vigilant
in performing evaluations against improved databases and measurement technology to assess the
realism of model performance and to measure the strengths and weaknesses of the current state of
the-science as presented in the CMAQ modeling system.  Some suggestions for extensions and
community involvement are provided below, but certainly not limited to:

(1)  Modeling airborne and deposition of atmospheric toxic pollutants:  A key opportunity for
CMAQ is the development of a modeling capability for toxic pollutants into the CMAQ chemical
transport modeling system.  Models of airborne toxic pollutant provide an important tool for
understanding the transport and chemical pathways that are concerns to human exposure
assessments and for risk assessments and its management.  It also provides a powerful means to
assess the exchange of toxic compounds between the atmosphere and sensitive ecosystems.
However, developing such models is challenging.  Air toxics arise from a wide variety of sources,
which may have a wide range of chemical lifetimes and reactivity.  Consequently, their
toxicological impacts as well as their time-space distributions may be highly variable.  These
complex mixtures of reactive compounds can exhibit wide range and variability in physical
properties and exist at various gas, liquid or particulate ambient metastates.  Modeling paradigms
might evolve from introducing gas-particulate partitioning of the semi-volatile species (Ching et
al., 1997) to more fundamental modeling with detailed chemical mechanisms.

(2)  Development of modeling capability to link with human exposure models:  With the ability to
simulate toxic pollutant processes in addition to the current photochemical oxidants and
particulates, it is planned to transport the CMAQ model to a finer than urban scale to link with
human exposure models.  International efforts to determine and understand the etiology of
adverse human effects of air pollutants, especially those that or associated with fine particles is
underway.  Modeled concentrations of air pollutant constituents at neighborhood scales when
coupled with the limited numbers of sampling provide a powerful basis for driving human
exposure models.  With this necessary data, human exposure models provide a basis for the
causality studies and for risk assessment research.  Models haye not yet been developed to predict
the spatial and temporal distributions of the various causal pollutant classes under current
consideration at the neighborhood scale.  The downscaling requirements represent a great
challenge to the scientific basis in current meteorological processors.

(3)  Advanced data assimilation capability:  As air quality modeling efforts extend to finer
horizontal resolutions and time scales, it becomes increasingly important to make use of higher
frequency asynoptic meteorological data such as satellite, radar wind profilers and NEXRAD.
There is a need  to develop and extend data assimilation techniques to these meteorological data
sources.  Improved methods for introducing cloud information into the CMAQ should also be
investigated. Such fields have significant impact on chemical photolysis rates, the atmospheric and
surface energy budgets, the stability and dispersive power of the boundary layer, and aqueous
chemistry.

(4)  Development of an air quality predictive mode:  With further development of the emissions
and meteorological modeling systems, plans include developing a predictive capability in the
CMAQ modeling system.  CMAQ is currently developed in retrospective mode for performing
assessments and for implementation of the National Ambient Air Quality Standards (NAAQS).  In
principle, CMAQ may be extended to a forecast mode in order to provide air quality advisories
for pollutants such as ozone and visibility and for operational support for issues such as smoke
and PM/haze from prescribed fires.  These capabilities will require the development and testing of
day-specific emissions inventories and a real time data processing system for the meteorological
mode.

( 5)  Up and downscale links to global scale models:  New linkages with other areas of modeling,
including scale and media, are envisioned.  It is hoped that information from the urban and
regional CMAQ applications and from global modeling applications can be bridged.  Because
CMAQ offers the state of science to simulate atmospheric process as realistically as possible at
time scales commensurate with reality, CMAQ output can be used to benchmark or examine the
parametric basis of process formulations in global models.  From a downscale perspective, global
model output can be used to improve or enhance the initial and boundary conditions in CMAQ
regional and urban scale simulations.

(6)  Ecosystem modeling:  Efforts to combine environmental modeling techniques to encompass
an entire ecosystem is needed to address issues including: (a) nutrient cycle modeling, which
includes pathways through the atmosphere, water bodies, and soil; and (b) acidic wet and dry
deposition into sensitive ecosystems, including critical load analyses.  With this ecosystem
modeling approach, air quality issues can be studied in combination with other aspects of
environmental health.  For example, nitrogen deposition can cause adverse nutrient loadings to
ecosystems that can result in a reduction of water quality due to adverse biological responses.
Further, toxic deposition can lead to adverse indirect hun1an exposure from bioaccumulation
through the food chain.

## 1.8 References

Binkowski, F.S., and U. Shankar,  1995: The Regional Particulate Model: Part I.  Models
description and  preliminary results. J.Geophys.  Res., 100(Dl2): 26191-26209.

Bott, A.,  1989: A positive definite advection scheme obtained by nonlinear renormalization of the
advective fluxes, Mon.  Wea.  Rev.  117:  1006-1015.

Byun D. W., A. Hanna, C. J.  Coats, and D. Hwang, 1995a: Models-3 air quality model prototype
science and computational concept development.  Transactions of Air &  Waste Management
Association Specialty Conference on Regional Photochemical Measurement and Modeling
Studies, Nov. 8-12, San Diego, CA.  1993, 197-212.

Byun, D. W., C. J.  Coats, D. Hwang, S. Fine, T. Odman, A. Hanna and K. J.  Galluppi,  l 995b:
Prototyping and implementation of multiscale air quality models for high performance computing.
Mission Earth Symposium, Phoenix, AZ, April 9-13, 1993. 527-532.

Byun, D.W., D. Dabdub,  S. Fine,  A. F. Hanna, R.  Mathur, M. T. Odman, A. Russell, E. J.
Segall, J. H. Seinfeld, P. Steenkiste, and J. Young, 1996:  Emerging· Air Quality Modeling
Technologies for High Performance Computing and Communication Environments, Air Pollution
Modeling and Its Application XI, ed. S.E. Gryr).ing and F. Schiermeier. 491-502.

Byun, D.W., J.K.S. Ching, J. Novak, and J. Young,  1997:  Development and Implementation of
the EPAis Models-3 Initial Operating Version: Community Multi-scale Air Quality (CMAQ)
Model, 1998a: Twenty-Second NATO/CCMS International Technical Meeting on Air Pollution
Modelling and Its Application, 2-6 June, 1997.  Air Pollution Modeling and Its Application XII,
ed. S.E. Gryning and N. Chaumerliac, Plenum Publishing Coorp. 357-368.

Byun, D.W., J. Young., G.  Gipson., J.  Godowitch., F. Binkowsk, S. Roselle, B. Benjey, J.  Pleim,
J.K.S. Ching., J. Novak, C.  Co~ts, T. Odman, A. Hanna, K. Alapaty, R.  Mathur, J.  McHenry, U.
Shankar, S. Fine, A. Xiu, and C. Jang,  1998b: Description of the Models-3 Community Multiscale
Air Quality (CMAQ) model.  Proceedings of the American Meteorological Society 78th Annual
Meeting, Phoenix, AZ, Jan.  11-16, 1998: 264-268.

Carmichael G.R. and L.K. Peters, 1984a: An Eulerian transport/transformation/removal model for
802 and sulfate-I. Model development, Atmos.  Environ.  18: 937-952.

Carmichael G.R. and L.K. Peters, 1984b: An Eulerian transport/transformation/removal model for
S02 and sulfate-II. Model calculation of SOx transport in the Eastern United States, Atmos.
Environ.  20:  173-188.

Carmichael G.R., L.K. Peters, and R.D. Saylor, 1991: The STEM-II regional scale acid
deposition and photochemical oxidant model-I. An overview of model development and
applications, Atmos. Environ.  25A: 2077-2090.

Chang, J.S., P.B. Middleton, W.R. Stockwell, C.J. Walcek, J.E. Pleim, H.H. Landsford, S.
Madronich, F.S. Binkowski, N.L. Seaman, and D.R. Stauffer, 1990: "The Regional Acid
Deposition Model and Engineering Model." NAP AP SOS/T Report 4, in National Acid
Precipitation Assessment Program: State of Science and Technology,  Volume 1.  National Acid
Precipitation Assessment Program, 722 Jacksn Place, N.W., Washington, D.C., September 1990.

Chang, J.S., R.A. Brost, LS.A. Isaksen, S. Madronich, P. Middleton, W.R. Stockwell, and C.J.
Walcek, 1987: A three-dimensional Eulerian acid deposition model: Physical concepts and
formulation." J.  Geophy.  Res.  92:  14681-14700.

Ching, J.K.S., D.W. Byun, A. Hanna, T. Odman, R. Mathur, C. Jang, J. McHenry, K.  Galluppi,
1995: Design requirements for multiscale air quality models. Mission Earth Symposium, Phoenix,
AZ, April 9-13 p532-538."

Ching, J.K.S., D.W. Byun, J.  Young, F. Binkowsk., J. Pleim, S. Roselle, J.  Godowitch, W.
Benjey., and G. Gipson,  1998: Science features in Models-3 Community multiscale air quality
system. Proceedings of the American Meteorological Society 78th Annual Meeting, Phoenix, AZ,
Jan.  11-16, 1998:269-273

Ching, J.K.S., Francis S. Binkowski, and 0. Russell Bullock, Jr., 1997: Deposition of semivolatile
toxic air pollutants to the Great Lakes: A regional modeling approach. In " Atmospheric
Deposition of Contaminants to the Great Lakes and Coastal Waters, Ed by Joel Baker, SET AC
Press.  pp 293-~04.

Coats, C.  J., A.H. Hanna, D. Hwang, and D.W. Byun, 1995:.Model engineering concepts for air
quality models in an integrated environmental modeling system.  Transactions of Air &  Waste
Management Association Specialty Conference on Regional Photochemical Measurement and
Modeling Studies, Nov. 8-12, San Deigo, CA.  1993, 213·-223.

Colella, P., and P. L. Woodward, 1984: The Piecewise Parabolic Method (PPM) for gas(cid:173)
dynamical simulations,  J  Comput.  Phys. 54:  174-201.

Dennis, R, 1998: The Environmental Protection Agency's third generation air quality modeling
system: An overall perspective. Proceedings of the American Meteorological Society 78th Annual
Meeting, Phoenix, AZ, Jan.  11-16, 1998: 255-258.

Dennis, R.L., D. W.  Byun, J.H. Novak, K.J.  Galluppi, C.J. Coats, and M.A. Vouk, 1996:  The
next generation of integrated air quality modeling: EPA's Models-3. Atmospheric Environment,
30, No.  12, 1925-1938.

Eder, B.K., D.H. Coventry, C; Bollinger and T.L. Clark: RELMAP, 1986: A Regional
Lagrangian Model of Air Pollution User's Guide.  U.S. Environmental Protection Agency Report
EPA/600/8-86/013, Research Triangle park, NC 146 pp.

Eder, B.K., J.M. Davis and P. Bloomfeld, 1994: An automatic classificatioJ:! scheme designed to
better elucidate the dependence of ozone on meteorology. J  Appl. Meteor, 33: 1182-1199.

Eder, B.K. and S. LeDuc,  1996: Can selected RADM simulations be aggregated to estimate
annual concentrations of fine particulate matter?  Reprint from the 11th Annual  International
Symposium on the Measurement of Toxic and  Related Pollutants, May 7-10, 199(>, RTP, NC
732-739.

Gillani, N.V., A. Biazar, Y. Wu, J.  Godowitch, J. Ching, R. Imhoff, 1998: The Plume-in-Grid
treatment of major elevated point source emissions in Models-3.  10th Joint  Conference on
Applications of Air Pollution Meteorology  with A WMA, January 11-16, 1998, Phoenix, AZ,
Amer. Meteorol. Soc., Boston, MA.

Grell, G.A., J. Dudhia, and D.R. Stauffer, 1993:.A description of the Fifth-Generation Penn
State/NCAR  Mesoscale Model(MM5). NCAR Technical Note,  NCAR/TN-398+IA

Jacobson M. and R.P. Turco, 1994:  SMVGEAR: A Sparse-Matrix, vectorized Gear code for
atmospheric models. Atmos.  Environ.  28: 273-284.

Jeffries, H. E. and S. Tonnesen, 1994: A comparison of two photochemi.cal reaction mechanisms
using mass balance and process analysis, Atmos. Environ, 28(18): 2991-3003.

Lamb, R.G., 1983a: A Regional Scale (1000 km) Model of Photochemical Air Pollution,  Part I:
Theoretical Formulation. EPA-600/3-83-035, U.S. Environmental Protection Agency, Research
Triangle Park, NC.

Lamb, R.G.,  1983b: A Regional Scale (I 000 km) Model of Photochemical Air Pollution,  Part 2:
Input Processor Network Design. EPA-600/3-84-085, U.S. Environmental Protection Agency,
Research Triangle Park, NC.

Morris, R.E. and T.C. Meyers, 1990:  User's Guide for the  Urban Airshed Model,  Volume  I:
User's Manual for UAM(CB-IV).  EPA-450/4-90-007A. U.S. Environmental Protection Agency,
Research Triangle Park, NC.

Novak, J., J. Young, D.W. Byun, C.  Coats, G.  Walter, W. Benjey, G.  Gipson, S. LeDuc,  1998:
Models-3: A unifying framework for environmental modeling and assessments.  Preprint Volume,
10th Joint AMS and A&WMA Conference on the Applications of Air Pollution Meteorology,
Phoenix, AZ, Jan 11-16, 1998: 259-263.

Pleim J.E., and J.S. Chang, 1992: A non-local closure model in the convective boundary layer.
Atm Environ., 26A: 965-981.

Pleim, J.E., and A. Xiu, 1995: Development and testing of a surface flux and planetary boundary
layer model for application in mesoscale models, J.  Appl. Meteor., 34:16-32.

Wilkinson, J.G., C.F. Loomis, D.E. McNally, R.A. Emigh, and T.W. Tesche, 1994: Technical
Formulation Document: SARMAPILMOS Emissions Modeling System (EMS-95), Final Report,
Lake Michigan Air Directors Consortium and the California Air Resources Board, AG-90/TS26
and AG-90ffS27, 120pp.

Venkatram, A.K. and P.K. Karamchandani, 1998: The ADOM II Scavenging Module.  ENSR
Consulting and Engineering Report 0780-004-205.  Camarillo, CA.

This chapter is taken from Science Algorithms of the EPA Models-3 Community
Multiscale Air Quality (CMAQ) Modeling System, edited by D. W. Byun and J. K. S.
Ching, 1999.
