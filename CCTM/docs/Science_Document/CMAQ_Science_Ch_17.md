Chapter 17 
============

AN AGGREGATION AND EPISODE SELECTION SCHEME 

DESIGNED TO SUPPORT MODELS-3 CMAQ 

Richard D. Cohn 

Analytical Sciences, Inc. 

Durham, NC 27713 

Brian K. Eder"  and Sharon K. LeDuc .. 

Atmospheric Modeling Division 

National Exposure Research Laboratory 

Research Triangle Park, NC 27711 

ABSTRACT 

In support of studies mandated by the 1990 Clean Air Act Amendments, the Models-3 
Community Multiscale Air Quality (CMAQ) model can be used to estimate pollutant 
concentrations and deposition associated with specified emission levels.  Assessment studies 
require CMAQ-based distributional estimates of ozone,  acidic deposition,  PM2.5,  and visibility on 
seasonal and annual time frames.  Because it is not financially feasible to execute CMAQ over 
such extended time periods,  CMAQ must be executed for a finite number of episodes under a 
variety of meteorological classes.  A statistical procedure called aggregation, must then be applied 
to the CMAQ outputs to derive seasonal and annual estimates. 

The objective of this research is to develop an aggregation approach and set of episodes that 
would support model-based distributional estimates (over the continental domain) of air quality 
parameters.  The approach utilized cluster analysis and the 700 mb u and v wind field components 
over the time period  1984-1992 to define homogeneous meteorological clusters.  A total of 20 
clusters (five per season) were identified by the technique.  A stratified sample of 40 events was 
selected from the clusters, using a systematic sampling technique. 

This stratified sample is then evaluated through a comparison of aggregated estimates of the mean 
extinction coefficients (bc:J to the actual mean bcxt observed at 201  stations nationwide for a nine 
year period (1984-1992).  The bcxt•  a measure of visibility, was selected for use in the evaluation 
for two reasons.  First, of all of the air quality parameters simulated by CMAQ, this visibility 
parameter provides one of the most spatially and temporally comprehensive data sets available, 
and second, the bcxt  can serve as a surrogate for PM2.5 for which little data exists.  Results from 
the evaluation revealed a high level of agreement (r = 0.988) indicating that the aggregation and 
episode selection scheme was indeed representative. 

"On assignment from the National Oceanic and Atmospheric Administration, U.S.  Department of Commerce. 
Corresponding author address: Brian Eder, l'vID-80,  Research Triangle Park, NC 27711. E-mail: 
eder@hpcc.epa.gov 

.. On assignment from the National Oceanic and Atmospheric Administration, U.S.  Department of Commerce. 

t(~O  AN AGGREGATION AND EPISODE SELECTION SCHEME DESIGNED TO 
SUPPORT MODELS-3 CMAQ 

Introduction 

'"' 
,, 

"''" 

','' 
';,,11111111111:::, 
',,1,,,,11111111, 

1:::::111111111111, 
"''" 

'' 

,, 
''""'' 

1

• 

" 

111
'

11:. 

11111111111111

'"'""""' 

"""""""'""""" 

This chapter describes the development of an aggregation and episode selection scheme to 
support the derivation of annual and seasonal estimates of air quality parameters in Models-3 
, CI\1.AQ.  Refovant background information regarding this activity,  as well as a precise statement 
of the present objectives, is provided in this section.  Section 17.2 summarizes the important 
elements of the approach and the overall strategy, including the rationale behind the methods and 
the limitations associated with them.  Details of the development of an aggregation and episode 
selection are provided in sections 17.3,  17.4, and 17.5.  Section 17.6 contains an example 
application and evaluation of the methodology using one particular air quality parameter (bcxt  ), 
and section  17. 7 provides a summary and discussion. 

### 17.1.1  Background 

In support of studies mandated by the 1990 Clean Air Act Amendments, the Models-3 
Community Multiscale Air Quality (CMAQ) model is used by EPA Program Offices to estimate 
deposition ancf rur concentrations associated with specified levels of emissions.  Assessment 
studies and effects models require CMAQ-based distributional estimates (such as annual and 
sea5onal averages) of ozone, acidic deposition, and measures related to visibility.  Such estimates 
would ideally be obtained by using CMAQ to simulate atmospheric chemical processes associated 
with meteorological conditions occurring on a daily basis over several years.  However, for 
logistical and cost reasons it is not currently feasible to execute CMAQ over an extended time 
period such as a full year.  Therefore, in practice CMAQ must be executed for a finite number of 
episodes or "events," which are selected to represent a variety of meteorological classes.  A 
st~!~S!ic'!;l procedure called aggregation, must then be applied to the outputs from CMAQ  to 
d~[~ve ~~,,~,required annual- and seasonal-average estimates from this finite number of events.  The 
objective of the research described in  this chapter is to develop such an aggregation approach and 
evaluate its effectiveness using the bcxt· 

"' 

"'" 

11111111111111 

l,,,,,,,,,111 

111,,,11111111111111111, 

'"'"""""""'"" 

T~e tw,s~c problem of developing representative meteorological categories has been explored by 
other researchers for a variety of purposes, including Fernau and Samson (1990a,b ); Davis and 
Kalkstein (1990);  Eder et al.  (1994).  The approach used here is based on a variation of the 
methods previously used by Brook et al.  (1995a,b) in selecting a 30-event aggregation set for the 
Regional Acid Depositio~ Model (RADM).  The approach of Brook et al.  involved four major 
components.  Cluster analysis of wind fields was used to determine meteorologically 
representative categories.  The determination of the number of clusters to retain was based upon 
within-group variance patterns and prior work by Femau and  Samson (1990a,b).  A procedure for 
aggregating the episodic results into annual totals and averages involved frequency-weighted 
s1ims and estimated deposition-precipitation relationships.  Event selection procedures were 
designed to emphasize·categories that accounted for most of the annual wet sulfate deposition, 

' 

,,,,,,,1111''" 

1
: 

11'

II, 

I 

1"illl111" 

'" 1111, 

"1 

'"  111111111 

while also representing some winter and dry events.  A summary of some specific elements of 
their approach is provided in Table 17-1. 

Table 17-1  Summary of Methodology Used by Brook et al.  (l 995a;  1995b) for RADM 

· 1.  Determination of categories 

· ;,• 

·= 

· UsedV:Varc:l;s {1963) method·~f cluster analysis, which riiinimiies ~thin~clu~ter sµms:of squares, hi.:an  ·· 
~g9j9m~rative, hlerarchlcal mode for wind flqw parameters;  .l1sed east~rn North Amerlcan.zon.at .u an.d 
nierfdfon~t v ~q.~kf:>.a wln-C:(c;Omponents for OOOo·:uTC with  5° s·pat~al reSQt'-1tiq~ (two ... varici~~es·~t 48 grid: 
·::, 

.  . ': 

; .. :··  . 

. ::;:. 

:·  .;;· 

: . 

. 

. 

·. 

'·  Clustered.'.'eonsecutlve'' ~day periods from 1979, 19Sf, and 198:(with subsequent classification of . 
. ,.remei~lng !'running" ~~day P$.r'i~ds ~om 19:7.9~1985 into ohe ... C)f these·.~1usters.:·..  ·:. 
: 

. ···  .  . . 

·:,. 

...  nodes over tliree days). 
• 

, 

'2.  Deter/pination.o(number at cluster~ to r~"tain 

.,. 

· • 

·' :Examh~~d i~epwlse increase~ffh within-gro~p variance with. ·decreasltig num~::rof 'cluSt~i'S. e~pre$sed 

. .. 
. · 
· •  .:  Re~alned 19 clusters.based OJ1  results for wind flow,  sulfate'.. deposition, and prior work t;>y  Ferna~ an~ 
· 
· 

: Sa!"Tison  (1990a,b).  Used q~antitatlve information but somewhat subjective criteria. 

·through F-Statlstic. 

. . .. 

· 

· 

· 

. 

. 

.: 

,. 

.,:. 

3.  De.velopn:ient of aggregation procedure·.·: 

.. · 
.·:·. 

'" 

,.  . 

· .. 

··  ,,. 
. 

···. 

· .. ::. 

. . 

:.· 

· •  Estimated total deposition for th~ group of sampled.categories, ffom the sampled e~ents (weight~; 
sum;:.actounting for the number:of...events sampled from each cluster and frequency of occiJrre.r;ice o_f 
· ·· 
the clusters)." 
· .. 

· 
•··  Scaled up by ratio.of total annual.deposition to an estimate of annual deposition that Is bas~d ori 

.  .  . 

• ·. 

· · 

, . 

·. 

. 

. 

., 

a:ggr~ating the sampled categories.  Estimates used either mean deposition from. a sampled eategory 
ordepo"Sitlon esti~ated.~rom the,deposition-precipita~loh relations~~p in _th~eategory.  .... 

..· 

.. 

·. 

., 

. 

,.· 

... 

4.  Selection of "optimum" set of events 

~  : subdiVided t~e 19 categories Into .;;et and dry subsets,  resulting in 3a new ~ategorles> 

.·.·. 

. 

· 

• 

·Primarily represented categories that accounted for most of the annual wet ~ulfate.depo~itlon (at least 
7~% .When Combined).  Se1~cted 19 ·even.ts from thes~· categories.  AJso  seleCted  t~ events from Winter 
(5) and dry (6f events to represent seasonal deposition differences, dry "periods,  and possible nonlinear 
· ··· 
effeets; 
. 

.,'•  Number of events selected from each category Was  based on· proportionality to the:frequency of.,: 
'occurrence of the category and· the percent ottotal sulfate accounted for by the· category. 
·' 
· 
:Ex.ai.nlned 20 potential" sets (ea.eh randomly-:generated) meeting all crlterla, selected. the one ~hat 
:., . 
,.. 
'.::··  minjmized.RMSE for annual sulfate depos~lo.n (prim~rily} and precipitation (secondarily} at ~3 sites.: . 
.. ·set~cted sets in stages, first choosing a set .. of 19 events from the wet categories and then a:.set .. of 11 
,., 
·  events fr'?m winter and dry periods. 

..  · · 

·.::  . · 

· · · · · 

··· 

· · 

··· 

··: 

· 

. 

· 

· 

·· 

·· 

:··.·· 

### 17.1.2  Objectives 

The present objectives differ somewhat from those which motivated the earlier research.  RADM 
was primarily designed to address issues involving acidic deposition.  CMAQ addresses a more 
diverse cofiection of air quality parameters with equal importance.  In addition, CMAQ will be 
applied to a continental domain that is significantly larger than geographic area for which 
eStimates are supplied by RADM.  The extension from the RADM domain to a continental 
domain is extremely ambitious as it relates to episode selection and aggregation.  Therefore, the 
development of an approach that accommodates this larger continental domain is  particularly 
c~~lenging. 

111" 

'1' 

II' 

,,11111111111111111''"1; 

111,,Jllllllll1 

'1111111'11'  '!I 

11111 

,,,111 

11:,.,,111111 

11

I 

The objective of the activity described in this chapter is the development of an aggregation and 
episode selection approach that supports model-based annual and seasonal air quality estimates 
,.  t~1~~ ~~""~~, !,,~~,~l1  .a~1111111~~~~t~, <:'dt.h resl?ect to sampling uncertainty) as those achieved by RADM, in 
~!!~i~eraJJ.oJ! Q,(~~ JJ?:O.f~ general applicability envisioned for CMAQ both with respect to air 
quality parameters and geographic representation.  This accuracy must be preserved while 
minimizing any additional cost.  In essence, "cost" refers to the number of events for which the 
model provides estimates, each of which adds cost in the form of both computational processing 
time and human labor. 
i7:2  Sum~~ry of the Approach 
, ti~ amJysis ,~~ ~rri'~~,,,,~ut~ 11J,hases, wi~h\~t~rmati~n gatli~red in each p~~~~ co~tAh~ting t~ 
th1e 'design of the next.  For this reason, this chapter is structured to present complete descriptions 
Ill  of the methodology and results achieved in each phase, in sequence, in sections 17.3,  17.4, and 
...  17,5.  T.his se<;tion is intended as an overview of the process prior to those detailed descriptions. 
This overview consists of descriptions of some key elements of the methodology, the rationale, 
s~pe, arid limitations associated with the methodology,  and the strategy used to move in phases 
toward the final result. 

"'II:"',  I''" 

ii'",,,, 

''!il11"ll1 

1
1
1
1'"lllllllll111

: 

!  .. 

"Ill 

1
1
1llllll 
1

"::"::::!!!ll11: 

'1' 

1
1
11111

1
1

1111 

,'" 

.,,:·,;,, 

,lllill'"·,'1'' 

1 iillllliii

. 

''I'" 

.: 

' 

"'1·111'1l

" 

.. 

·II' 

1
1111.
.' 

·.·.'. 

### 17.2.1  Basic Elements of the Methodology 

Simply stated, the methods that we have employed involve the determination of meteorologically 
representative categories, the selection of events from those categories, and the use of evaluative 
tools to ensure that the detailed aspects of those activities are defined in such a way as to achieve 
..  optimal results, to the extent that we can measure optimality. 

1,1,1,l1l1lllllll1'·1:.·1'.· 

,,'il:l:l:1:
·, 
1

1
!!!! 

'',1111111!

!l"!ll··:lli1:1lllllllll 

,II,· 

,!!: 

;i 

··· 

! 

1, 

1111 

11111111111111 

, 

.Jllllii: 

'::'" 

1 

111111!

/\specific goal is the definition of meteorological categories that account for a significant 
. p:~portioiiof the variability exhibited by the air quality characterizations of interest.  The basic 
"::  approach used in the current analysis for the determination of categories and event selection 
c~12;l})Onents is related to that of Brook et al.  (1995a,b), but certain fundamental considerations 
have been modified to reflect the differences inherent in the present objectives as described in 
section 17 .1.  The common element is the cluster analysis of zonal u and meridional v wind 
,,  components to define meteorological categories. 

111111 

,,,,,h 

11111 

:lll1111li1il11' 

1111111111111111, 

"'ll,l,',1

,,,, 

''I 

.I 

The definition of meteorologic~ categories is designed to support the selection of events from 

' tli9se cat~gories in a process known as stratified sampling (Cochran,  1977).  Stratified sampling 
exploits the internal homogeneity of the meteorological categories, or "strata," to achieve more 
precise estimates than would be possible using simple random sampling (i.e.,  randomly selecting 
events without regard to meteorological category).  Certain variations of stratified sampling are 
relevant to this analysis.  One relatively inefficient option for invoking stratified sampling would 
involve selecting the same number of events from each category/stratum.  This is known as "equal 
allocation."  An alternative is "proportional allocation," which involves selecting numbers of 
events in direct proportion to the size of the stratum.  Thus, more events are selected from strata 
that contain large numbers of events than from smaller strata.  This is potentially much more 
efficient than equal allocation, in the sense that it leads to much more precise (i.e., lower variance) 
estimates.  Estimates exhibiting absolute maximum efficiency (i.e., minimum variance) are 
obtained by modifying this method slightly so that the number of events selected from each 
stratum is in direct proportion to the product of the size of the stratum times the internal 
variability (as characterized by the standard deviation of the measurement of interest) within the 
stratum.  Thus,  strata exhibiting significant variability among events are sampled more heavily 
than strata in which events are more uniform.  This is known as "optimum allocation," which is 
identical to proportional allocation when within-stratum variances are equal. 

While wind flow parameters were used to define the meteorological categories, other 
meteorological parameters were used in subsequent phases of the analysis to refine aspects of the 
episode selection methodology,  and as evaluative tools to assess the effectiveness of the 
approach.  These parameters include visibility (as represented by the be,J,  temperature, and 
relative humidity.  Their specific roles are discussed in more detail in the following sections. 

### 17.2.2  Rationale, Scope, and Limitations 

As stated previously, the approach to selecting an aggregation and episode scheme is based upon 
the definition of meteorological categories that account for a significant proportion of the 
variability exhibited by the air quality characterizations of interest.  Strictly speaking these 
characterizations include parameters such as acidic deposition, air concentrations, and measures 
of visibility.  Therefore, it might be argued that the definition of categories should be formulated 
directly using these parameters that are ultimately of interest.  However, it is equally important 
that the model simulate the particular transport mechanisms involved in the associated 
atmospheric processes, and in particular that source-attribution analyses be facilitated.  This 
requires that categories be defined with an emphasis on wind flow parameters.  Indeed, 
characterizations of basic wind field  patterns in essence describe frontal passages, along with all of 
the meteorological properties typically associated with them. 

In view of the importance placed on the accurate simulation of transport mechanisms, a complete 
evaluation of the episode selection and aggregation methodology would require an assessment of 
the accuracy with which transport is represented.  However, this accuracy cannot be measured,  as 
there is no technique available to support a direct, quantifiable assessment of the representation of 
atmospheric transport.  In addition, with the exception ofbcxt>  there are little air quality data 
available with the required spatial and temporal resolution and range to support a direct 
evaluation of the methods described in this chapter with regard to the outcome parameters that 
will be of primary interest in CMAQ. 

For these reasons two meteorological parameters (in addition to bcxJ,  which are known to be 
related to many of the air quality parameters of interest, and for which appropriately resolved data 
are available, were used as evaluative measures in this analysis.  Specifically, temperature and 
relative humidity were used; however, primary emphasis will be placed upon the b0'"' which 
,,,  provides a surrogate measure for fine particles.  It must be recognized that this constitutes a 

secondary evaluative tool, in the sense that the effectiveness of the approach cannot be directly 
measured as it relates to atmospheric transport or to specific air quality parameters, both of which 
are primary outcomes.  For this reason, the methods were not developed and refined solely 
toward the goal of optimizing performance associated with the estimation of visibility.  Instead, 
this performance was evaluated in combination with other considerations that were believed to be 
important but for which performance may not be readily quantifiable. 

Jl:,:1111111' 

111111111111111' 

'11111,,,1•" 

""'" 

11 

11111111111

""11,ll'' 

111'11111'"' 

llh 

### 17.2.3  Strategy 

The basic strategy used for the selection of events to support aggregation-based estimation is 
described in the steps outlined below.  The term "cluster" describes a collection of events that are 
d~1~p;~ ~<;?,,,,,,,~~"""'m~t~,~~<;?,~!'.>~ically si~lar ~ased upon cluster analysis results.  The term "stratum" 
d~§~q})~ ~,,, ~C>I!~ctj~:m of 1T1:~te~r~logically similar events to be used in stratified sampling.  In this 
chapter, "stratum" and "cluster" are essentially interchangeable since the clusters defined in the 
' arl~ysis are ultimately used as the strata for sampling purposes. 

"  ,, 

~~~ 

11::" 

:1111' 

"" 

', 

ii::"';:, 

•j 

111,, 

1. 

,  ' 

, 

,, 

,••' 

'11111, 

, 

"" 

•11111111" 

•· 

,,, 

""  2. 

We explored different approaches to the cluster analysis of wind components, each of 
which resulted in the definition ofa set of clusters (strata) ofmeteorologically similar 
1  ev~nts, for possible use in alternative stratification schemes.  Some alternatives that were 
explored include annually defined clusters (i.e., strata defined using cluster analysis of 
daily wind field  data from several years, without regard to season), seasonally defined 

""1111 

,,,,,,,,  clusters (i.e., strata defined using distinct cluster analyses of wind field  data pertaining to 
different seasons), and regionally defined clusters (i.e., strata defined using distinct cluster 
analyses of wind field data for different geographic regions). 

The alternative stratification schemes explored in step  1 were compared using relative 
efficiencies and meteorological considerations.  The concept of relative efficiency relates 
to the yariance associated with an estimate derived using different sampling schemes.  We 
" explored the variances associated with estimates of the annual means of the evaluative 
meteorological parameters described above (visibility, temperature, relative humidity). 
T~~ r~I,~!:i~,~, effic!ency of each stratification scheme is defined as the ratio of the variance 
associated with simple random sampling to the variance associated with stratified sampling 
, using iil'ai scheme.  A large refative efficiency is indicative of a high degree of precision 
(lower variance) associated with the estimate of interest. As discussed previously,  since 
these evaluative meteorological parameters do not afford a complete, direct assessment of 
the validity of process with regard to the parameters to which it is ultimately to be applied, 
meteorological considerations were taken into account in combination with the relative 
efficiencies in order to select a general stratification scheme.  The selected scheme 
(seasonal stratification) was considered for more detailed refinement in step 3. 

We determined an appropriate number of clusters to retain in combination with an 
acceptable number of events that would be necessary to achieve the objectives.  The 
determination was based on estimated standard deviations associated with several 
alternative formulations,  as well as other considerations.  The standard deviations relate 
specifically to estimates of the annual means and 90th percentiles of the evaluative 
meteorological parameters.  The "other considerations" included the objective of matching 
or exceeding the performance of the current aggregation methodology used for RADM,  as 
well as a goal of avoiding unsampled clusters.  Unsampled clusters (the inclusion of 
clusters for which no events would be selected) were considered to be undesirable because 
there would be no information available in the aggregation process to account for events 
contained in such a cluster.  In concept, our view was that any unsampled cluster should 
be combined with another cluster to which it is most similar.  In practice, we achieved this 
by constraining ourselves to collections of clusters that were adequate to support the 
sampling of at least one event from each cluster.  Twenty clusters were ultimately 
retained,  consisting of five  clusters defined in each of the four seasons,  and the number of 
events necessary to achieve the objectives was determined to be 40. 

A stratified sample of 40 events was randomly selected from the 20 clusters defined in step 
3.  Proportional allocation was used in  determining the number of events to be selected 
from each cluster (stratum).  Optimum allocation was considered but was not used for 
several reasons,  including:  ( 1) it requires the quantification of the variance of a primary 
outcome parameter, whereas only secondary evaluative outcomes (visibility, temperature, 
relative humidity) were available as discussed previously, and (2) the variance of any 
outcome parameter varies geographically,  so that optimum allocation would likely result in 
differing numbers of events depending upon the geographic location, whereas proportional 
allocation would not.  It was verified that,· at least based upon the evaluative outcome 
parameters that are available,  in most geographic locations the distribution of events that 
arises from proportional allocation does not differ substantially from that arising from 
optimum allocation. 

Details concerning the implementation of this approach are discussed in the following sections. 
Sections  17.3  and  17.4 correspond to steps  I and 2 in the strategy outlined above,  and  section 
17. 5 includes a discussion of steps 3 and 4. 

17.3  Cluster Analysis of Wind Fields 

The cluster analysis of wind components is described in this section.  This includes a description 
of the wind field  data, the basic analysis technique, variations of the basic technique, and methods 
of presentation.  Some graphical results of exploratory analyses are also included. 

17~3.l  Description of Wind Data 

To accommodate the continental domain and to achieve adequate spatial resolution,  the cluster 
,~2:1ysis involves data at 336 grid nodes with 2.5°  spatial resolution, as obtained from the 
NQ:E:PlN~~'.'!Q-year reanalysis project (Kalnay et al, 1996).  In this analysis,  700 mb wind 
~owponents for  1800 UTC have been used, in consideration of the mountainous western regions 
:n tf~,111 S,,~11W,~n,,; .... ,11111111111~~,mers ~f!P.~ ~~d were cut back to guard against excessive influence from 
ocean-based meteorology.  Graphical illustrations of this domain are referenced later in this 
section. 

" 1 " "  ' 1111111111 " " 111111 

' ! 11111 ·  " 11111 ' ' 

, 11111111111,,  •  ' 

' " I 

" " ' "  " ' 

' " 

' 

" " 

: ' 

' I " " 

' 

" 

' llli 

111111!:,,, 

1111•!:ii:111111111111: 

' 11·11111111 

1111:''', 

11111111 

17.3.2  Basic Cluster Analysis Technique 

Cluster analysis, in the present formulation,  involves the classification of a set of observations into 
categories that are internally homogeneous with respect to defined multivariate relationships in the 
data.  In this case "multivariate" refers to the multiple variables used to characterize wind fields, 
,,  eonsisting of the u and v components applicable to each location within the previously described 
domain and extending over an event that includes multiple days. 

A 12-year period (1984-1995) was considered in the exploratory cluster analyses, later refined to 
a 9-year period (1984-1992) for the final clustering upon which episode selection was based. 
Since CMAQ is actually run for a 5-day period for each event (the first two days establish initial 
conditions, and model predictions from days 3-5 are saved as a "3-day event"), 5-day periods 
were clustered rather than 3-day periods.  To make the analysis computationally feasible,  the first, 
third, and fifth days of each 5-day event were considered.  Based upon the performance noted by 
Femau and Samson (1990a,b), Ward's method of cluster analysis was used (Ward,  1963), 
minimizing within-cluster sums of squares, in an agglomerative (i.e., moving from many clusters 
toward fewer clusters), hierarchical (i.e., once clusters are joined they cannot be separated) 
process.  Thus, if a single observation (event) is considered to consist of2,016 elements (the 2 u 
,~211111111!" ~~_eon~~ts x  33~ grid nodes  x  3 days considered per event), then the objective of the 
9lq§t~r an!!lysis is to divide these observations into clusters (categories) for which the within(cid:173)
···  gl~11~!er ,~g:t,,,Q,~,,11~9uare~ (sum of squared differences between the elements of individual 
ob~~rvations qi::  m~s) is minimized. 

, 

, 

,
1111111

I 

,,,1111!111111 

111li1111  ""··1 

I' 

11 

1111111,, 

'I 

' 

" 

' 

' 

""' 

'" 

11111111:; 

,1111111!11111111

1
:::1111111111!'' 

, 11111111111111111' 

·1:,,,.11' 

111111

"

11

1
11 

"

''11111111111'' 

In the exploratory analyses,  clusters were initially defined based upon "consecutive" rather than 
"running," or overlapping 5-day periods from 1987-1992.  Then, each remaining event ("running" 
5-day periods from  1984 through 1995) was classified into the cluster that minimizes the sum 
(over the 3 3 6 grid nodes and three days) of the squared deviations of each u and v component 
from the cluster mean u and v.  In the final cluster analysis, using 1984-1992 data, consecutive 5-
day periods from  1984 through 1992 were clustered, and remaining events were classified into 
those clusters according to the same criteria described above.  Cluster analyses were carried out 
using SAS (SAS Institute Inc.,  1989); however, due to the extreme computational burden of these 
analyses it was necessary to calculate the distance matrix externally from the clustering procedure 
itself. 

17.3.3  Illustration of Cluster Analysis Results 

Representative results from the selected exploratory analyses,  as well as all results from the final 
cluster formulation,  are illustrated graphically.  Specifically, cluster definitions are illustrated using 
maps of cluster average wind fields.  In some cases, these are supplemented with specialized maps 
illustrating the intracluster variability in the wind fields.  Star chart histograms are also used to 
illustrate the frequency of occurrence of events from each cluster, for each month of the year. 

To provide proper perspective for a review of this analysis,  it is useful to consider preliminary 
results for a set of 3 0 clusters initially defined using annual data from  1984-199 5.  The most 
prevalent cluster (labeled Cluster 1) accounted for  12.19% of all  5-day events during this time 
period, with nearly all of those events occurring during the summer months.  Map-based graphs of 
mean wind vectors for this cluster are contained in Figure 17- I ( a-c ).  Three maps are included, 
representing mean vectors for days  1,  3,  and 5 of the 5-day event.  These maps illustrate largely 
stagnant conditions associated with an anticyclonic pattern centered over the southeastern U.S.; a 
zonal flow over southern Canada and a trough over the west coast of the U.S. 

As a second example, Figure  l 7-2(a-c) illustrates mean wind vectors for a somewhat less 
prevalent cluster that accounted for 3.65% of all  5-day events during this time period (ranking 
ninth in overall prevalence and labeled as Cluster 9).  Most of these events occurred between the 
months of October and April,  characterized by northwesterly winds associated with a large-scale 
trough moving through the central and eastern portion of th~ domain. 

While these maps are effective in illustrating average behavior associated with each cluster, they 
do not give any indication of the variability inherent in the clusters.  Figures 17-4 (a-c) and  17-5 
(a-c) contain additional maps that address this issue.  The first  map (Figure  l 7-3a) illustrates the 
wind field for the third day of an individual event (January 23-27,  1989) that was assigned to 
Cluster 9.  Comparison of this map to the mean wind field  for day 3 of Cluster 9 (Figure 17-2b) 
reveals fairly close resemblance between the two.  By contrast, the maps in Figures 17-3b and  17-
4c depict the third day from two other events belonging to Cluster 9.  These patterns do not 
resemble the mean wind field  as closely, and are indicative of the variability among individual 
events that were assigned to this cluster. 

Clearly it would be useful to simultaneously visualize the variability exhibited by all of the wind 
fields assigned to each day of Cluster 9,  and Figure 17-4 ( a-c) represents an attempt to do this. 
These maps contain the mean wind vectors for each day of Cluster 9 on a thinned-out grid that 
only includes alternating grid nodes.  In addition,  the maps contain groups of small dots,  each of 
which depicts the location of the wind vector arrowhead for an individual event assigned to this 
cluster.  The groups of dots collectively illustrate the distribution of arrowheads for all events 
b~I,gpging to Cluster 9. 

The dots surrounding each mean wind vector arrowhead appear in a somewhat circular pattern, 
and the extent of spread exhibited by the dots illustrates the degree of variability among wind 
vectors assigned to the cluster.  Similar patterns characterize the variability associated with other 
clusters (not shown).  Clearly there is substantial variability associated with the wind vectors 
as 1signed to incHvidi.ial  clusters.  This variability emphasizes the ambitious nature of the endeavor. 
In essence, the goal is to categorize many years worth of meteorological patterns into a finite 
number of classes.  Furthermore  each meteorological pattern does not simply describe a single 
location at a given point in time; it is required to simultaneously represent a broad spatial domain 
over a significant temporal period.  Indeed, it should not be surprising that a substantial amount of 
variability is associated with the result. 

llllllllllllllll'11lll11••llllllllll,,11lllllll111l 

'lllllllllllll11111lllll'",,1ll11lll: 

!11,,111'",:111111111111111" 

1111111111111111111111111111111111 

,11111111111111111111,• 

""""""""""' 

'"""" 

11111 

II" 

'"""' 

l 

1111 

" 

11

llllli111 

II 

" 

"I' 

111111•11111' 

"dlllllllllll 

'"""""""'"""'"""""" 

1
'11111111111111,11:,.'1
"11 
"

'll,,,,.11111,,,,llllll.,,,l,,,,1111111111111111""11"11h11llllllllllllllllllllllll1 

Figure 17-5 (a-e) contains star chart histograms of the 30 clusters defined using annual data. 
'Each chartillustrates the frequency of 5-day events belonging to a given cluster, and the clusters 
' th1s1m§~ly~,,,,111~1~111111!:>r1111111,~r~121
,1~cco~d~1~g to overall ~equency of occurrence.  As shown on the charts, 
events from Cluster 1 accounted for 12.19% of all 5-day events between  1984 and  1995, those 
from Cluster 2 accounted for  12.10% of all events, etc.  The numbers arranged radially on each 
chart depict the number of events belonging to the cluster from each month of the year.  The 
length of the line pointing to each month is proportional to this frequency of occurrence, and the 
ends of the lines are connected to facilitate the visualization of patterns. 

" 

Several observations may be made based upon these charts: 

• 

,;:1,,,, 

1·.1111111 

11111 

• 

,

Although defined using annual data, the cluster frequencies reveal definite seasonal 
tendencies.  That is,  clusters do not occur randomly throughout the year, but rather exhibit 
aJ~p.g~gcy to occur more frequently within specific seasons.  Thus, the annual clustering 
1  procedure successfully identifies and discriminates wind field  patterns that are associated 
,,,  : V{i~h s~soQally distinct meteorological classes. 
, : 'Yi~11il~1111 51!1~~1l1~r~to!1i~ni~~ s1:1ID.IIler events tend to b~ quite distinct from those containing 
,,  ~1!1E-t~,~1111111~:V~12ts,,,  and vie~ versa), many clusters contam events from a combined 
' transitional" season that includes both spring and fall  months. 

';:::11111111111111111:::::"' 
1
1
,
1',,,,:'":11111111111111111111111::' 

'1~11·'  "'Ill!  "  1111!1  ;,,,·1 

!11111'11111111 
·1.llllilillllllllllll: 

0
1
1
1
,',
,
1111111111ll
I:  .'l
lii:

11,,1ll",,111i,ulllli1111,,;;111' 

::::  ,,,1u,,,11:::'1''  111' 

, 
~11111,.  ,, 

":::11111111  '11!1'' 

I 
,::  ', 

'I·":,, .,111: 

ll1u11llllllll111' 

'11:,,,· :. 

.. ,,,'!11111:, 

"'"' 

,1, 

,,, 

,,11 

.Ill' 

,,,, 

"~' 

"Ill" 

1
" 

''I. 

" 

"' 

11

I 

'" 

I 

I 

·: 

I" 

' 

,. 

11 

111111!'':: 

1111,, 

111111;,,, 

111111111! 

1
1
1
1

1 

111

1111
1
1
1

111111::::111 

1111", 

1111111"1 

:,'!:111111; 

11111111 

1,1,11111111 

1111111111111 

111111111111111 

"'11111"'· 

:111111 

Ill' 

: 

·111111111 

11

1 

' :

·.:1111111:::

1111 

'111111 

:1"" 

:l1111111111'

11

h' 

• 

,111111::: 

1111:;111:'" 

1 

,

111111111111 

'1:• 

,,,,,111111 

I• 

I 

lllll", 

,,,,''1111111111111111111' 

11i11111111lllllll• 

,,'  ~1~1~,,,!~1~ m~s~ prevalent clusters heavily emphasize the summer months; each of these 
111  c:l~~,~~~1~111  !D:1~,!,~~~~,,,,,,,!11ore than,, ~v.:!ce a~n~~y, events as ~y ?ther cluster, ,,,,with the vast 
, Majonty of summer events contained m them. 

•  • 

"""" 

"11" 

"'  111111""111, 

'' 

' 

111111111111' 

'"" 

'""'' 

' 

'' 

" 

"11111111111111,,, 

11111111111'",lll' 

1111111111 i ; , 

",,,111111111111111:::11111'" 
: 11  ' 1111 i 111111111111111111 i i  1 

, , 

, 11 11 " 

' : 11111111 11111111111 

1 

111 
: " 
I : 

1 
11 
" 
" " 11 

:  , ,  , "  11 " " ; : ; ! 11 : : : ! ; 1 

1 
11 

, , , 

' " 111 ~ 1 ~ , 

,  " 

The disproportionate representation of summer events by two of the 3 0 clusters is  not surprising, 
si21~~ t!\~ ~In~""""~~,!,~,,!,~ ,~~ e~pectecl to be less variable in the summer.  However, seasonal 
differences in meteorology and atmospheric chemistry are important in explaining the variability 
eXhlbited by the arr quality parameters of interest.  The addition of more warm season clusters 
could provide improved resolution in this regard.  This is the primary motivation for conducting 
analyses using seasonally distinct clustering. 

17.4  Evaluation of Alternative Aggregation Approaches 

The previous illustration clarifies the motivation for investigating seasonally distinct clustering. 
Similarly, regionally distinct clustering offers an alternative that might provide improved 
resolution of wind field groupings on smaller spatial scales.  An added dimension to the problem is 
that the number of clusters to be retained clearly affects the degree of resolution.  To gain an 
understanding of the importance of these considerations as they relate to estimation of the 
meteorological parameters used as evaluative tools in this analysis, these and other alternatives 
were explored in several combinations.  These explorations, designed to expose patterns and 
trends from which to make an informed selection of a general approach, are described in this 
section. 

17.4.1  Description of Alternative Approaches 

Several variations of the u and v wind vector clustering were investigated.  As discussed 
previously, these were selected to investigate patterns and properties, and not necessarily to be 
considered as final  candidates for cluster definitions.  They are as follows: 

• 

• 

• 

• 

• 

• 

Annually defined strata with variations in the number of strata: 5,  I 0, 20, 30, 60, and 90 
clusters;  · 

Seasonally defined strata using warm and cold seasons: totals of I 0,  20, 30, and 60 
clusters equally divided between the warm season April-September period and the cold 
season October-March period; 

Seasonally defined strata approximately equally divided among summer (June, July, 
August), winter (December, January, February), and transitional (spring and autumn 
combined) seasons: totals of 20, 30, and 60 clusters; 

Seasonally defined strata equally divided between summer and winter seasons, and with 
approximately twice as many strata defined for the transitional season: totals of20 and 30 
clusters; 

Seasonally defined strata approximately equally divided among summer, winter, spring 
(March, April,  May), and autumn (September, October, November) seasons: totals of20 
and 30 clusters; and 

Regionally defined strata: 30 clusters identified in each of four separate cluster analyses 
for the northeast, southeast, northwest, and southwest subsets of the domain 

We also investigated the alternatives of strata defined to simply mirror the four seasons 
(disregarding··· .ai 

...  l clu ...  sters), str. ata defined by clustering of 3-day events rather than 5-day events, 

·  and ~trata ~~~ned ~y clustering the meteorological parameters used to evaluate the approach. 

111.1111. 

1 

'I,,.,, ............ 

···· 

······ 

17.4.2  Description of Meteorological Data 

II,, 
:'111111' 
111111111111' 

""

l';::::;lll 
1
11111
' 

11' 

,1111 

"'"""''11''111,, 

'11111111111"" 

1111"""' 

I', 

'111111111111"' 

"" 

,111111,,111 

11,, 

' 

" 

f~,~"'·~'~ ~~,~P1:8:YV~"""'~tr2;g.~,~~,!~5>11;,,~Sh,~1!1e.~, p~eliminary testing was performed  y examining the 
,,  ~E~r::tail!tY associated with tne use of cluster-based stratified sampling to estimate the annual 
mean of daily noontime levels of visibility, temperature,  and relative humidity, with primary 
emphasis placed upon visibility as discussed previously.  Visibility (units ofkm- 1) was specifically 
ex}Jressed as the light extinction (bm), less observations with precipitation, and less observations 
, with relative humidity greater than 90%.  The light-extinction coefficient is often used to 
cb!!f~Ct~tj,~1~, v~,~~,?H~;x, although in general, it has limited ability to predict human visibility.  The 
visual range vr \kmJ  can be estimated from the bcxt  by using the Koschmieder equation: 

,II""' 

'"""""''""'b 

I''"' 

'" 

'111111111'"1,, 

111111'':111· 

1111111111 

11111111 

:1111111111111111, 

111111111111111111::· 

11 

I• 

'1"' 

3.91 

(17-1) 

Temperature is in units of degrees Celsius, and relative humidity in percent.  These parameters 
were taken from 201  locations in the continental U.S. for which coverage was at least 99%, as 
, illustratecf fu figure  17~6:  Tllis was specifically defined as sites exhibiting at least 99% 
completeness for hght extmction coefficient; allowances were made for m1ssmg  observations that 
1W~1~e associated With precipitation SO  as fiOt  to bias the inclusion Of Sites toward drier climates. 

'""''""""""""  ..  '"'"""  """" 

1
""'""'""

"',::11111111!'" 

'11111111111111!'

·  "' 

1111111111

""""

11111

"·· 

• 

• 

• 

• 

• 

• 

"'

11!:

'"

11

11

11

11111111111111"',1111 

':,, 

1111111 

1"1,,,,,11111111111111111111''11111• 

1!llllli11illllllllll 

·1' 

11111 

•1 

·11111,,1111111111111111111 

1111111·1 

1111 

1111' 

i,,,11111 

11

1
"'

11, 

11

11111

::::111]' 

···"' 

"': 

.. 

' 

17.4.3  Analysis Methods 

'111!!!!1!'' 

li11111iiillll!!!!!lllllllllllll 

''111111111111111! 

,,,, 

,11 

.,1::::11! 

11

'11 

I  ''

11!!!!1111111111: 

'""'' 

' 

1111

· 

'!1111 

'! 

11 

' 

111111, 

'II" 

''" 

:1::: 

'lilll!: 

' 

I 

'1111111 

., 

'!11111111111 

11111111111

· 1:, 

111 

11111:, 

,;11

The average relative efficiency associated with the estimation of mean annual visibility, 
temperature, and relative humidity, using each alternative stratification scheme, was used for 
comparison of the schemes.  Specifically, at each location and for each scheme, the variance of an 
aggregation-based estimate (Cochran,  1977) of the annual mean was determined in three ways, 
assuming that the estimated mean was calculated using (1) a stratified sample with equal 
aJ~~,,<;-:,~!:i~i:t,,,~,~r2,,~,,~  ~,!,,~~t~ {2) ~sti:at~~ed ~a~pl~with pr<?portion~. allocation across strata, and (3) 
simple random sampling, with a total sample size consisting of the same numbers of events in each 

~~6i"~~dq~}i'r~:ct0~¥t{6~'t!1~ ~:!ii~J;~~;i~; ~:1;n: ::~~!~~d:~~ t~x~r:~s:~a~~~he 
'rei'~'tiveeffici~~~y"'()r each of those designs.  FlnaI(y,  those relative efficiencies were averaged 
across sites to provide an indication of the overall efficiency of each scheme. 

11111111.:1·;111111:·:1111111111111111111111111111111111111111111111111111111111111° 

11111111111111111111111. 

'""""'""'"""' 

"111··1 

"" 

::, 

"" 

,,, 

l'""ll 

,1111111111 

111 

11

·''''

' 

11111111 

,. 

' I i 

' : : 11111111 

11111 · 

' ' 

1111111111 

I 

' 11111 " "  ' 111 

1 I " " 11111111 '  1111111111111111111  " 

" I 'Iii ' i 1111111111 

; 111111 

11 
" 

1111

111111111 

11 ~ ! 1111111 

' " ' 11111111111 

' 11111 : 

Tltj.s !§ Q~1)t e?Cpiained using a specific example.  First,  suppose that the mean annual temperature 
at a given location is estimated as the average of the daily temperatures from  30 randomly 
,  sampled 3-day events from the period  1984-1992, completely disregarding any information 

rela~ed ~9 ~!usters; ...  Suppose that the standard deviation associated with that estimate is  1. 5 ° C,  so 
that a 95% confidence interval would yield the estimated mean±2.94°C (=1.96xl.5).  Second, 
suppose that the mean is instead estimated as a weighted average from 30 3-day events, using two 
events per cluster (i.e.,  stratified sampling with equal allocation), and using the frequency of 
occurrence of each cluster as the weight applied to the temperature from the corresponding event. 
Suppose that the standard deviation associated with that estimate is  1.0°C, compared to  I.S°C 
from simple random sampling.  Last,  suppose that 30 events are selected using proportional 
allocation (i.e., the number of events selected from a cluster is proportional to the number of 
events belonging to the cluster),  and that the standard deviation of the resulting estimated annual 
mean temperature is 0.8°C.  These standard deviations (1.5,  1.0, and 0.8) translate to variances of 
2.25,  1.0, and 0.64 for simple random sampling,  stratified sampling with equal allocation, and 
stratified sampling with proportional allocation,  respectively.  Thus, for this hypothetical location, 
the relative efficiency of stratified sampling with equal allocation is 2.25/1.0=2.25, and the relative 
efficiency of stratified sampling with proportional allocation is 2.25/0.64=3.52.  Proportional 
allocation is more efficient than equal allocation in the sense that it leads to lower variances and 
therefore tighter confidence intervals bounding the estimated mean. 

### 17.4.4  Results 

Tables 17-2 ( a-b) and  17-3  present mean relative efficiencies associated with annual means of the 
daily noontime temperature, relative humidity,  and extinction coefficient, as estimated using 
aggregation approaches based upon the various schemes described above.  In each table, results 
are presented to illustrate the relative efficiency of estimation methods using equal allocation 
(equal numbers of events selected to represent each cluster) and proportional allocation (numbers 
of events selected in direct proportion to the total number of events categorized into the given 
cluster).  Relative efficiencies reported in this section are valid for any number of events that 
might be selected, as relative efficiency is invariant to sample size under equal or proportional 
allocation 

In the case of proportional allocation, the relative efficiency actually refers to the minimum 
variance that might theoretically be achieved if proportional allocation were carried out precisely. 
In practice this might only be possible if a very large number of events were to be sampled, since 
the appropriate proportions might otherwise dictate sampling of fractional numbers of events from 
some clusters.  Therefore, the relative efficiency reported for proportional allocation may be 
thought of as an upper limit to the relative efficiency that might actually be achieved in practice. 
In all likelihood, this upper limit cannot be attained,  but it should be possible to achieve a relative 
efficiency occurring somewhere within the range defined by this upper limit and the relative 
efficiency associated with equal allocation of events among clusters. 

The first six rows in Table  l 7-2a illustrate relative efficiencies associated with various numbers of 
annually defined strata (i.e., clusters emerging from cluster analyses of daily wind field  data from 
1984-1995 without regard to season).  Several observations may be made by inspecting the first 
six rows in Table 17-2a: 

The relative efficiency associated with the estimation of mean temperature is consistently 
highest, followed by that of relative humidity.  This implies that the meteorological 
"Clusters are most useful in distinguishing between events with regard to temperature, and 
~1~~t ~1§15,ful111111!n gi~tj~~.~shin~ extinction coefficient. 

In the case of temperature, the variability associated with stratified sampling using wind 
field-based clusters is consistently less than the variability associated with simple random 
sampling (i.e., relative efficiencies are uniformly greater than  1.0).  Thus, in each scheme 
the clusters contribute important information that explains variation in temperature. 

For the estimation of mean relative humidity and mean extinction coefficient, the use of 
stratified sampling based upon wind field  clusters is consistently more efficient than simple 
random sampling if proportional allocation is approximately satisfied.  In most cases, the 
use of equal allocation actually results in less efficient estimation than simple random 
sampling; this reflects a wide range of stratum sizes that would be inappropriately 
represented using equal allocation. 

As would be expected, the efficiency associated with proportional allocation increases as 
th~ !1U!!!Qe~ ~(~trata ~c,~~~~'~'~' in~orporating more refined representations of the 
ril~t~qrglogical classes within the strata. 

' The efficiency associated with equal allocation decreases as the number of strata increases. 
The stratum sizes become more divergent when more strata are defined,  so that the 
ineffl~,!,~,pcy of equal allocation is magnified. 

Based upon the well-known properties of stratified sampling discussed above, our objective was 
""""'to"''"Ciesigri  and frnplement a scheme based upon approximate proportional allocation.  The numbers 
fo[,, equal allocation in these tables merely served to provide a lower bound on the efficiency that 
could b~ realiz~d. since it was known that the precise degree of efficiency reported for 
proportional allocation might not be achievable in practice due to the requirement of sampling 
integer-valued numbers of events. 

As discussed in section 17. 3 .3  above in regard to the analysis illustrated in the "3 O Strata" row of 
the table, summer events are disproportionately represented by two of the 30 clusters.  To 
evaluate the effect of improving the resolution of summer meteorological patterns, as well as the 
effects of imposing constraints that would alter the resolution of families  of clusters under a 
varlety of scenarios, various implementations of seasonally distinct clustering were investigated. 
,,,  T~~, l~~,t (oyi: t:QYI~ gf 'J)iJ~l~ ~ 7:7'!JJlustrat~ r~~ult~ associated with stratum definitions based upon 
,,,  a simple warm/cold seasonal dichotomy, in which separate cluster analyses were conducted to 
force equal numbers of strata for each season.  The following observations may be made based 
,i1ponthis portion of th

With the exception of extinction coefficient estimation under proportional allocation, 
stratification using seasonally defined clusters consistently yields improved efficiency over 
stratification using the same number of annually defined clusters.  Thus, although the 
annually defined clusters do adhere to a seasonal pattern, the improved resolution afforded 
by the forced inclusion of more warm weather clusters (and reduction of cold weather 
clusters) is particularly effective in explaining variation in temperature. 

For seasonally defined clusters, the relative gains in efficiency associated with using equal 
allocation are large as compared to the potential gains associated with using proportional 
allocation.  Thus, in departing from proportional allocation (which is not precisely 
achievable in  practice as discussed above),  seasonally defined clusters are likely to afford 
improved efficiency over annually defined clusters. 

Results associated with further seasonal stratification schemes are illustrated in Table  l 7-2b.  The 
first two rows correspond to approximately equal numbers of clusters divided among summer, 
winter,  and transitional (spring and autumn combined) seasons, the next two rows correspond to 
equal numbers of clusters divided between summer and winter with approximately twice as many 
transitional season clusters, and the fifth and sixth rows correspond to approximately equal 
numbers of clusters divided among four seasons.  In each case, the exact distributions are 
constrained to result in total numbers of strata that are directly comparable to the numbers of 
strata investigated in other seasonal and annual analyses. 

Under proportional allocation,  stratification schemes based on three or four seasons offer 
significantly improved efficiency in the estimation of mean temperature compared to two-season 
and annual stratification schemes with comparable total numbers of strata.  They also demonstrate 
slight but uniform improvement in the estimation of extinction coefficient, and mixed results in the 
estimation of relative humidity. 

Table 17-2a.  Mean relative efficiency' associated with estimation of the annual (1984-1995) 
mean of the indicated parameter, using various stratified2  sampling3  approaches relative to simple 
ran  om samp mg. 

/  1· 
c 
ontmenta  ana yszs. 

· 

r 

d 

Temperature 

Relative Humidity 

~H-Adjusted Extinctioli 

Coefficient 

' I 

~ Ill I ' 

1111

'1!

1:11111ii; 

1 "  ' ' 111111111  ' 
1 

,,,,lli1111i::::

,,1111111" 

'l  11' 
111 

I' 

Method 

5 Strata 

10 Strata 

20 Strata 

30 Strata 

60 Strata 

90 Strata 

1 O Strata Defined Seas-
onally (5 Warm, 5 Cold) 

20 strata Defined Seas-
onally (1 o Warm, 1 o 
Cold) 

30 Strata Defined Seas-
onally (15 Warm, 15 
Cold) 

Equal 

Allocation  Propor-
tlonal 

Across 
Strata 

Allocation 

Equal 

Allocation  Propor-
tlonal 

Across 
Strata 

Allocation 

Equal 

Allocation  Propor-

Across 
Strata 

tional  . •· 
Allocation 

2.34 

2.29 

2.11 

2.08 

1.81 

1.43 

2.62 

2.41 

2.50 

2.83 

2.86 

3.05 

3.10 

2.94 

1.14 

1.09 

0.88 

0.86 

0.76 

0.62 

1.11 

1.18 

1.22 

1.26 

1.28 

1.32 

1.33 

1.26 

1.01 

0.90 

0.69 

0.67 

0.59 

0.53 

0.93 

1.12 

1.14 

1.16 

1.17 

1.19 

1.20 

1.13 

2.91 

3.54 

1.11 

1.35 

0.89 

1.16 

3.14 

3.60 

1.17 

1.37 

0.91 

1.17 

60 Strata Defined Seas-
onally (30 Warm, 30 
Cold) 
1 
B~lf:!t1~~,,,,,,,!::,,ffi£,!,,~,,!:1~¥  is defined as the ratio of the variance associated with simple random sampling to 
, 
the variance associated with stratified sampling. The table entries are means of station-specific 
efficiency ratios, averaged across stations within the continental domain. 

2.74 

1.39 

1.20 

0.77 

0.98 

3.89 

2  Unless otherwise noted, stratum definitions are based on  annual clustering of 5-day events from a 
temporal subsample of the wind field data; the remainder of the sample is then classified into those 
strata. 

3  Reflects sampling of 3-day events from 1984-1995. 

111111111: 

1 

11111:111:

111111,, 

ll1 

'·ll1 

1111111,1111111111 

11111111111·,,, 

:11, 

1', 

'" 

,  Ii 

111111::::::'" 

'''!I:',, 

: 11 i i i ~ I 

::,\, 

' 

1]]11111111!' 

Ill' 
iii,,; 

,,,;,,,,l111111ii 

lllllli11illllllllll1 

1
'111 11 

'" 

1
1111111111 

,111:""

1
llllllili

1

:' 

:,,,;Ill 

1',·11111111 
',uullll,, 

17-16 

,11;1;r:1: 

,, 

'lllj,, 

Ill:'·" 

'" 

111111:11, 
'II" 

1 ' 
" 

11111111111111 

'11111::::ii' 

111, 
111111,.
11,"11111 
1lli1 

,1111111111111, 

111, 

I'":::: 

1
!11 

,:11111

,'p 

·li'1 

'''"""""" 

,11 

'1111!·:11" 

'Ill' 

1111:;,;il' 

! 

,  .. ',1111111 

' 

''""''' 

:ii 
111,1 

If, 

Table 17-2b.  Mean relative efficiency1 associated with estimation of the annual (1984-1995) 
mean of the indicated parameter, using various stratified sampling2 approaches relative to simple 
ran d 

r  Ct"  ti  I ·  
on men a  ana ysrs .. 

om samp mg. 

.. 

EP AJ600/R-99/030 

Temperature 

. Relative Humidity 

.. 

.. 

rtH-Adjusted Extinctior 

Coefficient 

. . 

Equal. 

Equal 

Allocation·  ·  Propor- Allocation 

Across 
Strata 

tlonal 

Allocation 

Across 
strata 

:Propor-
tional 

Allocation 

Equal 

Allocation 

eropor- .. 
tional 

Across 
Strata.··  Allocation 

2.66 

3.91 

0.95 

1.31 

0.90 

1.18 

2.60 

4.12 

0.87 

1.35 

0.78 

1.20 

3.31 

4.06 

1.06 

1.33 

0.89 

1.19 

2.91 

4.17 

0.96 

1.36 

0.80 

1.20 

3.23 

3.86 

1.11 

1.36 

0.92 

1.18 

2.82 

3.88 

1.03 

1.38 

0.84 

1.19 

2.93 

2.93 

1.21 

1.21 

1.12 

1.12 

2.24 

2.78 

0.97 

1.29 

0.76 

1.16 

0.92 

1.28 

0.74 

1.15 

0.63 

1.09 

.•. 

... 

.. 

. .. 
..  .. 

; 

.. 
Method 

20 Strata Defined Seas-
anally (6 Summer, 6 
Winter, 7 Transitional) 

30 Strata Defined Seas-
anally (1 o Summer,  1 O 
Winter, 1 O Transitional) 

20 Strata Defined Seas-
anally (5 Summer, 5 
Winter,  1 O Transitional) 

30 Strata Defined Seas-
anally (8 Summer, 8 
Winter,  14 Transitional) 

20 Strata Defined Seas-
anally (5  Summer, 5 
Winter, 5 Spring, 5 Fall) 

30 Strata Defined Seas-
anally (8 Summer, 8 
Winter, 7 Spring, 7 Fall) 

4 Strata Defined as 
Seasons (Dec-Feb, etc.) 

30 Strata Defined by 

Clustering 3-Day Events 

30 Strata; Seasonality 
Removed from Met. 
Parameters 

30 Strata Defined by 
Clustering Lt.  Ext. Coeff. 
1  Relative efficiency is defined as the ratio of the variance associated with simple random sampling to 

0.57 

1.89 

1.45 

0.30 

1.11 

0.51 

the variance associated with stratified sampling. The table entries are means of station-specific 
efficiency ratios,  averaged across stations within the continental domain. 

2  Reflects sampling of 3-day events from 1984-1995. 

The final four rows of Table 17-2b address issues that are useful in that they provide additional 
perspective for this analysis.  They are discussed in sequence below: 

• 

lli1111lll11111lllllli. 

1111
' 

In view of the improvement associated with seasonal stratification schemes relative to 
annual stratification, a natural question to ask is whether a seasonal scheme with one 
stratum per season (i.e.,  no cluster analysis) is sufficient to support comparable efficiency. 
Based upon the results in Table 17-2b, this method offers overall improvement relative to 
an annual scheme with an approximately equal number of strata (i.e., five strata as shown 
in the first row of Table 17-2a).  As might be expected, it is significantly less efficient than 
the use of 20 strata equally divided among four seasons, but not dramatically so. 

,111111111111111::::111111111111111•"" 

,, 

1111111111111111111111111111111111"''"' 

'111111111''1 

'1!11!1111111,11: 

"" 

• 

1111111111111''1 

othl~I~ nit~~~ questi~~ iswhethei-'ihe'clustenng of five-day everiis (using wind data 

II""'' 
fr.o.mth~ fj~~t, third,  and fifth  days) has a noticeable impact on this analysis relative to the 
clustering of three-day events that previous investigators have pursued.  Based upon 
results in Table 17-2b  the definition of30 strata using annual clustering of three-day 
,,,;;;ii1i,,,1111~~!~'hl"~1111111p'id'"""'"'~'~'~~ '~Iiitii~ results to those associated with 30 strata using five-day event 

clustering (fourth row of 17-2a) with respect to the evaluative parameters.  Note that this 
does not necessarily address differences with respect to the characterization of transport. 

• 

111111::11~: 

,,,!ii 

., 

,, 

' 

1 

:::::::111

,1111111111111'"''11 

1111,111,·:111111111,, 

Ill 

11111·111111111111111111111111 

"' 

·· 

"' 

... 

!!I 

1111111111111!, 

111111111:::; 

• 

1

11

,'llllll"""'lll""'. 
"'' 
11111111111, 
I 

1111'11··'

:11·11••' 

,, 

1111"' 

'11111111111111111111, 

111 

"''1111111"'"11 

·1111111111' 

11111,,,,111111111, 

·1:1, 

"''""'·""'Ill,"" 

1111111111111111111111111111111111. 

·:,,,,,::··:''1"11111111111"1"''111 

·1,,,,,11111111111111111111111:,111111111":1111111'"1111""'111' 

A consistent result""in these analyses is th~t the relative efficiency associated'"~ith the 
;,  ~,~1!,!11~11111!p~,:"2f.~~~,,,.\empe:~tur~ is mu~h g.reater than that associated :With the. estimation of 
: ..  mean relative hunudity, which m turn 1s shghtly greater than the relative efficiency 
associated with extinction coefficient.  Since the preliminary results demonstrate 
associations between the clusters and the seasons (even for annually defined clusters),  one 
might hypothesize that this characteristic is related to the more pronounced seasonal 
tiends associated with temperature and the less pronounced trends associated with 
exffnCiion coefficient.  Table l 7-2b illustrates relative efficiencies associated with the three 
.,,.  meteorological parameters following the removal of seasonal trends from each.  (Trends 
were removed by analyzing deviations of each parameter from a sinusoidal curve fitted to 
the raw data at each location.)  Indeed, the relative efficiencies for the three parameters 
"'  are much more comparable in this context, and much more similar to the results for 
····  extinction coefficient in other analyses.  This analysis lends support to the use of extinction 
•• ,.  coefficient as the primary evaluative outcome, because it reflects the ability of each 
. scheme to charactenze short-term meteorolog1cal patterns apart from long-term seasonal 
trends. 

"111111"'"""""""""". 

:11111111111::.:,,.·11:,,11111111111111111111111111: 

11
"111111111111"

11111illlllllllll!lll1 

",111111111111111111111 

111111•11' 

"" 

1111111 

"'' 

11111111 

'"""" 

'" 

" 

' : 11 ~ ' 

" 

• 

"""""""" 

I , 

• 

A tlnafinvestigation in Table  17-2b also relates to the utility of extinction coefficient as the 
,,,  primary evaluative outcome.  Under proportional allocation, the relative efficiencies 
·~~~~~,~~;~~Y'!~t l:fld~tt~a;fu~Ii;l1au~~~a~:ri~;::~~~u~n~~~a~ir:~~~~ ~t;i~!~~de~~~i;; 
(relative to simple random sampling) in the estimation of mean extinction coefficient.  To 
put this observation in its proper perspective, it is useful to consider the maximum relative 
efficiency that might be achieved from any stratified analysis.  In particular, a stratification 
of events was performed based upon extinction coefficient itself (rather than wind fields). 
Under these optimal conditions, the relative efficiency associated with a 30-stratum 
scheme was still only  1.45, compared to a range of 1.17-1.20 for other 30-stratum 
schemes based upon wind field  clusters.  Considered in this context, relative efficiencies 
encountered in these tables are encouraging.  (This clustering of extinction coefficient 
provides some useful perspective regarding this evaluative outcome parameter, but is not 
pursued as a recommended methodology for estimation based upon the rationale outlined 
in section 2.2.) 

To evaluatt? the utility of regionally distinct stratification schemes, the continental domain was 
divided into four quadrants and cluster analyses were performed on wind fields within each 
region,  resulting in four sets of 30 annually defined strata.  For each region,  relative efficiencies 
were summarized as in the previously described analyses.  Since the mean relative efficiencies are 
constrained to sites within each region,  results in Table  17-2a are inappropriate for comparison to 
these regional mean relative efficiencies.  Therefore, Table 17-3  also includes mean relative 
efficiencies only for the sites within each region, from comparable clustering of continental data. 
These are displayed in combination with the results for strata defined using regional data. 

For example, using 30 strata under proportional allocation,  clustering of wind fields in the 
northeast quadrant of the domain results in a mean relative efficiency (averaged over sites in that 
quadrant) of 1.65 associated with the estimation of mean temperature.  For 30 strata, from 
clusters defined over the entire continental domain, the mean relative efficiency, over those same 
northeast sites is 3.14.  The results in Table 17-3  collectively demonstrate that,  under proportional 
allocation, regional stratification produces either no gains or only slight gains in efficiency in both 
the southeast and southwest regions for any of the evaluative parameters.  In the northeast and 
northwest regions,  this technique is markedly less efficient than continental clustering with regard 
to temperature, and somewhat less efficient with regard to relative humidity.  There is only a 
negligible effect on extinction coefficient. 

This result has significant importance, because it demonstrates that in the northern half of the 
domain, clustering of continental wind field  data is  actually more effective than clustering regional 
data in explaining variation in  some meteorological parameters on a regional scale.  Thus, the 
wind field  patterns associated with different levels of temperature (and,  less markedly,  relative 
humidity) in a northern region are more distinctly identified on a continental scale than on a 
regional scale.  Furthermore, wind field  patterns associated with different levels of extinction 
coefficient (a primary evaluative parameter due to its close association with fine particles) in a 
given region are no more distinctly identified on a regional scale than on a continental scale. 

Table 17-3:  Mean ;~lative ~fflcie~cy1  associat~d with esti~ation of th~ annual (1984-1995) mean 
of the indicated parameter, using various stratified sampling2  approaches relative to simple 
ran  om samp mg. 

. 
eg10na  ana yszs. 

R 

d 

l' 

I 

I 

Temperature 

Relative Humidity 

RH-Adjusted Extinctiori 

Coefficient 

Region/Method 

Northeast; 30 strata Def. 
Using Regional Data 

Northeast; 30 Strata Def. 
Using Continental Data 

Southeast; 30 Strata Def. 
Using Regional Data 

Southeast; 30 Strata Def. 
Using Continental Data 

Southwest; 30 Strata Def. 
Using Regional Data 

Southwest; 30 Strata Def. 
Using Continental Data 

Northwest; 30 Strata Def. 
Using Regional Data 

Allocation  Propor- Allocation  Propor- Allocation  Propor-
tlonal 

.tional 

tional 

Allocation 

Allocation 

Equal 

Across 
Strata 

Allocation 

Equal 

Across 
Strata 

Equal 

Across 
Strata 

1.41 

1.65 

0.98 

1.15 

0.95 

1.27 

2.27 

3.14 

0.77 

1.20 

0.61 

2.35 

2.72 

1.04 

1.32 

0.77 

1.28 

1.22 

2.02 

2.62 

0.83 

1.24 

0.61 

1.20 

1.95 

2.42 

0.94 

1.26 

0.81 

1.01 

1.88 

2.43 

0.89 

1.22 

0.84 

1.04 

1.17 

1.66 

0.92 

1.27 

0.82 

1.08 

Northwest; 30 Strata Def. 
Using Continental Data 
1  Relative efficiency is defined as the ratio of the variance associated with simple random sampling to 

1.96 

1.47 

1.03 

2.82 

1.01 

0.71 

the variance associated with stratified sampling. The table entries are means of station-specific 
efficiency ratios,  averaged across stations within the continental domain. 

2  Reflects sampling of 3-day events from 1984-1995. 

lllllllllll;·lli1 

""''' 

11111111111111111111111111 

1111,,illllllllllllll 

•Ii 

'I• 

,  ";,111111111111111, 

11::,,11111111111, 

'l•'ll· 

"',,,1111111111111,'  1 

,11111 

11111111111111!":,;1  '11111111111111111 

i''lllllllllll1""llll" 

In .. Y!~Vf Qf!h~ ~Yf1::~nt o~jective of matching or exceeding the performance of the current RADM 
stratification scheme over the continental domain,  it is appropriate to compare the relative 
efficiency of the stratification scheme used in the aggregation ofRADM output compared to the 
refative efficiency of the schemes defined above.  Table 17-4 addresses this specific issue.  In 
addition to the results associated with equal and proportional allocation, this table illustrates the 
mean relative efficiencies associated with the actual allocation of the 30 RADM events, which lies 
somewhere between those extremes.  (Proportional allocation was not a specific goal of the 
methods developed for RADM, although it is not inconsistent with the less formally stated goal of 
selecting more events from the clusters that accounted for most of the acidic deposition.) 

Table 17-4.  Mean relative efficiency1 associated with estimation of the annual (1982-1985) mean 
of the indicated parameter, using various stratified sampling2 approaches relative to simple 
ran d 

. 
comparative ana yszs. 

om samp mg. 

RADM 

r 

I 

. 

BP A/600/R-99/030 

.. 

Temperatur.e . 

.. 
Relative Humidity 

.. 

. . 

RH~Adjusted Extinctior 
.. 

. 

Coefficient 

Actual 
Propor- Alloc.  of 
tional  RADM  Equal 

Actual 
Propor" Alloc.  01 
Equal 
tional  RADM 
Alloc.  Alloc.  Event~  Alloc.  Alloc.  Events  Alloc.  Alloc.  Events 

Actual 
Propor- Alloc.  of 
tional  RADM  Equal 

.. 

.. 

.. 

Method 

19 Existing RADM Strata 
Defined from  Clustering 
1979-83 3-Day Events 

38 Existing RADM Strata 
Defined After Separating 
Wet &  Dry Categories 

0.98 

1.58 

1.45 

0.76 

1.18 

1.07 

0.67 

1.18 

1.11 

0.83 

1.76 

1.12 

0.59 

1.19 

0.88 

0.49 

1.17 

0.92 

4 Strata Defined as 
Seasons (Dec-Feb, etc.) 
1  The table entries are means of station-spec1f1c efficiency ratios,  averaged across stations within the 

1.18 

2.77 

1.13 

1.13 

1.02 

1.17 

.. 

3.06 

3.05 

1.11 

RADM geographic domain. 

2  Reflects sampling of 3-day events from  1982-1985. 

In the RADM development,  19 strata were defined based upon clustering of3-day events 
occurring between 1979 and  1983.  These were then subdivided into wet and dry categories, 
ultimately resulting in twice as many (3 8) strata that are actually employed in aggregation-based 
estimation.  (Events from the 1982-1985 time period were classified into these strata, with 30 
events selected for use in RADM.)  The temporal and spatial domains applicable to the RADM 
development differ from those of the current analysis; therefore, any comparisons should be 
considered in this context. 

The RADM domain approximately corresponds to the combined northeast and southeast regions 
displayed in Table 17-3.  A comparison of Table 17-4 to Table  17-3  results under proportional 
allocation suggests superior performance associated with the strata identified using clustering of 
continental wind field  data, with respect to the outcome measures used here.  When the actual 
allocation ofRADM events is considered, the superiority of proportional allocation associated 
with the continental analysis is further elevated. 

## 17.5  Refinement of the Sampling Approach 

Based, in part,  on the results discussed in section 17.4, a stratified sampling scheme involving 
seasonal clustering based upon four distinct seasons was selected for further consideration and 
refinement.  The general superiority of three- and four-season stratification schemes was 
discussed previously in relation to results depicted in Table 17-2 (a-b ),  and a four-season scheme 
was selected following additional considerations regarding differences in emission patterns 
. 
between spring and autumn that would not be apparent using our evaluative parameters alone. t~ 

Another benefit of using this type of scheme is that it naturally lends itself to the development of 
~~~<?!laJ ~stimates based upon four-season partitions.  The derivation of such estimates from a 
two- or three-season scheme would not be as well defined,  and the estimates themselves would 
P~~gly be less precise. 

1111:::11 

"" 

1

''1111111111!1

" 

Having selected this general approach, refinements were needed to determine an appropriate 
nµmp~r of ~trc:i,ta,  and to arrive at an adequate number of events for sampling.  These refinements, 
aiid a description or the sample of events that ultimately was selected, are discussed in this 
section.  This aspect of the analysis was limited to a refined time frame consisting of the nine-year 
'11"'1~11~1R'~'d''fFoni"'f"'9'84:T992:wruch was targeted to ultimately represent baseline meteorology for use 
in modeling. 

· 

, 

### 17.5.1  Determination of Appropriate Numbers of Strata and Events 

The analysis proceeded under the assumption that, in order to satisfy the goal of matching or 
exceeding the performance of the RADM 30-event stratification scheme, a minimum of30 events 
w9µld ~~ required in the framework of the continental domain.  The precision associated with the 
·····estimation of annual means of the evaluative parameters was investigated for a range of30 to 60 
events,  and for  16, 20, 24, and 28 seasonally defined strata (4,  5,  6,  and 7 strata per season, 
respectively). 

These numbers of strata were chosen as candidates because 19 wind field-based clusters were 
defined in the RADM scheme, which offers a certain degree of resolution with regard to the 
c9i,ifacterization of transport (which is not specifically addressed by the evaluative parameters 
investigated here).  The range of 16 to 28 strata was selected to provide comparable resolution of 
wind fields and associated transport, noting that higher numbers of strata result in greater 
variation in the sizes of those strata, and this would force a more pronounced deviation from the 
goal of proportional allocation.  As discussed previously, a primary goal was to ensure that every 
stratum is sampled,  i.e.,  that there are no  clusters which go unrepresented in the final  set of 
events. 

"",  ""'""'" 

·1111111111111111""""""""'"" 

'" 

""""" 

:!!11111111111111111111111

"'11,,,,11• 

"""''" 

,, 

,111, 

""""' 

"'"' 

'II 

,,111111111' 

:, 

'""' 

, 

'" 

"'"'' 

•. 

:·:1 

II 

Note that we did not adhere to traditional rules of thumb regarding the determination of 
appropriate numbers of clusters to retain.  These rules are based upon an assumption that some 
finite number of clusters is appropriate to represent the variability inherent in these patterns, and 
that additional clusters beyond that point add relatively little information.  In reality, clusters 
d~fiQ~~<!~fi!lg this process represent a continuum, and traditional F-test statistics illustrate this 
continuum quite smoothly.  There is no magic number of clusters after which the relative 
importance of additional clusters drops noticeably.  Indeed, as cluster analysis is used here merely 
for the definition of strata and not as an end in itself, there is no compelling reason to be restricted 
by existing conventions regarding determinations of an optimal number of clusters. 

:::111111111111111 

,lllll'.,11111 

I 

I"' 

11111,11 

I' 

111 

,1111111111' 

111"11111 

,1111, 

11111111, 

'" 

,i; 

'"'1111,,,lll 

'"""''""11111111111: 

'1111 

Standard deviations associated with estimation under all of the combinations described above are 
iliusirate<l""IO:TableT7-s:  'Furtiieiinore, the process was repeated after seasonal adjustment of 
each outcome parameter (seasonality was removed by analyzing deviations from a fitted 
sinusoidal curve), to ensure that long-term seasonal trends do not unduly influence any pattern. 
The standard deviations in Table 17-5  provide an indication of the degree of precision associated 
with estimation, and are influenced by the observed within-stratum variability of the parameter. 
One would expect the inclusion of more strata to facilitate greater precision, unless this increased 
precision is offset by incurred deviations from proportional allocation. 

Table  17-5.  Standard deviation1 associated with estimation of the annual mean of the indicated 
parameter, using stratified sampling2 with 16, 20, 24,  or 28  strata and 30, 35, 40, 45,  50,  55,  or 60 
events.  Continental analysis. 

.  No. ot· 
Events 

Temperature, deg. c 

Relative Humidity, % 

.. 

RH-Adjusted Extinction 
Coefficient, km-1  (x103
) 

16 

20 

24 

28 

16 

20 

24 

28 

16 

Strata  Strata  Strata  Strata  Strata  Strata  Strata  Strata  ~trata  Strata  Strata  Strata 

20 

24 

28 

NOT SEASONALLY ADJUSTED 

30 

35 

40 

45 

50 

55 

60 

30 

35 

40 

45 

50 

55 

0.97 

0.98 

0.95 

0.95 

2.38 

2.40 

2.38 

2.40 

6.69 

6.73 

6.73 

6.77 

0.89 

0.89 

0.87 

0.87 

2.19 

2.20 

2.20 

2.21 

6.03 

6.08 

6.10 

6.23 

0.83 

0.83 

0.81 

0.81 

2.04 

2.05 

2.06 

2.05 

5.71 

5.63 

5.70 

5.71 

0.79 

0.77 

0.76 

0.75 

1.93 

1.92 

1.93 

1.93 

5.38 

5.34 

5.33 

5.36 

0.74 

0.74 

0.72 

0.72 

1.82 

1.83 

1.82 

1.82 

5.09 

5.06 

5.07 

5.08 

0.71 

0.70 

0.69 

0.68 

1.74 

1.74 

1.73 

1.73 

4.78 

4.79 

4.83 

4.83 

0.67 

0.67 

0.66 

0.65 

1.66 

1.66 

1.65 

1.65 

4.62 

4.62 

4.60 

4.60 

SEASONALLY ADJUSTED 

0.75 

0.76 

0.75 

0.75 

2.32 

2.34 

2.33 

2.35 

6.67 

6.70 

6.70 

6.75 

0.69 

0.69 

0.69 

0.69 

2.14 

2.15 

2.16 

2.16 

6.01 

6.05 

6.08 

6.21 

0.64 

0.64 

0.64 

0.64 

1.99 

2.01 

2.02 

2.01 

5.69 

5.61 

5.68 

5.69 

0.61 

0.60 

0.60 

0.60 

1.89 

1.88 

1.89 

1.88 

5.36 

5.32 

5.31 

5.34 

0.57 

0.57 

0.57 

0.57 

1.78 

1.79 

1.78 

1.78 

5.07 

5.04 

5.05 

5.06 

0.54 

0.54 

0.54 

0.54 

1.70 

1.70 

1.70 

1.69 

4.77 

4.78 

4.81 

4.81 

60 

4.59 
1  The table entnes reflect the standard dev1at1on  associated with  the average station-specific variances 

4.59 

4.60 

1.62 

1.62 

1.62 

1.62 

4.60 

0.52 

0.52 

0.51 

0.51 

(averaged across stations within the continental domain). 
2  Results reflect sampling of 3-day events from  1984-1992. 

For temperature and relative humidity,  the table suggests that for any given number of events, any 
effect associated with different numbers of strata is  negligible.  For extinction coefficient,  standard 
deviations for 3 0 and 3 5 event schemes increase slightly with increasing numbers of strata.  There 
.....  is ..  a minimum standard deviation associated with 20 strata for the 40-event scheme, and only 
negligible effects across strata for greater numbers of events. 

The objective of this determination was to develop a sufficiently large set of strata to provide 
some resolution with regard to the characterization of transport, yet be sufficiently concise to 
ayoid any reduction in precision that would result from unsampled strata or from the inability to 
approximately satisfy proportional allocation.  In consideration of these criteria and the above 
results, 20 strata were determined to constitute an appropriately sized set. 

...  figure 17-7 ( a-c) provides perspective regarding the relative precision provided by sample sizes of 
25 or more events associated with estimation of annual means of the ~vall1~tive parameters, based 
upon th~ ~se of2o seasonaily defined clusiers as strata.  The stand aid deviation displayed in each 
••  graph is actu8:!ly associated with the average of the variances across sites.  These graphs illustrate 
•  •':•  tli~ aciy@Jage of stratified sampling with proportional allocation relative to simple random 

,.,,, 

11

• 

1
11

.,

•• 

11111 

1111111

;. 

111 

111111111 

sampling.  They also illustrate the relative gains in precision (expressed as reduction in standard 
···ae\1at1on) thafarereruized as the number of events is increased.  The star symbols plotted on the 
··  .:;11:graphs indicate the actual standard deviation that would be realized for selected sample sizes 
· uilder a 20-stratum scheme, with events distributed in accordance with proportional allocation to 
the extent possible.  The selected sample sizes were chosen based upon practical limitations 
l11voIVingihe number of events that might realistically be implemented.  The stars do not fall 
sfrictly on the proportional allocation curve because of limitations associated with the sampling of 

·····  integer-valued numbers of events. 

These graphs indicate that, for temperature, it might be practical to achieve a standard deviation 
in the range of0.6-l.0°C, and that a much larger sample size would be needed to reduce the 
standard deviation below 0.5°C.  Similarly, standard deviations associated with estimates of mean 
relative humidity can possibly be achieved in the range of 1.5-2.5%, and realistic standard 
deviations associated with extinction coefficient might be in the range of0.0045-0.0070 1an- 1
Geographic variability with respect to many of these results is described later in this section. 

• 

Although the estimation of mean levels of parameters is likely to be a primary point of emphasis 
for many model-based results, the accurate estimation of extremes is also of significant 
importance.  This issue was specifically addressed by investigating the precision associated with 
the estimation of the 90th percentiles of the evaluative parameters . 

·:,,,. 

:11111111: 

•'"'' 

•• 

:11······111111111: •••• 

.. 

" 

························· 

··························11::·· 

·····:11:···::··································································· 

......................................................................................  y 

......  In contrast to the standard deviations associated with estimation of the mean,  there is no closed-
;:iiii' form solution to deternlliie the vanabiHt  associated with the estimation of 90th  ercentiles . 
Therefore  a Monte Carlo-type resampling approach was utilized to estimate these standard 
':lllllllllllllllllllll"'·111llllll1111111111111llll'"'"::1111111111llll11,,,,,,~1111•' 
~ii~1!2;~11~1121~11i1111  ,!,2,!~ 11~,ee~ifically involved randomly selecting 200 artificial samples of actual data, 
ea,~h~Onl?isting of the required number of events, from the 20 seasonally defined strata.  From 
···  e8:'chsample and at each site location, the 90th percentile of the parameter was estimated.  The 
v!;fiance of tlie resuliing collection of 200 estimates was averaged across sites,  and the associated 
standard deviation served as an estimate of the precision associated with the particular sample 
size.  This exercise was repeated for sets of 30, 40,  50, and 60 events. 

The graphs in Figure l 7-8(a-c) illustrate the resulting standard deviations, along with the standard 
deviations associated with estimation of the mean using the indicated number of events.  As would 
be expected, the standard deviations associated with estimation of the 90th percentile are higher 
than those associated with estimation of the mean of each parameter.  This difference is most 
pronounced for extinction coefficient, with standard deviations for 90th percentile estimation 
being approximately three times those for mean estimation.  For relative humidity,  they are 
approximately twice as large.  The difference is least pronounced for temperature, where the 
increase is less then 20%.  In each case, there is only slight, gradual improvement in the precision 
of 90th percentile estimation for sample sizes of greater than 40 events. 

_ The next step is to arrive at an appropriate number of events to be distributed among these 20 
strata.  Table 17-6 displays the standard deviation associated with the estimation of the annual 
mean of each evaluative parameter (both raw and seasonally adjusted) that would result from 
samples consisting of30, 40,  50, and 60 events.  For comparison purposes, these standard 
deviations represent average variances restricted to the RADM geographic domain.  The table 
also displays the standard deviation associated with the aggregation of 30 3-day events in a 
sample stratified using the original RADM clusters.  From this,  it is clear that any number of 
events would be sufficient to provide improved resolution relative to the RADM scheme, in the 
context of the evaluative parameters reviewed here. 

Although these results suggest that a 30-event sample would be sufficient to meet the objective of 
matching or exceeding the performance of the RADM approach with regard to estimation 
precision for these outcome parameters, other results displayed in this section demonstrate clear 
improvement in the precision associated with estimation of both means and extremes by moving 
from a 30-event sample to a 40-event sarriple.  In addition, a 40-event set is  needed to ensure 
equal precision to the RADM approach with regard to the estimation of wet deposition amounts. 
The RADM set of 30 events included 20 events from categories that were identified as "wet'', i.e., 
2
for which average wet S04 
- deposition exceeded the median for each cluster.  In other words, 20 
events were selected from the "wettest" 50% of all events. 

This oversampling of wet events was originally pursued to ensure the adequate representation of 
those events that contributed most significantly to wet deposition,  because the accurate 
characterization of wet deposition was the primary purpose ofRADM at that time.  However, 
concerns have since arisen that the disproportionate representation of these events may have 
introduced an overall bias with regard to the ambient concentrations of pollutants that are 
influenced by cloud cover and precipitation.  In view of these concerns, and since wet deposition 
is not the primary focus for CMAQ, the oversampling of wet events was deemed inappropriate for 
present purposes.  In the absence of this oversampling, it is still necessary to include a sufficient 
number of events to ensure that wet deposition is characterized as accurately here as in the 
RADM approach.  This would require approximately 20 wet events, and the same median-based 
definition of wet versus dry events implies that approximately 20 dry events should also be 
included.  Therefore, a total of 40 events was deemed necessary to satisfy all of the objectives. 

Table 17-6.  Standard deviation1 associated with estimation of the annual mean of the indicated 
parameter, using stratified sampling2 with 20 strata and 30, 40, 50, or 60 events.  RADM 
co 

. 
mparatJV<! a11atys1s. 

I 

Temperature, deg. c 

Relative Humidity, % 

RH-Adjusted Extinction 
Coefficient, km·1  (><103
) 

....... 
' 

20 Strata 

Existing 
RADM 
Sample 

20 Strata 

Existing 
RADM 
Sample 

20 Strata 

Existing 
RADM 
Sample 

(30 Events) 

(30 Events) 

{30 Events) 

.. 

NOT SEASONALLY ADJUSTED 

0.96 

0.82 

0.73 

0.66 

0.74 

0.63 

0.56 

0.51 

1.78 

0.82 

2.34 

2.01 

1.79 

1.63 

2.58 

SEASONALLY ADJUSTED 

2.32 

1.99 

1.77 

1.61 

2.41 

6.65 

5.60 

5.01 

4.59 

6.64 

5.60 

5.00 

4.58 

8.28 

8.23 

No. 
of 
Eve 
nts 

30 

40 

50 

60 

30 

40 

50 

60 

1  The table entnes reflect the standard dev1at1on associated with the average stat1on-spec1fic variances 

(averaged across stations within the RADM geographic domain). 

2  "Existing RADM Sample" results reflect sampling of 3-day events from 1982-1985.  Other results 

reflect sampling of 3-day events from 1984-1992. 

Recalling that for 40 events the precision associated with the estimation of mean annual extinction 
coefficient (the primary evaluative parameter) was optimized using 20 seasonally defined strata 
(Table 17-5), a final plan was adopted for sampling 40 events from 20 strata (5  strata per season) 
using approximately proportional allocation. 

Figure 17-9(a-d) displays star chart histograms of the 20 clusters defined as strata in this 
arrangement.  Each chart illustrates the frequency of occurrence of 5-day events belonging to a 
given cluster, and the clusters themselves are ordered according to overall frequency of 
occurrence.  The numbers arranged radially on each chart depict the number of events belonging 
to the cluster from each month of the year.  Map-based graphs of mean wind vectors for day 3 of 
each cluster are contained in Figures 17-1 O through 17-29. 

In order to examine the impact of this scheme geographically (in the context of the precision 
associated with the estimation of mean levels of the evaluative parameters),  we examined 
graphically (not shown), the standard deviation at each site location alongside the actual mean 
with which that standard deviation is associated.  Similarly, analogous information associated with 
the estimation of 90th percentiles were examined.  This examination confirmed  the relative 
geographic uniformity with respect to the standard deviations, which serves as support for the use 
of"average" standard deviations in drawing conclusions throughout this section. 

17 .5.2  Selection of Stratified Sample of Events 

A stratified sample of events was randomly selected from the 20 seasonally defined strata for the 
period  1984-1992.  The sample was selected without replacement to ensure that no single day 
was selected into more than one five-day event, i.e., that there was no overlap between selected 
events.  Systematic sampling (Cochran,  1977) was used within each stratum for which more than 
one event was to be selected.  Specifically, all events assigned to the stratum were ordered 
chronologically, an event was selected near the beginning of that ordering, and subsequent events 
were selected to be evenly spaced throughout the remainder of the ordering.  If k events were to 
be sampled from a cluster containing n events, to illustrate the simple case in which n/k is integer 
valued, the first event would be randomly selected from any of the chronologically first n/k events, 
and every (n/k)th subsequent event would be selected.  The purpose of this approach was to 
ensure appropriate representation over the entire nine-year period. 

Table  17-7 displays the total number of events belonging to each stratum, the number of events 
sampled, and the dates of the sampled events.  These dates are the middle dates of the three-day 
events for which the model ultimately is to be run (i.e., the last three days of the sampled five-day 
event).  This sample of 40 events includes representation from every month of the year, and from 
every year during the period  1984-1992.  Table 17-8 illustrates this representation by displaying 
the number of events selected from each month and from each year. 

Total# of 
Days in 
Stratum, 
Stratum  Season  1984-1992 

1 
2 
3 
4 
5 
6 
7 
8 
9 
10 
11 
12 
13 
14 
15 
16 
17 
18 
19 
20 

Spring 
Summer 
Autumn 
Winter 
Spring 
Winter 
Spring 
Summer 
Summer 
Winter 
Autumn 
Autumn 
Winter 
Autumn 
Summer 
Autumn 
Winter 
Summer 
Spring 
Spring 

292 
267 
238 
210 
200 
188 
185 
171 
171 
170 
168 
150 
139 
135 
129 
128 
102 
90 
89 
62 

1111111111' 

.,,1111:.111111111 

11,,,,111111111:" 

1111111111 

11111,, 
:Ill 
II" 

"'':1111111!!' 
1111111111' 
'1 'llh·1llllllll: 

1l•'1" 

Table 17-7.  Stratum sizes, number of sampled events per stratum,  and dates of events in sample. 
Dates shown are for middle dav of 3-day event. 

.. . ·~ ... 

, ···r:~'···-: 

: 

., 

Number 

Of 

Sampled 
Events 

.• 

, 

' 

Event Dates 

; ...... :~: ... :: .. ,· 

,,,, 

, 

, 

. 

.. 

3 
3 
3 
3 
2 
2 
2 
2 
2 
2 
2 
2 
2 
2 
2 
2 
1 
1 
1 
1 

12 March 1985, 08May1987, 27 March 1990 
17 July 1985, 20 August 1987, 10 August 1990 
08September1986, 12October1988, 08October1991 
04 January 1986, 15 December 1988, 02 December 1992 
07 May 1984, 06 March 1990 
03 January 1987, 07 January 1992 
01  April  1986, 26 March 1991 
05 August 1986, 29 June 1992 
07 August 1984, 12 July 1989 
18 January 1984, 25 January 1989 
18October1985, 12September1991 
17November1987, 14September1992 
19 February 1985, 27 January 1990 
17 October 1988, 24 November 1991 
03 July 1987, 09 July 1992 
25 November 1985, 07 November 1990 
18 December 1989 
22 July 1989 
09May1990 
30 April 1991 

····:;::;::::::::"""" 

Table 17-8.  Number of sampled events representing each month of the year and each year from 
the period  1984-1992. 

EPN600fR-99(030 

.. 

Month 

January 

February 

March 

April 

May 

June 

July 

August 

September 

October 

November 

December 

Number of Events in 

sample 

Year· 
... • 

•··. 

.. ···  Number of Events in 
'···.· 

~,mple· 

; 

6 

1 

4 

2 

3 

1 

5 

4 

3 

4 

4 

3 

1984 

1985 

1986 

1987 

1988 

1989 

1990 

1991 

1992 

3 

5 

4 

5 

3 

4 

6 

5 

5 

## 17.6  Application and Evaluation 

In this section,  examples of the aggregation calculation for annual mean concentrations and total 
wet depositions,  applicable to this sample of events,  are provided.  Following this example is a 
description of an evaluation exercise in which the aggregation calculation was carried out for light 
extinction coefficient, and the aggregated estimates were compared to the actual values based on 
data from all of the days in the period. 

### 17.6.1  Application of the Aggregation Procedure 

Aggregation calculations will be applied to model-based depositions and concentrations obtained 
for each sampled event, to achieve unbiased estimates for annual and seasonal means and other 
summary statistics within each grid cell.  Since the goal of sampling from every defined stratum is 
achieved in this approach, these calculations are simplified in comparison to earlier aggregation 
methods (NAPAP,  1991).  In essence, these aggregation calculations merely produce weighted 
means, totals,  or other summary measures, from the sample of events. 

To illustrate the aggregation approach, consider the estimation of a mean annual air concentration 
using model output for the 40 events selected above.  These events represent 20 strata; denote 
these using the subscript i, i=I,2,  ... , 20.  Let/; denote the frequency of occurrence associated 
with stratum i, i.e., the total number of3-day events belonging to the stratum during the period 
1984-1992.  For an individual grid cell, also let 
... :•··represent the mean model-based concentration associated with all sampled events from stratum i. 
··  Thus, for stiafa with a single sampled event, it is just the event mean concentration in the grid cell. 
"'''''''' 
For strata with two or three sampled events, it is the mean concentration for all of those events. 
Then the esiim'aied annual mean rur concentration is given by 

'"''''''''''''  " "'''"""" 

''"'''''''''' 

""'" 

' 

"" 

... 

,, 

Mean  Air Concentration  =  I= 1 

-

20 
EJ;CMODEL 
/ 

20 
EJ; 
/=l 

(17-2) 

II 

"' 

.llllllllllllll!"!lllii11,,,i:"'l11lll" 

.!!1111''  ,,,!

:illlll,,J,,,,111 .. 1111 .. 

·:1;:]111111111" 

"" 

,,,,. 

:, 

"'· 

"""'l"":':"'!!""'"';;·,,lllllllii 

:'!II: 

, 

,,,,,,,;;;;·"""' 

'lllu,' 

,,'  ""'• 

' 

:II 

, 

""')'"' 

"•:' 

II 

•11111 ......  "1111 

'"""" 

1
!::::1i1111," 

~,gm~!.~,,§fQ£!EP~t,,,2~~~ par~eters (e.g., dry depositions  and other summary statistics are 
calculated using similar methods.  The calculation for wet deposition is different,  primarily 
becaus,~ .,,the ~~~ghtin~}s partially dictated by precipitation.  Let 

.... 

·· 

" 

illllllllll:: 

1111· 

I 

111::::1111:.:' 

1111111":::1111 

'":111111!!!'' 

11111111 

re{)ie's'ent the mean 3-day modeled deposition for sampled events in stratum i, the mean 3-day 
modeled precipitation for sampled events in stratum i, and the total measured precipitation 
accounted for by all events belonging to stratum i, respectively.  Then the estimated total annual 
wet deposition is given by 

Total  Wet  Deposition 

lllllliiillll,,,, 

11:::11111111111111:" 

:1111111111111::::111 

111

!11111 

1''

x  PMEAS  x 

I 

1 
3X9 

(17-3) 

''"""" 

1r:11,,,,::: 

1111111111111":1 

T.W .. ~ ~xpression can be thought of as a weighted sum in which the model-estimated wet 
""  CQ,Q~~tI!!!:!P!1f2~,2: stftitumis ~.~!~~!ed by the total measured precipitation associated with the 
~111,  stratum.  The final component of this expression is included to reflect the fact that each day is 
counted three times in the calculated sum (due to the use of3-day events) and that the strata are 
defined over a nine-year period. 

### 17.6.2  Evaluation 

In order to determine the effectiveness of the aggregation technique and subsequent episode 
selection, comparisons were made between the observed mean bcxis for the period  1984-1992 and 
the aggregated estimates of that mean using the stratified sample of events described in 17.5.2 and 
listed in Table 17-7.  This preliminary evaluation, which includes simple  regression analysis,  is 
similar to that performed on RADM (Eder and LeDuc,  1996;  Eder et al., 1996). 

Results comparing the observed and aggregated mean bcxis  (based on Equation 17-2) are 
promising as seen in the scatterplot provide in Figure 17-30.  The correlation between the 201 
observed and aggregated mean bcxt  was very high correlation (r =  0.988).  Estimates of the 
regression coefficients between the observed and aggregated mean bcxts reveal an  intercept value 
of -0.0012 that is not significantly different from zero (a= 0.05):  The slope (1.018), however; is 
significantly different from  1.0, (a= 0.05) indicating a slight tendency for these particular 
episodes to provide an over-estimate of the expected mean bcxt·  This slight, "apparent" bias is, 
however, well within the expected variability associated with the particular set of episodes in this 
stratified sample.  To wit,  selection of a different random set of episodes would just as likely 
result in a slight under-estimate of the expected mean  bcxt· 

Perhaps a better way to illustrate the effectiveness of this technique can be shown through an 
examination of the percent deviation in aggregate estimates of the mean bcxi  (where the deviations 
are relative to the observed mean (aggregated bcxt - observed bcxt I bcxt  observed).  These percent 
deviations, which were calculated over the period  1984 -1992, are presented in Figure 17-31.  For 
the most part, the deviations are within ±  10%, indicating excellent agreement between the actual 
mean bcxt  and the aggregated estimates of the mean bcxt· 

The slight over prediction tendency mentioned above appears to be somewhat spatially biased as 
also seen in Figure 17-31.  As seen in the top of the figure,  areas of generally positive deviations 
(aggregation approach yields a higher bcxt>  hence lower visibilities than observed) appear to 
concentrate from east Texas into the southeastern states and again in the upper midwest between 
Minnesota and the Dakotas.  The states of California and Idaho also exhibit positive deviations. 
Negative deviations,  presented in the bottom of the Figure 17-31, tend to predominate from the 
northeast states into the Great Lake States and southwestward toward the states of Kansas, New 
Mexico and Arizona.  This spatial dependence of the estimates is,  once again, well within the 
expected variability.  Selection of a different random set of episodes would likely result in a 
different pattern of positive and negative deviations, as there is a natural tendency for sites at 
close proximity to behave in a similar fashion. 

The scatter plot in Figure  17-30  also reveals an increase in variance about the regression line 
starting at an observed mean bcxt  of 0.085.  Unlike the positive bias,  discussed above, this increase 
in variance does not appear to be spatially biased,  but rather exhibits a random distribution across 
the domain.  This is represented in Figure 17-31  by the scattering of the larger biases (i.e.  biases> 
5.0%) evenly across the domain. 

## 17.7  Summary and Discussion 

The objective of this research was to develop a new aggregation approach and set of events to 
support model-based distributional estimates of air quality parameters (acidic deposition, air 
concentrations, and measures related to Visibility) over the continental domain.  The basic 
approach is to define meteorological categories that account for a significant proportion of the 
variability exhibited by these air quality parameters, as well as the particular transport mechanisms 
involved,  so that source-attribution analyses are facilitated.  This requires that categories be 
d~.!i.rn~.4 Y!'.!t!1 !:!!! erp.phasis on wind flow parameters.  To this end, the cluster analysis of zonal u 
and meridional v wind field  components has been used to determine meteorologically 
representative categories. 

'1111"1111'11",,,,11 

"'""""'"" 

1111,,.lh 

""""""' 

111111!"

The research described in this chapter was carried out in three phases: 

1111!,ll 

"111111111: 

I 

'" 

1i1 

r::/111 

1'1.1111 

• 

1111111111111

1
' , , :  

: 11 ! ! ! 111111 i I : 

:11111i11i111, 

1111

lllll!

ii11·, 

•  lllli.::;.,. 

1111111111111 

.11111111111 

.1: 

:1111:::1::· 

1111111::::11:' 

• 

Phase  I: Various stratification schemes were evaluated and compared on different 
temporal and geographic scales to support the selection of a preferred general 
methodology.  The selected methodology involved clustering of wind field  data over the 
' c0iitinental domain within each of the four seasons, and defining strata to be equivalent to 
the resulting clusters.  This methodology demonstrated superior relative efficiency 
compared to methods defined on an annual time frame or on a regional scale for estimates 
involving the evaluative meteorological parameters, and is designed to support seasonal 
e5timati.on with both simplicity and precision. 

'  . "  . i 

, . I . 

, 

. " : . 

i i 11 : ~ : : ! 1111111111111111111 j : : 

... 

·:: .. 

111,1::: 

' 11111111111111111 : : : 

' 

' " : 

' , . . '  " 

::.lllu. 

,1  .. 

. , : ' i I i 
, 

. : ' 11 ' 

' 

. ; : 

. . I : : : ' ' ' . " ' . : " " . . . ; "  · 1. 

'1  .. 

'· 

1111. 

•1111 

. , ,  

' ·  

,1111. 

, 

, 

, 

,."'II' 

11 11111111111111111111111, 

PQ,~,~~111111~11;  ~11~~.~~~!:!:tio~~ w,~re.mad~T~~arding appropriate n~mbers of clusters and .events 
to ~upport samphng usmg the general methodology selected m Phase I.  The resultmg 
scheme involved a total of20 clusters (5  per season), and 40 events, defined over the time 
.•:  period 1984-1992.  This scheme affords superior precision to previous approaches for 
,,  estimates involving the evaluative meteorological parameters, supports approximately 
equivalent representation of wet events to those approaches without oversampling, and 
provides adequate resolution of wind field  patterns that characterize transport. 

.1111111'  " 0111'1 

' "   ""'""""'••' 

llh,,,',,11,''I 

,111111··1111111111 

1111''111"111 

1.''!111 

1, 

,, 

'" 

Phase 3:  A stratified sample of events was selected under approximate proportional 
allocation, using systematic sampling within strata, in accordance with the scheme 
determined in Phase 2.  This sample was successfully evaluated through a comparison of 
aggregated estimates of the  mean bex1  to the actual mean bcxt>  revealing a high level of 
agreement, although there was a slight tendency of the aggregation and episode selection 
technique to over-estimate the expected mean bcxt· 

The goal of this research was to categorize many years worth of meteorological patterns into a 
few classes.  This represents a very ambitious goal,  and it should not be surprising that there is 
substantial variability associated with the wind vectors assigned to individual clusters. 
Nevertheless, the results described above suggest that the approach achieves a reasonable 
characterization of frontal passage scenarios and leads to clusters that explain variation in the 
evaluative meteorological parameters used in this analysis (temperature, relative humidity, and 
visibility), and therefore can be used to achieve improved estimates of these parameters relative to 
estimates obtained from simple random sampling.  Moreover, the constrained definition of distinct 
seasonally-based clusters brings further improvement to the ability of the clusters to explain the 
variation in these parameters, and therefore leads to more precise estimates associated with them. 
The evaluative parameters were selected for their known associations with many air quality 
parameters of interest, thus suggesting that the clusters should also be effective in defining strata 
from which events can be selected to estimate those air quality parameters. 

## 17.8  References 

Brook, J.R., P.J.  Samson, and S.  Sillman,  l 995a. Aggregation of selected three-day periods to 
estimate annual and seasonal wet deposition totals for sulfate, nitrate, and acidity.  Part I:  A 
synoptic and chemical climatology for eastern North America. Journal of Applied Meteorology, 
34, 297-325. 

Brook, J.R., P.J.  Samson, and S.  Sillman,  l 995b. Aggregation of selected three-day periods to 
estimate annual and seasonal wet deposition totals for sulfate, nitrate, and acidity.  Part II: 
Selection of events, deposition totals, and source-receptor relationships. Journal of Applied 
Meteorology,  34, 326-339. 

Cochran, W.G.,  1977. Sampling Techniques.  Wiley & Sons, New York. 

Davis, R. E.  and L. S.  Kalkstein,  1990. Development of an automated spatial synoptic 
climatological classification. International Journal of Climatology,  10, 769-794. 

Eder, B.  K.,  J.M. Davis and P.  Bloomfield,  1994. An automated classification scheme designed 
to better elucidate the dependence of ozone on meteorology.  Journal of Applied Meteorology, 
33,  1182-1199. 

Eder, B.  K.,  S.K. LeDuc and F.  Vestal,  1996.  Aggregation of selected RADM simulations to 
estimate annual ambient air concentrations of fine particulate matter.  Ninth Joint Conference on 
Applications of Air pollution Meteorology with the A&WMA, Jan.28-Feb. 2, Atlanta, GA. 

Eder, B. K.  and  S.K.  LeDuc,  1996.  Can  selected RADM simulations be aggregated to estimate 
annual concentrations of fine particulate matter?  Proceedings of the International Specialty 
Conference on the  Measurement of Toxic and Related Air Pollutants, May 7 - 9, Research 
Triangle Park, NC. 

Fernau, M.E.  and P.J.  Samson,  1990a. Use of cluster analysis to define periods of similar 
meteorology and precipitation chemistry in eastern North America. Part I:  Transport patterns. 
Journal of Applied Meteorology, 29, 735-750. 

Femau, M.E. and P.J.  Samson,  1990b. Use of cluster analysis to define periods of similar 
meteorology and precipitation chemistry in eastern North America.  Part II:  Precipitation patterns 
and pollutant deposition. Journal of Applied Meteorology, 29,  751-761. 

Kalnay, E., M.  Kanamitsu, R.  Kistler, W. Collins, D.  Deaven, L.  Gandin, M.  Iredell,  S.  Saha, G. 
\Vhite, J.  \Voollen,  Y. Zhu, M.  Chelliah, W.  Ebisuzak.i,  W. Higgins, J.  Janowiak, K.  Mo, C. 
Ropelewsk.i, J.  Wang, A.  Leetmaa, R.  Reynolds, R.  Jenne and D. Joseph,  1996. The 
NCEP/NCAR 40-year reanalysis project.  Bull. Amer. Meteor.  Soc.,  77, 437-471. 

NAPAP,  199!:  National Acid Precipitation Assessment Program  1990 Integrated Assessment 
Report, National Acid Precipitation Assessment Program, 722 Jackson Place NW, Washington, 
D.C. 

SAS  Institute Inc.,  1989.  SAS/STAI® User's Guide,  Version  6,  Fourth Edition,  Volume  I.  SAS 
Institute Inc.,  Cary, NC. 

''Iii 

' 

\Vard, J.H.,  1963. Hierarchical grouping to optimize an objective function. Journal of the 
American Statistical Association 58, 236-244. 

This chapter is taken from Science Algorithms of the EPA Models-3 Community 
J't.fultiscale Air Quality (CMAQ) Modeling System, edited by D. W. Byun and J. K. S. 
Ching, 1999. 

Fig.17.1  (a) Mean wind vectors for day  1 of annually defined cluster 
1 (of 30). 


Fig.17 .2 ( c) Mean wind vectors for day 5 of annually defined cluster 
9 (of30). 

Fig.  17.3 (a) Actual wind vectors for 25 January 1986, day 3 of cluster 
9 (of30). 

Fig.  17.3  (b) Actual wind vectors for  13  January  1988, day 3 of cluster 
9(of30). 

Fig.  17.3  (c) Actual wind vectors for 8 January 1990, day 3 of cluster 9 
(of30). 

Fig.  l7.4(a) Mean and distribution of wind vect~r~ for day  I of 
91,1~st.~r 9(of30). 

Fig..  17.4 (b) Mean and distribution of wind vectors for day 3 of 
'111!,,,,clq§tei: 9 (of30). 


Fig.  17.4 ( c) Mean and distribution of wind vectors for day 5 of cluster 9 (of 3 0). 

frequencies  of  30  Clusters  (Annually  Defined) 

Cluster No.  1:  12.197. 

1
'

'

1111ii:!lllllll' 

llli;,, 

Jan 

0 

Ila• 
2 

Apr 
7 

Uay 
32 

Od 
II 

m  Jul 

127 

Jun 
77 

Cluster No.  2:  12.103 

1111 

,,, 

,, 

;'~n 
0 

Oct 

" 

Apr 
10 

Au9 
112 

Jul 
t 32 

Jun 
90 

Cluster No.  3:  5.53% 

Cluster "No.  4:  5.37% 

Jan 

0 

Jan 

I 

Ap< 
18 

Aug 
20 

Jul 
53 

Jun 
69 

Oct 
38 

Sop 
21 

Aug 

3 

Jun 
•6 

Jul 

• 

.... 

15 

Ap< 
27 

llay 
50 

Cluster No.  5:  4.96'7. 

Cluster No.  6:  4.703 

Jon 
25 

Oct 
47 

Apr 
21 

Jon 

2 

rob 

0 

Doc 

• 

Nov 
II 

Oct 
73 

Sop 
63 

liar 

• 
Ap• 
a 

lloy 
17 

;il11' 
""II;, 
,, 

I  I 

11111111111 

'1:'"111 

1lll111i111::, 

Fig.  17.5 (a) Monthly frequencies of annually derived clusters for clusters 1-6. 

Aug 
s 

Jul 
6 

Jun 
12 

Aug 

0 

Jun 

0 

Jul 
0 

,,:111111111 

::I::"''' 
''1111, 

,1111::"11' 

:111111.11 

l''.111 
Ii 

I:, 

"' 

illh 

,, 

,!111111 

17-42 

,,"111111111:, 

:1111 

111111 

,,11111,, 

11111

' 

'1111 
:111,,, 
,111• 

''I::: 

11111: 

:iiiiii:' 


Frequencies  of  30  Clusters  (Annually  Defined) 

Cluster No.  7:  4.29% 

Cluster No.  8:  4.093 

Jon 
0 

Jon 
2 

>lov 
2 

Oct 
25 

Sop 
39 

Apr 
10 

Oct 
1• 

War 
3 

Apr 
15 

!joy 
32 

Aug 
35 

Jul 
26 

Jun 
26 

Aug 
27 

Jul 
23 

Jun 
29 

Cluster No.  9:  3.65% 

Cluster No.  10:  3.61'l';; 

Jon 
23 

Oct 
26 

.... 

19 

Apr 
13 

1'ay 
3 

Jon 
19 

Oc1 

• 

Apr 
32 

Aug 

0 

Jul 
0 

Jun 
0 

Aug 

0 

Jun 

1 

Jul 
0 

Cluster No.  11:  3.473 

Cluster No.  12:  3.403 

Jon 
31 

Jan 
35 

Oct 
7 

Uor 
35 

Apr 
I• 

Uoy 
2 

Ocl 
6 

S•p 
0 

Apr 
0 

Fig.  17. 5 (b) Monthly frequencies of annually derived clusters for clusters 7-12. 

Aug 

0 

Jul 
0 

Jun 
0 

Aug 

0 

Jul 
0 

Jun 
0 

17-43 

"11111 

····1111 
1111
!!111 

··1

' " : : ~ 

1111111111111 

1111111111111: 

. . ~ ! 11111111 

1,,,, 
,1111111111 

11, 

... 

;11111111•11• 

11ili11 

'111111 

"11111' 

'lhlll 

""'

11

111 

111, 

,·1111· 

•Ill! 

" 

:11111 

·Ill!:' 

,'1'1!1

, 

11

Frequencies  of  30  Clusters  (Annually  Defined) 

Cluster No.  13:  2. 797. 

Cluster No.  1 4:  2. 79% 

Oc1 
a 

Sop 
0 

Mor 
18 

Apr 
0 

'lay 
3 

Aug 
0 

Jun 

0 

Jul 
0 

Aug 

0 

Jul 
0 

Jun 

0 

Cluster No.  15:  2.677. 

Cluster  No.  16:  2.35% 

11111111 

lllllllllli11 

1111111111111' 

1,·,,,111111111111111,,, 

:11111111111 

1111··111111111111111111111' 

Oct 

" 

Sep 
I 

Aug 

0 

J..i 
I 

Jun 

' 

Cluster No.  17:  2.17'7. 

1111111 ! II 

1 

11111 ~ ' 

I II 

" II 1 I 1 

11111 

"  " 

II 

Jon 
10 

'" 

111 

Oct 
3 

Sop 
0 

"or 
18 

Apr 
21 

Uoy 
3 

AUQ 

0 

Jul 
0 

Jun 
0 

Cluster No.  18:  2.08% 

,,111r:1111111111, 

Jon 
12 

,,1i, 

011 a 

""r 

1 

Oct 
1 

Apr 
19 

""Y 
3 

11111111 

,,.111111 

1111111111: 

,,, 

111111111 

,,11 

"'111111111111' 

11111111111111111111 

Aug 
.~. 

Jul 
0 

Jun 

0 

Aug 

0 

Jul 
0 

Jun 

0 

Fig.  17. 5 (c) Monthly frequencies of annually derived clusters for clusters  13-18. 

Frequencies  of  30  Clusters  (Annually  Defined) 

Cluster No.  19:  1.853 

Cluster  No.  20:  1 .803 

Apr 
0 

Aug 

0 

Jul 
0 

Jun 
0 

Aug 

0 

Jul 
0 

Jun 
0 

Cluster No.  21:  1. 76% 

Cluster  No.  22:  1.67% 

Oct 
J 

..,., 

~ 

Ap• 
0 

t.loy 
0 

Aug 

0 

Jul 
0 

Jun 
0 

Aug 

0 

Jun 
0 

Jul 
0 

Cluster No.  23:  1 .583 

Cluster No.  24:  1.55% 

Fig.  17. 5 ( d) Monthly frequencies of annually derived clusters for clusters  19-24. 

Aug 

0 

Jul 
0 

Jun 
0 

17-45 

Frequencies  of 30  Clusters  (Annually  Defined) 

Cluster No.  25:  1.487. 

Cluster No.  26:  1 .443 

Jon 

' 

'11'11111• 

111
• 

'11111. 

1111111111111111111111'"''  c);'t 
z 

Apr 
4 

1111111111111""

111

' 

:111111;1,,:11111111!!

11

' 

11111111,lllll 

J.ug 

0 

Jul 
0 

Jun 
0 

C:!usler No.  27:  1 . .307. 

Cluster No.  28:  1.21% 

Jon 
3 

1'pr 
13 

Jun 
3 

Jul 
0 

Cluster No.  29:  1.147. 

Cluster No.  30:  1 .003 

11111111111" 

111111111111" 

1
1111r'' 

Oct 
111111~11, 

111111111111111111,,, 

JGn 
5 

Apr 
3 

Uoy 
2 

Apr 
5 

Fig.  17.5 (e) Monthly frequencies of annually derived clusters for clusters 25-30. 

Aug 
0 

Jun 
1 

Jut 
0 

Aug 
0 

Jul 
0 

Jun 
0 

1 1111illllllll' 

;11::1111 

111111!1!11111•'11111'' 

!11111!"'111 

11111 

1 :111111111 

'

11111

1111111 

Ir""'· 

11, 

,, 

;11111 

,:II' 

,, 

·11111,,,,,,:111111,,,lll· 

1
::!:::1111111111: 

11111,'' 

11111;,,,,,,,;;,, 

11,,' 

1111111111i''I 
,, 

'I' 

1 

:11111111

111111, 

!11111111' 

"!!111111::," 

'''''llii1 

'iilllllllll 
111111 

'1111111111 

Fig.  17. 6 Site locations for 20 I  meteorological parameters used in the evaluation. 

Standard Deviation Versus Sample Size: Temperature 
Av91'age  (Across StaHons) Standard D•vlallon Associated with 

EsHmaUon or 1984-1992 Mean rrom Aggregation or 3-0ay Eplsodn 

2.5 

2.0 

--
0 
" C> 
C> ... a 
C> 
"C  1.5 
. 
>  1.0 
~ 
.. "C 
"C ... 
c .. - 0.0 

0.5 

(/) 

\ 
\ 

\ ' ' 
'',', ........ 

..... ..... ----------------------------

0 

50 

100 

150 

200 

250 

300 

Number of Episodes In Sample 

Samplng 

Fig.  17. 7 (a) Standard deviation of estimated mean temperature 
versus sample size. 

Standard Deviation Versus Sample Size: Relative Humlcllty 

Av•rag• (Across stations) Standard Oevlatlon Associated with 

Esttmatton or 1984-1992 Mean from Aggre~atlon of 3-Day Episodes 

--~ 

> Cl 
0 
.... 
"C 
al 
"C c 

:11111::1111111:  Cl 

"111111  . . .  

(/) 

4.0 

3.5 

3.0 
2.5 
2.0 
1.5 
1.0 
0.5 
"'lllli 
0.0 

111111'' 

'llr',,, 

1111111111111111 

t,:1111;111:, 

1:' 

111111111,, 

\ 

\ 
\, 

',· 

.................... 

-..... -------------------

------

250 

SOQ 

0 

,,,50 

100 

.150 

200 

Numb'!.r of Epis~des i~ San:!ple 

,111111111· 

:111111111111111: 

'111 11111111111111 

11;;;;11111m::11111111w 

11111,,,111111!'' 

Sampling 

I'' 

I' 

1111111111111111: 

,11111'''' 

,,, 

:ll'';lii 

II 

II' 

11111111  Fig.  17. 7 (b) Standard deviation of estimated mean relative humidity 
ver~s sample size. 

17-48 

Standard Deviation Verws Sample Size:  RH-Adj Extinction Coeff 

Average (Across Stations) Standard Deviation Associated with 

Estimation of 1984-1992 Mean lfom Aggregation of 3-0ay Episodes 

EP A/600/R-99/030 

.... 
~ 

I 

> 0 
0 
"C ... 

Ill 
"C 
c 
Ill 

(I,) 

0.010 

0.008 

0.006 

0.004 

0.002 

0.000 

----

0 

50 

100 

150 

200 

250 

300 

Number of Episodes In Sample 

SampRng 

Fig.  17. 7 ( c) Standard deviation of estimated mean extinction 
coefficient (adjusted for relative humidity) versus sample size. 

Standard Deviation Versus Sample Size: Temperature 

Average (Across Stations) Standard D!Mation Associated v.ilh  Estimation of 
1984-1992 Mean & 90lh Pen rtom Aggregallon of 3-Day Episodes In 20 Strata 

1.4 

1.2 

1.0 

0.8 

0.6 

0.4 

0.2 

0.0 

0 
Cll 
0 
0 .... 
Cl 
0 -a 

> 0 
0 
-a 
.... 
«I -a 
c 

ftl -(/) 

..... _ --

----... _ 

------.._. _______ __ 

20 

30 

40 

50 

60 

70 

Number of Episodes In Sample 

Es11maUonof. .. 

+---+--*- Mtan 
... - . - - 90thPe!I 

Fig.  17.8 (a) Standard deviation of estimated mean and 90th 
percentile temperature versus sample size. 

Standard Deviation Versus Sample Size: Relative Humidity 
"  Average (Acrou Stations) Standard Devlafon Auoclated with Esllmallon of 
1984-1992 MNn & 90lh Pct! from Aggregation of 3-Day Epi&Odea In 20 Strata 

'ii 

;  . 

......... .......... 

.......... .......... ........ _ 

-----...,.__ ______ ~ 

6 

"'0"5 
~ 
. 4 
> 
83 
-a ..... 
~2 
c 
al 
Ci)  1 

lllllliiii'', 

'1111,, 

':1111111 

0 

,,1  20 

30 

40 

50 

60 

70 

Number of Episodes In Sample 

Elttmatlon or •.• 

*-+--*  ....... 
.,._ ... _,,. 

9Clt11Pctl 

Fig.  17. 8 (b) Standard deviation of estimated mean and 90th 
percentile relative humidity versus sample size. 

1
: 

11111"''

,"1111111111111 

"' 

~il<fard Deviation Versus Sample Size: RH-Adj  Extinction Coeff 
Awrag• (Across staUons) standard D....taHonAssociatlldwilh Estimation of 
1QB4-1Q92 MHn & 90!h PcU tromAggregaUon or 3-Day Episodes in 20 strata 

'"t"":ll1'i1/1,I'' 

1l1'1'11!

1

1t'!11i"' 
11!

1

11111111111111 

1
1111,,,::11111,lllliiii::, 

11111111111

"'"

1
" 

I" 

•,111 

"11111111111 

0.030 

0.025 

0.020 

0.015 

0.010 

0.005 

..... _ 
--

----·-------.... -------

I 

-..... 
~ 
. 
> C> 
0 
-a ... 
"" 
"'C c 
cd 

-Cl.) 

'11.111111111111  0.000 

,• 

20 

30 

40 

50 

60 

70 

Nt.mber of Episodes In Sample 

j  Estlmadon oL 

_...........,..  Mun 
"*-*"- 901h Ptll 

I 

) 

.. · 

:""""'"" 

: . 
'Ii} 

'.'.": 
111
,:::, 

lll""I' 
:' 
11111111111111: 

~~~1ll1i111illlll 

··· .....  \  " · :II 

•  th 
Fig.  17.8 \.C)  Standard deviation of estimated mean and 90 
percentile extinction coefficient (adjusted for relative humidity) 
versus sample size. 

. 
:: 

" 

· 

111 

, 

... ,. 

111111 

,,,:':· 

,. 

"' 

,,,,, 

. 

·· 

17-50 

"''Ill' 
.. ,:111:. 

1111 

Frequencies  of  20  Clusters  (5  Winter,  5  Spring,  5  Summer,  5  Autumn) 

Cluster No.  1:  8.89%  (SPRING) 

Cluster No.  2:  8. 13%  (SUMMER) 

EP A/600/R-991030 

Oct 
0 

"a• 127 

Ap• 
7~ 

"ay 
90 

Cluster No.  3:  7.25%  (AUTUMN) 

Cluster  No.  4:  6.39%  (WINTER) 

Cluster No.  5:  6.09%  (SPRING) 

Cluster  No.  6:  5. 72%  (WINTER) 

Aug 

0 

Jun 

0 

Jul 
0 

Fig.  17. 9 (a) Monthly frequencies of seasonally derived clusters for clusters  1-6. 

Aug 

0 

Jun 

0 

Jul 
0 

Jun 

0 

Jul 
0 

Frequencies  of  20  Clusters  (5  Winter,  5  Spring,  5  Summer,  5  Autumn) 

Cluster No.  7:  5.63'7.  (SPRING) 

Jon 

Cluster No.  8:  5.213 (SUMMER) 

lh,''lll 

Jon 
0 

,II• 

,II, 

'"'"11111111 

Oct 
0 

Mor 
35 

Woy 
9• 

''1111111
' 

ll1111lllh 

'I' 

'I' 

·111;,;;1111111111,, 

'"""'11111 

/.ug 

0 

Jut 
0 

Jun 
0 

Oct 
0 

Apr 
0 

Aug 
7, 

Jul 
53 

Jun 

" 

Cluster No.  9:  5.21 ~(SUMMER) 

Cluster  No.  10:  5.18'7.  (WINTER) 

1111111111, 

,1111111··:11111111 

/.ug 
·~ 

Jun 
6~ 

Jut 
61 

llor 
0 

Apr 
0 

t.loy 
0 

Oct 
0 

Sop 
0 

Aug 

0 

Jul 
0 

Jun 
0 

Cluster No.  11:  5.12'7.  (AUTUMN) 

Cluster No.  12:  4.573  (AUTUMN) 

: I ! ~ 

,11111111111::::11111 

111111111 

llor 
0 

Uoy 
0 

1111111111:111,,!1::' 

111111111111;1; 

1
'  I!,,: 

1
'
1!11

"'I,, 

"':i' 

" 

,II' 

1111111111111 

Q'll 

'11111 

Aug 

Ju,, 

Q 

J~I'. 

Fig.  17. 9 (b) Monthly frequencies of seasonally derived clusters for clusters 7-12. 

·1111111111111111 

•11111111· 

17-52 

11111111,,m111111 

111111::,111:,1111 

'" 

'''"' 

11

1111il:;

,illllllll[1, 

1ll"u1illll 

,,"111111

1 

11'' 

'I' 

"·,  ,iillllllllll 

11111111111'!11 

Frequencies  of  20  Clusters  (5  Winter,  5  Spring,  5  Summer,  5  Autumn) 

Cluster  No.  1 3:  4.23%  (WINTER) 

Cluster No.  14:  4. 1 1 % {AUTUMN) 

EPN600/R-99/030 

Nov 
58 

Oct 
55 

Sop 
2• 

"0' 
0 

Ap• 
0 

t.loy 
0 

Aug 

0 

Jul 
0 

Jun 
0 

Aug 

0 

Jul 
0 

Jun 
0 

Cluster  No.  1 5:  3.93%  (SUMMER} 

Cluster No.  1 6:  3.90%  {AUTUMN) 

"0' 
0 

Ap• 
0 

"oy 
0 

Oct 
13 

Aug 

0 

Jul 
0 

Jun 

0 

Cluster  No.  17:  3. 11 % (WINTER) 

Cluster No.  18:  2. 7 4%  (SUMMER) 

Oct 
0 

Ap• 
0 

Fig.  17.9 (c) Monthly frequencies of seasonally derived clusters for clusters 13-18. 

Aug 

0 

Jul 
0 

Jun 
0 

Aug 
13 

Jul 
3• 

Jun 
•3 

17-53 

"

" 

"''  Frequencies  of  20  Clusters  (5  Winter,  5  Spring,  5  Summer,  5  Autumn) 

Cluster No.  19:  2. 7 n7- (SPRING) 

Cluster No.  20:  1.897. (SPRING) 

Jon 
0 

;; 

1illllllllllllllllll. 1 

'll!I 
'"  0 

Aug 
0 

Jun 
0 

Jul 
0 

Fig.  17.9 (d) Monthly frequencies of seasonally derived clusters for clusters  19-20. 

11111111111'"11: 

11111111111111111111 

11111111111" 

''11111111111111111!' 

11111111111111111111,,,, 

Ill' 

""'I': 

",,  ''l•1" 

',111111111• 

I' 

" 

•II',, 

Fig.  17 .10 Mean wind vectors for day 3 of seasonally (Spring) defined 
d~sfer i ( of20). 

:111 

1.,,,, 

•111111111111 

1, 

11111"' 

:1 

, 

""II 

Fig.  17.11  Mean wind vectors for day 3 of seasonally (Summer) 
defined cluster 2 (of20). 

Fig.  17.12 Mean wind vectors for day 3 of seasonally (Autumn) defined 
cluster 3 (of 20). 

17-55 


,,,,,II 

Ill',,'' 

lh 

1111111111!1! 

11111111111111111111 

1lliiii1illlllllllll:,, 

'

Fig.  17 .13  Mean wind vectors for day 3 of seasonally (Winter) defined 
,,  cluster 4 (of 20). 

:1111111111:::." 

'1111111111!!111 

1111111111' 

Fig.  17.14 Mean wind vectors for day 3 of seasonally (Spring) defined 
cluster 5 (of20). 

Fig.  17.15 Mean wind vectors for day 3 of seasonally (Winter) defined 
cluster 6 (of20). 

Fig.  17 .16 Mean wind vectors for day 3 of seasonally (Spring) defined 
cluster 7 (of20). 

Fig.  17 .17 Mean wind vectors for day 3 of seasonally (Summer) 
..... defined cluster 8 (of20) . 

",,,,,,,,,,:::  d~f!ned cll;!ster 9 (of 20). 

1s.' 'Mean 

.... "· .. 

·'.··  ~d vecto;s for day 3 of'~easonally'(summer) 


Fig.  17.19 Mean wind vectors for day 3 of seasonally (Winter) 
defined cluster 10 (of 20). 

Fig.  17 .20 Mean wind vectors for day 3 of seasonally (Autumn) 
defined cluster 11  (of20). 

Fig.  17.21  Mean wind vectors for day 3 of seasonally (Autumn) 
defined cluster 12 (of20). 

Fig.  17.22 Mean wind vectors for day 3 of seasonally (W°mter) 
defined cluster 13  (of20). 

Fig.  17.23  Mean wind vectors for day 3 of seasonally (Autumn) defined 
cluster 14 (of 20). 

Fig.  17.24 Mean wind vectors for day 3 of seasonally (Summer) 
defined cluster 15  (of 20). 

Fig.  17.25 Mean wind vectors for day 3 of seasonally (Autumn) 
defined cluster 16 (of20). 

Fig.  17 .26 Mean wind vectors for day 3 of seasonally (Winter) defined 
cluster 17(of20). 

Fig.  17.27 Mean wind vectors for day 3 of seasonally (Summer) defined 
cluster 18 (of 20). 

Fig.  17 .28 Mean wind vectors for day 3 of seasonally (Spring) defined 
cluster 19 (of 20). 

Fig.  17.29 Mean wind vectors for day 3 of seasonally (Spring) 
defined cluster 20(of20). 

Extinction Coefficient (km -1) 

I 

I 
I 

~ 
.l~ 
.?• 
··················+····  ···'·····l·····. 

c:  0.2 
~ 
'(;  0.15  · 
fJ 
~  0.1 
1l 
~0.05  ···············;·~·~······ .... ··1···················1·········· 

·k."'k1 

I  ~:J-

! 

I 
! 
j 

I 

I 
I 

) 

O· 

·'"  l 

0 

0.05 

0.1 

Observed Mean 

I 
0.15 

0.2 

'Ill 

I 

Fig.  17.30 Scatterplot of the observed mean b0x1 (km"1
aggregated estimate of the mean bcxt (km-1
1992. 

)  for the period  1984-

)  versus the 

Fig.  17.31  Spatial variation of the bias of the aggregated estimates of the mean bext 
(km-1) for the period  1984-1992.  (Deviations (%) are relative to the observed mean: 
aggregate-observed/observed).  Top figure indicates sites with positive bias, bottom 
figure sites with negative bias. 
