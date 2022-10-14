#### 2D-VBS Chemical Mechanism

This chemical mechanism couples the saprc07tic gas chemistry, the 2D-VBS mechanism for organic aerosol chemistry, aero7 for other aerosol chemistry, and cloud chemistry. The 2D-VBS mechanism was developed by Tsinghua University. Details are given by Zhao et al., Scientific Reports, 2016, Zhao et al., ES&T, 2015, and Chang et al., One Earth, 2022.

Regarding the emission input and processing, the CMAQ team provides three versions of CMAQ_Control namelists. The streamlined versions of CMAQ_Control_DESID_saprc07tic_2DVBS_aq.nml and CMAQ_Control_DESID_saprc07tic_2DVBS_aq_Fire.nml allows emissions of the VBS species to be mapped from existing, highly lumped emissions without source resolution. In other words, when these two versions of namelists are used, the emission input files should be the same as those used for the saprc07tic_ae6_aq and saprc07tic_ae7_aq mechanisms. The difference between the above two versions is that the later uses different speciation profiles for fire and non-fire emissions. The CMAQ team recommends using CMAQ_Control_DESID_saprc07tic_2DVBS_aq_Base.nml, the version used at Tsignhua for detailed studies to date, by default if possible. Please refer to the comments in this namelist for the requirements on emission inputs.

References:
Zhao, B., Wang, S. X., Donahue, N. M., Jathar, S. H., Huang, X. F., Wu, W. J., Hao, J. M., and Robinson, A. L.: Quantifying the effect of organic aerosol aging and intermediate-volatility emissions on regional-scale aerosol pollution in China, Scientific Reports, 6, 28815, DOI 10.1038/srep28815, 2016.
Zhao, B., Wang, S. X., Donahue, N. M., Chuang, W., Hildebrandt Ruiz, L., Ng, N. L., Wang, Y. J., and Hao, J. M.: Evaluation of one-dimensional and two-dimensional volatility basis sets in simulating the aging of secondary organic aerosols with smog-chamber experiments, Environmental Science & Technology, 49 (4), 2245-2254, DOI 10.1021/es5048914, 2015.
Chang, X., Zhao, B., Zheng, H. T., Wang, S. X., Cai, S. Y., Guo, F. Q., Gui, P., Huang, G. H., Wu, D., Han, L. C., Xing, J., Man, H. Y., Hu, R. L., Liang, C. R., Xu, Q. C., Qiu, X. H., Ding, D., Liu, K. Y., Robinson, A. L., Donahue, N. M.: Full-volatility emission framework corrects missing and underestimated secondary organic aerosol sources, One Earth, 5(4), 403-412, DOI 10.1016/j.oneear.2022.03.015, 2022.

C  Revision History:
C     6/1/2015: First version was coded by Bin Zhao (Tsinghua University) in CMAQ version 5.0.1.
C     9/1/2021: Migrated to CMAQ version 5.3.2 by Xing Chang (Tsinghua University).
C     7/21/2022: Migrated to CMAQ version 5.4 by Bin Zhao (Tsinghua University), Xing Chang (Tsinghua University), and Ben Murphy (U.S. EPA), which was released as part of the CMAQ public repository.

