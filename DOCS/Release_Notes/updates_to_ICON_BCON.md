# Updates to the ICON/BCON Preprocessors

[Shawn Roselle](mailto:roselle.shawn@epa.gov), [Christian
Hogrefe](mailto:hogrefe.christian@epa.gov), [Fahim Sidi](mailto:sidi.fahim@epa.gov), and [Kristen
Foley](mailto:foley.kristen@epa.gov), U.S. Environmental Protection Agency

## Brief Description

Several bug fixes have been addressed in the ICON and BCON updates.  This
includes: 
1. compiler info issues in the scripts for ICON/BCON
2. ICON/BCON compilation problems in v5.3 resulting from changes in files
symbolicly linked to the CCTM/src

Several enhancements are also included in the ICON/BCON updates: support for
nesting from polar stereographic projection grids; support for nesting from time
independent gridded files; support vertical interpolation by sigma, Z, and PRES;
support for time interpolation from gridded files with coarse time resolution.

The preprocessors have also been simplified by:
1. enabling selection of IC/BC type (regrid, profile) as a runtime option instead
of a compile-time option
2. removal of the species mapping; users will need to use the *combine* 
utility for species mapping
3. removal of parallel processing code from ICON (feature no longer used/needed)
4. reducing the memory requirements for ICON (by loop redesign and dynamic array
allocation)

The updates also include a new profile dataset, developed from an annual average
HCMAQ simulation for a marine remote grid cell over the Pacific Ocean (latitude
37<sup>o</sup>N, longitude -157<sup>o</sup>W) from a 2016 HCMAQ CMAQv5.3
*CB6R3M_AE7_KMTBR* simulation.

## Significance and Impact

ICON and BCON can now be used to generate ICs/BCs from HCMAQ simulations, using
vertical and horizontal interpolation methods available in ICON and BCON.

The profile data being release with CMAQv5.3 is different from previous model
releases.  The profile data included in this release is based on a HCMAQ
simulation that used chemical mechanism *CB6R3M_AE7_KMTBR*. ICON and BCON will no
longer map profile data species to different chemical mechanisms. Users will
need to use the *combine* utility on ICON/BCON output for species mapping to a
different mechanism.


## Affected Files

#### Files modified:

PREP/bcon/scripts/bldit_bcon.csh  
PREP/bcon/scripts/run_bcon.csh  
PREP/bcon/src/common/BC_PARMS.F  
PREP/bcon/src/common/HGRD_DEFN.F  
PREP/bcon/src/common/VGRD_DEFN.F  
PREP/bcon/src/common/bcon.F  
PREP/bcon/src/common/lat_lon.F  
PREP/bcon/src/common/lr_interp.F  
PREP/bcon/src/common/opn_bc_file.F  
PREP/bcon/src/m3conc/m3_bcout.F  
PREP/bcon/src/m3conc/m3_ck_bdy.F  
PREP/bcon/src/m3conc/m3_ck_ctms.F  
PREP/bcon/src/m3conc/m3_ck_met.F  
PREP/bcon/src/m3conc/m3_driver.F  
PREP/bcon/src/m3conc/m3_vinterp.F  
PREP/bcon/src/profile/prof_bcout.F  
PREP/bcon/src/profile/prof_driver.F  
PREP/bcon/src/profile/prof_vinterp.F  
PREP/bcon/src/tracer/trac_bc.F  
PREP/bcon/src/tracer/trac_driver.F  
PREP/icon/scripts/bldit_icon.csh  
PREP/icon/scripts/run_icon.csh  
PREP/icon/src/common/HGRD_DEFN.F  
PREP/icon/src/common/IC_PARMS.F  
PREP/icon/src/common/VGRD_DEFN.F  
PREP/icon/src/common/icon.F  
PREP/icon/src/common/lat_lon.F  
PREP/icon/src/common/lr_interp.F  
PREP/icon/src/common/opn_ic_file.F  
PREP/icon/src/m3conc/m3_ck_ctmmet.F  
PREP/icon/src/m3conc/m3_ck_ctms.F  
PREP/icon/src/m3conc/m3_ck_icmet.F  
PREP/icon/src/m3conc/m3_driver.F  
PREP/icon/src/m3conc/m3_icout.F  
PREP/icon/src/m3conc/m3_vinterp.F  
PREP/icon/src/profile/prof_driver.F  
PREP/icon/src/profile/prof_icout.F  
PREP/icon/src/profile/prof_vinterp.F  
PREP/icon/src/tracer/trac_driver.F  
PREP/icon/src/tracer/trac_ic.F  

#### Files added:


PREP/bcon/README.md  
PREP/bcon/src/profile/avprofile_cb6r3maero7_kmtbr_hemi2016_v53beta2_m3dry_col051_row068.csv  
PREP/bcon/src/profile/avprofile_racm_ae6_aq_derived_from_cb6r3m_ae7_kmtbr_hemi2016_v53beta2_m3dry_col051_row068.csv
PREP/bcon/src/profile/avprofile_saprc07tc_ae6_aq_derived_from_cb6r3m_ae7_kmtbr_hemi2016_v53beta2_m3dry_col051_row068.csv
PREP/bcon/src/profile/avprofile_saprc07tic_ae7i_aq_derived_from_cb6r3m_ae7_kmtbr_hemi2016_v53beta2_m3dry_col051_row068.csv
PREP/bcon/src/profile/legacy_PM_toxics_profile.csv  
PREP/icon/README.md  
PREP/icon/src/profile/avprofile_cb6r3maero7_kmtbr_hemi2016_v53beta2_m3dry_col051_row068.csv  
PREP/icon/src/profile/avprofile_racm_ae6_aq_derived_from_cb6r3m_ae7_kmtbr_hemi2016_v53beta2_m3dry_col051_row068.csv
PREP/icon/src/profile/avprofile_saprc07tc_ae6_aq_derived_from_cb6r3m_ae7_kmtbr_hemi2016_v53beta2_m3dry_col051_row068.csv
PREP/icon/src/profile/avprofile_saprc07tic_ae7i_aq_derived_from_cb6r3m_ae7_kmtbr_hemi2016_v53beta2_m3dry_col051_row068.csv
PREP/icon/src/profile/legacy_PM_toxics_profile.csv  

#### Files deleted:

PREP/bcon/src/common/CGRID_SPCS.F  
PREP/bcon/src/common/UTILIO_DEFN.F  
PREP/bcon/src/common/gc_spc_map.F  
PREP/bcon/src/common/get_envlist.f  
PREP/bcon/src/common/lst_spc_map.F  
PREP/bcon/src/common/ngc_spc_map.F  
PREP/bcon/src/mech_conv/Common_Codes  
PREP/bcon/src/mech_conv/bc_profile_RADM2_AERO5.dat  
PREP/bcon/src/mech_conv/convdat.F  
PREP/bcon/src/mech_conv/radm2_to_cb05/Makefile  
PREP/bcon/src/mech_conv/radm2_to_cb05/cb05_table.f  
PREP/bcon/src/mech_conv/radm2_to_cb05/mech_table.ext  
PREP/bcon/src/mech_conv/radm2_to_cb05/out_fl_name.ext  
PREP/bcon/src/mech_conv/radm2_to_racm2/Makefile  
PREP/bcon/src/mech_conv/radm2_to_racm2/mech_table.ext  
PREP/bcon/src/mech_conv/radm2_to_racm2/out_fl_name.ext  
PREP/bcon/src/mech_conv/radm2_to_racm2/racm2_table.f  
PREP/bcon/src/mech_conv/radm2_to_saprc07t/Makefile  
PREP/bcon/src/mech_conv/radm2_to_saprc07t/mech_table.ext  
PREP/bcon/src/mech_conv/radm2_to_saprc07t/out_fl_name.ext  
PREP/bcon/src/mech_conv/radm2_to_saprc07t/saprc07t_table.f  
PREP/bcon/src/mech_conv/radm2_to_saprc99/Makefile  
PREP/bcon/src/mech_conv/radm2_to_saprc99/mech_table.ext  
PREP/bcon/src/mech_conv/radm2_to_saprc99/out_fl_name.ext  
PREP/bcon/src/mech_conv/radm2_to_saprc99/saprc99_table.f  
PREP/bcon/src/mech_conv/wrdate.f  
PREP/bcon/src/prof_data/cb05_ae6_aq/bc_profile_CB05.dat  
PREP/bcon/src/prof_data/racm2_ae6_aq/bc_profile_RACM2.dat  
PREP/bcon/src/prof_data/saprc07t_ae6_aq/bc_profile_SAPRC07T.dat  
PREP/bcon/src/prof_data/saprc99_ae6_aq/bc_profile_SAPRC99.dat  
PREP/bcon/src/profile/avprofile_cb6r3aero6_hemi2016_col051_row068.csv  
PREP/icon/src/common/CGRID_SPCS.F  
PREP/icon/src/common/UTILIO_DEFN.F  
PREP/icon/src/common/gc_spc_map.F  
PREP/icon/src/common/get_envlist.f  
PREP/icon/src/common/lst_spc_map.F  
PREP/icon/src/common/ngc_spc_map.F  
PREP/icon/src/common/setup_logdev.F  
PREP/icon/src/common/subhdomain.F  
PREP/icon/src/m3conc/mapping_init.F  
PREP/icon/src/mech_conv/Common_Codes  
PREP/icon/src/mech_conv/convdat.F  
PREP/icon/src/mech_conv/ic_profile_RADM2_AERO5.dat  
PREP/icon/src/mech_conv/radm2_to_cb05/Makefile  
PREP/icon/src/mech_conv/radm2_to_cb05/cb05_table.f  
PREP/icon/src/mech_conv/radm2_to_cb05/mech_table.ext  
PREP/icon/src/mech_conv/radm2_to_cb05/out_fl_name.ext  
PREP/icon/src/mech_conv/radm2_to_racm2/Makefile  
PREP/icon/src/mech_conv/radm2_to_racm2/conv.log  
PREP/icon/src/mech_conv/radm2_to_racm2/convdat.F  
PREP/icon/src/mech_conv/radm2_to_racm2/mech_table.ext  
PREP/icon/src/mech_conv/radm2_to_racm2/out_fl_name.ext  
PREP/icon/src/mech_conv/radm2_to_racm2/racm2_table.f  
PREP/icon/src/mech_conv/radm2_to_racm2/wrdate.f  
PREP/icon/src/mech_conv/radm2_to_saprc07t/Makefile  
PREP/icon/src/mech_conv/radm2_to_saprc07t/mech_table.ext  
PREP/icon/src/mech_conv/radm2_to_saprc07t/out_fl_name.ext  
PREP/icon/src/mech_conv/radm2_to_saprc07t/saprc07t_table.f  
PREP/icon/src/mech_conv/radm2_to_saprc99/Makefile  
PREP/icon/src/mech_conv/radm2_to_saprc99/mech_table.ext  
PREP/icon/src/mech_conv/radm2_to_saprc99/out_fl_name.ext  
PREP/icon/src/mech_conv/radm2_to_saprc99/saprc99_table.f  
PREP/icon/src/mech_conv/wrdate.f  
PREP/icon/src/par/distr_env.c  
PREP/icon/src/par/mpcomm_init.F  
PREP/icon/src/par/par_term.F  
PREP/icon/src/par/reconfig_domain.F  
PREP/icon/src/par/shift_map.F  
PREP/icon/src/prof_data/cb05_ae6_aq/ic_profile_CB05.dat  
PREP/icon/src/prof_data/racm2_ae6_aq/ic_profile_RACM2.dat  
PREP/icon/src/prof_data/saprc07t_ae6_aq/ic_profile_SAPRC07T.dat  
PREP/icon/src/prof_data/saprc99_ae6_aq/ic_profile_SAPRC99.dat  


## References

NA           

-----

## Internal Records:

#### Relevant Pull Requests:

[PR #423](https://github.com/USEPA/CMAQ_Dev/pull/423)  
[PR #448](https://github.com/USEPA/CMAQ_Dev/pull/468)  
[PR #498](https://github.com/USEPA/CMAQ_Dev/pull/498)  

#### Commit IDs:

[3a5cd7154d95afdfe1f484a2129ba2597b2e9d9c](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/3a5cd7154d95afdfe1f484a2129ba2597b2e9d9c)  
[ae51bdb1fe6bcfd96ecba54ac03828753223ca0a](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/ae51bdb1fe6bcfd96ecba54ac03828753223ca0a)  
[95e3fbabe3006860eae1991cd11a5d72e8dc0402](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/95e3fbabe3006860eae1991cd11a5d72e8dc0402)  
[211149949f12c4bcd17d90ed36eee2ce7ec565e9](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/211149949f12c4bcd17d90ed36eee2ce7ec565e9)  
[d8118ef125d922efef22ce38b4b16596050ebb0d](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/d8118ef125d922efef22ce38b4b16596050ebb0d)  
[f53e013357702b8a638b30fb356d4b01244e143c](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/f53e013357702b8a638b30fb356d4b01244e143c)  
[6a8f1468e56101e14c54b43299093b5aa31c8289](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/6a8f1468e56101e14c54b43299093b5aa31c8289)  
[e7d5dc0daaa25b58b0b85ab04130a645179411f3](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/e7d5dc0daaa25b58b0b85ab04130a645179411f3)  
[0a8eb0576c0c9d7a4850472c3306818fb20ada9d](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/0a8eb0576c0c9d7a4850472c3306818fb20ada9d)  
[a63e1e8a639dc8f30a2eda72e27bffaa01be11aa](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/a63e1e8a639dc8f30a2eda72e27bffaa01be11aa)  
[86f2aeedd9bf482e8ce40c1041be8bac230b7ab6](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/86f2aeedd9bf482e8ce40c1041be8bac230b7ab6)  
[f44518953e6a919841ff2d06b7974248b32ab4c9](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/f44518953e6a919841ff2d06b7974248b32ab4c9)  
[f4c66ade91efff224d6f1fbeccb4fd04b2936b23](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/f4c66ade91efff224d6f1fbeccb4fd04b2936b23)  
[a7643a1e931ba52a691a5d21362bb2289009ae02](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/a7643a1e931ba52a691a5d21362bb2289009ae02)  
[1edb101cce965e07ac54dac9640990475fea641c](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/1edb101cce965e07ac54dac9640990475fea641c)  
[9472bf74cb927fa79484124d55af8012f890410f](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/9472bf74cb927fa79484124d55af8012f890410f)  
[fe14f24202101ad39b0e2cbdf5fd160bfab173e5](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/fe14f24202101ad39b0e2cbdf5fd160bfab173e5)  
[e3ba74c4df5c861de02929c794e15f944a62fc59](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/e3ba74c4df5c861de02929c794e15f944a62fc59)  
[e8c262b045b6d30c9e01d667c5e197ab9f857fbd](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/e8c262b045b6d30c9e01d667c5e197ab9f857fbd)  
[18c6c9cd21bd86e0581a27966f8b1d8bf06d9fd4](https://github.com/USEPA/CMAQ_Dev/pull/468/commits/18c6c9cd21bd86e0581a27966f8b1d8bf06d9fd4)  
[7303473e2c6476d00dec125aa7d392fc92510c64](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/7303473e2c6476d00dec125aa7d392fc92510c64)  
[664ec51cbe72fa705e12a3af3dce5dba097ed408](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/664ec51cbe72fa705e12a3af3dce5dba097ed408)  
[98fd974797a254671c8a35cfcb66f6fd3d527ac4](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/98fd974797a254671c8a35cfcb66f6fd3d527ac4)  
[cc07ddadedba3977c3d10cf4770280126cfde4c4](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/cc07ddadedba3977c3d10cf4770280126cfde4c4)  
[e83a983b52ae36205393159c61fccb91a4e9c7a4](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/e83a983b52ae36205393159c61fccb91a4e9c7a4)  
[5895455c54ba39d9ef62e0f12050e84c7c72af1b](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/5895455c54ba39d9ef62e0f12050e84c7c72af1b)  
[694c2ab5c2a7ea3fc4fe70e0d6d2c497dc82ab24](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/694c2ab5c2a7ea3fc4fe70e0d6d2c497dc82ab24)  
[ec827a2c42df84bf99de531bd4a5f184f869a834](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/ec827a2c42df84bf99de531bd4a5f184f869a834)  
[1e56ad8c796d9220aec43b13a4859a0cb6d67e0e](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/1e56ad8c796d9220aec43b13a4859a0cb6d67e0e)  
[be049552a412d8c436a1c01d943101e8162263ef](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/be049552a412d8c436a1c01d943101e8162263ef)  
[4ad29ef98276003d579d6eead108fff082f639c8](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/4ad29ef98276003d579d6eead108fff082f639c8)  
[e390418a747cc7319e0b36c9b104dd79800301e4](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/e390418a747cc7319e0b36c9b104dd79800301e4)  
[d7764e82909210e9c09b6b487e2b585689d0b4b5](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/d7764e82909210e9c09b6b487e2b585689d0b4b5)  
[f45c7407c5168122f2dad800203a239e3a65c77b](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/f45c7407c5168122f2dad800203a239e3a65c77b)  
[89825a9efda776486fe5d20cfb62aa8361126ece](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/89825a9efda776486fe5d20cfb62aa8361126ece)  
[c2e8d8649b5808059809f9d993317801d41a68ab](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/c2e8d8649b5808059809f9d993317801d41a68ab)  
[68dec42205ee2d3ee83b072bd628003995bc54e4](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/68dec42205ee2d3ee83b072bd628003995bc54e4)  
[b2304c5c7354f57fd3b116b9734e4f546d5aee16](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/b2304c5c7354f57fd3b116b9734e4f546d5aee16)  
[8f87d20540fd5e76e31f7007064fa252ee7a3064](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/8f87d20540fd5e76e31f7007064fa252ee7a3064)  
[bab82834e82f4ada9754758c7312cfdc2c12cbcc](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/bab82834e82f4ada9754758c7312cfdc2c12cbcc)  
[bdbd124ba3a088ad5a755a5a638e0f9a9ef0acea](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/bdbd124ba3a088ad5a755a5a638e0f9a9ef0acea)  
[abd0f64f8af2f602033a4b386cec8e831d570168](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/abd0f64f8af2f602033a4b386cec8e831d570168)  
[01dc9d9ca125350a23c0dac6e52f7adf4b889a75](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/01dc9d9ca125350a23c0dac6e52f7adf4b889a75)  
[85a38860169c6e5b0b26cf24507615fd9edf369e](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/85a38860169c6e5b0b26cf24507615fd9edf369e)  
[67ed955129650875d158d66808dfeab91fb9c9c6](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/67ed955129650875d158d66808dfeab91fb9c9c6)  
[d15f5cf34615fb1f04776b5031beb42a893b776b](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/d15f5cf34615fb1f04776b5031beb42a893b776b)  
[e660c04f23b0e80edab67cdc3c181e64ff7ed4e5](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/e660c04f23b0e80edab67cdc3c181e64ff7ed4e5)  
[2412892f902183774d8a09d5b1e91d8c1543cc77](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/2412892f902183774d8a09d5b1e91d8c1543cc77)  
[fd7b7a12180059aca963cbf6286f7be2802a2241](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/fd7b7a12180059aca963cbf6286f7be2802a2241)  
[5929957039de6636773b8c0c2baaa3dc592bac8d](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/5929957039de6636773b8c0c2baaa3dc592bac8d)  
[ce1c592ac5100aa6bf255ebe7893bdf502cc2e75](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/ce1c592ac5100aa6bf255ebe7893bdf502cc2e75)  
[ec8c805dee43a0c8c28c839de9c38137d9d73fa5](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/ec8c805dee43a0c8c28c839de9c38137d9d73fa5)  
[346d3902322fbdbae5ea5532d61990d623c380f3](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/346d3902322fbdbae5ea5532d61990d623c380f3)  
[6ffd6d34c4d524335207bf919db5c4827bba2acd](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/6ffd6d34c4d524335207bf919db5c4827bba2acd)  
[7dffa5d380695853344c54163d970799bdd80e8b](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/7dffa5d380695853344c54163d970799bdd80e8b)  
[364f638c198f27d9085b4f95cb447e0c3d2f4dfc](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/364f638c198f27d9085b4f95cb447e0c3d2f4dfc)  
[8aedad67e49706c510c9a7cad733a7c9c6a70f0a](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/8aedad67e49706c510c9a7cad733a7c9c6a70f0a)  
[90baa5094efcc24eae36ba213a8aa899455c20be](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/90baa5094efcc24eae36ba213a8aa899455c20be)  
[60da3adeafe0ce557dbe35f5be81e1d7921ae076](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/60da3adeafe0ce557dbe35f5be81e1d7921ae076)  
[7d7786105bf437704cf0d7e10b6a312493ab6d50](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/7d7786105bf437704cf0d7e10b6a312493ab6d50)  
[95f05c03b29e9340ac860db934121965af1bda3c](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/7d7786105bf437704cf0d7e10b6a312493ab6d50)  
[d541855e4b3384d48f9bb1ee9d5c3add18f8ed50](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/d541855e4b3384d48f9bb1ee9d5c3add18f8ed50)  
[e87b6aba6164987e33679d01213d21541fc2933d](https://github.com/USEPA/CMAQ_Dev/pull/498/commits/e87b6aba6164987e33679d01213d21541fc2933d)  

-----
