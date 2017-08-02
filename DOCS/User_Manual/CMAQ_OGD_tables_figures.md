<!-- BEGIN COMMENT -->

[Home](README.md)

<!-- END COMMENT -->

CMAQ Operational Guidance document
==
List of Figures
--

[Figure 4‑1. CMAQ Modeling Framework](CMAQ_OGD_ch04_science.md#Figure4-1)

[Figure 4-2. CMAQ Chemistry-Transport Model (CCTM) and input processors](CMAQ_OGD_ch04_science.md#Figure4-2)

[Figure 4-3. Meteorology preprocessing for CMAQ with MCIP](CMAQ_OGD_ch04_science.md#Figure4-3)

[Figure 4-4. Initial and boundary conditions preprocessing for CMAQ](CMAQ_OGD_ch04_science.md#Figure4-4)

[Figure 4-5 Crop calendar data preprocessor for the CCTM](CMAQ_OGD_ch04_science.md#Figure4-5)

[Figure 4-6. CMAQ chemistry-transport model and associated preprocessors](CMAQ_OGD_ch04_science.md#Figure4-6)

[Figure 4-7. Data flow between EPIC, the meteorological model, and CMAQ from Cooter et al. (2012)](CMAQ_OGD_ch04_science.md#Figure4-7)

[Figure 7‑1.CMAQ core programs](CMAQ_OGD_ch07_programs_libraries.md#Figure7-1)

[Figure 7‑2. BCON input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-2)

[Figure 7‑3. Calmap input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-3)

[Figure 7‑4. CCTM input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-4)

[Figure 7‑5. CHEMMECH and CSV2NML input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-5)

[Figure 7‑6. ICON input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-6)

[Figure 7‑7. JPROC input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-7)

[Figure 7‑8. MCIP input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-8)

[Figure 9-1. CMAQ Horizontal grid Convention](CMAQ_OGD_ch09_grid_defn.md#Figure9-1)

[Figure 9-2. CMAQ benchmark grid](CMAQ_OGD_ch09_grid_defn.md#Figure9-2)

[Figure 10-1. 36-km four-day modeling period IC/BC schematic](CMAQ_OGD_ch10_new_simulation.md#Figure10-1)

[Figure 10-2. 12-km nested two-day modeling period IC/BC schematic](CMAQ_OGD_ch10_new_simulation.md#Figure10-2)

**List of Tables**

[Table 5‑1. Software required for running CMAQ](CMAQ_OGD_ch05_sys_req.md#Table5-1)

[Table 5‑2. Optional support software for CMAQ](CMAQ_OGD_ch05_sys_req.md#Table5-2)

[Table 5‑3. NetCDF and I/O API compilation options for CMAQ](CMAQ_OGD_ch05_sys_req.md#Table5-3)

[Table 5‑4. Config_cmaq.csh Configuration Variables](CMAQ_OGD_ch05_sys_req.md#Table5-4)

[Table 6-1. Possible Time Step Structures in I/O API Files](CMAQ_OGD_ch06_req_lib.md#Table6-1)

[Table 6-2. Possible Data Type Structures in I/O API Files](CMAQ_OGD_ch06_req_lib.md#Table6-2)

[Table 6-3. Possible values for OPEN(3) FSTATUS](CMAQ_OGD_ch06_req_lib.md#Table6-3)

[Table 6-4. IO API data retrieval routines](CMAQ_OGD_ch06_req_lib.md#Table6-4)

[Table 6-5. I/O API data manipulation utilities](CMAQ_OGD_ch06_req_lib.md#Table6-5)

[Table 7‑1. BCON input files](CMAQ_OGD_ch07_programs_libraries.md#bcon-input-files)

[Table 7‑2. BCON output files](CMAQ_OGD_ch07_programs_libraries.md#Table7-2)

[Table 7‑3. Calmap input files](CMAQ_OGD_ch07_programs_libraries.md#calmap-input-files)

[Table 7‑4. Calmap output files](CMAQ_OGD_ch07_programs_libraries.md#calmap-output-files)

[Table 7‑5. Required CCTM input files](CMAQ_OGD_ch07_programs_libraries.md#cctm-input-files)

[Table 7‑6. Optional CCTM input files](CMAQ_OGD_ch07_programs_libraries.md#Table7-6)

[Table 7‑7. CCTM base output files](CMAQ_OGD_ch07_programs_libraries.md#Table7-7)

[Table 7‑8. CCTM optional output files](CMAQ_OGD_ch07_programs_libraries.md#Table7-8)

[Table 7‑9. CHEMMECH input files](CMAQ_OGD_ch07_programs_libraries.md#Table7-9)

[Table 7‑10. CHEMMECH output files](CMAQ_OGD_ch07_programs_libraries.md#Table7-10)

[Table 7‑11. CSV2NML input files](CMAQ_OGD_ch07_programs_libraries.md#Table7-11)

[Table 7‑12. CSV2NML output files](CMAQ_OGD_ch07_programs_libraries.md#Table7-12)

[Table 7-13. CREATE_EBI input files](CMAQ_OGD_ch07_programs_libraries.md#create_ebi-input-files)

[Table 7-14. CREATE_EPI output files](CMAQ_OGD_ch07_programs_libraries.md#create_ebi-output-files)

[Table 7‑15. ICON input files](CMAQ_OGD_ch07_programs_libraries.md#icon-input-files)

[Table 7‑14. ICON output files](CMAQ_OGD_ch07_programs_libraries.md#create_ebi-output-files)

[Table 7-17. INLINE_PHOT_PREPROC input files](CMAQ_OGD_ch07_programs_libraries.md#inline_phot_preproc-input-files)

[Table 7‑18. INLINE_PHOT_PREPROC output files](CMAQ_OGD_ch07_programs_libraries.md#inline_phot_preproc-output-files)

[Table 7‑19. JPROC input files](CMAQ_OGD_ch07_programs_libraries.md#jproc-input-files)

[Table 7‑20. JPROC output files](CMAQ_OGD_ch07_programs_libraries.md#jproc-output-files)

[Table 7‑21. MCIP input files](CMAQ_OGD_ch07_programs_libraries.md#mcip-input-files)

[Table 7‑22. MCIP output files](CMAQ_OGD_ch07_programs_libraries.md#mcip-output-files)

[Table 8‑1. CMAQ input files](CMAQ_OGD_ch08_input_files.md#Table8-1)

[Table 8‑2. Coordinate sytem description segment of GRIDDESC](CMAQ_OGD_ch08_input_files.md#Table8-2)

[Table 8‑3. Grid definition segment of GRIDDESC](CMAQ_OGD_ch08_input_files.md#Table8-3)

[Table 8‑4. GC species namelist file format](CMAQ_OGD_ch08_input_files.md#Table8-4)

[Table 8-5. IC_PROFILE format description](CMAQ_OGD_ch08_input_files.md#Table8-5)

[Table 8-6. BC_PROFILE format description](CMAQ_OGD_ch08_input_files.md#Table8-6)

[Table 8-7. CSQY format description](CMAQ_OGD_ch08_input_files.md#Table8-7)

[Table 8-8 ET file format description](CMAQ_OGD_ch08_input_files.md#Table8-8)

[Table 8-9. PROFILES file format description](CMAQ_OGD_ch08_input_files.md#Table8-9)

[Table 8-10 TOMS Data Profile](CMAQ_OGD_ch08_input_files.md#Table8-10)

[Table 8-11. JTABLE file format description](CMAQ_OGD_ch08_input_files.md#Table8-11)

[Table 8‑12. OMI data format](CMAQ_OGD_ch08_input_files.md#Table8-12)

<!-- BEGIN COMMENT -->

[Home](README.md)

<!-- END COMMENT -->
