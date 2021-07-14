# HONO Deposition Fix for the STAGE Deposition Option

## Brief Description
The HONO dry deposition flux in CMAQ v5.3 to v5.3.2 can be negative when surface heterogeneous production exceeds deposition. Similar negative values are found in both m3dry and STAGE and is documented in issue #724 (HONO dry deposition flux for the M3DRY deposition option in the research branch). This pull request fix HONO dry deposition with the STAGE deposition option. Pull requests #744 and #755 address this issue for the M3Dry deposition option. 

## Significance and Impact

Testing was done for the 2016 benchmark and CONUS domains with the STAGE option for all combinations of NH3 and Hg bidirectional exchange and heterogeneous HONO production options and with ISAM simulations. Predicted HONO dry deposition with existing model can be negative while HONO dry deposition with the revised model is positive for all dry deposition, grid cell average and MOSAIC and ISAM outputs. Negative Hg dry deposition fluxes were removed from dry deposition, MOSAIC and ISAM outputs and negative NH3 dry deposition fluxes were removed from the MOSAIC deposition option. Two new variables were added to capture Hg emissions or heterogeneous HONO production, HG_Emis and HONO_Het respectively, if those options are selected in the run script. The diagnostic NH3 dry deposition flux in CMAQ v5.3 - v5.3.2 was replaced with the actual dry deposition flux resulting in up to a 2% change, more typically less than 1% for the CONUS domain, in the modeled deposition. Dry depositions of other chemical species and model concentrations are unaffected by the changes. 

Additionally, several large redundant 4D arrays in MOSAIC were removed and some variables in vdiff/acm2_stage were renamed to more accurately indicate the data that they contain. 

## Affected Files

CCTM/src/depv/stage/DEPV_DEFN.F
CCTM/src/depv/stage/HGSIM.F
CCTM/src/depv/stage/MOSAIC_MOD.F
CCTM/src/depv/stage/NH3_BIDI_MOD.F
CCTM/src/depv/stage/STAGE_DATA.F
CCTM/src/depv/stage/STAGE_MOD.F
CCTM/src/vdiff/acm2_stage/VDIFF_MAP.F
CCTM/src/vdiff/acm2_stage/opddep.F
CCTM/src/vdiff/acm2_stage/vdiffacmx.F
CCTM/src/vdiff/acm2_stage/vdiffproc.F

## References:
None

-----
## Internal Records
#### Relevant Pull Requests:

Operational Branch
[PR #740](https://github.com/usepa/cmaq_dev/pull/740)

Research Branch
[PR #741](https://github.com/usepa/cmaq_dev/pull/741) 

#### Commit IDs:

Operational Branch

4cf8bf482dcdb8bad93dae66cd7f9b339fb46fee
81b3a429746f58e5cdd680d3ec4a312e69bbb168
2f261161dcf6e93bccdaa400b16a7d9042380ccb
8241e460d0214448493b137acde4f619128c06f6
216e2d47f5962cc6f9cd5b8aac86e6e7792c5111
0983636d86b0d61da86ff22b2a8022f9981274d9

Research Branch

2e833d9812d8c1e8ac4ce8cfe39ed6a1688ec3d7
39a3c8a1d80f3c26b94fbef8e3dde4a26be4c7cf
7ba3bd4dbddf55bb544cc9951e3df89b1b21afa8
6562de1efda2a3539a0e2d43b5101f461032bae6
4ccec9f16e8123bb8ae34483e5964a98d7119526


