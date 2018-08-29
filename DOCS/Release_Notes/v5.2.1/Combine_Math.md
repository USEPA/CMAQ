# Improve the mathematical capabilities of the Combine post-processing utility  

**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA  

## Brief Description

The pull request makes how combine evaluates species formula obey mathematical rule of operations, i.e., powers first, multiple/divide next, and add/subtract last. The exception is when the formula includes an conditional operator which has the highest rank in the order operations. Examples follow. When A and X equal 3, the current version of combine incorrectly evaluates A\*-X, A\*-X^2+A\*X^-2, A\*X^3, and -A\*X^3. The update version removes these errors.

Changes also supplemented the functions available for creating a formula while added statements in the source code to prevent floating errors when using these functions. Note that FAVEG, FMAX, FMIN and FSTDEV functions are not new but were not previously documented. The READ file was updated with table describing both new and old functions.

## Significance and Impact

The changes attempt to increase the ease and flexibility at creating formula in species definition files. 

## Affected Files:

POST/combine/src/module_specdef.F  
POST/combine/src/utils.F  

## References:    

-----
## Internal Records:


### Relevant Pull Requests:
  [PR #257](https://github.com/USEPA/CMAQ_Dev/pull/257)

### Commit IDs:

58385e7736c28d28a58e381fa331760cf50df924  
562090a4315de6da4c7bded13114e796524d3d08  
34657db683d1de7af039fadf74f6cabdd5c8b2fd  
d5460c9cfaec40baa0f8b6bd4404e08764d52a3e  
d20550b557eddb9e0e76987b61dae65e94daa4c5  
a2559200c71ed66cb083d1303aa44890c313f34c  
95b751e218a272cff3bd34c1f55c81c4ec58e500  
