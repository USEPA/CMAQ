# SOA bisection upper bound

**Author/P.O.C.:**, [Havala Pye](mailto:pye.havala@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

The upper bound on the root of the equation solved in SOA_DEFN.F (equation 19 in Pye et al., 2017) by the bisection method was adjusted. The upper bound corresponds to all semivolatile organic moles and nonvolatile organic moles in the particle.
In the event of cold temperatures, the saturation concentrations may be very low, indicating that all semivolatiles are in the particle and the upper bound is the correct root of the equation. Due to calculations in single precision,
it is possible for the function to not change sign if the upper bound is slightly below the total aerosol moles. This fix increases the upper bound a tiny amount to make sure the function changes sign
in the interval between the lower bound (only nonvolatile moles) and upper bound (all moles).

## Significance and Impact

If the code is not able to find the root, it stops and prints a message. That situation should no longer be encountered due to numerical issues. No effect on concentrations predicted.

## Affected Files

aero/aero6/SOA_DEFN.F

## References

Pye, H. O. T., Murphy, B. N., Xu, L., Ng, N. L., Carlton, A. G., Guo, H., Weber, R., Vasilakos, P., Appel, K. W., Budisulistiorini, S. H., Surratt, J. D., Nenes, A., Hu, W., Jimenez, J. L., Isaacman-VanWertz, G., Misztal, P. K., and Goldstein, A. H.: On the implications of aerosol liquid water and phase separation for organic aerosol mass, Atmos. Chem. Phys., 17, 343-369, doi:10.5194/acp-17-343-2017, 2017. [link](http://www.atmos-chem-phys.net/17/343/2017/)

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #118](https://github.com/usepa/cmaq_dev/pull/118) superceeds  
[PR #111](https://github.com/usepa/cmaq_dev/pull/111)


#### Commit IDs:
dcc376663aa4224392353ef4e55c784aff272191  
144c002bf1fcfb60ba3ed44afa284b0ab34ab343

-----
