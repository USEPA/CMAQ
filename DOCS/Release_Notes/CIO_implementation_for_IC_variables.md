#  An implementation to enhance IC variable handling in centralized I/O (cio)

[David Wong](mailto:wong.david-c@epa.gov), U.S. Environmental Protection Agency

## Brief Description

This implementation within CMAQ's centralized I/O (cio) utilizes the fact that initial conidtion (IC) variables are read in once to enhance code efficiency. Two types of calculation which are applying to non-IC variable: checking a requested time falls within the time stamps of data stored within the circular buffer,  and computing interpolation ratios, are omitted.

## Significance and Impact

It does not change any simulation result.

## Files Affected
* cio/centralized_io_module.F

## Internal Records
#### Relevant Pull Requests:
[PR #613](https://github.com/usepa/cmaq_dev/pull/613)

#### Commit IDs:
ef3c16d3e0e1922c738d2c79b41724fc7f0ca7bc
