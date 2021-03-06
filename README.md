# DEPP - Differential Evolution Parallel Program 

[![DOI](https://joss.theoj.org/papers/10.21105/joss.01701/status.svg)](https://doi.org/10.21105/joss.01701)

DEPP is an open-source optimization software based on Differential Evolution heuristic. It was designed to solve non-linear optimization problems of real-valued functions (objective functions) constrained to boxed domains. Additionally, this software is gradient-free, it is not code-invasive, i.e., objective function is treated as a "black-box", it is tolerant to failures, to poor precision calculated or noisy objective functions, it uses parallelization, that is particularly useful for handling computationally expensive objective functions, and it allows a Response Surface Methodology-Differential Evolution hybridization for convergence acceleration. Code is written using standard Fortran 2008 and applies the object-oriented paradigm, so that new differential evolution and/or hybridization strategies may be implemented more easily.

For details, please, see the documentation page: https://github.com/gbertoldo/DEPP/wiki

# VERSION

Version 1.0.3
