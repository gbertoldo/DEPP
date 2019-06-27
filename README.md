# DEPP - Differential Evolution Parallel Program
DEPP is an open source optimization software based on the Differential Evolution heuristic. It was designed to solve non-
linear optimization problems of a real valued function (objective function) constrained to a boxed domain. Additionally, this software is gradient-free, it is not code invasive, i.e., objective function is treated as a "black-box", it is tolerant to failures, to poor precision calculated or noisy objective functions, it uses parallelization, that is particularly useful for handling computationally expensive objective functions and it allows a Response Surface Methodology-Differential Evolution hybridization for convergence acceleration. Code is written in standard Fortran 2008 and applies the object-oriented paradigm, so that new differential evolution and/or hybridization strategies can be implemented more easily.

For details, please, see the documentation page: https://github.com/gbertoldo/DEPP/wiki
