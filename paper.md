---
title: 'DEPP - Differential Evolution Parallel Program'
tags:
  - Optimization
  - Differential Evolution
  - MPI
  - Fortran 2008
authors:
  - name: Jonas Joacri Radtke
    orcid: 0000-0002-5930-8371
    affiliation: 1
  - name: Guilherme Bertoldo
    orcid: 0000-0002-1888-9181
    affiliation: 1
  - name: Carlos Henrique Marchi
    orcid: 0000-0002-1195-5377
    affiliation: 2
affiliations:
 - name: Federal University of Technology - Paraná
   index: 1
 - name: Federal University of Paraná
   index: 2
date: 2 July 2019
bibliography: paper.bib
---

# Summary

Optimization is a mathematical problem often found in science and engineering. Currently, however, there is no general method to face this problem. Solutions are generally addressed by two approaches, both iterative: (a) quasi-Newton methods `[@Griva:2009]` and (b) heuristic methods `[@Coley:1999; @Feoktistov:2006]`. Each one has advantages depending on the problem to be optimized. Quasi-Newton methods, in general, converge faster then heuristic methods, provided the function to be optimized (the objective function) is smooth. Heuristic methods, on the other hand, are more appropriate to deal with noisy objective functions, to handle failures in the calculation of the objetive function and are less susceptible to be retained in local optimum than quasi-Newton methods. In order to combine the robustness of heuristic methods with the high convergence speed of quasi-Newton methods, Loris Vincenzi and Marco Savoia `[@Vincenzi:2015]` proposed coupling Differential Evolution heuristic with Response Surfaces. Fitting Response Surfaces during optimization and finding their optima mimics quasi-Newton methods. Authors showed that this approach reduced significantly the effort to reach the solution within a given tolerance (in general, more than 50% compared to the original heuristic method). Based on the paper of Loris Vincenzi and Marco Savoia, but applying a different algorithm`[@Radtke:2019b]`, a software called ``DEPP``, an achronym for Differential Evolution Parallel Program, was elaborated. 

``DEPP`` source code `[@Radtke:2019a]` is written in Fortran 2008 standard, it is based on the object-oriented paradigm and it includes MPI paralellization. The main algorithm was elaborated to simplify the development and extension of the code, relying on abstract classes and polymorphism. Following this idea, design patterns `[@Freeman:2004]` were also applied. Object instances are generated through the Factory Design Pattern and Adapter Design Pattern was applied to encapsulate MPI commands, for instance. In this way, users may implement new optimization methods without changing the main algorithm. 

Due to its supporting theory, ``DEPP`` is well suited to address optimization problems of multimodal, noisy, poor-precision-calculated and failure-susceptible objetive functions, taking advantage of acceleration provided by parallelization and hybridization models, like Differential Evolution-Response Surface coupling.  

# References
