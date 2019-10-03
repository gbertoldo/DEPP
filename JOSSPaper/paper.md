---
title: 'DEPP - Differential Evolution Parallel Program'
tags:
  - Optimization
  - Differential Evolution
  - MPI
  - Fortran 2008
authors:
  - name: Jonas Joacir Radtke
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

Optimization is a mathematical problem often found in science and engineering. Currently, however, there is no general method to face this problem. Solutions are generally addressed by two approaches, both iterative: (a) quasi-Newton methods [@Griva:2009] and (b) heuristic methods [@Coley:1999; @Feoktistov:2006]. Each one has advantages depending on the problem to be optimized. Quasi-Newton methods, in general, converge faster then heuristic methods, provided the function to be optimized (the objective function) is smooth. Heuristic methods, on the other hand, are more appropriate to deal with noisy objective functions, to handle failures in the calculation of the objetive function and are less susceptible to be retained in local optimum than quasi-Newton methods. In order to combine the robustness of heuristic methods with the high convergence speed of quasi-Newton methods, Loris Vincenzi and Marco Savoia [@Vincenzi:2015] proposed coupling Differential Evolution heuristic with Response Surfaces. Fitting Response Surfaces during optimization and finding their optima mimics quasi-Newton methods. Authors showed that this approach reduced significantly the effort to reach the solution within a given tolerance (in general, more than 50% compared to the original heuristic method). Based on the paper of Loris Vincenzi and Marco Savoia, but applying a different algorithm[@Radtke:2019a], a software called ``DEPP``, an achronym for Differential Evolution Parallel Program, was elaborated. 

``DEPP`` source code [@Radtke:2019a] is written in Fortran 2008 standard, it is based on the object-oriented paradigm and it includes MPI paralellization. The main algorithm was elaborated to simplify the development and extension of the code, relying on abstract classes and polymorphism. Following this idea, design patterns [@Freeman:2004] were also applied. Object instances are generated through the Factory Design Pattern and Adapter Design Pattern was applied to encapsulate MPI commands, for instance. In this way, users may implement new optimization methods without changing the main algorithm. 

Due to its supporting theory, ``DEPP`` is well suited to address optimization problems of multimodal, noisy, poor-precision-calculated and failure-susceptible objetive functions, taking advantage of acceleration provided by parallelization and hybridization models, like Differential Evolution-Response Surface coupling.  

Software description {#sec:description}
====================

Software Architecture {#sec:arch}
---------------------

DEPP is written in FORTRAN 2008 standard language[@Brainerd2015] with
the Message Passing Interface (MPI) directives[@Tennessee2009] and takes
advantage of the Object-Oriented Paradigm.

The folder structure of DEPP is shown in Figure \[fig:depptree\]. The
directory *depp\_input* contains the input (text) files which defines
the control parameters of the optimization. Results of DEPP are saved in
the *depp\_output* directory. The source code is within *depp\_src*
directory. This directory also contains the Bash[@Ramey2016] script
*compile.sh*, which compiles DEPP source code and generates the
executable *depp.x* in the root of the file structure. The interface
between DEPP and the external program (EP), which calculates the
objective function, is defined in the *interface* directory. Finally,
the script *run.sh* runs DEPP using MPI.

![Folder structure of
DEPP.](folder_structure.png){width="50.00000%"}

[\[fig:depptree\]]{}

Thanks to the Object-Oriented Programming, the source code was designed
to simplify the implementation of new methods. The algorithm works on
abstract classes separated, basically, in (i) population initializers,
(ii) search strategies, (iii) fitness calculation and (iv) stop
conditions. The concrete classes are generated using the Factory Design
Pattern[@Freeman2004]. All MPI commands are encapsulated into proper
classes, following the logic of the Adapter Design Pattern. In this way,
users interested in the implementation of new methods will not concern
about MPI details.

Interfacing DEPP with an external program is exemplified in *interface*
directory. This folder contains three examples based on C++ and FORTRAN
2008 languages. Any programming language may be used for interfacing.

The basic algorithm of DEPP is illustrated in
Figure \[fig:deppalgorithm\]. DEPP reads the input data from
*depp\_input* directory (optionally, it may start from some backup). The
stop condition is analyzed. In the first iteration, the stop condition
is generally not satisfied (it may not be true when starting from a
backup). When stop condition is satisfied, the iterative procedure is
finished and the output data is saved in the *depp\_output* directory.
Otherwise, a trial population is generated. DEPP sends the information
to a set of threads (T1,$\cdots$, TN) using MPI. Then, each of these
threads runs a copy of the external program (EP) for a given individual
of the population. After that, DEPP receives the calculated objective
function and compares their fitness with the fitness of the current
population. Only the best individuals are held. A backup is optionally
generated and the iterative cycle is restarted. During the calculation
of the objective function, some failures may occur. DEPP handles
failures using an error code returned by the external program. If the
error code is 0, the calculations were performed correctly; if 1 is
returned, then a failure occurred and the trial individual is discarded;
if 2 is returned, then a failure occurred, the trial individual is
discarded and a new one is generated. All the failures are registered
for posterior user’s analysis.

![DEPP basic
algorithm.](algorithm1.png){width="50.00000%"}

[\[fig:deppalgorithm\]]{}

Supporting theory {#sec:theory}
-----------------

The following sections describe the optimization theory behind the
current implementation of DEPP. It must be emphasized that DEPP was
designed based on an Object-Oriented Paradigm, in such a way to simplify
the implementation of new and better methods.

# References
