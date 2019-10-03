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

Optimization is a mathematical problem often found in science and engineering. Currently, however, there is no general method to face this problem. Solutions are generally addressed by two approaches, both iterative: (a) quasi-Newton methods [@Griva:2009] and (b) heuristic methods [@Coley:1999; @Feoktistov:2006]. Each one has advantages depending on the problem to be optimized. Quasi-Newton methods, in general, converge faster then heuristic methods, provided the function to be optimized (the objective function) is smooth. Heuristic methods, on the other hand, are more appropriate to deal with noisy objective functions, to handle failures in the calculation of the objetive function and are less susceptible to be retained in local optimum than quasi-Newton methods. 

Among the heuristic methods, Differential Evolution (DE)[@Storn:1997; @Price:2005] had emerged as a simple and efficient method for finding the global maximum. This method is based on the principles of biological evolution.

In order to combine the robustness of heuristic methods with the high convergence speed of quasi-Newton methods, Loris Vincenzi and Marco Savoia [@Vincenzi:2015] proposed coupling Differential Evolution heuristic with Response Surfaces [@Khuri:1996; @Myers:2009]. Fitting Response Surfaces during optimization and finding their optima mimics quasi-Newton methods. Authors showed that this approach reduced significantly the effort to reach the solution within a given tolerance (in general, more than 50% compared to the original heuristic method). 

Based on the paper of Loris Vincenzi and Marco Savoia, but applying a different algorithm, a software called ``DEPP``, an achronym for Differential Evolution Parallel Program, was elaborated. ``DEPP`` source code is written in Fortran 2008 standard[@Brainerd:2015], it is based on the object-oriented paradigm and it includes MPI paralellization[@Tennessee:2009]. The main algorithm was elaborated to simplify the development and extension of the code, relying on abstract classes and polymorphism. Following this idea, design patterns [@Freeman:2004] were also applied. Object instances are generated through the Factory Design Pattern and Adapter Design Pattern was applied to encapsulate MPI commands, for instance. In this way, users may implement new optimization methods without changing the main algorithm. 

Due to its supporting theory, ``DEPP`` is well suited to address optimization problems of multimodal, noisy, poor-precision-calculated and failure-susceptible objetive functions, taking advantage of acceleration provided by parallelization and hybridization models, like Differential Evolution-Response Surface coupling. 

In the following sections, a brief description of DEPP, its supporting theory and some examples are presented. 


Software description {#sec:description}
====================

Software Architecture {#sec:arch}
---------------------

DEPP is written in FORTRAN 2008 standard language[@Brainerd:2015] with the Message Passing Interface (MPI) directives[@Tennessee:2009] and takes
advantage of the Object-Oriented Paradigm.

The folder structure of DEPP is shown in Figure \[fig:depptree\]. The directory *depp\_input* contains the input (text) files which defines
the control parameters of the optimization. Results of DEPP are saved into *depp\_output* directory. The source code is within *depp\_src* directory. This directory also contains the Bash[@Ramey:2016] script *compile.sh*, which compiles DEPP source code and generates the executable *depp.exe* in the root of the file structure.  The interface between DEPP and the external program (EP), which calculates the objective function, is defined in the *interface* directory. Finally, the script *run.sh* runs DEPP using MPI.

![Folder structure of DEPP.](folder_structure.png){width="50.00000%"}
[\[fig:depptree\]]{}

Thanks to the Object-Oriented Programming, the source code was designed to simplify the implementation of new methods. The algorithm works on
abstract classes separated, basically, in (i) population initializers, (ii) search strategies, (iii) fitness calculation and (iv) stop conditions. The concrete classes are generated using the Factory Design Pattern[@Freeman:2004]. All MPI commands are encapsulated into proper classes, following the logic of the Adapter Design Pattern. In this way, users interested in the implementation of new methods will not concern about MPI details.

Interfacing DEPP with an external program is exemplified in *interface* directory. This folder contains three examples based on C++ and FORTRAN 2008 languages, although many other programming languages may be used for interfacing.

The basic algorithm of DEPP is illustrated in Figure \[fig:deppalgorithm\]. DEPP reads the input data from *depp\_input* directory (optionally, it may start from some backup). The stop condition is analyzed. In the first iteration, the stop condition is generally not satisfied (it may not be true when starting from a
backup). When stop condition is satisfied, the iterative procedure is finished and the output data is saved in the *depp\_output* directory. Otherwise, a trial population is generated. DEPP sends the information to a set of threads (T1,$\cdots$, TN) using MPI. Then, each of these threads runs a copy of the external program (EP) for a given individual of the population. After that, DEPP receives the calculated objective function and compares their fitness with the fitness of the current
population. Only the best individuals are held. A backup is optionally generated and the iterative cycle is restarted. During the calculation of the objective function, some failures may occur. DEPP handles failures using an error code returned by the external program. If the error code is 0, the calculations were performed correctly; if 1 is returned, then a failure occurred and the trial individual is discarded; if 2 is returned, then a failure occurred, the trial individual is discarded and a new one is generated. All the failures are registered for posterior user’s analysis.

![DEPP basic algorithm.](algorithm1.png){width="70.00000%"}
[\[fig:deppalgorithm\]]{}

Supporting theory {#sec:theory}
-----------------

The following sections describe the optimization theory behind the current implementation of DEPP. It must be emphasized that DEPP was designed based on an Object-Oriented Paradigm, in such a way to simplify the implementation of new and better methods.

### Differential Evolution {#sec:DE}

Differential Evolution algorithm is based on the principles of biological evolution. Basically, it works as follow. Given a population (set of discrete points in the domain of search), new individuals (trial points) are generated by “mutation” and “crossing over”. Here, “mutation” means a rule for creating new trial individuals while “crossing over” means a rule of exchanging information between a trial individual and an individual of the population. The fitness (objective function) of the new individuals is compared to the fitness of the parent individuals and the worse individuals are eliminated. The procedure is repeated for several generations (iterations) until a criterion of convergence is satisfied within a given tolerance.

Current version of DEPP focus on a particular kind of maximization problem[^1], often found in practical applications, i.e.,
$$\begin{aligned}
        \underset{\boldsymbol{x}}{\text{maximize}} &\quad f(\boldsymbol{x}) \\
        \text{subject to}&\\
        &\quad \boldsymbol{L} \le \boldsymbol{x}  \le \boldsymbol{U},
\end{aligned}
\label{eq:maximization}$$ 
where $f$ is a real valued function, also known as objective function, depending on a D-dimensional vector $\boldsymbol{x}=(x_1, \cdots, x_D)$ of real variables. The variable $\boldsymbol{x}$ is confined in a box with lower $\boldsymbol{L}=(L_1,\cdots, L_D)$ and upper $\boldsymbol{U}=(U_1,\cdots, U_D)$ bounds.

The first step toward solving the optimization problem (\[eq:maximization\]) is the population initialization. For the first generation ($g=1$), a population (set of points) $\{\boldsymbol{x}_i\}$ $1\le i \le N_p$ is created randomly within the box $\boldsymbol{L} \le \boldsymbol{x}  \le \boldsymbol{U}$, *e.g.* 
$$x_{ij}=L_j+(U_j-L_j) \mathcal{R},\quad 1\le j \le D,$$ 
where $\mathcal{R}$ is a random number uniformly distributed in $[0,1)$ and $x_{ij}$ is the $j$-th element of the $i$-th individual.

After the population initialization, the fitness of each individual $\boldsymbol{x}_i$ is calculated.

For the next generations, trial individuals are calculated based on the population of the previous generations. Trial individuals are created by a process of mutation and crossing over as follows. For each individual $\boldsymbol{x}_i$ of the previous population, other three distinct individuals ($\boldsymbol{x}_{i_1}$, $\boldsymbol{x}_{i_2}$ and $\boldsymbol{x}_{i_3}$) of the same population are randomly selected and a mutant individual $\boldsymbol{v}$ is created 
$$\boldsymbol{v}=\boldsymbol{x}_{i_1}+F(\boldsymbol{x}_{i_3}-\boldsymbol{x}_{i_2}),$$ 
where $F$ is the differentiation parameter. Then, the mutant individual suffers a crossing over with $\boldsymbol{x}_i$, that produces the trial individual $\boldsymbol{u}$. The components $u_j$ ($1 \le j \le D$) of $\boldsymbol{u}$ are given by 
$$u_{j}=\begin{cases} 
v_{j} &  \text{if}~~\mathcal{R} < C_r~~\text{or}~~j = j^*, \\
x_{ij} & \text{otherwise}.
\end{cases}
\label{eq:crossover}$$ 
In Eq. (\[eq:crossover\]), $C_r$ is the crossover parameter and $j^*$ is an integer randomly chosen between 1 and $D$. If the trial individual violates the constraints, i.e. $\boldsymbol{L} \le \boldsymbol{u} \le \boldsymbol{U}$, then the process of generating a trial individual is repeated.

The fitness of the trial individual is compared with the fitness of $\boldsymbol{x}_i$. If $f(\boldsymbol{x}_i)\le f(\boldsymbol{u})$, then $\boldsymbol{u}$ replaces
$\boldsymbol{x}_i$. This is the so called selection.

New generations are created until a stopping condition is satisfied. DEPP implements a composition of the following stopping conditions: (i) best fitness stagnation, (ii) maximum number of generations and (iii) population convergence measure. In the first one, the generation cycle is interrupted if no improvement of the fitness function is achieved within $N_A$ generations. The second condition establishes the maximum number of generations allowed. Finally, in the third condition, the
cycle is interrupted if the P-measure $P_m$[@Feoktistov:2006] is lower or equal to a given tolerance. P-measure is evaluated as
$$P_m=\max_{1 \le i \le N_p} |\boldsymbol{x}'_i-\boldsymbol{x}'_m|,$$ 
where $|\cdot|$ is the $L_2$ norm, the components of $\boldsymbol{x}'_i$ are given by
$$x'_{ij}=\frac{x_{ij}-L_j}{U_j-L_j}, \quad 1 \le j \le D$$ 
and
$$\boldsymbol{x}'_m=\frac{1}{N_p}\sum_{i=1}^{N_p}\boldsymbol{x}'_i.$$

The approach presented in this section has three parameters: the population size $N_p$, the differentiation parameter $F$ and the crossover parameter $C_r$. Feoktistov[@Feoktistov:2006] gives reference values/ranges of these parameters based on optimization studies performed on many test functions.

# References
