#    DEPP - Differential Evolution Parallel Program
#
#    Copyright (C) 2019 by Jonas Joacir Radtke, Guilherme Bertoldo and Carlos Henrique Marchi
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#    
#    Contact:
#          Jonas Joacir Radtke (a)
#                 E-mail: jonas.radtke@gmail.com
#
#          Guilherme Bertoldo (a)
#                 E-mail: glbertoldo@gmail.com
#
#          Carlos Henrique Marchi (b)
#                 E-mail: chmcfd@gmail.com
#    Institution
#          (a) Federal University of Technology - Paraná - UTFPR
#              Linha Santa Bárbara, s/n, Francisco Beltrão, Paraná, Brazil
#              Zip Code 85601-970
#              
#          (b) Federal University of Paraná - UFPR
#              Curitiba, Paraná, Brazil
#              Caixa postal 19040
#              Zip Code 81531-980
#

flist="
depp_mpi.f90 
./util/depp_gauss_solver.f90
./util/depp_class_timer.f90 
./util/depp_random_generator.f90 
./util/depp_class_ifile.f90 
./util/depp_class_calendar.f90
./util/depp_string.f90
depp_class_log_output_control.f90
depp_class_system_variables.f90 
./util/depp_class_btimer.f90 
./util/depp_qsort.f90 
depp_class_ehist.f90 
./fitness_calculator/depp_class_fitness_calculator_exit_code.f90
./fitness_calculator/depp_class_abstract_fitness_calculator.f90
./fitness_calculator/depp_class_external_fitness_calculator.f90
./fitness_calculator/depp_class_fitness_calculator_factory.f90
./search_strategy/depp_search_tools.f90 
./search_strategy/population_initializers/depp_class_abstract_population_initializer.f90
./search_strategy/population_initializers/depp_class_population_initializer_uniform_random.f90
./search_strategy/population_initializers/depp_class_population_initializer_factory.f90
./search_strategy/depp_class_abstract_parallel_processed_data.f90
./search_strategy/depp_class_abstract_search_strategy.f90 
./search_strategy/depp_class_abstract_search_strategy_factory.f90
./search_strategy/DE/depp_class_DE_RAND_1.f90 
./search_strategy/RSM/depp_RSM_tools.f90
./search_strategy/RSM/depp_class_abstract_RSM.f90
./search_strategy/RSM/depp_class_RSM_Quadratic_Model.f90
./search_strategy/RSM/depp_class_RSM_Incomplete_Quadratic_Model.f90
./search_strategy/RSM/depp_class_RSM_factory.f90
./search_strategy/RSM/depp_class_DE_RSM_hybridization_control.f90
./search_strategy/RSM/depp_class_RSM_search_strategy.f90
./search_strategy/RSM/depp_class_DE_RSM.f90 
./search_strategy/depp_class_search_strategy_factory.f90 
./search_strategy/depp_class_parallel_processed_trial_population.f90
./stop_condition/depp_class_abstract_stop_condition.f90
./stop_condition/depp_class_max_generations_stop_condition.f90
./stop_condition/depp_class_no_improvement_stop_condition.f90
./stop_condition/depp_class_p_measure_stop_condition.f90
./stop_condition/depp_stop_condition_factory.f90
./stop_condition/depp_class_composite_stop_condition.f90
depp_class_optimizer.f90 
depp_main.f90
"

#mpif90 -std=f2008 -Wall -O3 -o depp.x $flist
mpif90 -std=f2008 -O3 -o depp.x $flist

mv ./depp.x ../
rm *.mod
