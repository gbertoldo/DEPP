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
./search_strategy/depp_class_abstract_parallel_processed_data.f90
./search_strategy/depp_class_abstract_search_strategy.f90 
./search_strategy/depp_class_abstract_search_strategy_factory.f90
./search_strategy/DE/depp_class_DE_RAND_1.f90 
./search_strategy/RSM/depp_class_abstract_RSM.f90
./search_strategy/RSM/depp_class_RSM_Quadratic_Model.f90
./search_strategy/RSM/depp_class_RSM_Incomplete_Quadratic_Model.f90
./search_strategy/RSM/depp_class_RSM_factory.f90
./search_strategy/RSM/depp_rsm_dynamic_control.f90 
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

#mpif90 -Wall -O3 -o depp.x $flist
mpif90 -o depp.x $flist

mv ./depp.x ../
rm *.mod
