mpif90 -Wall -O3 -o depp.x  depp_mpi.f90 depp_global_parameters.f90 depp_timer.f90 depp_random_generator.f90 depp_class_ifile.f90 depp_class_system_variables.f90 depp_qsort.f90 depp_class_abstract_search_strategy.f90 depp_class_DE_RAND_1.f90 depp_search_strategy_factory.f90 depp_class_ehist.f90 depp_input.f90 depp_tools.f90 depp_output.f90 depp_rsm.f90 depp_rsm_dynamic_control.f90 depp_hybrid.f90 depp_no_improvement_stopping_condition.f90 depp_stopping_condition_module.f90 depp_main.f90
mv ./depp.x ../
rm *.mod
