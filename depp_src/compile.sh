mpif90 -Wall -O3 -o depp.x  depp_timer.f90 depp_mpi.f90 depp_qsort.f90 depp_input.f90 depp_tools.f90 depp_output.f90 depp_rsm.f90 depp_rsm_dynamic_control.f90 depp_hybrid.f90 depp_no_improvement_stopping_condition.f90 depp_stopping_condition_module.f90 depp_main.f90
mv ./depp.x ../
rm *.mod
