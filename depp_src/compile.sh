mpif90 -Wall -O3 -o depp.x depp_qsort.f90 depp_input.f90 depp_tools.f90 depp_output.f90 depp_rsm.f90 depp_hybrid.f90 depp_main.f90
mv ./depp.x ../
rm *.mod
