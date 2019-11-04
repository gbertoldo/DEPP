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

#!/bin/bash

echo "Generating the executable that will be used to check the code..."
echo "================================================================"
echo
gfortran  -std=f2008 -Wall -O3  verification_test_mpi.f90 ../src/util/depp_string.f90 ../src/util/depp_class_ifile.f90 verification_test.f90 -o test.x
rm *.mod > /dev/null

cd ../examples/TestFunctions/
sh ./compile.sh
cd - > /dev/null
mv ../examples/TestFunctions/fitness.x ./


echo
echo "Performing code verification of pure DE algorithm..."
echo "===================================================="
echo
./test.x depp_parameters1.txt


echo
echo "Performing code verification of DE-RSM algorithm..."
echo "==================================================="
echo
./test.x depp_parameters2.txt


echo
echo "Cleanning directory..."
rm -rf  depp_output depp_par.txt fitness.x test.x 
