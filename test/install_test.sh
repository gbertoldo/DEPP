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

printf "Preparing to run ../example/Fortran2008..."
if ! cd ../examples/Fortran2008/ > /dev/null
then
   printf "fail! Stopping...\n"
   exit
fi



if ! bash ./compile.sh > /dev/null
then
   printf "fail! Stopping...\n"
   exit
else
   printf " ok! \n"
fi



printf "Running ../example/Fortran2008..."
if ! mpirun -np 2 depp.x depp_parameters.txt > /dev/null
then
   printf "fail! Stopping...\n"
   exit
else
   printf " ok! \n"
fi


FNAME=$(ls ./depp_output/*logfile.txt)


RESULT=$(cat $FNAME | grep "fittest: The best fitness found")

if [ "$RESULT" != "" ]
then
   echo
   echo "SUCCESS!"
   echo
else
   echo
   echo "FAILURE!"
   echo
   exit
fi


bash clear.sh

printf "Preparing to run ../example/C++..."
if ! cd ../C++/ > /dev/null
then
   printf "fail! Stopping...\n"
   exit
fi



if ! bash ./compile.sh > /dev/null
then
   printf "fail! Stopping...\n"
   exit
else
   printf " ok! \n"
fi



printf "Running ../example/C++..."
if ! mpirun -np 2 depp.x depp_parameters.txt > /dev/null
then
   printf "fail! Stopping...\n"
   exit
else
   printf " ok! \n"
fi


FNAME=$(ls ./depp_output/*logfile.txt)


RESULT=$(cat $FNAME | grep "fittest: The best fitness found")

if [ "$RESULT" != "" ]
then
   echo
   echo "SUCCESS!"
   echo
else
   echo
   echo "FAILURE!"
   echo
   exit
fi

bash clear.sh

printf "Preparing to run ../example/TestFunctions..."
if ! cd ../TestFunctions/ > /dev/null
then
   printf "fail! Stopping...\n"
   exit
fi



if ! bash ./compile.sh > /dev/null
then
   printf "fail! Stopping...\n"
   exit
else
   printf " ok! \n"
fi



printf "Running ../example/TestFunctions..."
if ! mpirun -np 2 depp.x depp_parameters.txt > /dev/null
then
   printf "fail! Stopping...\n"
   exit
else
   printf " ok! \n"
fi


FNAME=$(ls ./depp_output/*logfile.txt)


RESULT=$(cat $FNAME | grep "fittest: The best fitness found")

if [ "$RESULT" != "" ]
then
   echo
   echo "SUCCESS!"
   echo
else
   echo
   echo "FAILURE!"
   echo
   exit
fi

bash clear.sh


cd ../../test
