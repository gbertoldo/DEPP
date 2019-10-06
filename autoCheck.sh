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

clear
printf "\n\nBefore continuing, make sure you have installed a Fortran 2008 compiler and a C++ compiler.\n"
printf "Scripts ./compile.sh,\n" 
printf "        ./interface/C++/compile.sh,\n"
printf "        ./interface/Fortran2008/compile.sh and \n"
printf "        ./interface/Functions/compile.sh\n" 
printf "use GNU Fortran compiler (gfortran) and GNU C++ compiler (g++).\n"
printf "If you have different compilers, please edit these scripts properly.\n"
printf "You will also need MPI. For more details, see\n\n" 
printf "https://github.com/gbertoldo/DEPP/wiki#how-to-install-depp\n\n"

while true; do
    read -p "Proceed?  " yn
    case $yn in
        [Yy]* ) break;;
        [Nn]* ) exit;;
        * ) echo "Please answer yes (y) or no (n).  ";;
    esac
done


printf "Making scripts executable..."
chmod u+x ./compile.sh
chmod u+x ./run.sh
chmod u+x ./interface/C++/compile.sh
chmod u+x ./interface/Fortran2008/compile.sh
chmod u+x ./interface/Functions/compile.sh
printf " ok.\n"

printf "Trying to compile DEPP's source code..."
if ! ./compile.sh > /dev/null
then
   echo "failed! Stopping..."
   exit
fi
printf " ok.\n"

printf "Trying to compile source code of examples..."
pushd ./interface/C++/ > /dev/null 
if ! ./compile.sh > /dev/null
then
   echo "failed! Stopping..."
   exit
fi
popd > /dev/null
pushd ./interface/Fortran2008/ > /dev/null
if ! ./compile.sh > /dev/null
then
   echo "failed! Stopping..."
   exit
fi
popd > /dev/null
pushd ./interface/Functions/ > /dev/null
if ! ./compile.sh > /dev/null
then
   echo "failed! Stopping..."
   exit
fi
popd > /dev/null
printf " ok.\n"

printf "Running an example..."
if ! ./run.sh > /dev/null
then
   echo "failed! Stopping..."
   exit
fi
printf " ok.\n"
