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

stopflag=false

DEPENDENCIES="mpirun mpif90 g++ gfortran"

echo "Checking dependencies..."

for prog in $DEPENDENCIES
do

   result=$(which $prog)
   
   if [ "$result" == "" ]
   then
      echo "    $prog was not found!"
      stopflag=true
   else
      echo "    $prog was found!"
   fi

done

if [ "$stopflag" == "true" ] 
then
   echo "Aborting..."
   exit
else
   echo "All dependencies satisfied!"
fi



printf "Getting root folder path...\n"
DEPPROOT=$PWD



printf "Entering source code directory... "
if ! cd $DEPPROOT/src > /dev/null 
then
   printf "fail! Stopping...\n"
   exit
else
   printf "ok! \n"
fi



printf "Compiling source code... "
if ! sh ./compile.sh > /dev/null 
then
   printf "fail! Stopping...\n"
   exit
else
   printf "ok! \n"
fi



printf "Adding DEPP's path to system's PATH variable..."
PATHTODEPP=$(echo 'export PATH=$PATH:"'$DEPPROOT/bin'"')
export PATH=$PATH:'"'$DEPPROOT/bin'"'
echo $PATHTODEPP >> $HOME/.bashrc
source ~/.bashrc



result=$(which depp.x)
if [ "$result" == "" ]
then
   printf "fail! Stopping...\n"
   exit
else
   printf "ok! \n"
fi



printf "Returning to root directory... "
if ! cd $DEPPROOT > /dev/null 
then
   printf "fail! Stopping...\n"
   exit
else
   printf "ok! \n"
fi


