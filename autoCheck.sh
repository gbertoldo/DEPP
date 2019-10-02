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
