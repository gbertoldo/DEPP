#!/bin/bash


# In the input file ($1), finds the line containing the pattern $2. 
# Returns the first column of that line.
function ReadData(){
   
   # $1 = input file
   # $2 = pattern to be find
   
   local readData=""
   
   # Checking the number of parameters
   if [[ "$#" != "2" ]] 
   then
       
      return
      
   fi
   
   readData=$(cat $1 | grep "$2" | awk '{print $1}')

   echo $readData 

}

# SETTING UP PARAMETERS
NR=50 # Number of runs


# Extracting informations about simulation case
pfile="./depp_input/input_parameters.txt"
sname=$(ReadData "$pfile" "= sname:    Simulation name")

if [[ -e "experiment_$sname.csv" ]]
then

   rm experiment_$sname.csv
   
else

   touch experiment_$sname.csv

fi


# Compiling DEPP
./compile.sh

echo "i", "fittest", "generations", "tcpu (s)", "x(1)" >> experiment_$sname.csv

# Running DEPP NR times
for i in $(seq 1 $NR) 
do 

   mpirun -np 4 ./depp.x

   # Extracting informations about the current simulation

   pfile="./depp_output/function14/function14-summary.txt"

   fittest=$(ReadData "$pfile" "= fittest: The best fitness found")
   generations=$(ReadData "$pfile" "= g:       Final number of generations")
   tcpu=$(ReadData "$pfile" "= tcpu:    Total CPU time")
   x1=$(ReadData "$pfile" "= x( 1):    The best value for the")

   echo $i, $fittest, $generations, $tcpu, $x1 >> experiment_$sname.csv

done
