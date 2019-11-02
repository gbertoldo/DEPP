#/bin/sh


cd ./src

printf "Compiling DEPP..."  
if ! sh ./compile.sh > /dev/null
then
   printf "Failed! Stopping..."
   exit
fi
printf " ok!\n"

cd ../bin

MYSHELL=$(basename $SHELL)

echo $MYSHELL

case $MYSHELL in
   "bash") PATHTODEPP=$(echo 'export PATH=$PATH:"'$PWD'"'); 
            $(PATHTODEPP); 
            echo $PATHTODEPP >> $HOME/.bashrc ;;
   
   "tcsh") echo "TC shell not implemented yet" ;;
   
    "ksh") echo "K shell not implemented yet" ;;
   
    "zsh") echo "Z shell not implemented yet" ;;
   
        *) echo "Shell not identified. To complete installation, put DEPP's bin directory\n";
           echo $PWD;
           echo "\nin your PATH variable." ;;
esac

echo $PATHTODEPP

