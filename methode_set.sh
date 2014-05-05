#!/bin/sh
# Job name
#BSUB -J METHODENNAME_SETNAME
#
# File / path where STDOUT will be written, the %J is the job id
#BSUB -o METHODENNAME_SETNAME.out%J
#
# File / path where STDERR will be written, the %J is the job id
#BSUB -e METHODENNAME_SETNAME.err%J
#
# Request the time you need for execution in minutes
# The format for the parameter is: [hour:]minute,
# that means for 80 minutes you could also use this: 1:20
#BSUB -W JOBZEITINMINUTEN
#
# Request vitual memory you need for your job in MB
#BSUB -M RAMINMB
#
# Request the number of compute slots you want to use
#BSUB -n ANZAHLCORES
#
# Specify the MPI support
#BSUB -a openmpi
#
#

# Loading the required module
module load openmpi
# Give an overview about all load modules
module list

# See typical batch-system environment variables - for activation remove character "#"
#export| grep -i LSB

# Space holder - not necessary
echo "-----------------"
echo " "

# Simple "hello world" program output - Should be generated three times in the output file
cd /home/ex66pimo/teamSVN/trunk
#mpirun Rscript bin/make-data.R
#echo "DONE WITH FIRST JOB"
echo "$LSB_HOSTS" | sed -e "s/ /\n/g" | cat > hostfile.$LSB_JOBID
mpirun  -n ANZAHLCORES  -hostfile hostfile.$LSB_JOBID  Rscript bin/runCluster.R METHODENNAME SETNAME
