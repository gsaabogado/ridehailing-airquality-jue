#!/bin/sh
#BSUB -n 1
#BSUB -q s_medium
#BSUB -e /work/cmcc/ls01122/uber_pol/replication_package/test_naaqs_se.err
#BSUB -o /work/cmcc/ls01122/uber_pol/replication_package/test_naaqs_se.out
#BSUB -P 0560
#BSUB -M 64G
#BSUB -R "rusage[mem=64G]"
#BSUB -R "span[hosts=1]"
#BSUB -W 06:00
#BSUB -J test_naaqs_se

export MKL_NUM_THREADS=1
export OMP_NUM_THREADS=1

source /etc/profile.d/modules.sh
module load intel-2021.6.0/R/4.3.1 --auto

cd /work/cmcc/ls01122/uber_pol/replication_package
Rscript code/test_naaqs_se.R
