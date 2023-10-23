#!/bin/sh
#$ -l mem=4G,time=:20:
cd iv_mediation
Rscript=/nfs/apps/R/4.0.3/bin/Rscript
export R_LIBS_USER=/ifs/home/msph/epi/ntw2117/R_4.0
${Rscript} _research/dcme_sim.R $1 $2
