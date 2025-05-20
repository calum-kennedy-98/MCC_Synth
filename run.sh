#!/bin/bash -l

#$ -N mcc_synth_pipeline
#$ -cwd
#$ -l h_rt=02:00:00
#$ -l mem_free=16G
#$ -pe smp 4

# Load required modules
module -f unload compilers mpi gcc-libs
module load cmake/3.27.3
module load pandoc
module load glpk
module load r/r-4.4.2_bc-3.20

# Activate renv and run pipeline
Rscript -e 'renv::restore(); targets::tar_make()'
