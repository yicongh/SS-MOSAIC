#!/bin/bash

#SBATCH --partition=jat_all
#SBATCH --output=outputs/coso.out
#SBATCH --open-mode=append

source /scratch/yicongh/privmods/intel/oneapi/setvars.sh

echo STARTED @ $( date )

python runme.py

echo ENDED @ $( date )
