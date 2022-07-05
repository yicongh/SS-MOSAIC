'''==================================================================
              THIS FUNCTION COMPILES THE MOSAIC MODEL
=================================================================='''

import os
import numpy  as np
import pandas as pd

from functions.f_get_root import get_root
from functions.f_use_prep import use_prep

import variables as ns

def comp_model(model,params):

    # GET ROOT DIRECTORY:
    # ===============================================================
    root = get_root()
    
    # COMPILE AND COPY TO CURRENT DIRECTORY:
    # ===============================================================
    # GET COMPILE PATH:
    path = '%s/models/%s.%s.comp'%(root,model,ns.run_name)
    
    # MAKE COPY:
    os.system("cd %s/models/ ; cp -a %s %s.%s.comp"%(root,model,model,ns.run_name))
    
    # RUN THE PREPROCESSOR:
    # ===============================================================
    use_prep(path,params,ns)
    
    # COMPILE THE SOM-TOMAS MODEL:
    # ===============================================================
    print('MOSAIC COMPILING...')
    os.system('cd %s ; make clean >/dev/null; make -s'%path)
    print('MOSAIC COMPILED in %s'%path)
    
    os.system(' cd .. ; rm mosaic.exe')
    os.system('cp %s/mosaic.exe ../mosaic.exe'%(path))
    os.system('rm -rf %s'%path)
    print('MOSAIC COPIED to CURRENT CASE.')
