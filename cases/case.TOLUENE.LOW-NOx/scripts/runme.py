'''==============================================================
            THIS IS THE WRAPPER FOR THE MOSAIC MODEL
=============================================================='''

import os
import numpy  as np
import pandas as pd
import scipy.optimize    as sio
import matplotlib.pyplot as plt

from functions.f_feed4ward import feed4ward
from functions.f_fit_model import fit_model

from variables import DICT

# SIMPLE-SOM PARAMETERS:
# ===============================================================
xx = np.array([DICT['mFRAG'], DICT['mLOSS'], DICT['pHOM'], DICT['dLVP'], \
               DICT['p1'], DICT['p2'], DICT['p3'], DICT['p4']])

# INITIAL GUESS FOR FITTING:
x0 = np.zeros(len(xx))

x0[0] = np.copy(xx[0])
x0[1] = np.copy(xx[1])
x0[2] = np.copy(xx[2])
x0[3] = np.copy(xx[3])
x0[4] = xx[4]*1000.
x0[5] = xx[5]*1000.
x0[6] = xx[6]*1000.
x0[7] = xx[7]*1000.

# FIT CONFIGURATIONS:
# ===============================================================
# CONTROL SWITCH
# 0 FOR FORWARD
# 1 FOR FITTING:
switch = int(DICT['FIT'])

# RUN THE MODEL (FIT/FORWARD):
# ===============================================================
if switch == 1:
    
    # REFRESH RECORD FILE:
    f1 = open('outputs/diag.params','w').close()

    # INDEX OF VALUEs TO HOLD CONST.:
    dex1 = [2]
    dex2 = [i for i in range(len(x0)) if i not in dex1] 
    
    # GET SLICES:
    x00 = np.array(x0[dex2])
    x01 = np.array(x0[dex1])
    
    # LOWER AND UPPER BOUNDS:
    bL = [0.01,  0.001,   0.0, 1.000, 1e-2, 1e-2, 1e-2, 1e-2]
    bU = [5.00,  4.000, 100.0, 3.000,  1e4,  1e4,  1e4,  1e4]
    
    bL = tuple(np.array(bL)[dex2])
    bU = tuple(np.array(bU)[dex2])

    # RELATIVE DIFFERENTIAL STEP FOR FITTING:
    ds = [0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001]
    
    ds = tuple(np.array(ds)[dex2])
    
    # RUN FITTING:
    results = sio.least_squares(fit_model,x00,
                diff_step=ds,bounds=(bL,bU),loss='linear',
                ftol=1e-6,xtol=1e-6,verbose=0,max_nfev=10,
                kwargs={'dex':(dex1,dex2),'x01':x01})
    
    # PRINT FITS:
    fits = results.x

    oo = np.zeros(max(max(dex1),max(dex2))+1)

    for i,dex in enumerate(dex1):
        oo[dex] = x01[i]

    for i,dex in enumerate(dex2):
        oo[dex] = fits[i]

    with open('outputs/diag.params','a') as ff:
        ff.write('xx = np.array([%.5f,%.5f,%.5f,%.5f,%.5f,%.5f,%.5f,%.5f])'%tuple(list(oo)))
        
    # SAVE FITs:
    np.save('./outputs/out.FITS.npy',fits)

    # RUN FORWARD:
    feed4ward(oo)

elif switch == 0:
    # VISUALIZE:
    feed4ward(xx); print('FINISHED')

else:
    pass





