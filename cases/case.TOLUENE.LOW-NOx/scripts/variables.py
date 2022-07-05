'''====================================================================
              THIS IS THE INPUT VARIABLE LIST FOR MOSAIC
===================================================================='''

import json
import numpy  as np
import pandas as pd
import matplotlib.pyplot as plt

# LOAD INPUT DICT.:
# =====================================================================
DICT = json.load(open('./data/in.DICT.json','r'))

# FIT CONTROL:
mFIT_O2C = DICT['FIT_O2C']

# MODEL PARAMETERs:
# =====================================================================
# RUN NAME:
run_name = DICT['Run ID']

# TOTAL SIMULATION TIME [dd:hh:mm:ss]:
t_end = '00:%02i:00:00'%DICT['tEND']

# AIR CONDITIONS - PRESSURE [atm],
# TEMPERATURE [K],
# RELATIVE HUMIDITY [%]:
Pres = 1.
Temp = 298.0
RH   = 0.

# SWITCH FOR VAPOR WALL LOSS,
# HET. CHEM. AND
# OLIGOMERIZATION:
VWL  = 1
HET  = 0
OLIG = 0

# VWL RATE [s-1]:
kvap_on = 4.0e-4

# BULK DIFFUSIVITY [cm2 s-1]:
Db = 1e-10

# OH UPTAKE COEFFICIENT AND:
# HET. CHEM. LOSS FRACTION:
yOH   = 1.0
hLOSS = 0.5

# OLIGOMER FORMATION [cm3 s-1] AND
# DISSOCIATION [s-1] RATES:
ko_f = 1e-25#*0.
ko_d = 4e-4#*0.

# NUCLEATION RATE [cm-3 s-1]:
NUC = 10.*0.

# NUCLEATION START AND
# END TIMES [s]:
NUC_T0 = 0.
NUC_T1 = 0. #3600.

# INITIAL GAS CONCENTRATIONS:
# =====================================================================
df_GAS = pd.read_excel('data/init_conc.xlsx')

# OH PARAMETERIZATIONS - CONC. [cm-3] AND
# TIMESCALE [hr-1]:
OH = {'AX1':DICT['OH_AX1'],'BX1':DICT['OH_BX1'], \
      'AX2':DICT['OH_AX2'],'BX2':DICT['OH_BX2']}

# VOC INITAL CONC. [ppb],
# MOLECULAR WEIGHT [g mol-1],
# REACTION CONST. [cm3 s-1] AND
# LOG10 SATURATION CONC.:
VOC = {'Conc0':DICT['VOC0'],'MW':DICT['MW'], \
       'kOH':DICT['kOH'],'Csat':DICT['CSAT'],'CNO':7.}

# AEROSOL SIZE DISTRIBUTION:
# =====================================================================
# NUMBER OF BINS:
nBINS = 20

# LOWER AND UPPER BOUNDS [um]:
BD_LOWER = 0.015
BD_UPPER = 1.105

# BIN BOUNDS [um]:
BOUNDS = np.logspace(np.log10(BD_LOWER),np.log10(BD_UPPER),num=nBINS+1)

# LOG WIDTH OF BINS:
WIDLOG = np.log10(BOUNDS[1:]/BOUNDS[:-1])

# BIN DIAMETERS [um]:
DIAM = np.sqrt(BOUNDS[1:]*BOUNDS[:-1])

# INITIAL SIZE DISTRIBUTION - NUMBER CONCENTRATION [# cm-3],
# MEDIAN DIAMETER [um] AND
# SPREAD:
N1   = 5000.
CMD1 = 0.093
SIG1 = 1.7

N2   = 0.
CMD2 = 0.09
SIG2 = 1.7

# INITIAL NUMBER CONC. IN BINS [cm-3]:
NUM1 = 2.303*N1/np.sqrt(2.*np.pi)/np.log(SIG1)*np.exp(-np.log(DIAM/CMD1)**2./2./np.log(SIG1)**2.)
NUM2 = 2.303*N2/np.sqrt(2.*np.pi)/np.log(SIG2)*np.exp(-np.log(DIAM/CMD2)**2./2./np.log(SIG2)**2.)

INIT_NUM = (NUM1 + NUM2)*WIDLOG

# INITIAL MOLE CONC. IN BINS [umol m-3]:
INIT_MOLE = np.pi/6*(DIAM*1e-6)**3.*1700.*INIT_NUM*1e6*1e3/96.*1e6
