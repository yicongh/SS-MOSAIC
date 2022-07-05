'''=====================================================================
                  THIS FILE READS THE MEASURED DATA
====================================================================='''

import os
import json
import numpy  as np
import pandas as pd

from functions.f_get_root import get_root

# READ INPUT DICT.:
# ======================================================================
DICT = json.load(open('./data/in.DICT.json','r'))

# READ SOA:
# ======================================================================
# ROOT PATH:
root = get_root()

# READ DATA:
df = pd.read_excel('%s/data/%s'%(root,DICT['FNAME_SOA']))

x_obs = df['time'].values
y_obs = df['SOA'].values

index = x_obs <= 12.0
x_obs = x_obs[index]
y_obs = y_obs[index]

index = y_obs > 0.0
x_obs = x_obs[index]
y_obs = y_obs[index]

# READ DATA:
xx_obs = 0.
yy_obs = 0.

try:
    df = pd.read_excel('%s/data/%s'%(root,DICT['FNAME_O2C']))

    xx_obs = df['time'].values
    yy_obs = df['O2C'].values

    index  = xx_obs <= 12.0
    xx_obs = xx_obs[index]
    yy_obs = yy_obs[index]

except:
    pass
