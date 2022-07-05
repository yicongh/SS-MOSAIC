'''==================================================================
                  THIS FUNCTION RUNS THE MODEL FORWARD
=================================================================='''

import numpy             as np
import pandas            as pd
import matplotlib.pyplot as plt

#import matplotlib; matplotlib.use('Agg')

from functions.f_comp_model import comp_model
from functions.f_run_model  import run_model

import variables  as ns
import data.mdata as mdata

def feed4ward(params):

    # COMPILE THE MODEL:
    # ===============================================================
    comp_model('MOSAIC.v1',params)

    # RUN THE MODEL AND GET OUTPUTS:
    # ===============================================================
    outs = run_model(ns)

    df_AEOUT = outs['df_AEOUT']
    df_SOA   = outs['df_SOA']
    df_SIZE  = outs['df_SIZE']
    df_AENUM = outs['df_AENUM']
    df_VOC   = outs['df_VOC']
    df_NCONC = outs['df_NCONC']
    df_O2C   = outs['df_O2C']
    
    # SAVE DATA:
    # ===============================================================
    with pd.ExcelWriter('outputs/model_outs.xlsx') as xls:
        df_SOA.to_excel(xls,sheet_name='df_SOA')
        df_O2C.to_excel(xls,sheet_name='df_O2C')
        xls.close()
    
    # MAKE PLOTS:
    # ===============================================================
    plt.figure()
    ax = plt.gca()

    tt = df_SOA['time'].values
    yy = df_SOA['SOA'].values
    zz = df_SOA['OLIG'].values

    ttt = mdata.x_obs
    yyy = mdata.y_obs

    print('SOA = ',yy[-1])
    print('OLG = ',zz[-1])

    ax.plot(ttt,yyy,color='grey',marker='o',linestyle='None')
    
    ax.plot(tt,yy,color='r',label='SOA')
    ax.plot(tt,zz,color='r',linestyle='--',label='DIMER')
    
    ax.set_xlabel('Time (hours)')
    ax.set_ylabel('SOA ($\mu$g m$^{-3}$)')

    ax.set_xlim((0.,ns.DICT['tEND']))
    ax.set_ylim((0.,None))

    ax.legend(loc=2)
    
    plt.savefig('outputs/fig.SOA.%s.png'%ns.run_name,bbox_inches='tight')
    
    # MAKE PLOTS:
    # ===============================================================
    plt.figure()
    ax = plt.gca()

    tt = df_O2C[0].values
    yy = df_O2C[3].values; #yy[0] = yy[1]
    
    ttt = mdata.xx_obs
    yyy = mdata.yy_obs
    
    ax.plot(ttt,yyy,color='grey',marker='o',linestyle='None')
    
    ax.plot(tt,yy,color='r',label='SOA')
    
    ax.set_xlabel('Time (hours)')
    ax.set_ylabel('O:C')

    ax.set_xlim((0.,ns.DICT['tEND']))
    ax.set_ylim((0.,1.))

    ax.legend(loc=2)
    
    plt.savefig('outputs/fig.O2C.%s.png'%ns.run_name,bbox_inches='tight')
    
    plt.show()
