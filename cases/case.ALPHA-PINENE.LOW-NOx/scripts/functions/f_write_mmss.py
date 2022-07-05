'''=====================================================================
         THIS FUNCTION WRITES THE MECHANISM AND SPECIES FILES
====================================================================='''

import math
import numpy  as np
import pandas as pd

def write_mmss(path,params,ns):

    # GET PARAMETERS:
    # ==================================================================
    mFRAG = params[0]
    fLOSS = np.exp(-params[1])
    dLVP  = params[3]

    pHOM = params[2]    
    PO1  = params[4]
    PO2  = params[5]
    PO3  = params[6]
    PO4  = params[7]

    POs = [i/sum([PO1,PO2,PO3,PO4]) for i in [PO1,PO2,PO3,PO4]]

    PO1 = POs[0]
    PO2 = POs[1]
    PO3 = POs[2]
    PO4 = POs[3]
    
    # VOLATILITY DIST. OF ADDING 1, 2, 3 or 4 OXYGENS:
    # ==================================================================
    SIGMAX = 1.

    DIST_PO1 = np.array([np.exp(-(1.*dLVP-(i+1))**2./SIGMAX) for i in range(2)])
    DIST_PO2 = np.array([np.exp(-(2.*dLVP-(i+2))**2./SIGMAX) for i in range(3)])
    DIST_PO3 = np.array([np.exp(-(3.*dLVP-(i+3))**2./SIGMAX) for i in range(4)])
    DIST_PO4 = np.array([np.exp(-(4.*dLVP-(i+4))**2./SIGMAX) for i in range(5)])
    
    DIST_PO1 = DIST_PO1/np.sum(DIST_PO1)
    DIST_PO2 = DIST_PO2/np.sum(DIST_PO2)
    DIST_PO3 = DIST_PO3/np.sum(DIST_PO3)
    DIST_PO4 = DIST_PO4/np.sum(DIST_PO4)
    
    # GENERIC FUNCTIONALIZATION DISTRIBUTION:
    # ==================================================================
    nFRAG = 2
    nFUNC = 8

    CHEMDIST_SOM = np.zeros(nFUNC+1+nFRAG)
    CHEMDIST_OXY = np.zeros(nFUNC+1+nFRAG)

    CHEMDIST_SOM[nFRAG]   = 0.
    CHEMDIST_SOM[nFRAG+1] = PO1*DIST_PO1[0]
    CHEMDIST_SOM[nFRAG+2] = PO1*DIST_PO1[1] + PO2*DIST_PO2[0]
    CHEMDIST_SOM[nFRAG+3] = PO2*DIST_PO2[1] + PO3*DIST_PO3[0]
    CHEMDIST_SOM[nFRAG+4] = PO2*DIST_PO2[2] + PO3*DIST_PO3[1] + PO4*DIST_PO4[0]
    CHEMDIST_SOM[nFRAG+5] = PO3*DIST_PO3[2] + PO4*DIST_PO4[1]
    CHEMDIST_SOM[nFRAG+6] = PO3*DIST_PO3[3] + PO4*DIST_PO4[2]
    CHEMDIST_SOM[nFRAG+7] = PO4*DIST_PO4[3]
    CHEMDIST_SOM[nFRAG+8] = PO4*DIST_PO4[4]

    CHEMDIST_OXY[nFRAG]   = 0.
    CHEMDIST_OXY[nFRAG+1] = (PO1*DIST_PO1[0]*1.)/CHEMDIST_SOM[nFRAG+1]
    CHEMDIST_OXY[nFRAG+2] = (PO1*DIST_PO1[1]*1. + PO2*DIST_PO2[0]*2.)/CHEMDIST_SOM[nFRAG+2]
    CHEMDIST_OXY[nFRAG+3] = (PO2*DIST_PO2[1]*2. + PO3*DIST_PO3[0]*3.)/CHEMDIST_SOM[nFRAG+3]
    CHEMDIST_OXY[nFRAG+4] = (PO2*DIST_PO2[2]*2. + PO3*DIST_PO3[1]*3. + PO4*DIST_PO4[0]*4.)/CHEMDIST_SOM[nFRAG+4]
    CHEMDIST_OXY[nFRAG+5] = (PO3*DIST_PO3[2]*3. + PO4*DIST_PO4[1]*4.)/CHEMDIST_SOM[nFRAG+5]
    CHEMDIST_OXY[nFRAG+6] = (PO3*DIST_PO3[3]*3. + PO4*DIST_PO4[2]*4.)/CHEMDIST_SOM[nFRAG+6]
    CHEMDIST_OXY[nFRAG+7] = (PO4*DIST_PO4[3]*4.)/CHEMDIST_SOM[nFRAG+7]
    CHEMDIST_OXY[nFRAG+8] = (PO4*DIST_PO4[4]*4.)/CHEMDIST_SOM[nFRAG+8]

    # FRAGMENTATION PROBABILITY:
    # ==================================================================
    maxCSAT = 9
    minCSAT = -3
    nCOMP   = maxCSAT - minCSAT + 1

    # SATURATION CONC.:
    j_CSATLOG = np.arange(maxCSAT,minCSAT-1,-1)

    # FRAG. PROB.:
    j_PFRAG = 1. - np.exp(mFRAG*(j_CSATLOG - maxCSAT)/maxCSAT)

    # PROB. DISTRIBUTION FOR ALL SPECIES:
    # ==================================================================
    CHEMDIST_SOM_MATRIX = np.zeros((nFUNC+1+nFRAG,nCOMP))
    CHEMDIST_OXY_MATRIX = np.zeros((nFUNC+1+nFRAG,nCOMP))
    
    for j in range(nCOMP):
        
        for jj in range(nFRAG+1,nFUNC+1+nFRAG):
            CHEMDIST_SOM_MATRIX[jj,j] = CHEMDIST_SOM[jj]*(1. - j_PFRAG[j])
            CHEMDIST_OXY_MATRIX[jj,j] = CHEMDIST_OXY[jj]
            
        for jj in range(nFRAG):
            CHEMDIST_SOM_MATRIX[jj,j] = j_PFRAG[j]/float(nFRAG)*(1. - fLOSS)
            CHEMDIST_OXY_MATRIX[jj,j] = 1.

    # GAS-PHASE CHEMISTRY KERNEL:
    # ==================================================================
    jj_CHEMKERNEL_SOM = np.zeros((nCOMP,nCOMP))
    jj_CHEMKERNEL_OXY = np.zeros((nCOMP,nCOMP))

    for j in range(nCOMP):

        SLICE_SOM = CHEMDIST_SOM_MATRIX[:,j]
        SLICE_OXY = CHEMDIST_OXY_MATRIX[:,j]

        for k in range(nFRAG):
            if ((j-k-1)<0):
                pass
            else:
                jj_CHEMKERNEL_SOM[j-k-1,j] = SLICE_SOM[nFRAG-k-1]
                jj_CHEMKERNEL_OXY[j-k-1,j] = SLICE_OXY[nFRAG-k-1]

        for k in range(nFUNC):
            if ((j+k+1)<nCOMP-1):
                jj_CHEMKERNEL_SOM[j+k+1,j] = SLICE_SOM[nFRAG+k+1]
                jj_CHEMKERNEL_OXY[j+k+1,j] = SLICE_OXY[nFRAG+k+1]
            else:
                jj_CHEMKERNEL_SOM[nCOMP-1,j] += SLICE_SOM[nFRAG+k+1]
                jj_CHEMKERNEL_OXY[nCOMP-1,j] += SLICE_OXY[nFRAG+k+1]*SLICE_SOM[nFRAG+k+1]/jj_CHEMKERNEL_SOM[nCOMP-1,j]
                    
    # OH-OXIDATION REACTION RATES:
    # ==================================================================
    kxA =  1.56341E-13
    kxB = -7.12119E-13
    kxC = -8.22305E-12
    kxD = -5.62410E-13
    kxE = -5.69473E-13
    kxF =  6.62928E-11

    j_kRXN = (kxA*dLVP + kxD)*(j_CSATLOG**2) + (kxB*dLVP + kxE)*j_CSATLOG + (kxC*dLVP + kxF)
    
    # WRITE THE ODE.f FILE:
    # ==================================================================
    with open('%s/source_ftn/gas/raws/raw.ode_bio.f90'%path,'r') as ff:
        lines = np.array([i.strip('\n') for i in ff.readlines()])

    # INDEX START:
    index = np.where(lines == '!FLAG1')[0][0] + 1
    
    # FOR SOM SPECIES:
    for j in reversed(range(nCOMP)):
        
        if j_CSATLOG[j] < 0:
            name = 'iCN%i'%(abs(j_CSATLOG[j]))
        else:
            name = 'iC%i'%(j_CSATLOG[j])
        
        # WRITE PRODUCTION:
        add   = '      p_BIO(%s) = &'%name
        lines = np.insert(lines,index,add); index += 1
        
        for k in range(nCOMP):
            add = '      %.2e*r_BIO(%i)'%(jj_CHEMKERNEL_SOM[j,nCOMP-1-k],k+1)
            
            if k != nCOMP - 1:
                add += ' + &'
            else:
                add += '\n'
            
            lines = np.insert(lines,index,add); index += 1

        # WRITE DEPLETION:
        add   = '      d_BIO(%s) = r_BIO(%i)\n'%(name,nCOMP-j)
        lines = np.insert(lines,index,add); index += 1
        
    # FOR OXYGEN SPECIES:
    for j in reversed(range(nCOMP)):
        
        # WRITE PRODUCTION:
        add   = '      p_OXY(%i) = &'%(nCOMP-j)
        lines = np.insert(lines,index,add); index += 1
        
        for k in range(nCOMP):
            add = '      %.2e*r_BIO(%i)'%(jj_CHEMKERNEL_SOM[j,nCOMP-1-k]*jj_CHEMKERNEL_OXY[j,nCOMP-1-k],k+1)
            
            if k != nCOMP - 1:
                add += ' + &'
            else:
                add += '\n'
            
            lines = np.insert(lines,index,add); index += 1

        # WRITE DEPLETION:
        add   = '      d_OXY(%i) = r_BIO(%i)*j_GASMOLE_OXY(%i)/ &'%(nCOMP-j,nCOMP-j,nCOMP-j)

        #add   = '      d_OXY(%i) = r_BIO(%i)*0.0 '%(nCOMP-j,nCOMP-j)
        
        lines = np.insert(lines,index,add); index += 1
        
        add   = '      MAX(j_GASMOLE_SOM(%i),j_GASMOLE_OXY(%i)+0.001)\n'%(nCOMP-j,nCOMP-j)

        lines = np.insert(lines,index,add); index += 1

    # WRITE:
    with open('%s/source_ftn/gas/ode_bio.f90'%path,'w') as ff:
        for line in lines:
            ff.write(line + '\n')

    with open('outputs/diag.ode_bio.f90','w') as ff:
        for line in lines:
            ff.write(line + '\n')

    # WRITE THE GASRATES.f FILE:
    # ==================================================================
    with open('%s/source_ftn/gas/raws/raw.gasrates_bio.f90'%path,'r') as ff:
        lines = np.array([i.strip('\n') for i in ff.readlines()])

    # INDEX START:
    index = np.where(lines == '!FLAG1')[0][0] + 1

    # OFFSET:
    OFF = 17
    
    for j in reversed(range(nCOMP)):
        
        if j_CSATLOG[j] < 0:
            name = 'iCN%i'%(abs(j_CSATLOG[j]))
        else:
            name = 'iC%i'%(j_CSATLOG[j])

        # WRITE RATES:
        add   = '      r_BIO(%i) = rk_BIO(%i)*S(iOH)*S(%s)'%(nCOMP-j,nCOMP-j+OFF-1,name)
        lines = np.insert(lines,index,add); index += 1
        
    # WRITE:
    with open('%s/source_ftn/gas/gasrates_bio.f90'%path,'w') as ff:
        for line in lines:
            ff.write(line + '\n')

    # WRITE THE GASRATECONSTANTS.f FILE:
    # ==================================================================
    with open('%s/source_ftn/gas/raws/raw.gasrateconstants_bio.f90'%path,'r') as ff:
        lines = np.array([i.strip('\n') for i in ff.readlines()])

    # INDEX START:
    index = np.where(lines == '!FLAG1')[0][0] + 1

    # OFFSET:
    OFF = 17

    for j in reversed(range(nCOMP)):
        
        # WRITE RATES:
        add   = '      rk_BIO(%i) = %.2e'%(nCOMP-j+OFF-1,j_kRXN[j])
        lines = np.insert(lines,index,add); index += 1
    
    # WRITE:
    with open('%s/source_ftn/gas/gasrateconstants_bio.f90'%path,'w') as ff:
        for line in lines:
            ff.write(line + '\n')
        
    # SPLIT THE PRECURSOR:
    # ==================================================================
    # PRECURSOR CSAT.:
    CSAT = ns.VOC['Csat']

    # INDEXEs:
    DEX_H = maxCSAT - round(CSAT)
    DEX_L = DEX_H + 1

    # FRACTIONs:
    FRAC_H = 1. - math.ceil(CSAT) + CSAT
    FRAC_L = 1. - FRAC_H
    
    # WRITE THE EQUATIONS FOR THE PRECURSOR:
    # ==================================================================
    # HOM ARRAY:
    HOMs = np.zeros(nCOMP); HOMs[-1] = pHOM    
    
    # COEFFICIENTs:
    COEFFs = (jj_CHEMKERNEL_SOM[:,DEX_H]*(1 - pHOM) + HOMs)*FRAC_H + \
             (jj_CHEMKERNEL_SOM[:,DEX_L]*(1 - pHOM) + HOMs)*FRAC_L
    
    # HOM OXY. ARRAY:
    HOM_OXYs = np.zeros(nCOMP); HOM_OXYs[-1] = 10.
    
    #print(pd.DataFrame(jj_CHEMKERNEL_OXY))
    #exit()

    # OXY. COEFFICIENTs:
    OXYs = (jj_CHEMKERNEL_OXY[:,DEX_H] + HOM_OXYs)*FRAC_H + \
           (jj_CHEMKERNEL_OXY[:,DEX_L] + HOM_OXYs)*FRAC_L
    
    # WRITE INPUTs:
    with open('../inputs/in.VOC_COEFF','w') as ff:
        ff.write((len(COEFFs)*'%.4f ')%tuple(COEFFs))
        
    with open('../inputs/in.VOC_OXY','w') as ff:
        ff.write((len(OXYs)*'%.4f ')%tuple(COEFFs*OXYs))
