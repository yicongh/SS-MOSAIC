  �(  H   k820309              19.0        A�`                                                                                                          
       source_ftn/aerosol/module_coag_3d.f90 MODULE_COAG_3D          @       �                                  
                                                           
                                                                                                             #         @                                                      #LUN    #STR              
                                                       
                                                    1 #         @                                                      #MOSAIC_COAG_3D_1BOX%NTOT_USED    #ISTAT_COAG 	   #IDIAGBB_IN 
   #ITSTEP    #DTCOAG_IN    #TEMP_BOX    #PATM_BOX    #RHOAIR_BOX    #RBOX    #FACT_APMASSMR    #FACT_APNUMBMR    #FACT_APDENS    #FACT_APDIAM    #ADRYDENS_BOX    #AWETDENS_BOX    #ADRYDPAV_BOX    #AWETDPAV_BOX    #ADRYQMAS_BOX                                                                                                                                                                                                                                                                                 
D                                 	                      
                                  
                     
  @                                                    
                                      
                
  @                                   
                
                                      
                
                                      
               
D                                                    
     p          5 r        5 r                                
                                      
                
                                      
                
                                      
                
                                      
                
D                                     �             
     p �        p �        p            p �        p                                    
D                                     �             
     p �        p �        p            p �        p                                    
D                                     �             
     p �        p �        p            p �        p                                    
D                                     �             
     p �        p �        p            p �        p                                    
D                                     �             
     p �        p �        p            p �        p                          #         @                                                      #NBIN    #NBIN_MAXD    #NTYPE    #NTYPE_MAXD    #NCOMP    #NCOMP_MAXD     #IFLAGAA !   #LUNOUT "   #DPDRY #   #NUM_DISTRIB $   #VOL_DISTRIB %   #SUM_NUM &   #SUM_VOL '             
                                                       
                                                       
                                                       
                                                       
                                                       
                                                        
                                  !                     
                                  "                    
                                 #                    
      p        5 � p        r    p          5 � p        r      5 � p        r        5 � p        r      5 � p        r                               
                                 $                    
      p        5 � p        r    p          5 � p        r      5 � p        r        5 � p        r      5 � p        r                               
                                 %                    
        p        5 � p        r    p        5 � p        r    p          5 � p        r      5 � p        r      5 � p        r         5 � p        r      5 � p        r      5 � p        r                                 
D                                &                   
     p          p            p                                   
D     �                           '                    
       p          5 � p        r     p         p        p           & p         5 � p        r       p              5 � p        r     p         p          p                          #         @                                  (                    #LUNOUT )   #NBIN *   #NBIN_MAXD +   #NTYPE ,   #NTYPE_MAXD -   #NCOMP .   #NCOMP_MAXD /   #TEMPK 0   #PRESS 1   #DELTAT 2   #NSUBSTEP_IN 3   #VPCUT 4   #VPDRY_IN 5   #DPDRY 6   #DPWET 7   #DENSDRY 8   #DENSWET 9   #NUM_DISTRIB :   #VOL_DISTRIB ;                                                   
                                  )                     
  @                               *                     
  @                               +                     
  @                               ,                     
  @                               -                     
                                  .                     
                                  /                     
  @                              0     
                
  @                              1     
                
                                 2     
                
                                  3                    
      �                           4                    
 "   p           & p         5 � p        r +         5 � p        r +   p         p                                   
                                 5                    
 !     p        5 � p        r +   p          5 � p        r +     5 � p        r -       5 � p        r +     5 � p        r -                              
                                 6                    
      p        5 � p        r +   p          5 � p        r +     5 � p        r -       5 � p        r +     5 � p        r -                              
  @                              7                    
      p        5 � p        r +   p          5 � p        r +     5 � p        r -       5 � p        r +     5 � p        r -                              
                                 8                    
      p        5 � p        r +   p          5 � p        r +     5 � p        r -       5 � p        r +     5 � p        r -                              
  @                              9                    
       p        5 � p        r +   p          5 � p        r +     5 � p        r -       5 � p        r +     5 � p        r -                              
D                                :                    
 #      p        5 � p        r +   p          5 � p        r +     5 � p        r -       5 � p        r +     5 � p        r -                              
D                                ;                    
 $        p        5 � p        r -   p        5 � p        r +   p          5 � p        r +     5 � p        r -     5 � p        r /       5 � p        r +     5 � p        r -     5 � p        r /                     #         @                                  <                    #NBIN =   #NBIN_MAXD >   #NTYPE ?   #NTYPE_MAXD @   #ITYPE A   #JTYPE B   #TEMPK C   #PRESS D   #DPWET E   #DENSWET F   #BCKERNEL G             
                                  =                     
                                  >                     
                                  ?                     
                                  @                     
                                  A                     
                                  B                     
                                 C     
                
                                 D     
               
                                 E                    
 .     p        5 � p        r >   p          5 � p        r >     5 � p        r @       5 � p        r >     5 � p        r @                              
                                 F                    
 /     p        5 � p        r >   p          5 � p        r >     5 � p        r @       5 � p        r >     5 � p        r @                              D                                G                    
 0      p        5 � p        r =   p          5 � p        r =     5 � p        r =       5 � p        r =     5 � p        r =                        �   =      fn#fn (   �   @   J   MODULE_DATA_MOSAIC_KIND       @   J   MODULE_PEG_UTIL +   ]  p       R8+MODULE_DATA_MOSAIC_KIND ,   �  Z       PEG_MESSAGE+MODULE_PEG_UTIL 0   '  @   a   PEG_MESSAGE%LUN+MODULE_PEG_UTIL 0   g  L   a   PEG_MESSAGE%STR+MODULE_PEG_UTIL $   �  K      MOSAIC_COAG_3D_1BOX F   �  @     MOSAIC_COAG_3D_1BOX%NTOT_USED+MODULE_DATA_MOSAIC_MAIN /   >  @   a   MOSAIC_COAG_3D_1BOX%ISTAT_COAG /   ~  @   a   MOSAIC_COAG_3D_1BOX%IDIAGBB_IN +   �  @   a   MOSAIC_COAG_3D_1BOX%ITSTEP .   �  @   a   MOSAIC_COAG_3D_1BOX%DTCOAG_IN -   >  @   a   MOSAIC_COAG_3D_1BOX%TEMP_BOX -   ~  @   a   MOSAIC_COAG_3D_1BOX%PATM_BOX /   �  @   a   MOSAIC_COAG_3D_1BOX%RHOAIR_BOX )   �  �   a   MOSAIC_COAG_3D_1BOX%RBOX 2   �  @   a   MOSAIC_COAG_3D_1BOX%FACT_APMASSMR 2   �  @   a   MOSAIC_COAG_3D_1BOX%FACT_APNUMBMR 0     @   a   MOSAIC_COAG_3D_1BOX%FACT_APDENS 0   R  @   a   MOSAIC_COAG_3D_1BOX%FACT_APDIAM 1   �  �   a   MOSAIC_COAG_3D_1BOX%ADRYDENS_BOX 1   F	  �   a   MOSAIC_COAG_3D_1BOX%AWETDENS_BOX 1   �	  �   a   MOSAIC_COAG_3D_1BOX%ADRYDPAV_BOX 1   �
  �   a   MOSAIC_COAG_3D_1BOX%AWETDPAV_BOX 1   b  �   a   MOSAIC_COAG_3D_1BOX%ADRYQMAS_BOX '     �       COAG_3D_CONSERVE_CHECK ,     @   a   COAG_3D_CONSERVE_CHECK%NBIN 1   M  @   a   COAG_3D_CONSERVE_CHECK%NBIN_MAXD -   �  @   a   COAG_3D_CONSERVE_CHECK%NTYPE 2   �  @   a   COAG_3D_CONSERVE_CHECK%NTYPE_MAXD -     @   a   COAG_3D_CONSERVE_CHECK%NCOMP 2   M  @   a   COAG_3D_CONSERVE_CHECK%NCOMP_MAXD /   �  @   a   COAG_3D_CONSERVE_CHECK%IFLAGAA .   �  @   a   COAG_3D_CONSERVE_CHECK%LUNOUT -     $  a   COAG_3D_CONSERVE_CHECK%DPDRY 3   1  $  a   COAG_3D_CONSERVE_CHECK%NUM_DISTRIB 3   U  �  a   COAG_3D_CONSERVE_CHECK%VOL_DISTRIB /   �  �   a   COAG_3D_CONSERVE_CHECK%SUM_NUM /   }  T  a   COAG_3D_CONSERVE_CHECK%SUM_VOL %   �  g      MOVINGCENTER_COAG_3D ,   8  @   a   MOVINGCENTER_COAG_3D%LUNOUT *   x  @   a   MOVINGCENTER_COAG_3D%NBIN /   �  @   a   MOVINGCENTER_COAG_3D%NBIN_MAXD +   �  @   a   MOVINGCENTER_COAG_3D%NTYPE 0   8  @   a   MOVINGCENTER_COAG_3D%NTYPE_MAXD +   x  @   a   MOVINGCENTER_COAG_3D%NCOMP 0   �  @   a   MOVINGCENTER_COAG_3D%NCOMP_MAXD +   �  @   a   MOVINGCENTER_COAG_3D%TEMPK +   8  @   a   MOVINGCENTER_COAG_3D%PRESS ,   x  @   a   MOVINGCENTER_COAG_3D%DELTAT 1   �  @   a   MOVINGCENTER_COAG_3D%NSUBSTEP_IN +   �  �   a   MOVINGCENTER_COAG_3D%VPCUT .   �  $  a   MOVINGCENTER_COAG_3D%VPDRY_IN +      $  a   MOVINGCENTER_COAG_3D%DPDRY +   $  $  a   MOVINGCENTER_COAG_3D%DPWET -   H  $  a   MOVINGCENTER_COAG_3D%DENSDRY -   l  $  a   MOVINGCENTER_COAG_3D%DENSWET 1   �  $  a   MOVINGCENTER_COAG_3D%NUM_DISTRIB 1   �   �  a   MOVINGCENTER_COAG_3D%VOL_DISTRIB &   H"  �       BROWNIAN_KERNEL_3DSUB +   #  @   a   BROWNIAN_KERNEL_3DSUB%NBIN 0   V#  @   a   BROWNIAN_KERNEL_3DSUB%NBIN_MAXD ,   �#  @   a   BROWNIAN_KERNEL_3DSUB%NTYPE 1   �#  @   a   BROWNIAN_KERNEL_3DSUB%NTYPE_MAXD ,   $  @   a   BROWNIAN_KERNEL_3DSUB%ITYPE ,   V$  @   a   BROWNIAN_KERNEL_3DSUB%JTYPE ,   �$  @   a   BROWNIAN_KERNEL_3DSUB%TEMPK ,   �$  @   a   BROWNIAN_KERNEL_3DSUB%PRESS ,   %  $  a   BROWNIAN_KERNEL_3DSUB%DPWET .   :&  $  a   BROWNIAN_KERNEL_3DSUB%DENSWET /   ^'  $  a   BROWNIAN_KERNEL_3DSUB%BCKERNEL 