!     ==========================================================================
!                 THIS SUBROUTINE DEFINES ODEs FOR BIO-REGIME CHEMISTRY
!     ==========================================================================

      SUBROUTINE ODE_BIO
  
      USE mod_MAIN
      USE mod_GAS

      IMPLICIT NONE
      
      REAL(R8) :: OXY_AVE

!FLAG1
      p_BIO(iCN3) = &
      5.48e-02*r_BIO(1) + &
      6.98e-02*r_BIO(2) + &
      8.89e-02*r_BIO(3) + &
      1.11e-01*r_BIO(4) + &
      1.08e-01*r_BIO(5) + &
      4.31e-02*r_BIO(6) + &
      3.64e-02*r_BIO(7) + &
      2.93e-04*r_BIO(8) + &
      2.38e-04*r_BIO(9) + &
      0.00e+00*r_BIO(10) + &
      0.00e+00*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_BIO(iCN3) = r_BIO(1)

      p_BIO(iCN2) = &
      4.70e-03*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      5.24e-05*r_BIO(3) + &
      1.80e-03*r_BIO(4) + &
      3.37e-02*r_BIO(5) + &
      9.47e-02*r_BIO(6) + &
      1.85e-02*r_BIO(7) + &
      4.60e-02*r_BIO(8) + &
      1.36e-04*r_BIO(9) + &
      3.03e-04*r_BIO(10) + &
      0.00e+00*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_BIO(iCN2) = r_BIO(2)

      p_BIO(iCN1) = &
      4.70e-03*r_BIO(1) + &
      4.63e-03*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      6.67e-05*r_BIO(4) + &
      2.30e-03*r_BIO(5) + &
      4.30e-02*r_BIO(6) + &
      1.21e-01*r_BIO(7) + &
      2.36e-02*r_BIO(8) + &
      5.86e-02*r_BIO(9) + &
      1.73e-04*r_BIO(10) + &
      3.86e-04*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_BIO(iCN1) = r_BIO(3)

      p_BIO(iC0) = &
      0.00e+00*r_BIO(1) + &
      4.63e-03*r_BIO(2) + &
      4.53e-03*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      8.50e-05*r_BIO(5) + &
      2.93e-03*r_BIO(6) + &
      5.47e-02*r_BIO(7) + &
      1.54e-01*r_BIO(8) + &
      3.00e-02*r_BIO(9) + &
      7.47e-02*r_BIO(10) + &
      2.21e-04*r_BIO(11) + &
      4.92e-04*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_BIO(iC0) = r_BIO(4)

      p_BIO(iC1) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      4.53e-03*r_BIO(3) + &
      4.41e-03*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      1.08e-04*r_BIO(6) + &
      3.73e-03*r_BIO(7) + &
      6.97e-02*r_BIO(8) + &
      1.96e-01*r_BIO(9) + &
      3.82e-02*r_BIO(10) + &
      9.52e-02*r_BIO(11) + &
      2.81e-04*r_BIO(12) + &
      6.26e-04*r_BIO(13)

      d_BIO(iC1) = r_BIO(5)

      p_BIO(iC2) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      4.41e-03*r_BIO(4) + &
      4.26e-03*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      1.38e-04*r_BIO(7) + &
      4.75e-03*r_BIO(8) + &
      8.88e-02*r_BIO(9) + &
      2.49e-01*r_BIO(10) + &
      4.87e-02*r_BIO(11) + &
      1.21e-01*r_BIO(12) + &
      3.58e-04*r_BIO(13)

      d_BIO(iC2) = r_BIO(6)

      p_BIO(iC3) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      4.26e-03*r_BIO(5) + &
      4.06e-03*r_BIO(6) + &
      0.00e+00*r_BIO(7) + &
      1.76e-04*r_BIO(8) + &
      6.05e-03*r_BIO(9) + &
      1.13e-01*r_BIO(10) + &
      3.18e-01*r_BIO(11) + &
      6.20e-02*r_BIO(12) + &
      1.54e-01*r_BIO(13)

      d_BIO(iC3) = r_BIO(7)

      p_BIO(iC4) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      4.06e-03*r_BIO(6) + &
      3.81e-03*r_BIO(7) + &
      0.00e+00*r_BIO(8) + &
      2.24e-04*r_BIO(9) + &
      7.71e-03*r_BIO(10) + &
      1.44e-01*r_BIO(11) + &
      4.05e-01*r_BIO(12) + &
      7.90e-02*r_BIO(13)

      d_BIO(iC4) = r_BIO(8)

      p_BIO(iC5) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      3.81e-03*r_BIO(7) + &
      3.49e-03*r_BIO(8) + &
      0.00e+00*r_BIO(9) + &
      2.85e-04*r_BIO(10) + &
      9.82e-03*r_BIO(11) + &
      1.84e-01*r_BIO(12) + &
      5.15e-01*r_BIO(13)

      d_BIO(iC5) = r_BIO(9)

      p_BIO(iC6) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      0.00e+00*r_BIO(7) + &
      3.49e-03*r_BIO(8) + &
      3.09e-03*r_BIO(9) + &
      0.00e+00*r_BIO(10) + &
      3.63e-04*r_BIO(11) + &
      1.25e-02*r_BIO(12) + &
      2.34e-01*r_BIO(13)

      d_BIO(iC6) = r_BIO(10)

      p_BIO(iC7) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      0.00e+00*r_BIO(7) + &
      0.00e+00*r_BIO(8) + &
      3.09e-03*r_BIO(9) + &
      2.57e-03*r_BIO(10) + &
      0.00e+00*r_BIO(11) + &
      4.63e-04*r_BIO(12) + &
      1.59e-02*r_BIO(13)

      d_BIO(iC7) = r_BIO(11)

      p_BIO(iC8) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      0.00e+00*r_BIO(7) + &
      0.00e+00*r_BIO(8) + &
      0.00e+00*r_BIO(9) + &
      2.57e-03*r_BIO(10) + &
      1.91e-03*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      5.89e-04*r_BIO(13)

      d_BIO(iC8) = r_BIO(12)

      p_BIO(iC9) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      0.00e+00*r_BIO(7) + &
      0.00e+00*r_BIO(8) + &
      0.00e+00*r_BIO(9) + &
      0.00e+00*r_BIO(10) + &
      1.91e-03*r_BIO(11) + &
      1.07e-03*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_BIO(iC9) = r_BIO(13)

      p_OXY(1) = &
      3.73e-01*r_BIO(1) + &
      4.75e-01*r_BIO(2) + &
      5.23e-01*r_BIO(3) + &
      4.62e-01*r_BIO(4) + &
      3.28e-01*r_BIO(5) + &
      2.15e-01*r_BIO(6) + &
      1.10e-01*r_BIO(7) + &
      1.92e-03*r_BIO(8) + &
      9.52e-04*r_BIO(9) + &
      0.00e+00*r_BIO(10) + &
      0.00e+00*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_OXY(1) = r_BIO(1)*j_GASMOLE_OXY(1)/ &
      MAX(j_GASMOLE_SOM(1),j_GASMOLE_OXY(1)+0.001)

      p_OXY(2) = &
      4.70e-03*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      5.24e-05*r_BIO(3) + &
      3.45e-03*r_BIO(4) + &
      6.75e-02*r_BIO(5) + &
      1.90e-01*r_BIO(6) + &
      5.55e-02*r_BIO(7) + &
      1.38e-01*r_BIO(8) + &
      5.44e-04*r_BIO(9) + &
      1.21e-03*r_BIO(10) + &
      0.00e+00*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_OXY(2) = r_BIO(2)*j_GASMOLE_OXY(2)/ &
      MAX(j_GASMOLE_SOM(2),j_GASMOLE_OXY(2)+0.001)

      p_OXY(3) = &
      4.70e-03*r_BIO(1) + &
      4.63e-03*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      6.67e-05*r_BIO(4) + &
      4.39e-03*r_BIO(5) + &
      8.59e-02*r_BIO(6) + &
      2.43e-01*r_BIO(7) + &
      7.07e-02*r_BIO(8) + &
      1.76e-01*r_BIO(9) + &
      6.92e-04*r_BIO(10) + &
      1.54e-03*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_OXY(3) = r_BIO(3)*j_GASMOLE_OXY(3)/ &
      MAX(j_GASMOLE_SOM(3),j_GASMOLE_OXY(3)+0.001)

      p_OXY(4) = &
      0.00e+00*r_BIO(1) + &
      4.63e-03*r_BIO(2) + &
      4.53e-03*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      8.50e-05*r_BIO(5) + &
      5.59e-03*r_BIO(6) + &
      1.09e-01*r_BIO(7) + &
      3.09e-01*r_BIO(8) + &
      9.00e-02*r_BIO(9) + &
      2.24e-01*r_BIO(10) + &
      8.82e-04*r_BIO(11) + &
      1.97e-03*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_OXY(4) = r_BIO(4)*j_GASMOLE_OXY(4)/ &
      MAX(j_GASMOLE_SOM(4),j_GASMOLE_OXY(4)+0.001)

      p_OXY(5) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      4.53e-03*r_BIO(3) + &
      4.41e-03*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      1.08e-04*r_BIO(6) + &
      7.12e-03*r_BIO(7) + &
      1.39e-01*r_BIO(8) + &
      3.94e-01*r_BIO(9) + &
      1.15e-01*r_BIO(10) + &
      2.85e-01*r_BIO(11) + &
      1.12e-03*r_BIO(12) + &
      2.51e-03*r_BIO(13)

      d_OXY(5) = r_BIO(5)*j_GASMOLE_OXY(5)/ &
      MAX(j_GASMOLE_SOM(5),j_GASMOLE_OXY(5)+0.001)

      p_OXY(6) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      4.41e-03*r_BIO(4) + &
      4.26e-03*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      1.38e-04*r_BIO(7) + &
      9.08e-03*r_BIO(8) + &
      1.78e-01*r_BIO(9) + &
      5.01e-01*r_BIO(10) + &
      1.46e-01*r_BIO(11) + &
      3.64e-01*r_BIO(12) + &
      1.43e-03*r_BIO(13)

      d_OXY(6) = r_BIO(6)*j_GASMOLE_OXY(6)/ &
      MAX(j_GASMOLE_SOM(6),j_GASMOLE_OXY(6)+0.001)

      p_OXY(7) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      4.26e-03*r_BIO(5) + &
      4.06e-03*r_BIO(6) + &
      0.00e+00*r_BIO(7) + &
      1.76e-04*r_BIO(8) + &
      1.16e-02*r_BIO(9) + &
      2.26e-01*r_BIO(10) + &
      6.39e-01*r_BIO(11) + &
      1.86e-01*r_BIO(12) + &
      4.63e-01*r_BIO(13)

      d_OXY(7) = r_BIO(7)*j_GASMOLE_OXY(7)/ &
      MAX(j_GASMOLE_SOM(7),j_GASMOLE_OXY(7)+0.001)

      p_OXY(8) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      4.06e-03*r_BIO(6) + &
      3.81e-03*r_BIO(7) + &
      0.00e+00*r_BIO(8) + &
      2.24e-04*r_BIO(9) + &
      1.47e-02*r_BIO(10) + &
      2.88e-01*r_BIO(11) + &
      8.13e-01*r_BIO(12) + &
      2.37e-01*r_BIO(13)

      d_OXY(8) = r_BIO(8)*j_GASMOLE_OXY(8)/ &
      MAX(j_GASMOLE_SOM(8),j_GASMOLE_OXY(8)+0.001)

      p_OXY(9) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      3.81e-03*r_BIO(7) + &
      3.49e-03*r_BIO(8) + &
      0.00e+00*r_BIO(9) + &
      2.85e-04*r_BIO(10) + &
      1.88e-02*r_BIO(11) + &
      3.67e-01*r_BIO(12) + &
      1.04e+00*r_BIO(13)

      d_OXY(9) = r_BIO(9)*j_GASMOLE_OXY(9)/ &
      MAX(j_GASMOLE_SOM(9),j_GASMOLE_OXY(9)+0.001)

      p_OXY(10) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      0.00e+00*r_BIO(7) + &
      3.49e-03*r_BIO(8) + &
      3.09e-03*r_BIO(9) + &
      0.00e+00*r_BIO(10) + &
      3.63e-04*r_BIO(11) + &
      2.39e-02*r_BIO(12) + &
      4.68e-01*r_BIO(13)

      d_OXY(10) = r_BIO(10)*j_GASMOLE_OXY(10)/ &
      MAX(j_GASMOLE_SOM(10),j_GASMOLE_OXY(10)+0.001)

      p_OXY(11) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      0.00e+00*r_BIO(7) + &
      0.00e+00*r_BIO(8) + &
      3.09e-03*r_BIO(9) + &
      2.57e-03*r_BIO(10) + &
      0.00e+00*r_BIO(11) + &
      4.63e-04*r_BIO(12) + &
      3.04e-02*r_BIO(13)

      d_OXY(11) = r_BIO(11)*j_GASMOLE_OXY(11)/ &
      MAX(j_GASMOLE_SOM(11),j_GASMOLE_OXY(11)+0.001)

      p_OXY(12) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      0.00e+00*r_BIO(7) + &
      0.00e+00*r_BIO(8) + &
      0.00e+00*r_BIO(9) + &
      2.57e-03*r_BIO(10) + &
      1.91e-03*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      5.89e-04*r_BIO(13)

      d_OXY(12) = r_BIO(12)*j_GASMOLE_OXY(12)/ &
      MAX(j_GASMOLE_SOM(12),j_GASMOLE_OXY(12)+0.001)

      p_OXY(13) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      0.00e+00*r_BIO(7) + &
      0.00e+00*r_BIO(8) + &
      0.00e+00*r_BIO(9) + &
      0.00e+00*r_BIO(10) + &
      1.91e-03*r_BIO(11) + &
      1.07e-03*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_OXY(13) = r_BIO(13)*j_GASMOLE_OXY(13)/ &
      MAX(j_GASMOLE_SOM(13),j_GASMOLE_OXY(13)+0.001)

      

      p_BIO(iOH)= 0.0
      d_BIO(iOH)= 0.0

      p_BIO(iISOP) = 0.0
      d_BIO(iISOP) = r_BIO(14)

      RETURN
      END SUBROUTINE ODE_BIO






