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
      1.32e-02*r_BIO(1) + &
      1.89e-02*r_BIO(2) + &
      2.52e-02*r_BIO(3) + &
      2.88e-02*r_BIO(4) + &
      3.69e-02*r_BIO(5) + &
      3.77e-02*r_BIO(6) + &
      4.34e-02*r_BIO(7) + &
      2.75e-02*r_BIO(8) + &
      2.66e-02*r_BIO(9) + &
      0.00e+00*r_BIO(10) + &
      0.00e+00*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_BIO(iCN3) = r_BIO(1)

      p_BIO(iCN2) = &
      2.52e-01*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      1.96e-03*r_BIO(3) + &
      7.33e-03*r_BIO(4) + &
      4.34e-03*r_BIO(5) + &
      1.53e-02*r_BIO(6) + &
      1.07e-02*r_BIO(7) + &
      3.48e-02*r_BIO(8) + &
      1.28e-02*r_BIO(9) + &
      3.82e-02*r_BIO(10) + &
      0.00e+00*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_BIO(iCN2) = r_BIO(2)

      p_BIO(iCN1) = &
      2.52e-01*r_BIO(1) + &
      2.51e-01*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      2.81e-03*r_BIO(4) + &
      1.05e-02*r_BIO(5) + &
      6.23e-03*r_BIO(6) + &
      2.19e-02*r_BIO(7) + &
      1.53e-02*r_BIO(8) + &
      4.99e-02*r_BIO(9) + &
      1.83e-02*r_BIO(10) + &
      5.48e-02*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_BIO(iCN1) = r_BIO(3)

      p_BIO(iC0) = &
      0.00e+00*r_BIO(1) + &
      2.51e-01*r_BIO(2) + &
      2.48e-01*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      4.04e-03*r_BIO(5) + &
      1.51e-02*r_BIO(6) + &
      8.93e-03*r_BIO(7) + &
      3.14e-02*r_BIO(8) + &
      2.20e-02*r_BIO(9) + &
      7.15e-02*r_BIO(10) + &
      2.62e-02*r_BIO(11) + &
      7.86e-02*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_BIO(iC0) = r_BIO(4)

      p_BIO(iC1) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      2.48e-01*r_BIO(3) + &
      2.45e-01*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      5.79e-03*r_BIO(6) + &
      2.16e-02*r_BIO(7) + &
      1.28e-02*r_BIO(8) + &
      4.51e-02*r_BIO(9) + &
      3.16e-02*r_BIO(10) + &
      1.03e-01*r_BIO(11) + &
      3.76e-02*r_BIO(12) + &
      1.13e-01*r_BIO(13)

      d_BIO(iC1) = r_BIO(5)

      p_BIO(iC2) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      2.45e-01*r_BIO(4) + &
      2.41e-01*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      8.30e-03*r_BIO(7) + &
      3.10e-02*r_BIO(8) + &
      1.84e-02*r_BIO(9) + &
      6.47e-02*r_BIO(10) + &
      4.53e-02*r_BIO(11) + &
      1.47e-01*r_BIO(12) + &
      5.40e-02*r_BIO(13)

      d_BIO(iC2) = r_BIO(6)

      p_BIO(iC3) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      2.41e-01*r_BIO(5) + &
      2.35e-01*r_BIO(6) + &
      0.00e+00*r_BIO(7) + &
      1.19e-02*r_BIO(8) + &
      4.45e-02*r_BIO(9) + &
      2.64e-02*r_BIO(10) + &
      9.27e-02*r_BIO(11) + &
      6.49e-02*r_BIO(12) + &
      2.11e-01*r_BIO(13)

      d_BIO(iC3) = r_BIO(7)

      p_BIO(iC4) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      2.35e-01*r_BIO(6) + &
      2.26e-01*r_BIO(7) + &
      0.00e+00*r_BIO(8) + &
      1.71e-02*r_BIO(9) + &
      6.38e-02*r_BIO(10) + &
      3.78e-02*r_BIO(11) + &
      1.33e-01*r_BIO(12) + &
      9.31e-02*r_BIO(13)

      d_BIO(iC4) = r_BIO(8)

      p_BIO(iC5) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      2.26e-01*r_BIO(7) + &
      2.13e-01*r_BIO(8) + &
      0.00e+00*r_BIO(9) + &
      2.45e-02*r_BIO(10) + &
      9.16e-02*r_BIO(11) + &
      5.42e-02*r_BIO(12) + &
      1.91e-01*r_BIO(13)

      d_BIO(iC5) = r_BIO(9)

      p_BIO(iC6) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      0.00e+00*r_BIO(7) + &
      2.13e-01*r_BIO(8) + &
      1.95e-01*r_BIO(9) + &
      0.00e+00*r_BIO(10) + &
      3.51e-02*r_BIO(11) + &
      1.31e-01*r_BIO(12) + &
      7.78e-02*r_BIO(13)

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
      1.95e-01*r_BIO(9) + &
      1.69e-01*r_BIO(10) + &
      0.00e+00*r_BIO(11) + &
      5.04e-02*r_BIO(12) + &
      1.88e-01*r_BIO(13)

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
      1.69e-01*r_BIO(10) + &
      1.31e-01*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      7.23e-02*r_BIO(13)

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
      1.31e-01*r_BIO(11) + &
      7.73e-02*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_BIO(iC9) = r_BIO(13)

      p_OXY(1) = &
      6.38e-02*r_BIO(1) + &
      9.15e-02*r_BIO(2) + &
      1.15e-01*r_BIO(3) + &
      1.81e-01*r_BIO(4) + &
      1.99e-01*r_BIO(5) + &
      2.51e-01*r_BIO(6) + &
      2.18e-01*r_BIO(7) + &
      1.84e-01*r_BIO(8) + &
      1.06e-01*r_BIO(9) + &
      0.00e+00*r_BIO(10) + &
      0.00e+00*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_OXY(1) = r_BIO(1)*j_GASMOLE_OXY(1)/ &
      MAX(j_GASMOLE_SOM(1),j_GASMOLE_OXY(1)+0.001)

      p_OXY(2) = &
      2.52e-01*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      1.96e-03*r_BIO(3) + &
      7.51e-03*r_BIO(4) + &
      8.69e-03*r_BIO(5) + &
      3.10e-02*r_BIO(6) + &
      3.21e-02*r_BIO(7) + &
      1.05e-01*r_BIO(8) + &
      5.10e-02*r_BIO(9) + &
      1.53e-01*r_BIO(10) + &
      0.00e+00*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_OXY(2) = r_BIO(2)*j_GASMOLE_OXY(2)/ &
      MAX(j_GASMOLE_SOM(2),j_GASMOLE_OXY(2)+0.001)

      p_OXY(3) = &
      2.52e-01*r_BIO(1) + &
      2.51e-01*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      2.81e-03*r_BIO(4) + &
      1.08e-02*r_BIO(5) + &
      1.25e-02*r_BIO(6) + &
      4.45e-02*r_BIO(7) + &
      4.60e-02*r_BIO(8) + &
      1.50e-01*r_BIO(9) + &
      7.32e-02*r_BIO(10) + &
      2.19e-01*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_OXY(3) = r_BIO(3)*j_GASMOLE_OXY(3)/ &
      MAX(j_GASMOLE_SOM(3),j_GASMOLE_OXY(3)+0.001)

      p_OXY(4) = &
      0.00e+00*r_BIO(1) + &
      2.51e-01*r_BIO(2) + &
      2.48e-01*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      4.04e-03*r_BIO(5) + &
      1.54e-02*r_BIO(6) + &
      1.79e-02*r_BIO(7) + &
      6.38e-02*r_BIO(8) + &
      6.60e-02*r_BIO(9) + &
      2.16e-01*r_BIO(10) + &
      1.05e-01*r_BIO(11) + &
      3.14e-01*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_OXY(4) = r_BIO(4)*j_GASMOLE_OXY(4)/ &
      MAX(j_GASMOLE_SOM(4),j_GASMOLE_OXY(4)+0.001)

      p_OXY(5) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      2.48e-01*r_BIO(3) + &
      2.45e-01*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      5.79e-03*r_BIO(6) + &
      2.21e-02*r_BIO(7) + &
      2.56e-02*r_BIO(8) + &
      9.15e-02*r_BIO(9) + &
      9.47e-02*r_BIO(10) + &
      3.09e-01*r_BIO(11) + &
      1.51e-01*r_BIO(12) + &
      4.51e-01*r_BIO(13)

      d_OXY(5) = r_BIO(5)*j_GASMOLE_OXY(5)/ &
      MAX(j_GASMOLE_SOM(5),j_GASMOLE_OXY(5)+0.001)

      p_OXY(6) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      2.45e-01*r_BIO(4) + &
      2.41e-01*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      8.30e-03*r_BIO(7) + &
      3.18e-02*r_BIO(8) + &
      3.68e-02*r_BIO(9) + &
      1.31e-01*r_BIO(10) + &
      1.36e-01*r_BIO(11) + &
      4.44e-01*r_BIO(12) + &
      2.16e-01*r_BIO(13)

      d_OXY(6) = r_BIO(6)*j_GASMOLE_OXY(6)/ &
      MAX(j_GASMOLE_SOM(6),j_GASMOLE_OXY(6)+0.001)

      p_OXY(7) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      2.41e-01*r_BIO(5) + &
      2.35e-01*r_BIO(6) + &
      0.00e+00*r_BIO(7) + &
      1.19e-02*r_BIO(8) + &
      4.56e-02*r_BIO(9) + &
      5.27e-02*r_BIO(10) + &
      1.88e-01*r_BIO(11) + &
      1.95e-01*r_BIO(12) + &
      6.37e-01*r_BIO(13)

      d_OXY(7) = r_BIO(7)*j_GASMOLE_OXY(7)/ &
      MAX(j_GASMOLE_SOM(7),j_GASMOLE_OXY(7)+0.001)

      p_OXY(8) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      2.35e-01*r_BIO(6) + &
      2.26e-01*r_BIO(7) + &
      0.00e+00*r_BIO(8) + &
      1.71e-02*r_BIO(9) + &
      6.53e-02*r_BIO(10) + &
      7.56e-02*r_BIO(11) + &
      2.70e-01*r_BIO(12) + &
      2.79e-01*r_BIO(13)

      d_OXY(8) = r_BIO(8)*j_GASMOLE_OXY(8)/ &
      MAX(j_GASMOLE_SOM(8),j_GASMOLE_OXY(8)+0.001)

      p_OXY(9) = &
      0.00e+00*r_BIO(1) + &
      0.00e+00*r_BIO(2) + &
      0.00e+00*r_BIO(3) + &
      0.00e+00*r_BIO(4) + &
      0.00e+00*r_BIO(5) + &
      0.00e+00*r_BIO(6) + &
      2.26e-01*r_BIO(7) + &
      2.13e-01*r_BIO(8) + &
      0.00e+00*r_BIO(9) + &
      2.45e-02*r_BIO(10) + &
      9.37e-02*r_BIO(11) + &
      1.08e-01*r_BIO(12) + &
      3.87e-01*r_BIO(13)

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
      2.13e-01*r_BIO(8) + &
      1.95e-01*r_BIO(9) + &
      0.00e+00*r_BIO(10) + &
      3.51e-02*r_BIO(11) + &
      1.34e-01*r_BIO(12) + &
      1.56e-01*r_BIO(13)

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
      1.95e-01*r_BIO(9) + &
      1.69e-01*r_BIO(10) + &
      0.00e+00*r_BIO(11) + &
      5.04e-02*r_BIO(12) + &
      1.93e-01*r_BIO(13)

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
      1.69e-01*r_BIO(10) + &
      1.31e-01*r_BIO(11) + &
      0.00e+00*r_BIO(12) + &
      7.23e-02*r_BIO(13)

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
      1.31e-01*r_BIO(11) + &
      7.73e-02*r_BIO(12) + &
      0.00e+00*r_BIO(13)

      d_OXY(13) = r_BIO(13)*j_GASMOLE_OXY(13)/ &
      MAX(j_GASMOLE_SOM(13),j_GASMOLE_OXY(13)+0.001)

      

      p_BIO(iOH)= 0.0
      d_BIO(iOH)= 0.0

      p_BIO(iISOP) = 0.0
      d_BIO(iISOP) = r_BIO(14)

      RETURN
      END SUBROUTINE ODE_BIO






