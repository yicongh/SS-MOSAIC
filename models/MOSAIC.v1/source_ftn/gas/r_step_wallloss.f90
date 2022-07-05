!    ======================================================================================
!                       THIS SUBROUTINE STEPS FORWARD FOR VAPOR WALL LOSS
!    ======================================================================================

     SUBROUTINE STEP_WALLLOSS(kvap_on,VWL,mdt)
     
     USE mod_MAIN, ONLY: R8,nCOMP,CNN,kCN3
     USE mod_GAS,  ONLY: GAS_ON_WALL,j_GASMOLE_OXY,j_WLLMOLE_OXY
     
     IMPLICIT NONE

     REAL(R8) :: kvap_on
     INTEGER  :: VWL
     REAL(R8) :: mdt
     
     REAL(R8) :: dSOM(nCOMP)

     REAL(R8) :: CSAT(nCOMP)
     REAL(R8) :: CWALL(nCOMP)
     REAL(R8) :: kvap_ff(nCOMP)     
     REAL(R8),PARAMETER :: LOWER = -3

     REAL(R8) :: GAS_PREV(nCOMP)
     REAL(R8) :: WAL_PREV(nCOMP)

     REAL(R8) :: GAS_CURR(nCOMP)
     REAL(R8) :: WAL_CURR(nCOMP)
     
     INTEGER :: i,j,k

     IF (VWL.EQ.0) RETURN
     
!    WALL-EQUIVALENT OA MASS:
     DO j = 1,nCOMP
        CSAT(j) = 10.**(LOWER+j-1)

        IF (CSAT(j).GT.1e4) THEN
           CWALL(j) = 1e4
        ELSE IF (CSAT(j).GT.1e0) THEN
           CWALL(j) = 16.*(CSAT(j)**0.6)
        ELSE
           CWALL(j) = 16.
        END IF
     END DO
     
!    VWL REVERSE RATES : 
     kvap_ff = kvap_on*CSAT/CWALL

!    STEP FOR WALL LOSS:
     GAS_PREV(:) = CNN(kCN3:kCN3+nCOMP-1)
     WAL_PREV(:) = GAS_ON_WALL(:)

     GAS_CURR = GAS_PREV*EXP(-kvap_on*mdt) + WAL_PREV*(1. - EXP(-kvap_ff*mdt))
     WAL_CURR = WAL_PREV*EXP(-kvap_ff*mdt) + GAS_PREV*(1. - EXP(-kvap_on*mdt))


     DO j = 1,nCOMP

     CNN(kCN3+j-1)  = MAX(GAS_CURR(j),1e6)
     GAS_ON_WALL(j) = MAX(WAL_CURR(j),1e6)
     
     END DO

!    STEP FOR WALL LOSS - OXYGEN:
     GAS_PREV(:) = j_GASMOLE_OXY(:)
     WAL_PREV(:) = j_WLLMOLE_OXY(:)

     GAS_CURR = GAS_PREV*EXP(-kvap_on*mdt) + WAL_PREV*(1. - EXP(-kvap_ff*mdt))
     WAL_CURR = WAL_PREV*EXP(-kvap_ff*mdt) + GAS_PREV*(1. - EXP(-kvap_on*mdt))

     DO j = 1,nCOMP

     j_GASMOLE_OXY(j) = MAX(GAS_CURR(j),1e6)
     j_WLLMOLE_OXY(j) = MAX(WAL_CURR(j),1e6)
     
     END DO

!     WHERE (GAS_PREV.GT.100.)
!        j_GASMOLE_OXY = j_GASMOLE_OXY*GAS_CURR/GAS_PREV
!     ELSEWHERE
!        j_GASMOLE_OXY = 0.0
!     END WHERE
     
     RETURN
     END SUBROUTINE STEP_WALLLOSS
