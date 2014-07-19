*
* $Id: ihwphi.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihwphi.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/00 13/04/93  17.53.44  by  O.Couet
*-- Author :
      SUBROUTINE IHWPHI(IOPT,KPHI,APHI,IPHI1,IPHI2)
************************************************************************
*                                                                      *
*     IHWPHI                                     Date:    29.03.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Find critical PHI sectors                              *
*                                                                      *
*     Input: IOPT    - options: 1 - from BACK to FRONT 'BF'            *
*                               2 - from FRONT to BACK 'FB'            *
*            KPHI    - number of phi sectors                           *
*            APHI(*) - PHI separatrices                                *
*                                                                      *
*     Output: IPHI1  - initial sector                                  *
*             IPHI2  - final sector                                    *
*                                                                      *
************************************************************************
#include "higz/hcscrn.inc"
      REAL              APHI(*)
      INTEGER           IPHI(2)
*-
      RAD    = ATAN(1.)*4./180.
      IF (APHI(KPHI+1) .EQ. APHI(1)) APHI(KPHI+1) = APHI(KPHI+1) + 360.
      DPHI   = ABS(APHI(KPHI+1) - APHI(1))
      IF (DPHI .NE. 360.) THEN
        APHI(KPHI+2) = (APHI(1) + APHI(KPHI+1))/2. + 180.
        APHI(KPHI+3) = APHI(1) + 360.
        KPHI   = KPHI + 2
      END IF
*
**       F I N D   C R I T I C A L   S E C T O R S
*
      K      = 0
      DO 100 I=1,KPHI
        PHI1   = RAD*APHI(I)
        PHI2   = RAD*APHI(I+1)
        X1     = TN(1,1)*COS(PHI1) + TN(2,1)*SIN(PHI1)
        X2     = TN(1,1)*COS(PHI2) + TN(2,1)*SIN(PHI2)
        IF (X1.GE.0. .AND. X2.GT.0.) GOTO 100
        IF (X1.LE.0. .AND. X2.LT.0.) GOTO 100
        K      = K + 1
        IF (K .EQ. 3)           GOTO 998
        IPHI(K)= I
  100 CONTINUE
      IF (K .NE. 2)             GOTO 998
*
**       F I N D   O R D E R   O F   C R I T I C A L   S E C T O R S
*
      PHI1   = RAD * (APHI(IPHI(1)) + APHI(IPHI(1)+1)) / 2.
      PHI2   = RAD * (APHI(IPHI(2)) + APHI(IPHI(2)+1)) / 2.
      Z1     = TN(1,3)*COS(PHI1) + TN(2,3)*SIN(PHI1)
      Z2     = TN(1,3)*COS(PHI2) + TN(2,3)*SIN(PHI2)
      IF ((Z1.LE.Z2 .AND. IOPT.EQ.1).OR.(Z1.GT.Z2 .AND. IOPT.EQ.2)) THEN
        IPHI1 = IPHI(1)
        IPHI2 = IPHI(2)
      ELSE
        IPHI1 = IPHI(2)
        IPHI2 = IPHI(1)
      END IF
      GOTO 999
*
  998 WRITE(*,*)
     &  'IHWPHI: Something strange: num. of critical sectors .NE. 2'
      IPHI1  = 1
      IPHI2  = 2
  999 RETURN
      END
