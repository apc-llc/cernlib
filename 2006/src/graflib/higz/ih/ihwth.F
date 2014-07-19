*
* $Id: ihwth.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihwth.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/00 20/04/93  14.20.34  by  O.Couet
*-- Author :
      SUBROUTINE IHWTH(IOPT,PHI,KTH,ATH,ITH1,ITH2)
************************************************************************
*                                                                      *
*     IHWTH                                      Date:    07.04.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Find critical THETA sectors for given PHI sector       *
*                                                                      *
*     Input: IOPT        - options: 1 - from BACK to FRONT 'BF'        *
*                                   2 - from FRONT to BACK 'FB'        *
*            PHI         - PHI sector                                  *
*            KTH         - number of THETA sectors                     *
*            ATH(*)      - THETA separatrices                          *
*                                                                      *
*     Output: ITH1  - initial sector                                   *
*             ITH2  - final sector                                     *
*                                                                      *
************************************************************************
#include "higz/hcscrn.inc"
      REAL              ATH(*)
      INTEGER           ITH(2)
*-
      RAD    = ATAN(1.)*4./180.
      DTH    = ABS(ATH(KTH+1) - ATH(1))
      IF (DTH .NE. 360.) THEN
        ATH(KTH+2) = (ATH(1) + ATH(KTH+1))/2. + 180.
        ATH(KTH+3) = ATH(1) + 360.
        KTH   = KTH + 2
      END IF
*
**       F I N D   C R I T I C A L   S E C T O R S
*
      COSPHI = COS(PHI*RAD)
      SINPHI = SIN(PHI*RAD)
      K      = 0
      DO 100 I=1,KTH
        TH1    = RAD*ATH(I)
        TH2    = RAD*ATH(I+1)
        CALL IHWZN(COS(TH1)*COSPHI,COS(TH1)*SINPHI,-SIN(TH1),Z1)
        CALL IHWZN(COS(TH2)*COSPHI,COS(TH2)*SINPHI,-SIN(TH2),Z2)
        IF (Z1.GE.0. .AND. Z2.GT.0.) GOTO 100
        IF (Z1.LE.0. .AND. Z2.LT.0.) GOTO 100
        K      = K + 1
        IF (K .EQ. 3)           GOTO 998
        ITH(K)= I
  100 CONTINUE
      IF (K .NE. 2)             GOTO 998
*
**       F I N D   O R D E R   O F   C R I T I C A L   S E C T O R S
*
      TNCONS = TN(1,3)*COS(PHI*RAD) + TN(2,3)*SIN(PHI*RAD)
      TH1    = RAD * (ATH(ITH(1)) + ATH(ITH(1)+1)) / 2.
      TH2    = RAD * (ATH(ITH(2)) + ATH(ITH(2)+1)) / 2.
      Z1     = TNCONS*SIN(TH1) + TN(3,3)*COS(TH1)
      Z2     = TNCONS*SIN(TH2) + TN(3,3)*COS(TH2)
      IF ((Z1.LE.Z2 .AND. IOPT.EQ.1).OR.(Z1.GT.Z2 .AND. IOPT.EQ.2)) THEN
        ITH1 = ITH(1)
        ITH2 = ITH(2)
      ELSE
        ITH1 = ITH(2)
        ITH2 = ITH(1)
      END IF
      GOTO 999
*
  998 WRITE(*,*)
     &  'IHWTH: Something strange: num. of critical sectors .NE. 2'
      ITH1   = 1
      ITH2   = 2
  999 RETURN
      END
