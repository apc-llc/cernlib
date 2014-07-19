*
* $Id: rlego.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: rlego.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION RLEGO(P1,P2)
C.----------------------------------------------------------------------
C.
C.    CALCULATE PHI-RAPIDITY DISTANCE IN LEGO PLOT FOR P1 AND P2
C.    LAST UPDATE: 31/10/88
C.
C.----------------------------------------------------------------------
      DIMENSION P1(5),P2(5)
C--
C--   RAPIDITY DIFFERENCE...
      DETA1=(P1(5)+P1(1))*(P2(5)-P2(1))
      DETA2=(P1(5)-P1(1))*(P2(5)+P2(1))
      IF (DETA2.LE.1.E-6) DETA2=1.E-6
      DETA=ABS(DETA1/DETA2)
      IF (DETA.LE.1.E-10) DETA=1.E-10
      DELETA=0.5*LOG(DETA)
C--
C--   PHI DIFFERENCE
      APHI=((P1(2)*P2(2)+P1(3)*P2(3))/
     &MAX(SQRT((P1(2)**2+P1(3)**2)*(P2(2)**2+P2(3)**2)),1.E-6))
      IF (ABS(APHI).GT.1.0) APHI=1.
      DELPHI=ACOS(APHI)
C--
C--   LEGO DISTANCE
      RLEGO=SQRT(DELETA**2+DELPHI**2)
      RETURN
      END
