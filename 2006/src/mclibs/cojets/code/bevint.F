*
* $Id: bevint.F,v 1.1.1.1 1996/01/11 14:14:32 mclareni Exp $
*
* $Log: bevint.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:32  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BEVINT(KL1,KL2,NP,INV)
C     *********************************
C--  SETS EVINT PARAMETERS IN BRADLP, FOR IRADLP=1
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/event.inc"
#include "cojets/evint.inc"
#include "cojets/itapes.inc"
#include "cojets/jet.inc"
#include "cojets/qcds.inc"
      DO 1 L=1,2
      DO 1 J=1,4
1     PFLABV(J,L,1)=0.
      DO 3 I=1,NP
      COSG=(P(1,1)-P(NP,1))*P(I,1)+(P(1,2)-P(NP,2))*P(I,2)
     1    +(P(1,3)-P(NP,3))*P(I,3)
      IF(FLOAT(INV)*COSG.LT.0.) GO TO 4
      DO 5 J=1,4
5     PFLABV(J,1,1)=PFLABV(J,1,1)+P(I,J)
      GO TO 3
4     CONTINUE
      DO 6 J=1,4
6     PFLABV(J,2,1)=PFLABV(J,2,1)+P(I,J)
3     CONTINUE
C
      PCMV(1)=SQRT(PFLABV(1,1,1)**2+PFLABV(2,1,1)**2+PFLABV(3,1,1)**2)
      AMV(1,1)=SQRT(ABS(PFLABV(4,1,1)**2-PCMV(1)**2))
      AMV(2,1)=SQRT(ABS(PFLABV(4,2,1)**2-PCMV(1)**2))
      IFLFV(1,1)=KL1
      IFLFV(2,1)=KL2
      COSCMV(1)=PFLABV(3,1,1)/PCMV(1)
      PFLAB1=PFLABV(1,1,1)
      PFLAB2=PFLABV(2,1,1)
      PHIV(1)=ATAN2X(PFLAB2,PFLAB1)
C
      RETURN
      END
