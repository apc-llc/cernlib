*
* $Id: bevber.F,v 1.1.1.1 1996/01/11 14:14:32 mclareni Exp $
*
* $Log: bevber.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:32  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BEVBER(KL1,KL2,NP,IFBOS)
C     ***********************************
C--  SETS EVINT PARAMETERS IN BRADLP, FOR IRADLP=2
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
      IF(NP.EQ.3) GO TO 50
      PCMV(1)=SQRT(ABS(P(1,4)**2-P(1,5)**2))
      AMV(1,1)=P(1,5)
      AMV(2,1)=P(2,5)
      IFLFV(1,1)=KL1
      IFLFV(2,1)=KL2
      COSCMV(1)=P(1,3)/PCMV(1)
      PHIV(1)=ATAN2X(P(1,2),P(1,1))
      DO 1 I=1,2
      DO 1 J=1,4
1     PFLABV(J,I,1)=P(I,J)
      RETURN
50    CONTINUE
      IF(IFBOS.EQ.3.OR.IFBOS.EQ.4) GO TO 51
      COSG=(P(1,1)-P(3,1))*P(2,1)+(P(1,2)-P(3,2))*P(2,2)
     1    +(P(1,3)-P(3,3))*P(2,3)
      IF((IFBOS.EQ.2.OR.IFBOS.EQ.1).AND.COSG.LT.0) GO TO 51
      DO 52 J=1,4
      PFLABV(J,1,1)=P(1,J)+P(2,J)
52    PFLABV(J,2,1)=P(3,J)
      GO TO 53
51    CONTINUE
      DO 54 J=1,4
      PFLABV(J,1,1)=P(1,J)
54    PFLABV(J,2,1)=P(3,J)+P(2,J)
53    CONTINUE
      PCMV(1)=SQRT(PFLABV(1,1,1)**2+PFLABV(2,1,1)**2+PFLABV(3,1,1)**2)
      AMV(1,1)=SQRT(ABS(PFLABV(4,1,1)**2-PCMV(1)**2))
      AMV(2,1)=SQRT(ABS(PFLABV(4,2,1)**2-PCMV(1)**2))
      IFLFV(1,1)=KL1
      IFLFV(2,1)=KL2
      COSCMV(1)=PFLABV(3,1,1)/PCMV(1)
      PFLAB1=PFLABV(1,1,1)
      PFLAB2=PFLABV(2,1,1)
      PHIV(1)=ATAN2X(PFLAB2,PFLAB1)
      RETURN
      END
