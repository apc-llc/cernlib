*
* $Id: stabph.F,v 1.1.1.1 1996/01/11 14:14:43 mclareni Exp $
*
* $Log: stabph.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:43  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE STABPH
C     *****************
C-- REDUCES PARTICLE STREAM TO STABLE PARTICLES ONLY
C-- IORIG SET TO IPACK*JETN+(JETSET NO.)
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/event.inc"
#include "cojets/itapes.inc"
#include "cojets/jetset.inc"
#include "cojets/parqua.inc"
C
C-- CHANGE JORIG OF PRIMARY TOP DECAY JETS SO AS TO REFER TO TOP JET
      IF(NJSET.EQ.0) GO TO 10
      DO 11 I=1,NJSET
      IF(JORIG(I).GE.0) GO TO 11
      IOR=MOD(ABS(JORIG(I)),JPACK)
      JET=ABS(JORIG(I))/JPACK
      JOR=JETQUA(INT(PARHAD(IOR,7)))
      JORIG(I)=-(JPACK*JET+JOR)
11    CONTINUE
10    CONTINUE
C
C-- CHANGE IORIG OF PARTICLES SO AS TO REFER TO JET OF ORIGIN
      IF(NPART.EQ.0) RETURN
      IP=0
      DO 1 I=1,NPART
      IF(IDCAY(I).NE.0) GO TO 1
      IP=IP+1
      DO 2 J=1,7
2     PARHAD(IP,J)=PARHAD(I,J)
      IDENT(IP)=IDENT(I)
      IDCAY(IP)=IDCAY(I)
      JET=ABS(IORIG(I))/IPACK
      IQ=PARHAD(I,7)
      IF(IQ.EQ.0) THEN
        IORIG(IP)=0
      ELSEIF(IQ.GT.0) THEN
        IORIG(IP)=-(IPACK*JET+JETQUA(IQ))
      ELSE
        IORIG(IP)=IPACK*JET
      ENDIF
1     CONTINUE
      NPART=IP
      RETURN
      END
