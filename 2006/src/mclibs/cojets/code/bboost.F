*
* $Id: bboost.F,v 1.1.1.1 1996/01/11 14:14:31 mclareni Exp $
*
* $Log: bboost.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:31  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BBOOST
C     *****************
C-- LORENTZ BOOST OF BOSON DECAY PRODUCTS TO LAB SYSTEM
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/boson.inc"
#include "cojets/event.inc"
#include "cojets/evint.inc"
#include "cojets/itapes.inc"
#include "cojets/jet.inc"
#include "cojets/jetnpc.inc"
#include "cojets/jetset.inc"
#include "cojets/maxn.inc"
#include "cojets/njsetb.inc"
#include "cojets/parqua.inc"
#include "cojets/weakon.inc"
      DIMENSION A(4),V1(4),V2(4)
      DATA ZH,WH/2.,3./,DH/4./
C
      NEW=1
      DO 10 I=1,NP
      NPART=NPART+1
      DO 11 J=1,4
11    V1(J)=P(I,J)
      CALL LORLAB(V1,V2,NEW)
      NEW=0
      DO 12 J=1,4
12    PARHAD(NPART,J)=V2(J)
      PARHAD(NPART,5)=P(I,5)
      PARHAD(NPART,6)=K(I,2)
10    CONTINUE
C
      DO 30 I=1,2
      DO 31 J=1,4
31    V1(J)=PFLABV(J,I,1)
      CALL LORLAB(V1,V2,NEW)
      DO 32 J=1,4
32    PFLABV(J,I,1)=V2(J)
30    CONTINUE
C
      MNPART=MAX(MNPART,NPART)
      MNJTP=MAX(MNJTP,NPART)
C
      IF(WEAKON.EQ.ZH.AND.ICHDB.GT. 6) GO TO 25
      IF(WEAKON.EQ.WH.AND.ICHDB.GT.15) GO TO 25
      DO 20 I=1,NP
20    PARHAD(I,7)=-ABS(IORIG(I))/IPACK
      RETURN
C
25    CONTINUE
      DO 26 IQ=1,NQUA
      IF(PARQUA(IQ,7).LT.0.) GO TO 26
      DO 27 J=1,4
27    PARQUA(IQ,J)=0.
      DO 28 I=1,NPART
      IF(INT(PARHAD(I,7)).NE.IQ) GO TO 28
      DO 29 J=1,4
29    PARQUA(IQ,J)=PARQUA(IQ,J)+PARHAD(I,J)
28    CONTINUE
      QUAM2=PARQUA(IQ,4)**2-PARQUA(IQ,1)**2-PARQUA(IQ,2)**2
     *-PARQUA(IQ,3)**2
      PARQUA(IQ,5)=0.
      IF(QUAM2.GT.0.) PARQUA(IQ,5)=SQRT(QUAM2)
26    CONTINUE
C
C-- RESET PJSET USING FRAGMENTATION PARTICLES
      NJSET1=NJSETB+1
      DO 40 JQ=NJSET1,NJSET
40    JDCAY(JQ)=-JDCAY(JQ)
      DO 46 IQ=1,NQUA
      IF(PARQUA(IQ,7).LT.0.) GO TO 46
      JQ=JETQUA(IQ)
      DO 41 J=1,5
41    PJSET(J,JQ)=PARQUA(IQ,J)
46    CONTINUE
C
42    IFLAG=0
      JQ=NJSETB
43    JQ=JQ+1
      IF(JQ.GT.NJSET) GO TO 45
      IF(JDCAY(JQ).GE.0) GO TO 43
      JQ1=ABS(JDCAY(JQ))/JPACK
      IF(JDCAY(JQ1).LT.0) GO TO 43
      JQ2=ABS(JDCAY(JQ))-JPACK*JQ1
      IF(JDCAY(JQ2).LT.0) GO TO 43
      IFLAG=1
      JDCAY(JQ)=ABS(JDCAY(JQ))
      DO 44 J=1,4
44    PJSET(J,JQ)=PJSET(J,JQ1)+PJSET(J,JQ2)
      PJSET(5,JQ)=SQRT(ABS(PJSET(4,JQ)**2-PJSET(1,JQ)**2
     +                    -PJSET(2,JQ)**2-PJSET(3,JQ)**2))
      GO TO 43
45    IF(IFLAG.EQ.1) GO TO 42
      RETURN
      END
