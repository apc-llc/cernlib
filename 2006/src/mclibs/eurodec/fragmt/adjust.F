*
* $Id: adjust.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: adjust.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE ADJUST(Q0,I1,I2,IADCUT)
C.----------------------------------------------------------------------
C.
C.    THIS SUBROUTINE ADJUSTS THE ENERGY AND MOMENTUM IN THE CMS (Q0)
C.    OF THE I1-I2 PARTICLES IN /RESULT/
C.    IADCUT = 1, FAILS TO ADJUST ENERGY/MOMENTUM.
C.    LAST UPDATE: 10/04/89
C.
C.----------------------------------------------------------------------
#include "eurodec/result.inc"
      DIMENSION PBOOST(5),BET(3),P(2,NTMAX)
      DATA DELALF/ 1.E-5/
C--
C--   INITIALIZE
      ALF=1.
      IADCUT=1
      SUMM=0.
      DO 10 J=1,5
   10 PBOOST(J)=0.
C--
C--   DETERMINE BOOST VECTOR AND SUM OF MASSES**2
      DO 30 I=I1,I2
        IF (IDCAYT(I).EQ.0) THEN
          P(2,I)=PTEIL(5,I)**2
          SUMM=SUMM+PTEIL(5,I)
          DO 20 J=1,4
   20     PBOOST(J)=PBOOST(J)-PTEIL(J,I)
        ENDIF
   30 CONTINUE
      PBOOST(4)=-PBOOST(4)
C--
C--   CHECK ON MASS SUM AND ENERGY-MOMENTUM DIFFERENCE
      IF (SUMM.GE.Q0) RETURN
      PBOOST(5)=PBOOST(4)**2-PBOOST(1)**2-PBOOST(2)**2-
     &PBOOST(3)**2
      IF (PBOOST(5).LE.0.) RETURN
      PBOOST(5)=SQRT(PBOOST(5))
C--
C--   BOOST IS APPLIED TO CONSERVE MOMENTUM
      DO 40 J=1,3
   40 BET(J)=PBOOST(J)/PBOOST(5)
      GAM=PBOOST(4)/PBOOST(5)
      DO 60 I=I1,I2
        IF (IDCAYT(I).EQ.0) THEN
          PB=BET(1)*PTEIL(1,I)+BET(2)*PTEIL(2,I)+BET(3)*PTEIL(3,I)
          DO 50 J=1,3
   50     PTEIL(J,I)=PTEIL(J,I)+BET(J)*(PTEIL(4,I)+PB/(GAM+1.))
          PTEIL(4,I)=GAM*PTEIL(4,I)+PB
        ENDIF
   60 CONTINUE
C--
C--   MOMENTA ARE RESCALED WITH ALF, ALF IS SOLVED BY ITERATION
      DO 70 I=I1,I2
   70 IF (IDCAYT(I).EQ.0) P(1,I)=PTEIL(1,I)**2+PTEIL(2,I)**2+PTEIL(3,I)
     &**2
      DO 90 K=1,20
        FZ=-Q0
        DFZ=0.
        DO 80 I=I1,I2
          IF (IDCAYT(I).EQ.0) THEN
            E=P(2,I)+ALF*ALF*P(1,I)
            IF (E.LE.0.) GOTO 80
            E=SQRT(E)
            FZ=FZ+E
            DFZ=DFZ+ALF*P(1,I)/E
          ENDIF
   80   CONTINUE
        IF (DFZ.NE.0.) THEN
          DALF=-FZ/DFZ
          ALF=ALF+DALF
        ELSE
          CALL ERRORD(42,' ',0.)
          ALF=1.
          DALF=0.
        ENDIF
        IF (ABS(DALF).LT.DELALF) GOTO 100
   90 CONTINUE
C--
C--   ADJUST NOT CONVERGENT...
      RETURN
C--
C--   ADJUST ENERGY AND MOMENTUM NOW...
  100 DO 120 I=I1,I2
        IF (IDCAYT(I).EQ.0) THEN
          PTEIL(4,I)=0.
          DO 110 J=1,3
            PTEIL(J,I)=PTEIL(J,I)*ALF
  110     PTEIL(4,I)=PTEIL(4,I)+PTEIL(J,I)**2
          PTEIL(4,I)=SQRT(P(2,I)+PTEIL(4,I))
        ENDIF
  120 CONTINUE
      IADCUT=0
      RETURN
      END
