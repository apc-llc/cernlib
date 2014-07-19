*
* $Id: testd.F,v 1.1.1.1 1996/03/08 16:58:54 mclareni Exp $
*
* $Log: testd.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:54  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE TESTD
C.----------------------------------------------------------------------
C.
C.    TEST PROGRAM FOR EURODEC DECAYS.
C.    LOOP OVER PARTICLES IN PARTICLE TABLE.
C.    LAST UPDATE: 15/04/89
C.
C.----------------------------------------------------------------------
#include "eurodec/runinf.inc"
#include "eurodec/result.inc"
#include "eurodec/ptable.inc"
#include "eurodec/inpout.inc"
      DATA TLEFT/ 5./
C--
C--   INITIALIZE /RUNINF/
      IRUN=0
      IOFF=29
      WEIGTH=0.
      ICHTOT=0
C--
C--   START LOOP ON PARTICLES
   10 IRUN=IRUN+1
      IF (PNA(IRUN+IOFF).EQ.'ENDLIST') GOTO 50
C--
C--   COPY PARTICLE FROM PARTICLE TABLE TO /RESULT/
      ICHERR=0
      DO 40 NEVENT=1,10
        NTEIL=1
        INDEXT(1)=IPC(IRUN+IOFF)
C--
C--   SKIP LEPTOQUARKS...
        IF ((INDEXT(1).GT.1000).AND.(INDEXT(1).LT.1010)) GOTO 10
        IORIGT(1)=-10001
        IDCAYT(1)=0
        ITHEL(1)=0
C--
C--   GIVE PARTICLE RANDOM ENERGY, MOMENTUM AND GET MASS
        DO 20 I=1,3
   20   PTEIL(I,1)=5.*EURRAN(NEVENT+I)
        PTEIL(5,1)=PM(IRUN+IOFF)
        PTEIL(4,1)=SQRT(PM(IRUN+IOFF)**2+PTEIL(1,1)** 2+PTEIL(2,1)**2+
     &  PTEIL(3,1)**2)
C--
C--   LET THE PARTICLE DECAY...
        CALL DECAYS(1,NTEIL)
        IF (NEVENT.EQ.1.AND.IRUN.EQ.1) CALL PRTEVT(0)
C--
C--   STABLE PARTICLE ONLY: CHECK CHARGE SUM...
        SUMQ=-PCHARG(INDEXT(1))
        SUMCH=0.
        SUMNE=0.
        DO 30 I=1,NTEIL
          IF (IDCAYT(I).NE.0) GOTO 30
          CHARGE=PCHARG(INDEXT(I))
          IF (CHARGE.NE.0) THEN
            SUMCH=SUMCH+1.
            SUMQ=SUMQ+CHARGE
          ELSE
            SUMNE=SUMNE+1.
          ENDIF
   30   CONTINUE
        CALL HFILL(1,SUMCH,0.,1.)
        CALL HFILL(2,SUMNE,0.,1.)
        IF (SUMQ.GT.0001) THEN
          WRITE(LUN2,9000) IFIX(SUMQ)
          ICHERR=ICHERR+1
          CALL PRTEVT(0)
        ENDIF
        CALL TIMEL(TIMSEC)
        IF (TIMSEC.LT.TLEFT) THEN
          WRITE(LUN2,9010) INDEXT(1),ICHERR
          RETURN
        ENDIF
   40 CONTINUE
      WRITE(LUN2,9020) INDEXT(1),ICHERR
      ICHTOT=ICHTOT+ICHERR
      GOTO 10
   50 CONTINUE
      WRITE(LUN2,9030) ICHTOT
      RETURN
 9000 FORMAT(1H ,'***** ERROR: TOTAL CHARGE NOT CONSERVED: ',I3,' EVENT
     &DUMP: *****')
 9010 FORMAT(1H ,'***** TIME LIMIT IN PROCESSING ',I5,' WITH: ',I3,' CHA
     &RGE ERRORS ****')
 9020 FORMAT(1H ,'***** FINISHED PROCESSING ',I5,' DECAY WITH: ',I3,' CH
     &ARGE ERRORS ****')
 9030 FORMAT(1H ,'***** FINISHED PROCESSING ALL WITH: ',I3,' CHARGE ERRO
     &RS ****')
      END
