*
* $Id: prtevt.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: prtevt.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE PRTEVT(IPRT)
C.----------------------------------------------------------------------
C.
C.    THIS SUBROUTINE DUMPS AN EVENT STORED IN /MOMGEN/, /RESULT/
C.    TO OUTPUT UNIT: LUN2
C.    IPRT > 0, NO DUMP,
C.    IPRT = 0, DUMP PARTON AND PARTICLE INFORMATION,
C.    IPRT < 0, DUMP PARTON ONLY.
C.    IF THE CALCULATION OF SECONDARY VERTICES IS INHIBITED, A CALL TO
C.    PRTVTX IS MADE TO DUMP THE MAIN AND DECAY VERTICES.
C.    LAST UPDATE: 10/03/89
C.
C.----------------------------------------------------------------------
#include "eurodec/runinf.inc"
#include "eurodec/momgen.inc"
#include "eurodec/result.inc"
#include "eurodec/inpout.inc"
#include "eurodec/inivtx.inc"
      DIMENSION SUMVEC(5)
C--
C--   REALLY WANT A DUMP?
      IF (IPRT.GT.0) RETURN
C--
C--   PRINT RUN NUMBER, EVENT NUMBER AND EVENT WEIGHT
      WRITE(LUN2,9000)
      WRITE(LUN2,9010) IRUN,NEVENT,WEIGHT
      IF (NP.GT.0) THEN
        DO 10 K=1,5
   10   SUMVEC(K)=0.
C--
C--   PRINT PARTON INFORMATION
        WRITE(LUN2,9020)
        WRITE(LUN2,9030)
        DO 30 I=1,NP
          PT=SQRT(PCM(2,I)**2+PCM(3,I)**2)
          YM=PCM(4,I)-PCM(1,I)
          YP=PCM(4,I)+PCM(1,I)
          IF (YM.LE.0.) THEN
            Y=1000.
          ELSEIF (YP.LE.0.) THEN
            Y=-1000.
          ELSE
            Y=-.5*LOG(YM/YP)
          ENDIF
          DO 20 K=1,5
   20     SUMVEC(K)=SUMVEC(K)+PCM(K,I)
   30   WRITE(LUN2,9040) I,IP(I),APM(I),PCM(5,I),(PCM(K,I),K=1,4),Y,PT,
     &  IPHEL(I)
        PTSUM=SQRT(SUMVEC(2)**2+SUMVEC(3)**2)
        WRITE(LUN2,9050) SUMVEC(5),(SUMVEC(K),K=1,4),PTSUM
      ENDIF
C--
C--   CHECK ON PRINT OPTION...
      IF (IPRT.LT.0) RETURN
C--
C--   PRINT PARTICLE PARAMETERS
      IF (NTEIL.GT.0) THEN
        DO 40 K=1,4
   40   SUMVEC(K)=0.
        WRITE(LUN2,9060)
        WRITE(LUN2,9070)
        DO 60 I=1,NTEIL
          I1=IABS(IORIGT(I))
          JET=I1/10000
          I1=ISIGN((I1-10000*JET),IORIGT(I))
          ID=INDEXT(I)
          J1=IDCAYT(I)/10000
          J2=MOD(IDCAYT(I),10000)
          PT=SQRT(PTEIL(2,I)**2+PTEIL(3,I)**2)
          YM=PTEIL(4,I)-PTEIL(1,I)
          YP=PTEIL(4,I)+PTEIL(1,I)
          IF (YM.LE.0.) THEN
            Y=1000.
          ELSEIF (YP.LE.0.) THEN
            Y=-1000.
          ELSE
            Y=-.5*LOG(YM/YP)
          ENDIF
          IF (IDCAYT(I).EQ.0) THEN
            DO 50 K=1,4
   50       SUMVEC(K)=SUMVEC(K)+PTEIL(K,I)
            WRITE(LUN2,9080) I,JET,I1,ID,PTEIL(5,I),(PTEIL(K,I),K=1,4),
     &      Y,PT,ITHEL(I)
          ELSE
            WRITE(LUN2,9090) I,JET,I1,ID,PTEIL(5,I),J1,J2,(PTEIL(K,I),K
     &      =1,4),Y,PT,ITHEL(I)
          ENDIF
   60   CONTINUE
        PTSUM=SQRT(SUMVEC(2)**2+SUMVEC(3)**2)
        WRITE(LUN2,9100) (SUMVEC(K),K=1,4),PTSUM
C--
C--   PRINT VERTEX INFORMATION
        IF (ISVTX.EQ.1) CALL PRTVTX
      ENDIF
      RETURN
 9000 FORMAT(1H0,130('='))
 9010 FORMAT(1H ,6X,'RUN NO.:',I10,5X,'EVENT NO.:',I10,5X,
     &'EVENT WEIGHT:',E12.5)
 9020 FORMAT(1H0,6X,'PARTON PARAMETERS')
 9030 FORMAT(1H0,6X,'JET',10X,'TYPE',4X,'MASS',8X,'P',12X,'PX',11X,'PY',
     &11X,'PZ',11X,'E',8X,'RAPIDITY',7X,'PT',5X,'HELICITY')
 9040 FORMAT(1H ,4X,I5,7X,I7,F9.3,5F13.5,F11.4,F13.5,I6)
 9050 FORMAT(1H0,19X,'VECTOR-SUM:  ',5F13.5,3X,' PT-SUM:',F13.5)
 9060 FORMAT(1H0,6X,'PARTICLE PARAMETERS')
 9070 FORMAT(1H0,3X,'I',2X,'JET',3X,'ORIG',3X,'TYPE',4X,'MASS',6X,
     &'DECAY',10X,'PX',11X,'PY',11X,'PZ',11X,'E',8X,'RAPIDITY',7X,'PT',
     &5X,'HELICITY')
 9080 FORMAT(1H ,I4,I5,I7,I7,F9.3,5X,'STABLE',2X,4F13.5,F11.4,F13.5,I6)
 9090 FORMAT(1H ,I4,I5,I7,I7,F9.3,2X,I4,' - ',I4,4F13.5,F11.4,F13.5,I6)
 9100 FORMAT(1H0,32X,'VECTOR-SUM:  ',4F13.5,3X,' PT-SUM:',F13.5)
      END
