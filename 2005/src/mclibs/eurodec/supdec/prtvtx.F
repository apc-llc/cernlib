*
* $Id: prtvtx.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: prtvtx.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE PRTVTX
C.----------------------------------------------------------------------
C.
C.    THIS SUBROUTINE DUMPS THE VERTEX INFORMATION STORED IN /RESULT/
C.    TO OUTPUT UNIT: LUN2
C.    LAST UPDATE: 26/05/88
C.
C.----------------------------------------------------------------------
#include "eurodec/result.inc"
#include "eurodec/inpout.inc"
#include "eurodec/mixing.inc"
      LOGICAL DONE(NTMAX)
      CHARACTER*8 PNAME
      DIMENSION SECVER(3)
C--
C--   INITIALIZE...
      DO 10 I=1,NTEIL
   10 DONE(I)=.FALSE.
C--
C--   LOOP OVER PARTICLES AND PICK-UP DECAY VERTICES...
      DO 40 I=1,NTEIL
        IF (.NOT.DONE(I)) THEN
          DO 20 J=1,3
   20     SECVER(J)=SECVTX(J,I)
          IF (I.EQ.1) THEN
C--
C--   'MAIN' VERTEX...
              WRITE(LUN2,9000)
              WRITE(LUN2,9020) (SECVER(J),J=1,3)
              WRITE(LUN2,9010)
          ELSE
C--
C--   ...SECONDARY VERTICES
            I1=IABS(IORIGT(I))
            JET=I1/10000
            I1=I1-10000*JET
            CALL INDNAM(INDEXT(I1),PNAME)
            WRITE(LUN2,9030)
            WRITE(LUN2,9040) I1,PNAME,(SECVER(J),J=1,3)
            WRITE(LUN2,9030)
          ENDIF
C--
C--   CHECK WHETHER THERE ARE OTHER PARTICLES WITH THE SAME VERTEX
C--   (WITHIN THE MACHINE PRECISION !!)
          DO 30 J=1,NTEIL
            IF (.NOT.DONE(J)) THEN
              IF ((SECVTX(1,J).EQ.SECVER(1)).AND.(SECVTX(2,J).EQ.SECVER
     &        (2)).AND.(SECVTX(3,J).EQ.SECVER(3))) THEN
                CALL INDNAM(INDEXT(J),PNAME)
                IF (IDCAYT(J).EQ.0) THEN
                  WRITE(LUN2,9050) PNAME,J
                ELSE
                  J1=IDCAYT(J)/10000
                  J2=MOD(IDCAYT(J),10000)
                  WRITE(LUN2,9060) PNAME,J,J1,J2
                ENDIF
C--
C--    REMOVE THIS PARTICLE FROM THE LIST
                DONE(J)=.TRUE.
              ENDIF
            ENDIF
   30     CONTINUE
        ENDIF
   40 CONTINUE
      RETURN
 9000 FORMAT(1H0,32X,62('='))
 9010 FORMAT(1H ,32X,62('='))
 9020 FORMAT(1H ,32X,'MAIN VERTEX POSITION [CM] (',E10.4,', ',E10.4,', '
     &,E10.4,')')
 9030 FORMAT(1H ,23X,82('='))
 9040 FORMAT(1H ,23X,'(',I4,')',2X,A8,2X,' DECAY VERTEX POSITION [CM] ('
     &,E10.4,', ',E10.4,', ',E10.4,')')
 9050 FORMAT(1H ,47X,A8,2X,'(',I4,'  -  ','   STABLE  )')
 9060 FORMAT(1H ,47X,A8,2X,'(',I4,' --> ',I4,' - ',I4,')')
      END
