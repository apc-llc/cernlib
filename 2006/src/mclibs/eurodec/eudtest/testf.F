*
* $Id: testf.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: testf.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE TESTF
C.----------------------------------------------------------------------
C.
C.    TEST PROGRAM FOR EURODEC FRAGMENTATION AND DECAYS.
C.    LOOP OVER PARTONS IN PARTICLE TABLE.
C.    LAST UPDATE: 15/04/89
C.
C.----------------------------------------------------------------------
#include "eurodec/runinf.inc"
#include "eurodec/picons.inc"
#include "eurodec/momgen.inc"
#include "eurodec/result.inc"
#include "eurodec/ptable.inc"
#include "eurodec/inpout.inc"
      CHARACTER*8 FLAVOR(9)
      LOGICAL FIRST1,FIRST2
      DATA FLAVOR/'UP      ','DOWN    ','STRANGE ','CHARM   ','BOTTOM  '
     &           ,'TOP     ','LOW     ','HIGH    ','GLUON   '/
      DATA TLEFT/ 5./
C--
C--   INITIALIZE /RUNINF/, /MOMGEN/
      WEIGTH=0.
      NP=2
      ICHTOT=0
C--
C--   START LOOP ON PARTONS
      DO 40 IRUN=1,9
C--
C--   CONVERT PARTON NAME IN POINTER TO PARTICLE TABLE, FILL /MOMGEN/
        IPOINT=NAMPOI(FLAVOR(IRUN))
        IP(1)=IPC(IPOINT)
        IP(2)=-IP(1)
        APM(1)=PM(IPOINT)
        APM(2)=APM(1)
        Q0=93.
        IF ((Q0+10.).LE.(2.*APM(1))) Q0=2.*APM(1)+10.
        ENERGY=Q0/2.
        PPARTN=SQRT((ENERGY-APM(1))*(ENERGY+APM(1)))
        ICHERR=0
C--
C--   SIMULATE TWO-BODY DECAY, FRAGMENT PARTON PAIRS SEVERAL TIMES...
        DO 30 NEVENT=1,10
          THETA=TWOPI*EURRAN(NEVENT)
          PHI=TWOPI*EURRAN(NEVENT+1)
          PCM(1,1)=COS(THETA)*PPARTN
          PCM(2,1)=SIN(PHI)*SIN(THETA)*PPARTN
          PCM(3,1)=COS(PHI)*SIN(THETA)*PPARTN
          PCM(4,1)=ENERGY
          PCM(5,1)=PPARTN
          PCM(1,2)=-PCM(1,1)
          PCM(2,2)=-PCM(2,1)
          PCM(3,2)=-PCM(3,1)
          PCM(4,2)=ENERGY
          PCM(5,2)=PPARTN
          IPHEL(1)=0
          IPHEL(2)=0
C--
C--   LET THE PARTONS FRAGMENT AND PARTICLES DECAY...
          NTEIL=0
          NTEIL0=NTEIL
          CALL FRAGMT(1,NP,NTEIL0)
          IF (NEVENT.EQ.1.AND.IRUN.EQ.1) CALL PRTEVT(0)
C--
C--   LOOP OVER HADRONS AND RECONSTRUCT FRAGMENTATION FUNCTION...
          FIRST1=.TRUE.
          FIRST2=.TRUE.
          DO 10 I=1,NTEIL
            IORIG=ABS(IORIGT(I))
            JET=IORIG/10000
            IORIG=ISIGN((IORIG-10000*JET),IORIGT(I))
            IF ((FIRST1).AND.(IORIG.EQ.-1)) THEN
              HABS=SQRT((PTEIL(4,I)-PTEIL(5,I))*(PTEIL(4,I)+PTEIL(5,I)
     &        ))
              PHINPQ=(PTEIL(1,I)*PCM(1,1)+PTEIL(2,I)*PCM(2,1)+
     &        PTEIL(3,I)*PCM(3,1))/(HABS*PCM(5,1))
              Z1=(PTEIL(4,I)+HABS*PHINPQ)/(PCM(4,1)+PCM(5,1))
              CALL HFILL(IRUN,Z1,0.,1.)
              FIRST1=.FALSE.
            ENDIF
            IF ((FIRST2).AND.(IORIG.EQ.-2)) THEN
              HABS=SQRT((PTEIL(4,I)-PTEIL(5,I))*(PTEIL(4,I)+PTEIL(5,I)
     &        ))
              PHINPQ=(PTEIL(1,I)*PCM(1,2)+PTEIL(2,I)*PCM(2,2)+
     &        PTEIL(3,I)*PCM(3,2))/(HABS*PCM(5,2))
              Z2=(PTEIL(4,I)+HABS*PHINPQ)/(PCM(4,2)+PCM(5,2))
              CALL HFILL(IRUN,Z2,0.,1.)
              FIRST2=.FALSE.
            ENDIF
   10     CONTINUE
C--
C--   STABLE PARTICLE ONLY: CHECK CHARGE SUM...
          SUMQ=0.
          DO 20 I=1,NTEIL
            IF (IDCAYT(I).NE.0) GOTO 20
            SUMQ=SUMQ+PCHARG(INDEXT(I))
   20     CONTINUE
          IF (SUMQ.GT.0001) THEN
            WRITE(LUN2,9000) IFIX(SUMQ)
            ICHERR=ICHERR+1
            CALL PRTEVT(0)
          ENDIF
          CALL TIMEL(TIMSEC)
          IF (TIMSEC.LT.TLEFT) THEN
            WRITE(LUN2,9010) IP(1),IP(2),ICHERR
            RETURN
          ENDIF
   30   CONTINUE
        WRITE(LUN2,9020) IP(1),IP(2),ICHERR
        ICHTOT=ICHTOT+ICHERR
   40 CONTINUE
      WRITE(LUN2,9030) ICHTOT
      RETURN
 9000 FORMAT(1H ,'***** ERROR: TOTAL CHARGE NOT CONSERVED: ',I3,' EVENT
     &DUMP: *****')
 9010 FORMAT(1H ,'***** TIME LIMIT IN PROCESSING ',I5,I5,' PAIR WITH: '
     &,I3,' CHARGE ERRORS ****')
 9020 FORMAT(1H ,'***** FINISHED PROCESSING ',I5,I5,' PAIR WITH: ',I3,'
     & CHARGE ERRORS ****')
 9030 FORMAT(1H ,'***** FINISHED PROCESSING ALL WITH: ',I3,' CHARGE ERRO
     &RS ****')
      END
