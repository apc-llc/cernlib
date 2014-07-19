*
* $Id: hwuine.F,v 1.1.1.1 1996/03/08 17:02:17 mclareni Exp $
*
* $Log: hwuine.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:17  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, HWUINE.
*CMZ :-        -16/10/93  12.42.15  by  Mike Seymour
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWUINE
C     INITIALISES AN EVENT
C-----------------------------------------------------------------------
C     MODIFICATIONS FROM ORIGINAL (BY MHS):
C     ZEROES NEW RHOHEP VARIABLE
C     8/4/94 (BY BRW) ZEROES EMSCA
C-----------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      DOUBLE PRECISION HWRGEN, HWRGET, DUMMY
      INTEGER IHEP
      REAL TL
      LOGICAL CALLED
      COMMON/HWDBUG/CALLED
C---CHECK THAT MAIN PROGRAM HAS BEEN MODIFIED CORRECTLY
      IF (NEVHEP.GT.0.AND..NOT.CALLED) THEN
        WRITE (6,10)
 10     FORMAT (1X,'A call to the subroutine HWUFNE should be added to',
     &      /,' the main program, immediately after the call to HWMEVT')
        CALL HWWARN('HWUINE',500,*999)
      ENDIF
      CALLED=.FALSE.
C---CHECK TIME LEFT
      CALL HWUTIM(TL)
      IF (TL.LT.TLOUT) CALL HWWARN('HWUINE',200,*999)
C---UPDATE RANDOM NUMBER SEED
      DUMMY = HWRGET(NRN)
      NEVHEP=NEVHEP+1
      NHEP=0
      ISTAT=6
      IERROR=0
      EVWGT=AVWGT
      HVFCEN=.FALSE.
C---DECIDE WHETHER TO GENERATE SOFT UNDERLYING EVENT
      GENSOF=IPROC.GT.1000.AND.IPROC.LT.10000.AND.
     &      (IPROC.EQ.8000.OR.HWRGEN(0).LT.PRSOF)
      DO 100 IHEP=1,NMXHEP
        JMOHEP(1,IHEP)=0
        JMOHEP(2,IHEP)=0
        JDAHEP(1,IHEP)=0
        JDAHEP(2,IHEP)=0
  100 CONTINUE
      CALL HWVZRO(3*NMXHEP,RHOHEP)
      EMSCA=ZERO
  999 END
