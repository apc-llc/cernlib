CDECK  ID>, HWUINE.
*CMZ :-        -16/10/93  12.42.15  by  Mike Seymour
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWUINE
C-----------------------------------------------------------------------
C     INITIALISES AN EVENT
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,HWRGET,DUMMY
      REAL TL
      LOGICAL CALLED
      EXTERNAL HWRGEN,HWRGET
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
      ISLENT=1
      NQDK=0
C---DECIDE WHETHER TO GENERATE SOFT UNDERLYING EVENT
      GENSOF=IPROC.GT.1000.AND.IPROC.LT.10000.AND.
     &      (IPROC.EQ.8000.OR.HWRGEN(0).LT.PRSOF)
C Zero arrays
      CALL HWVZRO(2*NMXHEP,JMOHEP)
      CALL HWVZRO(2*NMXHEP,JDAHEP)
      CALL HWVZRO(4*NMXHEP,VHEP)
      CALL HWVZRO(3*NMXHEP,RHOHEP)
      EMSCA=ZERO
  999 END
