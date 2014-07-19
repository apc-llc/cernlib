CDECK  ID>, HWEINI.
*CMZ :-        -26/04/91  12.42.30  by  Federico Carminati
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWEINI
C-----------------------------------------------------------------------
C     INITIALISES ELEMENTARY PROCESS
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRSET,DUMMY,SAFETY
      EXTERNAL HWRSET
      PARAMETER (SAFETY=1.001)
      INTEGER NBSH,I
C---NO OF WEIGHT GENERATED
      NWGTS=0
C---ACCUMULATED WEIGHTS
      WGTSUM=0.
C---ACCUMULATED WEIGHT-SQUARED
      WSQSUM=0.
C---CURRENT MAX WEIGHT
      WBIGST=0.
C---LAST VALUE OF SCALE
      EMLST=0.
C---NUMBER OF ERRORS REPORTED
      NUMER=0
C---NUMBER OF ERRORS UNREPORTED
      NUMERU=0
C---FIND MAXIMUM EVENT WEIGHT IN CASES WHERE THIS IS REQUIRED
      IF (NOWGT) THEN
        IF (WGTMAX.EQ.0.) THEN
          NBSH=IBSH
          DUMMY = HWRSET(IBRN)
          WRITE(6,10) IPROC,IBRN,NBSH
   10     FORMAT(/10X,'INITIAL SEARCH FOR MAX WEIGHT'//
     &            10X,'PROCESS CODE IPROC = ',I11/
     &            10X,'RANDOM NO. SEED 1  = ',I11/
     &            10X,'           SEED 2  = ',I11/
     &            10X,'NUMBER OF SHOTS    = ',I11)
          NEVHEP=0
          DO 11 I=1,NBSH
          CALL HWEPRO
   11     CONTINUE
          WRITE(6,20)
   20     FORMAT(/10X,'INITIAL SEARCH FINISHED')
          IF (WBIGST*NWGTS.LT.SAFETY*WGTSUM)
     &                 WGTMAX=SAFETY*WBIGST
          CALL HWEFIN
          NWGTS=0
          WGTSUM=0.
          WSQSUM=0.
          WBIGST=0.
        ELSE
          WRITE(6,21) AVWGT,WGTMAX
   21     FORMAT(/1P,10X,'INPUT EVT WEIGHT   =',E12.4/
     &               10X,'INPUT MAX WEIGHT   =',E12.4)
        ENDIF
      ENDIF
C---RESET RANDOM NUMBER
      DUMMY = HWRSET(NRN)
      ISTAT=5
  999 END
