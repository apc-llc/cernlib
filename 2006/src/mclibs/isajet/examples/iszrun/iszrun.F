C-  written in ZEBRA format with trivial calorimetry.
C-  User must provide subroutines ISZUSR and ISZUSD
C-  start with ISZUSR example provided with ISZRUN patch
C-
C-     SDP May,1986
C-----------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/isabnk.inc"
#include "isajet/isaunt.inc"
#include "isajet/zebcom.inc"
#include "isajet/quest.inc"
#include "isajet/itapes.inc"
      CHARACTER*80 FILIS
      INTEGER BEGR,ENDR,NT,NF,IOS,IXWIPE,NDUMP
C
      ITLIS=2
      PRINT 4
    4 FORMAT(' Name of file for printout?')
      READ 2,FILIS
      OPEN(UNIT=ITLIS,FILE=FILIS,STATUS='NEW',FORM='FORMATTED')
      CALL MZEBRA(0)
C
C  File to be read
      NT=0
      PRINT 1
    1 FORMAT(/'  Data file with ISAJET events?')
      READ 2,FILISA
    2 FORMAT(A)
C
C  Number of events to dump
      PRINT 6
  6   FORMAT(/' Number of events to dump? [none if 0 or less]')
      READ *,NDUMP
100   CONTINUE
      NF=0
      ISUNIT=1
      OPEN(UNIT=ISUNIT,FILE=FILISA,STATUS='OLD',FORM='UNFORMATTED')
C
C  Initialize ZEBRA
      CALL ISAZEB('I')
      CALL UCTOH('ISAB',BEGR,4,4)
      CALL UCTOH('ISAF',ENDR,4,4)
C
C  Read beginning record
      CALL FZIN(ISUNIT,IXMAIN,LISAE,1,' ',0,0)
      IF(IQ(LISAE-4).NE.BEGR) THEN   ! not an ISAJET file
        PRINT 3,IQ(LISAE-4)
    3   FORMAT('  IT IS NOT AN ISAJET FILE, FIRST BANK IS ',A4,
     $   /,'  JOB TERMINATED')
        STOP
      ENDIF
C
C  Read events
   10 IXWIPE=IXCOM+IXMAIN
      CALL MZWIPE(IXWIPE)
      CALL FZIN (ISUNIT,IXMAIN,LISAE,1,' ',0,0)
      IOS = IQUEST(1)
      IF(IOS.EQ.0) THEN     ! handle non-event record
        IF(IQ(LISAE-4).EQ.BEGR) IOS=1
        IF(IQ(LISAE-4).EQ.ENDR) IOS=2
C               Move header to run division
        IF(IOS.EQ.1) THEN
          IXWIPE=IXCOM+IXDVR
          CALL MZWIPE(IXWIPE)
          CALL MZCOPY(IXMAIN,LISAE,IXDVR,LISAB,1,' ')
        ENDIF
      ENDIF
C
      IF (IOS .LT. 0) THEN
         WRITE (ITLIS,102) IQUEST(1)
 102     FORMAT ('0Error encountered with IQUEST(1) = ',I5)
      ENDIF

      IF(IOS.EQ.0) THEN   ! this is an event record
C
        NT=NT+1
        NF=NF+1
        CALL ISZUSR(ITLIS)            ! User provided subroutine
C                                     ! see example in ISAZEB
        IF(NDUMP.GT.0) THEN
          NDUMP=NDUMP-1
          CALL PRTEVZ(ITLIS)
        ENDIF
        GOTO 10
      ENDIF
      IF(IOS.LT.3) GOTO 10
      CLOSE(ISUNIT)
      PRINT 41,NF,NT
 41   FORMAT(' No. of event on file =, 'I6,'  total = ',I7)
      NF=0
      PRINT 5
   5  FORMAT(' Next file (STOP to stop)')
      READ 2,FILISA
      IF(FILISA.NE.'STOP') GO TO 100
C
      CALL ISZUSD(ITLIS)              ! user provided subroutine
C                                     ! for end of job (entry point
C                                     ! in ISZUSR example)
      STOP
      END
