*
* $Id: prelib.F,v 1.1.1.1 1996/02/15 17:53:24 mclareni Exp $
*
* $Log: prelib.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:24  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      PROGRAM PRELIB
C PROGRAM PRELIB FOR PRE-LIBRARY PROCESSING OF OBJECT-DECK FILES
C                     J.ZOLL & M.SOLDI, PISA, FEB-1976, MODIF. MAR-83
C                            modified : Dec. 1986
C
      COMMON /NAMES/ JMAIN,NMAIN,MAINS(200)
      COMMON /ALIAS/ JCSECT,MCSECT(2), NALIAS,MALIAS(100)
      COMMON /KARD/  MW(20), NAME(3), ISOL, INTG, NCONT
      COMMON /BUF/   KARD(50000)
C
      LOGICAL*1  MB(80), LNAME(12), LISOL(4), LINTG(4), LCONT(4)
      EQUIVALENCE  (MB(1),MW(1)), (LNAME(1),NAME(1)), (LISOL(1),ISOL)
     +,            (LINTG(1),INTG), (LCONT(1),NCONT)
C
      LOGICAL*1  LRAPP(4)
      EQUIVALENCE (LRAPP(1),IRAPP)
      DATA  IRAPP / 4H(R) /
C
      DATA  MMESD/Z02C5E2C4/,  MMEND/Z02C5D5C4/
      DATA  MMMAIN/4HMain/,   MMDEC/4HDeck/
      DATA  IBLANK/4H    /,   IQUES/4H????/
C
C
C-------           READ NAMES FOR MAIN PROGRAMS
C
      PRINT 9001
 9001 FORMAT ('1PROGRAM PRELIB EXECUTING.'/1X)
C
      JMAIN = 1
      NMAIN = 0
   21 READ (5,8000,END=24) MAINS(NMAIN+1),MAINS(NMAIN+2)
      IF (MAINS(NMAIN+1).EQ.IBLANK)   GO TO 24
      PRINT 9021, MAINS(NMAIN+1),MAINS(NMAIN+2)
 9021 FORMAT (' Stack Progam name  ',2A4)
      NMAIN = NMAIN + 2
      GO TO 21
C
   24 IF (NMAIN.NE.0)  PRINT 9024
 9024 FORMAT (1X)
      NDECK = 0
      ISOL  = IBLANK
      INTG  = 0
      NCONT = 0
C
C-------           START NEW DECK
C
   41 NWKARD = 0
      NALIAS = 0
      JCSECT = 0
      MCSECT(1) = IQUES
      MCSECT(2) = IBLANK
      READ (11,8000,END=91) MW
C!+
C     PRINT 9841, (MW(J),J= 1,12), (MW(J),J= 1,12)
C    +,           (MW(J),J=13,18), (MW(J),J=13,18)
C9841 FORMAT (1X/4X,12(1X,Z8)/4X,12(5X,A4)/
C    F        1X/22X,6(1X,Z8)/22X,6(5X,A4))
C!-
      IF (MW(1).EQ.MMEND)    GO TO 41
      NDECK = NDECK + 1
      IF (MW(1).EQ.MMESD)    GO TO 48
C
C----              STORE PREVIOUS CARD, READ NEXT CARD
C
   44 IF (NWKARD.GE.50000)   GO TO 81
      DO 45  J=1,20
   45 KARD(NWKARD+J) = MW(J)
      NWKARD = NWKARD + 20
C
      READ  (11,8000,END=96) MW
      IF (MW(1).EQ.MMEND)    GO TO 61
      IF (MW(1).NE.MMESD)    GO TO 44
C!+
C     PRINT 9841, (MW(J),J= 1,12), (MW(J),J= 1,12)
C    +,           (MW(J),J=13,18), (MW(J),J=13,18)
C!-
   48 JW =  5
      JB = 25
C
C-------           ESD-CARD, TYPES 0/1 ANALYSED ONLY
C
      LCONT(3)= MB(11)
      LCONT(4)= MB(12)
      NEND = NCONT + 12
C
   52 IF (JB.GE.NEND)        GO TO 44
      LINTG(4) = MB(JB)
      IF (INTG.NE.0)         GO TO 54
      IF (JCSECT.NE.0)       GO TO 55
      MCSECT(1)= MW(JW)
      MCSECT(2)= MW(JW+1)
      JCSECT = NWKARD + JW
      GO TO 57
C
   54 IF (INTG.NE.1)         GO TO 57
   55 MALIAS(NALIAS+1)= MW(JW)
      MALIAS(NALIAS+2)= MW(JW+1)
      NALIAS = NALIAS + 2
   57 JW = JW +  4
      JB = JB + 16
      GO TO 52
C
C-------           END-CARD READ, PUT ALIAS & NAME-CARDS
C
   61 CONTINUE
C!+
C     PRINT 9841, (MW(J),J= 1,12), (MW(J),J= 1,12)
C!-
      NAME(1) = MCSECT(1)
      NAME(2) = MCSECT(2)
      NAME(3) = IBLANK
      IPDECK  = MMDEC
      IF (MW(2).EQ.IBLANK)   GO TO 64
      IPDECK  = MMMAIN
      IF (JMAIN.GE.NMAIN)    GO TO 64
      NAME(1) = MAINS(JMAIN)
      NAME(2) = MAINS(JMAIN+1)
      JMAIN = JMAIN + 2
      IF (JCSECT.EQ.0)       GO TO 64
      KARD(JCSECT)   = NAME(1)
      KARD(JCSECT+1) = NAME(2)
C
   64 PRINT 9064, IPDECK,NAME
 9064 FORMAT (4X,A4,2X,3A4)
C
      CALL PREOUT (KARD,NWKARD)
      WRITE (21,8000) MW
C
C--                cards  ALIAS entry
C
      IF (NALIAS.EQ.0)       GO TO 71
      WRITE (21,9066) (MALIAS(J),J=1,NALIAS)
      PRINT     9067, (MALIAS(J),J=1,NALIAS)
 9066 FORMAT (8H ALIAS   ,2A4)
 9067 FORMAT (20X,8H ALIAS   ,2A4,1X,2A4,1X,2A4,1X,2A4)
C
C--                card  NAME deck(*)  with blank suppression
C
   71 JJ = 1
   75 LISOL(1) = LNAME(JJ)
      IF (ISOL.EQ.IBLANK)    GO TO 76
      JJ = JJ + 1
      IF (JJ.LT.9)           GO TO 75
C
   76 DO 77 J=1,3
      LNAME(JJ) = LRAPP(J)
   77 JJ = JJ + 1
      WRITE (21,9078) NAME
 9078 FORMAT (8H NAME    ,3A4)
      GO TO 41
C
C-------           Buffer overflow
C
   81 CALL PREOUT (KARD,NWKARD)
      NWKARD = 0
      JCSECT = 0
      GO TO 44
C
C-------           NORMAL EOF
C
   91 PRINT 9091, NDECK
      STOP
C
C----              ABNORMAL EOF
C
   96 PRINT 9096
      STOP
C
 8000 FORMAT (20A4)
 9091 FORMAT (1H0,I6,'  DECKS.')
 9096 FORMAT (1H0, 3(/1X,12(1H*)),'   UNEXPECTED EOF.')
      END
      SUBROUTINE PREOUT (MMM,NW)

      DIMENSION    MMM(NW)

      WRITE (21,8000) MMM
      RETURN

 8000 FORMAT (20A4)
      END
