*
* $Id: zdebug.F,v 1.1.1.1 1996/03/08 12:01:11 mclareni Exp $
*
* $Log: zdebug.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:11  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZDEBUG(IZ,LOUT,IERROR,KEY)
C
C     ******************************************************************
C     *                                                                *
C     *       DEBUG THE DATA BANK  STRUCTURE                           *
C     *       PRINT POINTERS TO DATA BANKS , LENGTHS                   *
C     *             AND NUMBER OF THE IDENTIFIERS                      *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),KEY(1),IKEY(6)
      DATA IPRINT/0/
C
C     ------------------------------------------------------------------
C
      IF (IPRINT.NE.0)                           GO TO 99
      JZ     = IZ(1)
      NWORDS = IZ(JZ + 14)
      IF (IERROR.EQ. - 1)                        GO TO 90
      IERROR=0
C
C            TEST LENGTH OF THE STRUCTURE
C
  10  IF (NWORDS.GT.0)                           GO TO 20
      IF (IPRINT.EQ.0)                           GO TO 90
      WRITE(LOUT,1100)NWORDS
      GO TO 99
  20  CONTINUE
C
C            TEST LENGTH OF USED AND UNUSED SPACE
C
      LUSED  = IZ(JZ + 15)
      LUNUS  = IZ(JZ + 1)
      IF (NWORDS.EQ.LUSED + LUNUS)               GO TO 30
      IF (IPRINT.EQ.0)                           GO TO 90
      WRITE(LOUT,1200)NWORDS,LUSED,LUNUS
      GO TO 99
  30  CONTINUE
C
      NBANK  = IZ(JZ + 3)
      IF (IPRINT.NE.0)WRITE(LOUT,1300)NWORDS,LUSED,LUNUS,NBANK
C
C             TEST IF FIRST UNUSED WORD CONTAIN 0.
C
      II     = IZ(JZ + 15)
      IFUNUS = IZ(II)
      IF (IFUNUS.EQ.0)                           GO TO 40
      IF (IPRINT.EQ.0)                           GO TO 90
      WRITE(LOUT,1400)IFUNUS
C
C            LOOP ON THE  NBANK  BANKS
C
  40  IF (IPRINT.NE.0)WRITE(LOUT,1600)
C
      IFIRST = JZ + IZ(JZ) + 3
  50  IF (IFIRST.GE.IZ(JZ + 15))                 GO TO 99
      NLONG  = IABS(IZ(IFIRST))
      IER    = 1
      IF (NLONG.GT.NWORDS - IFIRST)              GO TO 60
      ILAST  = IFIRST + NLONG - 1
      IF (IZ(IFIRST).LT.0)                       GO TO 70
      NID    = IZ(ILAST)
      IER    = 2
      IF (NID.LT.IZ(JZ + 12))                    GO TO 60
      IF (NID.GT.IZ(JZ + 13))                    GO TO 60
      NLINK  = IZ(ILAST - 1)
      IDATA=IFIRST+NLINK+1
      IF (IPRINT.NE.0)WRITE(LOUT,1700)
     +       IFIRST,IZ(IFIRST),ILAST,NID,IZ(NID)
     +      ,IZ(ILAST-3),IZ(ILAST-2)
      IER    = 3
      IF(IDATA.NE.IZ(NID))GO TO 60
      IER    = 4
      IF(IDATA+IZ(IDATA)+2.EQ.ILAST)GO TO 70
  60  IF (IPRINT.EQ.0)                           GO TO 90
      WRITE(LOUT,1800)IFIRST
      IF (IER.GT.1)WRITE(LOUT,2050)IZ(ILAST-3),IZ(ILAST-2),IZ(ILAST-1)
      IF (IER.EQ.1)WRITE(LOUT,2100)IZ(IFIRST)
      IF (IER.EQ.2)WRITE(LOUT,2200)NID
      IF(IER.EQ.3)WRITE(LOUT,2300)IDATA,NID
      IF(IER.EQ.4)WRITE(LOUT,2400)IZ(IDATA)
      GO TO 99
C
  70  IFIRST = ILAST + 1
      GO TO 50
C
  90  IPRINT = 1
      CALL UCTOH1(KEY,IKEY,6)
      WRITE(LOUT,1000)IKEY
      GO TO 10
C
 1000 FORMAT(///,' ***** ZDEBUG IS CALLED AT ',6A1,/)
 1100 FORMAT(' ZBOOK ERROR IN STRUCTURE LENGTH',I6)
 1200 FORMAT(' ZBOOK ERROR    STRUCTURE LENGTH',I6,' .NE. LENGTH'
     + ,' USED',I6,' + LENGTH UNUSED',I6)
 1300 FORMAT(' ZBOOK          STRUCTURE LENGTH',I6,'      LENGTH'
     + ,' USED',I6,'   LENGTH UNUSED',I6,'  NUMBER OF BANKS ',I6)
 1400 FORMAT(' FIRST UNUSED WORD SHOULD BE ZERO AND NOT ',I6)
 1500 FORMAT(' NUMBER OF BANKS  IS NOT REASONABLE',I6)
 1600 FORMAT(///,6X,'IFIRST',5X,'IZ(IFIRST)',5X,'ILAST',5X,
     +       'IZ(ILAST)',5X,'ID=IZ(IZ(ILAST))',
     +       5X,'NAME',7X,'NUMBER',/)
 1700 FORMAT(6X,I6,6X,I6,6X,I6,6X,I8,7X,I6,14X,A4,1X,I10)
 1800 FORMAT(' ***** BANK STARTING AT ADDRESS',I10,
     +       '  HAS BEEN OVERWRITTEN  ',/)
 2050 FORMAT(' ***** USER BANK NAME =',A4,/,
     +       ' ***** USER BANK NUMBER =',I10,/,
     +       ' ***** NUMBER OF POINTERS =',I10,/)
 2100 FORMAT(' ***** THE TOTAL NUMBER OF WORDS IN THE BANK =',
     +       I10,/,' ***** HAS BEEN OVERWRITTEN',/)
 2200 FORMAT(' ***** THE LAST WORD OF THE BANK =',I10,/,
     +       ' ***** IS NOT A VALID POINTER NUMBER',/)
 2300 FORMAT(' ***** THE BANK ADDRESS ',I10,/,
     +       ' ***** DOES NOT MATCH WITH THE POINTER NO =',I10,/)
 2400 FORMAT(' ***** THE NUMBER OF DATA WORDS ',I10,/,
     +       ' ***** HAS BEEN OVERWRITTEN',/)
C
  99  IF (IERROR.EQ. - 1)IPRINT = 0
      IF (IPRINT.NE.0)IERROR = IPRINT
      RETURN
      END
