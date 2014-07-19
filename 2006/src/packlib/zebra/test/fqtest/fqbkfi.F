*
* $Id: fqbkfi.F,v 1.2 1996/04/18 16:14:30 mclareni Exp $
*
* $Log: fqbkfi.F,v $
* Revision 1.2  1996/04/18 16:14:30  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:01  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE FQBKFI

C-    Read back after fan-out for FQBKFA or FQBKCO

#include "zebra/zstate.inc"
#include "zebra/zunit.inc"
#include "zebra/mzbits.inc"
#include "zebra/quest.inc"
C! +CDE, FZCI, FZCSEG.
#include "zebra/fzcseg.inc"
#include "test_include/testll.inc"
#include "test_include/testdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
#include "test_include/fqtlun.inc"
C--------------    END CDE                             --------------
      DIMENSION    MHEA(10,3)

#include "zebra/q_jbit.inc"

      CALL VFILL (MHEA,30,-99)
      CALL ZPHASE (4)
      IXWIPE = MZIXCO (IXSTOR+3,4,0,0)

      MINPR  = 19
      MAXPR  = 1
      JMAKE  = 0
      JGOOD  = 0



C--                POSITION PRECURSORS

   41 DO 49 JL=1,3
      IF (MHEA(3,JL).EQ.77777)     GO TO 49
      LUNIN = LUNTV(JL)
   46 IF (MHEA(3,JL).LT.JMAKE)     GO TO 47
      IF (MHEA(2,JL).EQ.JL+900)    GO TO 49
   47 MHEA(1,JL) = 9
      CALL FZIN (LUNIN,IXSTOR,0,0,'S',MHEA(1,JL),MHEA(2,JL))
C     WRITE (IQLOG,9847) JMAKE,LUNIN,(MHEA(J,JL),J=1,5)
C9847 FORMAT (10X,'JMAKE=',I4,' LUN=',I3,' reads MHEA(1:5)=',5I5)
      IF (IQUEST(1))         47,46,48
   48 IF (IQUEST(1).LT.5)  THEN
C.        IF (IQUEST(1).EQ.1)      GO TO 47
C.        CALL FZENDI (LUNIN,'.')
          GO TO 47
        ELSE
          MHEA(3,JL) = 77777
          IF (JL.EQ.1)             GO TO 98
        ENDIF
   49 CONTINUE

C--                LIFT EXPECTED STRUCTURE

      CALL CQHIDS (JMAKE,3)

      JSNAP = 0
      IF ((JMAKE.GE.MINPR) .AND. (JMAKE.LT.MAXPR))  JSNAP = 7

C--                Do 901

      IF (MHEA(3,1).NE.JMAKE)      GO TO 53

      LUNIN = LUNT1
      CALL FZIN (LUNIN,IXSTOR,0,0,'T',0,0)
      IF (IQUEST(1).NE.0)          GO TO 94
      MODCOM = 1
      IF (NQSEG.GE.2)  THEN
          IQSEGD(2) = 4
          MODCOM = 0
        ENDIF

      CALL FZIN (LUNIN,IXSTOR+3,LQUSER,1,'D',0,0)
      IF (IQUEST(1).NE.0)          GO TO 94
      M(62) = IQUEST(2)
      M(63) = IQUEST(3)
      M(64) = JMAKE
      IF (JSNAP.NE.0)          CALL DZSNAP ('INLHIG',IXSTOR,'WM.')
      CALL QCOMDS (MODCOM,LOGLEV)
      CALL MZWIPE (IXWIPE)
      JGOOD = JGOOD + 1

C--                Do 902

   53 IF (MHEA(3,2).NE.JMAKE)      GO TO 56

      LUNIN = LUNT2
      CALL FZIN (LUNIN,IXSTOR+3,LQUSER,1,'A',0,0)
      IF (IQUEST(1).NE.0)          GO TO 94
      M(62) = IQUEST(2)
      M(63) = IQUEST(3)
      M(64) = JMAKE
      IF (JSNAP.NE.0)          CALL DZSNAP ('INHIGH',IXSTOR,'WM.')
      CALL QCOMDS (1,LOGLEV)
      CALL MZWIPE (IXWIPE)
      JGOOD = JGOOD + 1

C--                Do 903

   56 IF (MHEA(3,3).NE.JMAKE)           GO TO 69

      IF (LL1.EQ.0)                     GO TO 69
      IF (JBIT(IQ(LL1),IQDROP).NE.0)    GO TO 69

      LUNIN = LUNT3
      CALL FZIN (LUNIN,IXSTOR+3,LQUSER,1,'A',0,0)
      IF (IQUEST(1).NE.0)          GO TO 94
      M(62) = IQUEST(2)
      M(63) = IQUEST(3)
      M(64) = JMAKE
      IF (JSNAP.NE.0)          CALL DZSNAP ('INLOW ',IXSTOR,'WM.')
      CALL QCOMDS (2,LOGLEV)
      JGOOD = JGOOD + 1

   69 CALL MZWIPE (IXSTOR+21)
      JMAKE = JMAKE + 1
      GO TO 41

C----              END OR BAD DATA

   94 CALL ZFATAM ('FQBKFI - unexpected exit')

   98 CALL FQTINF (LUNT1)
      CALL FQTINF (LUNT2)
      CALL FQTINF (LUNT3)
      CALL FZENDI (0,'TX')

      WRITE (IQLOG,9098) JGOOD
 9098 FORMAT (1X/' *!!!!--',I7,' d/s compared successfully.')
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
