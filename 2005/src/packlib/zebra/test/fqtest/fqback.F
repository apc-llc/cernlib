*
* $Id: fqback.F,v 1.2 1996/04/18 16:14:29 mclareni Exp $
*
* $Log: fqback.F,v $
* Revision 1.2  1996/04/18 16:14:29  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:01  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE FQBACK

C-    Sequential read-back

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
      DIMENSION    MHEA(10)
      EQUIVALENCE (MHEA(1),M(71))

#include "zebra/q_jbit.inc"
#include "zebra/q_jbyt.inc"


      CALL ZPHASE (3)
      IXWIPE = MZIXCO (IXSTOR+3,4,0,0)

      LUNIN  = LUNT1
      MINPR  = 19
      MAXPR  = 1
      JGOOD = 0
      JMAKE  = 0


   41 MHEA(1) = 9


      CALL CQHIDS (JMAKE,3)

      JSNAP = 0
      IF ((JMAKE.GE.MINPR) .AND. (JMAKE.LT.MAXPR))  JSNAP = 7

   42 MHEA(1) = 9
#if defined(CERNLIB_QTESTLIB)||defined(CERNLIB_QFIMDANY)
      IF (IFLXQ(6).NE.0)  THEN
          CALL FQMGET
          IF (IQUEST(1).NE.0)      GO TO 91
        ENDIF
#endif
      CALL FZIN (LUNIN,IXSTOR,0,0,'S',MHEA(1),MHEA(2))
      IF (IQUEST(1).NE.0)          GO TO 91
      IF (MHEA(2).NE.901)          GO TO 97
      IF (JBYT(JMAKE,2,3).EQ.7)    GO TO 51

      CALL FZIN (LUNIN,IXSTOR,0,0,'T',0,0)
      IF (IQUEST(1).NE.0)          GO TO 94
      MODCOM = 1
      IF (NQSEG.LT.2)              GO TO 49
      IQSEGD(2) = 4
      IF (JBIT(JMAKE,3).NE.0)  THEN
          IQSEGD(1) = -1
        ELSE
          MODCOM = 0
        ENDIF

   49 CALL FZIN (LUNIN,IXSTOR+3,LQUSER,1,'D',0,0)
      IF (IQUEST(1).NE.0)          GO TO 94
      M(62) = IQUEST(2)
      M(63) = IQUEST(3)
      M(64) = JMAKE
      IF (JSNAP.NE.0)          CALL DZSNAP ('INLHIG',IXSTOR,'WM.')
      CALL QCOMDS (MODCOM,LOGLEV)
      JGOOD = JGOOD + 1
      CALL MZWIPE (IXWIPE)

   51 MHEA(1) = 9
#if defined(CERNLIB_QTESTLIB)||defined(CERNLIB_QFIMDANY)
      IF (IFLXQ(6).NE.0)  THEN
          CALL FQMGET
          IF (IQUEST(1).NE.0)      GO TO 91
        ENDIF
#endif
      CALL FZIN (LUNIN,IXSTOR+3,LQUSER,1,'.',MHEA(1),MHEA(2))
      IF (IQUEST(1).NE.0)          GO TO 94
      M(62) = IQUEST(2)
      M(63) = IQUEST(3)
      M(64) = JMAKE
      IF (JSNAP.NE.0)          CALL DZSNAP ('INHIGH',IXSTOR,'WM.')
      CALL QCOMDS (1,LOGLEV)
      JGOOD = JGOOD + 1
      CALL MZWIPE (IXWIPE)

      IF (LL1.EQ.0)                     GO TO 69
      IF (JBIT(IQ(LL1),IQDROP).NE.0)    GO TO 69

#if defined(CERNLIB_QTESTLIB)||defined(CERNLIB_QFIMDANY)
      IF (IFLXQ(6).NE.0)  THEN
          CALL FQMGET
          IF (IQUEST(1).NE.0)      GO TO 91
        ENDIF
#endif
      CALL FZIN (LUNIN,IXSTOR+3,LQUSER,1,'.',MHEA(1),MHEA(2))
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

   91 IF (IQUEST(1).LT.0)    CALL ZFATAM ('FQBACK - READ ERROR.')
      IF (IQUEST(1).GE.5)          GO TO 98
      IF (IQUEST(1).LT.4)          GO TO 42
      CALL FZENDI (LUNIN,'C')
      GO TO 42

   94 CALL ZFATAM ('FQBACK - unexpected exit')

   97 CALL ZFATAM ('FQBACK - first u/header word is not 901')

   98 CALL FQTINF (LUNIN)
      CALL FZENDI (LUNIN,'TX')

      WRITE (IQLOG,9098) JGOOD
 9098 FORMAT (1X/' *!!!!--',I7,' d/s compared successfully.')
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
