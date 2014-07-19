*
* $Id: fqbkra.F,v 1.2 1996/04/18 16:14:30 mclareni Exp $
*
* $Log: fqbkra.F,v $
* Revision 1.2  1996/04/18 16:14:30  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:01  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE FQBKRA

C-    Random read-back

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
      EQUIVALENCE (LQDATB,LQUSER(4))
      DIMENSION    MHEA(10)
      EQUIVALENCE (MHEA(1),M(71))

#include "zebra/q_jbit.inc"

      CALL ZPHASE (3)

      LUNIN  = LUNT1

C--       get the Direct Access Table
      CALL FZIDAT (LUNIN,IXHOLD,LQDATB,1)
      IF (IQUEST(1).NE.0)          GO TO 93
      NDOEND = IQ(LQDATB+1)

C------     Read back of the data in 4 series taking every 4th d/s
C--              starting with d/s "js", with js = 1, 2, 3, 4

      JSERIE = 0
      JDO    = 2
      JGOOD  = 0

C----              Loop to read d/ss  js, js+4, js+8, js+12, ...

   41 JMAKE = IQ(LQDATB+JDO)
      CALL FZINXT (LUNIN, IQ(LQDATB+JDO+1), IQ(LQDATB+JDO+2))

      MHEA(1) = 9
      CALL CQHIDS (JMAKE,3)

      MHEA(1) = 9
      CALL FZIN (LUNIN,IXSTOR,0,0,'S',MHEA(1),MHEA(2))
      IF (IQUEST(1).NE.0)          GO TO 91
      IF (MHEA(2).NE.901)          GO TO 97

      CALL FZIN (LUNIN,IXSTOR,0,0,'T',0,0)
      IF (IQUEST(1).NE.0)          GO TO 92
      MODCOM = 1
      IF (NQSEG.LT.2)              GO TO 49
      IQSEGD(2) = 4
      IF (JBIT(JMAKE,3).NE.0)  THEN
          IQSEGD(1) = -1
        ELSE
          MODCOM = 0
        ENDIF

   49 CALL FZIN (LUNIN,IXSTOR+3,LQUSER,1,'D',0,0)
      IF (IQUEST(1).NE.0)          GO TO 92
      M(62) = IQUEST(2)
      M(63) = IQUEST(3)
      M(64) = JMAKE
      CALL QCOMDS (MODCOM,LOGLEV)
      JGOOD = JGOOD + 1
      CALL MZWIPE (IXSTOR+21)
      JDO = JDO + 12
      IF (JDO.LT.NDOEND)           GO TO 41

      JSERIE = JSERIE + 1
      IF (JSERIE.GE.4)             GO TO 98

      JDO = 3*JSERIE + 2
      IF (JDO.GE.NDOEND)           GO TO 98
      GO TO 41

C----              END OR BAD DATA

   91 IF (IQUEST(1).LT.0)    CALL ZFATAM ('FQBKRA - READ ERROR.')
   92 CALL ZFATAM ('FQBKRA - unexpected exit')

   93 CALL ZFATAM ('FQBKRA - DAT not obtained')

   97 CALL ZFATAM ('FQBKRA - first u/header word is not 901')

   98 CALL FQTINF (LUNIN)
      CALL FZENDI (LUNIN,'TX')
      CALL FQTINF (0)
      WRITE (IQLOG,9098) JGOOD
 9098 FORMAT (1X/' *!!!!--',I7,' d/s compared successfully.')
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
