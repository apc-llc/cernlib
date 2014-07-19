*
* $Id: fqmake.F,v 1.2 1996/04/18 16:14:32 mclareni Exp $
*
* $Log: fqmake.F,v $
* Revision 1.2  1996/04/18 16:14:32  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:01  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE FQMAKE

#include "zebra/zstate.inc"
#include "zebra/zunit.inc"
#include "zebra/mzbits.inc"
#include "zebra/quest.inc"
#include "test_include/cqc.inc"
#include "zebra/cqbkc.inc"
#include "test_include/testla.inc"
#include "test_include/testll.inc"
#include "test_include/testdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
#include "test_include/fqtlun.inc"
C--------------    END CDE                             --------------
      EQUIVALENCE (LQDATB,LQUSER(4))

      DIMENSION    MMDATB(5)
      DATA  MMDATB / 0     , 0, 0, 900, 2 /

#include "zebra/q_jbit.inc"

C----              Create the Direct Access Table bank

      CALL UCTOH ('DATB', MMDATB, 4,4)
      CALL MZLIFT (IXHOLD,L, LQDATB,1, MMDATB, 0)
      IQ(LQDATB+1) = 1

C-----             Ready

      LUNFZ  = LUNT1
      LOGLEV = LEVOUT
      CALL FZLOGL (LUNFZ,LOGLEV)

      CALL ZPHASE (1)
      JMAKE = 0


C--       write the DAT forward reference place-holder
      IF (IFLXQ(3).NE.0)  CALL FZODAT (LUNFZ, 0, 0)

C--       write start of run 11
      M(8)   = 20
      M(11) = JMAKE
      M(12) = MAXMAK
      CALL FZRUN (LUNFZ,11,M(8),M(9))
#if defined(CERNLIB_QTESTLIB)||defined(CERNLIB_QFIMDANY)
      IF (IFLXQ(6).NE.0)  CALL FQMPUT
#endif

C--------          Loop over all d/ss        -----------

   21 IF (JMAKE.EQ.MKBREA)         GO TO 25
      IF (JMAKE.EQ.MKBREA+10)      GO TO 24
      IF (JMAKE.EQ.MKBREA+20)      GO TO 24
      IF (JMAKE.NE.MKBREA+30)      GO TO 26

C--                test skip to StoR/EoR works

      IF (IFLXQ(2).EQ.0)           GO TO 24
      CALL FZENDO (LUNFZ,'IO')
   22 M(71) = 9
      CALL FZIN (LUNFZ,0,0,0,'R',M(71),M(72))
      IF (IQUEST(1).NE.1)    CALL ZFATAM ('FQMAKE - not StoR')
      IF (IQUEST(11).NE.14)        GO TO 22
      CALL FZIN (LUNFZ,0,0,0,'2',M(71),M(72))
      IF (IQUEST(1).NE.2)    CALL ZFATAM ('FQMAKE - not EoR')
      CALL FZENDI (LUNFZ,'O')

   24 CALL FZENDO (LUNFZ,'C')
#if defined(CERNLIB_QTESTLIB)||defined(CERNLIB_QFIMDANY)
      IF (IFLXQ(6).NE.0)  CALL FQMPUT
#endif
   25 CALL FZRUN  (LUNFZ,0,M(8),M(9))
#if defined(CERNLIB_QTESTLIB)||defined(CERNLIB_QFIMDANY)
      IF (IFLXQ(6).NE.0)  CALL FQMPUT
#endif

C----              Next set of 2 or 3 d/ss

   26 M(8) = 20
      M(11) = JMAKE
      M(12) = MAXMAK
      CALL CQHIDS (JMAKE,3)

      JSNAP = 0
      IF ((JMAKE.GE.MINPR) .AND. (JMAKE.LT.MAXPR))  JSNAP = 7
      IF (JSNAP.NE.0)          CALL DZSNAP ('OUT',IXSTOR,'MW.')

      M(9) =  901
      CALL FZOUT (LUNFZ, IXSTOR,  LQMAIN, 1, 'L', 2,M(8),M(9))
#if defined(CERNLIB_QTESTLIB)||defined(CERNLIB_QFIMDANY)
      IF (IFLXQ(6).NE.0)  CALL FQMPUT
#endif

      N = IQ(LQDATB+1)
      IQ(LQDATB+N+1) = JMAKE
      IQ(LQDATB+N+2) = IQUEST(5)
      IQ(LQDATB+N+3) = IQUEST(6)
      IQ(LQDATB+1)   = N + 3

      M(9) =  902
      CALL FZOUT (LUNFZ, IXSTOR+2,LQMAIN, 0, 'D', 2,M(8),M(9))
#if defined(CERNLIB_QTESTLIB)||defined(CERNLIB_QFIMDANY)
      IF (IFLXQ(6).NE.0)  CALL FQMPUT
#endif

      IF (LL1.EQ.0)                     GO TO 28
      IF (JBIT(IQ(LL1),IQDROP).NE.0)    GO TO 28
      M(9) =  903
      CALL FZOUT (LUNFZ, IXSTOR+1,LL1,    0, 'L', 2,M(8),M(9))
#if defined(CERNLIB_QTESTLIB)||defined(CERNLIB_QFIMDANY)
      IF (IFLXQ(6).NE.0)  CALL FQMPUT
#endif

   28 CALL MZWIPE (IXSTOR+21)
      JMAKE = JMAKE + 1
      IF (JMAKE.LT.MAXMAK)         GO TO 21

C--                Done, write the Direct Access Table

      NU  = IQ(LQDATB+1)
      NT  = IQ(LQDATB-1)
      INC = NU - NT
      CALL MZPUSH (IXHOLD,LQDATB, 0,INC, 'I')
      IF (IFLXQ(4).NE.0)  CALL FZODAT (LUNFZ, IXHOLD, LQDATB)
      IF (IFLXQ(5).NE.0)  CALL FQRSET (1, LUNFZ)
      CALL MZDROP (IXHOLD, LQDATB, 'L')

      CALL FQTINF (LUNFZ)

C--                Write the EoR for memory mode

#if defined(CERNLIB_QTESTLIB)||defined(CERNLIB_QFIMDANY)
      IF (IFLXQ(6).NE.0)  THEN
          CALL FZRUN  (LUNFZ,-1,0,0)
          CALL FQMPUT
        ENDIF
#endif
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
