*
* $Id: mzcote.F,v 1.1.1.1 1996/03/06 10:47:00 mclareni Exp $
*
* $Log: mzcote.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
      SUBROUTINE MZCOTE

C-    standard test of MZCOPY + MZATTM

#include "zebra/zstate.inc"
#include "zebra/zunit.inc"
#include "zebra/mzbits.inc"
#include "zebra/quest.inc"
#include "test_include/testla.inc"
#include "test_include/testll.inc"
#include "test_include/testdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
C--------------    END CDE                             --------------
      COMMON /SECSTO/SECFEN(4),LSMAIN,SECST(6000)
      DIMENSION    BUF(10000), TABLE(400)


#include "zebra/q_jbit.inc"

C     NQDEVZ = 7

      CALL MZSTOR (IXSTOR,'//', '.', FENCE
     +,            LQ,LA1,LX1,LQ(NQLIM2),IQ(NQN))

      CALL MZLOGL (IXSTOR,1)
      CALL MZWORK (IXSTOR,M,DLAST,0)
      CALL VZERO  (LQUSER,150)

      CALL MZDIV (IXSTOR,IXDV3,'DIV3',4000,6000, '.')
      CALL MZDIV (IXSTOR,IXDV4,'DIV4', 100, 100, 'R')
      IXWIPE = IXDV3

      CALL MZSTOR (IXSTSE,'SECSTO', '.', SECFEN
     +,            LSMAIN,SECST,SECST,SECST(2000),SECST(6000))
      IXDVSE = IXSTSE+2

      IXMEM  = 0

      CALL CQBKIN
      LOGLEV = 0
      LOGCOM = 0
      CALL MZLOGL (IXSTOR,LOGLEV)

      CALL ZPHASE (1)

      NCOUNT = 0
      MINPR  = 19
      MAXPR  = 1
C!    MAXMAK = 255   maximum
      MAXMAK = 24
      M(8)   = 8


      JMAKE = 0

#include "test_include/maxmak.inc"
* Ignoring t=pass

   21 CONTINUE
      IF (LOGLEV.GT.0)  CALL MZLOGL (IXSTOR,0)

      CALL CQHIDS (JMAKE,3)

      JSNAP = 0
      IF ((JMAKE.GE.MINPR) .AND. (JMAKE.LT.MAXPR))  JSNAP = 7
      IF (JSNAP.NE.0)          CALL DZSNAP ('OUT',IXSTOR,'MW.')

      IF (LOGLEV.GT.0)  CALL MZLOGL (IXSTOR,LOGLEV)

C------            'L' from div 1+2 -> 3 direct

CD    IF (JMAKE.LT.2)  THEN
CD        NQLOGD = 2
CD        NQDEVZ = 7
CD      ENDIF
      M(9) =  900
      CALL MZCOPY (IXSTOR,  LQMAIN,  IXSTOR+3,LQUSER,1, 'L')
      IF (IQUEST(1).NE.0)          GO TO 91
      M(62) = 1
      M(63) = 0
      M(64) = JMAKE
      IF (JSNAP.GT.0)          CALL DZSNAP ('INLHIG',IXSTOR,'WM.')
      CALL QCOMDS (0,LOGCOM)
      NCOUNT = NCOUNT + 1
CD    NQLOGD = 0
CD    NQDEVZ = 0
      CALL MZWIPE (IXWIPE)

C------            'D' from div 2 -> 3 via secondary div 2

CD    IF (JMAKE.LT.2)  THEN
CD        NQLOGD = 2
CD        NQDEVZ = 7
CD      ENDIF
      M(9) =  901
      CALL MZCOPY (IXSTOR+2,LQMAIN,  IXDVSE,  LSMAIN,1, 'D')
      IF (IQUEST(1).NE.0)          GO TO 91

      CALL MZCOPY (IXDVSE,  LSMAIN,  IXSTOR+3,LQUSER,1, 'D')
      IF (IQUEST(1).NE.0)          GO TO 91
      M(62) = 2
      M(63) = 0
      M(64) = JMAKE
      IF (JSNAP.GT.0)          CALL DZSNAP ('INHIGH',IXSTOR,'WM.')
      CALL QCOMDS (1,LOGCOM)
      NCOUNT = NCOUNT + 1
CD    NQLOGD = 0
CD    NQDEVZ = 0
      CALL MZWIPE (IXDVSE)
      CALL MZWIPE (IXWIPE)

C------            'L' from div 1 -> 3 direct

      IF (LL1.EQ.0)                     GO TO 28
      IF (JBIT(IQ(LL1),IQDROP).NE.0)    GO TO 28
CD    IF (JMAKE.LT.2)  THEN
CD        NQLOGD = 2
CD        NQDEVZ = 7
CD      ENDIF
      M(9) =  902
      CALL MZCOPY (IXSTOR+1,LL1,     IXSTOR+3,LQUSER,1, 'L')
      IF (IQUEST(1).NE.0)          GO TO 91
      M(62) = 3
      M(63) = 0
      M(64) = JMAKE
      IF (JSNAP.GT.0)          CALL DZSNAP ('INLOW ',IXSTOR,'WM.')
      CALL QCOMDS (2,LOGCOM)
      NCOUNT = NCOUNT + 1
CD    NQLOGD = 0
CD    NQDEVZ = 0
      CALL MZWIPE (IXWIPE)

C------            'LT' from div 1+2 -> 3 via BUF, off-set zero

   28 CONTINUE
CD    IF (JMAKE.LT.2)  THEN
CD        NQLOGD = 2
CD        NQDEVZ = 7
CD      ENDIF
      M(9) =  903
      IQUEST(3) = 10000
      IQUEST(4) = 0
      CALL MZCOPY (IXSTOR,  LQMAIN,  BUF,LENTRY,NWTO,   'LT')
      IF (IQUEST(1).NE.0)          GO TO 91

      IQUEST(1) = NWTO
      IQUEST(2) = 0
      CALL MZCOPY (BUF, LENTRY,      IXSTOR+3,LQUSER,1, 'F')
      IF (IQUEST(1).NE.0)          GO TO 91
      M(62) = 4
      M(63) = 1
      M(64) = JMAKE
      IF (JSNAP.GT.0)          CALL DZSNAP ('INLHIG',IXSTOR,'WM.')
      CALL QCOMDS (0,LOGCOM)
      NCOUNT = NCOUNT + 1
CD    NQLOGD = 0
CD    NQDEVZ = 0
      CALL MZWIPE (IXWIPE)

C------            'DT' from div 2 -> 3 via BUF, off-set 12

CD    IF (JMAKE.LT.2)  THEN
CD        NQLOGD = 2
CD        NQDEVZ = 7
CD      ENDIF
      M(9) =  904
      IQUEST(3) = 10000
      IQUEST(4) = 12
      CALL MZCOPY (IXSTOR+2,LQMAIN,  BUF,LENTRY,NWTO,   'DT')
      IF (IQUEST(1).NE.0)          GO TO 91

      IQUEST(1) = NWTO
      IQUEST(2) = 12
      CALL MZCOPY (BUF, LENTRY,      IXSTOR+3,LQUSER,1, 'F')
      IF (IQUEST(1).NE.0)          GO TO 91
      M(62) = 5
      M(63) = 1
      M(64) = JMAKE
      IF (JSNAP.GT.0)          CALL DZSNAP ('INHIGH',IXSTOR,'WM.')
      CALL QCOMDS (1,LOGCOM)
      NCOUNT = NCOUNT + 1
CD    NQLOGD = 0
CD    NQDEVZ = 0
      CALL MZWIPE (IXWIPE)

C------            'LT' from div 1+2 -> 3 via Attached store

CD    IF (JMAKE.LT.2)  THEN
CD        NQLOGD = 2
CD        NQDEVZ = 7
CD      ENDIF
      M(9) =  905
      IQUEST(3) = 10000
      IQUEST(4) = 0
      CALL MZCOPY (IXSTOR,  LQMAIN,  BUF,LENTRY,NWTO,   'LT')
      IF (IQUEST(1).NE.0)          GO TO 91

      CALL MZATTM (IXMEM,'ATTACH',BUF,1,NWTO,0,' ',TABLE)
      CALL MZCOPY (IXMEM+1, LENTRY,  IXSTOR+3,LQUSER,1, 'DI')
      IF (IQUEST(1).NE.0)          GO TO 91
      M(62) = 6
      M(63) = 2
      M(64) = JMAKE
      IF (JSNAP.GT.0)          CALL DZSNAP ('INLHIG',IXSTOR,'WM.')
      CALL QCOMDS (0,LOGCOM)
      NCOUNT = NCOUNT + 1
CD    NQLOGD = 0
CD    NQDEVZ = 0

      CALL MZWIPE (IXSTOR+21)
      JMAKE = JMAKE + 1
      IF (JMAKE.LT.MAXMAK)         GO TO 21

      WRITE (IQLOG,9081) NCOUNT
 9081 FORMAT (1X/1X,I8,' data-structures copied and compared.')
      RETURN

C----              Copy failed

   91 CALL ZFATAM ('MZCOTE - failed.')
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
