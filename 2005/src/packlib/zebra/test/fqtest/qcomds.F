*
* $Id: qcomds.F,v 1.3 1997/03/14 17:22:20 mclareni Exp $
*
* $Log: qcomds.F,v $
* Revision 1.3  1997/03/14 17:22:20  mclareni
* WNT mods
*
* Revision 1.2.2.1  1997/01/21 11:34:09  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.2  1996/04/18 16:14:33  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE QCOMDS (JMODEP,LOGLEV)

#include "zebra/zunit.inc"
#include "zebra/mzcn.inc"
#include "zebra/mzbits.inc"
#include "zebra/quest.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
#include "test_include/testll.inc"
#include "test_include/testdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
C--------------    END CDE                             --------------

#include "zebra/q_jbit.inc"

      JMODE  = JMODEP
      NLREC  = M(62)
      NPHREC = M(63)
      JMAKE  = M(64)

   10 NBANKS = 0
      LHNEX = LQSTA(KQT+3)
      LHEND = LQEND(KQT+3)
      LHSTA4= LQSTA(KQT+4)
      LHEND4= LQEND(KQT+4)

      LMNEX = LQSTA(KQT+1)
      LMEND = LQEND(KQT+1)
      LMSTA2= LQSTA(KQT+2)
      LMGO  = 0
      IF (JMODE-1)           11,13,15

C--                LOW + HIGH BANKS

   11 LMGO  = LQSTA(KQT+2)
      LMEND2= LQEND(KQT+2)
      GO TO 31

C--                HIGH BANKS ONLY

   13 LMNEX = LQSTA(KQT+2)
      LMEND = LQEND(KQT+2)
      GO TO 31

C--                LOW BANKS ONLY

   15 CONTINUE

C-------           POSITION NEXT BANK TO COMPARE

   31 JSW   = -777
      JL    = 0
      LH    = LHNEX
      IF (LH.GE.LHEND)  THEN
          IF (LHEND.EQ.LHEND4)     GO TO 32
          LHNEX = LHSTA4
          LHEND = LHEND4
          GO TO 31
        ENDIF

      CALL MZCHLN (-7,LH)
      IF (IQFOUL.NE.0)             GO TO 93
      LSH   = IQLS
      LHNEX = IQNX
      IF (JBIT(IQ(LSH),IQDROP).NE.0)      GO TO 31

   32 LM    = LMNEX
      IF (LM.LT.LMEND)             GO TO 33
      IF (LH.GE.LHEND)             GO TO 81
      IF (LMGO.EQ.0)               GO TO 92
      LM    = LMGO
      LMEND = LMEND2
      LMGO  = 0

   33 CALL MZCHLN (-7,LM)
      IF (IQFOUL.NE.0)             GO TO 93
      LSM   = IQLS
      LMNEX = IQNX
      IF (JBIT(IQ(LSM),IQDROP).NE.0)      GO TO 32

      IF (LH.GE.LHEND)             GO TO 92
      NBANKS= NBANKS + 1

C-------           COMPARE HAVE-BANK TO MUST-BANK

C--                CHECK SAME BANKS NAME

      IF (IQ(LSM-5).NE.IQ(LSH-5))    GO TO 93
      IF (IQ(LSM-4).NE.IQ(LSH-4))    GO TO 93
      IF (IQ(LSM-3).NE.IQ(LSH-3))    GO TO 93
      IF (IQ(LSM-2).NE.IQ(LSH-2))    GO TO 93
      IF (IQ(LSM-1).NE.IQ(LSH-1))    GO TO 93

C--                CHECK LINKS POINT TO SAME BANKS

      JSW = 0

      DO 49 JL=0,IQNL
      LLM = LQ(LSM-JL)
      LLH = LQ(LSH-JL)
      IF (LLH.NE.0)                GO TO 47
      IF (LLM.EQ.0)                GO TO 49
      IF (JBIT(IQ(LLM),IQDROP).NE.0)    GO TO 49

C--                CHECK POINTING TO LOW BANK AND MODE=1

      IF (LLM.GE.LMSTA2)           GO TO 44
      IF (JMODE.EQ.1)              GO TO 49
      GO TO 92

C--                CHECK POINTING TO HIGH BANK AND MODE = 2

   44 IF (JMODE.EQ.2)              GO TO 49
      GO TO 92

   47 IF (IQ(LLM-5).NE.IQ(LLH-5))       GO TO 92
   49 CONTINUE

C----              COMPARE DATA-PARTS OF CURRENT BANK

      JSW = 7

C--       compare integer/floating start region of the bank

      N = MIN (50,IQND)

      DO 52  JL=1,N
      IF (IQ(JL+LSM).NE.IQ(JL+LSH))  GO TO 93
   52 CONTINUE

C--       compare left half of double pre. numbers in bank D1 only

      IF (IQND.LT.60)              GO TO 59
      IF (IQ(LSM-5).NE.15)         GO TO 55

      DO 54  JL=51,60,2
      IF (IQ(JL+LSM).NE.IQ(JL+LSH))  GO TO 93
   54 CONTINUE

C----     compare fractional fl.p. numbers in words 61-70

   55 IF (IQND.LT.61)              GO TO 59
      N = MIN (70,IQND)

      DO 56  JL=61,N
      DIFF = Q(JL+LSM)  - Q(JL+LSH)
      IF (DIFF.EQ.0.)              GO TO 56
      SUM  = Q(JL+LSM)  + Q(JL+LSH)
      IF (DIFF/SUM.GT.1.0E-5)      GO TO 93
   56 CONTINUE
   59 CONTINUE
      GO TO 31

C-------           EXIT OK

   81 IF (LOGLEV.LE.0)      RETURN
      WRITE (IQLOG,9081) NBANKS,NLREC,NPHREC,JMAKE
 9081 FORMAT (5X,I5,' Banks compare ok after',2I6,' L/Ph records,'
     F,' JMAKE=',I4)
      RETURN

C----              COMPARISON FAILS

   92 IF (JBIT(IQ(LSH),IQDROP).EQ.0)  GO TO 93
      CALL MZGARB (IXSTOR+21,0)
*      CALL MZGARB (IXSTOR+21)   There was a bug here ??? V.Fine 10/05/96
      GO TO 10

   93 CALL VZERO (IQUEST(1),50)
      IQUEST(1) =  LH
      IQUEST(2) =  LHNEX
      IQUEST(3) =  LHEND
      IQUEST(4) =  LM
      IQUEST(5) =  LMNEX
      IQUEST(6) =  LMEND

      IQUEST(11) = JMODE
      IQUEST(12) = LSH
      IQUEST(13) = LSM
      IQUEST(14) = IQ(LSH-5)
      IQUEST(15) = IQ(LSM-5)
      IQUEST(16) = IQ(LSH-4)
      IQUEST(17) = IQ(LSM-4)
      IQUEST(18) = JSW
      IQUEST(20) = JL

      IF (JSW.LT.0)                GO TO 99
      IF (JSW.EQ.0)  JL= -JL-8
      IQUEST(21) = IQ(LSH+JL)
      IQUEST(22) = IQ(LSM+JL)

   99 CALL ZFATAM ('QCOMDS - BAD COMPARE.')
      END
*      ==================================================
#include "zebra/qcardl.inc"
