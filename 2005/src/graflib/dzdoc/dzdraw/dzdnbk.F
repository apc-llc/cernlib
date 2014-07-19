*
* $Id: dzdnbk.F,v 1.1.1.1 1996/03/04 16:13:16 mclareni Exp $
*
* $Log: dzdnbk.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:16  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDNBK(IXDIVP,LGOP,LBNK,LFW,LLW,IDH,IDRFLG)
 
*.
*...DZDNBK
*.
*.    FIND NEXT BANK IN DIV. IXDIV AFTER BANK AT LGOP
*.
*. INPUT     : IXDIVP   division index
*.             LGOP     link to bank to start with
*.
*. OUTPUT    : LBNK     link to bank (0 if not found)
*.             LFW, LLW first, last address of bank
*.             IDH      hollerith Id of bank
*.             IDRFLG   drop flag (1 if dropped)
*.
*. COMMON    :
*. SEQUENCE  : MZBITS MZCA   MZCB   MZCC   MZCN   QUEST  ZEBQ
*. CALLS     : MZCHLN MZCHLS MZSDIV ZFATAL
*. CALLED    :
*.
*. AUTHOR    : O. Schaile
*. VERSION   : 1.00
*. CREATED   :  3-Feb-88
*.**********************************************************************
*.
 
*
#include "zebra/mzbits.inc"
#include "zebra/quest.inc"
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
#include "zebra/mzcn.inc"
C--------------    END CDE                             --------------
      DIMENSION    IXDIVP(9), LGOP(9)
      DIMENSION    NAMESR(2)
      DATA  NAMESR / 4HDZDN, 4HBK   /
 
      IXIN = IXDIVP(1)
*      IDH  = IDHP(1)
*      IDN  = IDNP(1)
      LGO  = LGOP(1)
 
      CALL MZSDIV (IXIN,1)
      LSTA = LQSTA(KQT+JQDIVI)
      IQNX = LSTA
      LEND = LQEND(KQT+JQDIVI)
 
      IF (LGO.EQ.0)          GO TO 10
 
      IF (LGO.LT.LSTA)       GO TO 50
      IF (LGO.GE.LEND)       GO TO 50
 
      CALL MZCHLS (-7,LGO)
*      IF (IQFOUL.NE.0)       GO TO 92
 
      IF(IQFOUL.NE.0)THEN
          WRITE(*,*)' Error from MZCHLS, IQFOUL',IQFOUL
          WRITE(*,'(A)')IQ(KQS+LGO-4)
          WRITE(*,*)' Dump around link (-+10)'
          WRITE(*,'(4(5Z9/))')(IQ(KQS+LGO+K),K=-10,10)
      ENDIF
 
   10 LN   = IQNX
      IF (LN.GE.LEND)        GO TO 20
      CALL MZCHLN (-7,LN)
      IF (IQFOUL.NE.0)       GO TO 30
      IF (IQND.LT.0)         GO TO 10
*      IF (IQ(KQS+IQLS-5).NE.IDN)           GO TO 31
*      IF (IQID.NE.IDH)       GO TO 31
      IF (JBIT(IQ(KQS+IQLS),IQDROP).NE.0)THEN
           IDRFLG = 1
      ELSE
           IDRFLG = 0
      ENDIF
      ND = IQ(KQS+IQLS-1)
      NIO = JBYT(IQ(KQS+IQLS),19,4)
      NL =  IQ(KQS+IQLS-3)
      NSYS = NIO + NL + 10
      LFW = IQLS - NL - NIO - 1
      LLW = LFW + NSYS + ND
      IDH = IQID
      LBNK = IQLS
      RETURN
 
   20 LBNK = 0
      RETURN
 
C------            ERROR CONDITIONS
 
   30 NQCASE = 1
      NQFATA = 1
      IQUEST(17) = LN
   40 NQCASE = NQCASE + 1
   50 NQCASE = NQCASE + 1
      NQFATA = NQFATA + 6
      IQUEST(11) = IXIN
      IQUEST(12) = IDH
      IQUEST(13) = 0
      IQUEST(14) = LGO
      IQUEST(15) = LSTA
      IQUEST(16) = LEND
*
      WRITE(*,*)' Problems in DZDNBK, NQCASE, IQUEST(11:17)'
      WRITE(*,*) NQCASE, (IQUEST(K),K=11,17)
          WRITE(*,*)' Error from MZCHLS, IQFOUL',IQFOUL
          WRITE(*,'(A)')IQ(KQS+LGO-4)
          WRITE(*,*)' Dump around link (-+20)'
          WRITE(*,'(100(5Z9/))')(IQ(KQS+LGO+K),K=-20,20)
      GOTO 20
      END
***********************************************************************
