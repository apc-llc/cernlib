*
* $Id: fzhyln.F,v 1.1.1.1 1996/03/06 10:47:21 mclareni Exp $
*
* $Log: fzhyln.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:21  mclareni
* Zebra
*
*
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
      SUBROUTINE FZHYLN (LBEGIN,NWORDS,IOFLAG,*)

#include "bankparq.inc"
#include "bkfoparq.inc"
#include "dzc1.inc"
#include "mqsys.inc"
#include "qequ.inc"
#include "zmach.inc"
#include "zunit.inc"
      CHARACTER CHROUT*(*)
      PARAMETER (CHROUT = 'FZHYLN')
C       Bits 17 means integer and bit 18 bit data
      PARAMETER (MBINTS = 17 , MBBITS = 18)



#include "q_jbit.inc"

      IF (LBEGIN.EQ.0.OR.NWORDS.EQ.0)                      GO TO 999


      LN     = KQS + LBEGIN
      LE     = LN  + NWORDS

  100 IF (LN.GE.LE)                                        GO TO 999


          IF (LQ(LN+1).GT.0)                               GO TO 998

          ID     = LQ(LN)
          ND     = JBYT(LQ(LN+1),1,15)
          NS     = JBYT(LQ(LN+1),16,6)
          NL     = JBYT(LQ(LN+1),22,9)

          DO 110 I=1,NL
  110     LQ(LN+I) = LQ(LN+I+1)


          LQ(LN+MBKIOQ) = NL + NBKJMQ

          LS            = LN + NL + NBKHDQ
          IQWIDH(LS)    = ID
          IQWNL(LS)     = NL
          IQWNS(LS)     = NS
          IQWND(LS)     = ND


          LQLUP(LS)  = IQNIL
          LQLORG(LS) = IQNIL


          IF (NS.GT.0) THEN
              LQ (LS)       = LQ(LS-1)
              LQ(LS-1)      = 0
              CALL SBYT(0,IQ(LS),19,6)
          ELSE
              LQ(LS)     = 0
          ENDIF




          IF ((JBIT(IOFLAG,MBBITS).EQ.1).AND.
     X        (JBIT(IQ(LS),MBBITS).EQ.1)      )      THEN
C                Bit string data
              CALL SBYT(IFOBIQ,LQ(LN+MBKIOQ),JBKIOQ,NBKIOQ)
          ELSEIF ((JBIT(IOFLAG,MBINTS).EQ.1).AND.
     X            (JBIT(IQ(LS),MBINTS).EQ.1)      )  THEN
C                Integer data
              CALL SBYT(IFOINQ,LQ(LN+MBKIOQ),JBKIOQ,NBKIOQ)
          ELSE
C                Default is floating point data
              CALL SBYT(IFOFLQ,LQ(LN+MBKIOQ),JBKIOQ,NBKIOQ)
          ENDIF


      LN     = LS + ND + NBKCTQ

                                                           GO TO 100

  998 WRITE(IQPRNT,
     +      '(''0+++ HYDRA banks word 2 ND/NS/NL not negative '',Z8)')
     +      LQ(LN+1)
      RETURN 1

  999 RETURN
      END
