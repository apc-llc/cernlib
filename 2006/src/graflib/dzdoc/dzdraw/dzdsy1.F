*
* $Id: dzdsy1.F,v 1.1.1.1 1996/03/04 16:13:10 mclareni Exp $
*
* $Log: dzdsy1.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:10  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDSY1(LB,IB, L,LUNPRT)
*.
*...DZDSY1
*.
*. INPUT     : LB, IB   the array containing the ZEBRA store
*.                      as used in the call to MZSTOR
*.             L        link to bank to be dumped
*.             LUNPRT   unit for printing
*. OUTPUT    :
*.
*. CALLS     : UHTOC
*. CALLED    : DZDSYW
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*. Last mod  : 3 -Oct-90
*.**********************************************************************
*.
#include "zebra/mzbits.inc"
#include "dzdchv.inc"
      INTEGER LB(999), IB(999)
*
      CHARACTER*10 CTEMP
      CHARACTER*4 CHID
*
      IF(L .LE. 0)THEN
          GOTO 40
      ENDIF
      WRITE(LUNPRT,'(A)')' '
      WRITE(LUNPRT,'(A)')' System words + links'
      NIO = JBYT(IB(L),19,4)
      NL =  IB(L-3)
      IOFFBS = - (NIO + NL + 8 + 1)
      WRITE(LUNPRT,10200)' Offset to Bank-Centre',
     &JBYT(IB(L+IOFFBS),1,16)
      CALL DZDIOC(IB(L+IOFFBS),CLINE,NCH)
      WRITE(LUNPRT,10050)' I/O characteristic',CLINE(1:NCH)
      CLINE=' '
      IF(JBIT(IB(L),IQDROP).NE.0)CLINE(1:8) =' dropped'
      IF(JBIT(IB(L),IQMARK).NE.0)CLINE(9:16)='  marked'
      IF(JBIT(IB(L),IQCRIT).NE.0)CLINE(17:21)=' crit'
      IF(JBIT(IB(L),IQMARK).NE.0)CLINE(22:26)=' sysx'
      WRITE(LUNPRT,10010)' Bank status word (HEX)',IB(L),CLINE(1:26)
      WRITE(LUNPRT,10200)' Link to the bank',L
      CALL UHTOC(IB(L-4),4,CTEMP,4)
      CHID = CTEMP(1:4)
      WRITE(LUNPRT,10100)' Hollerith ID',CTEMP(1:4)
      WRITE(LUNPRT,10200)' Numerical ID',IB(L-5)
      WRITE(LUNPRT,10200)' Total number of links',IB(L-3)
      NS =  IB(L-2)
      WRITE(LUNPRT,10200)' Number of structural links',NS
      WRITE(LUNPRT,10200)' Number of data words',IB(L-1)
      LN = LB(L)
      IF(LN .GT. 1)THEN
        CALL UHTOC(IB(LN-4),4,CTEMP,4)
      ELSE
        CTEMP = '    '
      ENDIF
      WRITE(LUNPRT,10300)' Next link ',LN,CTEMP(1:4)
      LUP = LB(L+1)
      IF(LUP .GT. 1)THEN
        CALL UHTOC(IB(LUP-4),4,CTEMP,4)
      ELSE
        CTEMP = '    '
      ENDIF
      WRITE(LUNPRT,10300)' Up - link ',LUP,CTEMP(1:4)
      LORIG = LB(L+2)
*      IF(LORIG .GT. 1)THEN
*        NSU = IB(LUP-2)
*        IF(LORIG .GE. LUP .OR. LORIG .LT. LUP-NSU)THEN
*            IF(LUP.GT.1)THEN
*                CTEMP='linear str'
*            ELSE
*                CTEMP='top bank  '
*            ENDIF
*        ELSE
*            JB = LUP - LORIG
*            WRITE(CTEMP,'(A,I5)')'JBIAS =',JB
*        ENDIF
*      ELSE
*        IF(LORIG.EQ.0 .AND. LUP.EQ.0)THEN
*           CTEMP='stand alone'
*        ELSE
*           CTEMP = '    '
*        ENDIF
*      ENDIF
      WRITE(LUNPRT,10300)' Origin link ',LORIG,CTEMP(1:4)
      CTEMP = ' '
      IF(NS .GT. 0)THEN
        DO 20 I=1,NS
        LI = LB(L-I)
        IF(LI .NE. 0)THEN
            CALL UHTOC(IB(LI-4),4,CTEMP,4)
        ELSE
            CTEMP = '    '
        ENDIF
        WRITE(LUNPRT,10400)I, '. down link ',LI,CTEMP(1:4)
   20     CONTINUE
      ENDIF
      NR = IB(L-3) - NS
      IF(NR .GT. 0)THEN
        DO 30 I=1,NR
        LI = LB(L-NS-I)
        IF(LI .NE. 0)THEN
            CALL UHTOC(IB(LI-4),4,CTEMP,4)
        ELSE
            CTEMP = '    '
        ENDIF
        WRITE(LUNPRT,10400)I, '. ref link ',LI,CTEMP
   30     CONTINUE
      ENDIF
10000   FORMAT(A,T30,Z9)
10010   FORMAT(A,T30,Z9,A)
10050   FORMAT(A,T30,A)
10100   FORMAT(A,T30,'     ',A)
10200   FORMAT(A,T30,I9)
10300   FORMAT(A,T30,I9,' ',A)
10400   FORMAT(' ',I5,A,T30,I9,' ',A)
10500   FORMAT(' ',I2,A,T30,Z9)
   40   CONTINUE
      END
