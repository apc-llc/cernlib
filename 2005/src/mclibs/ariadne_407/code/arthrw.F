*
* $Id: arthrw.F,v 1.1.1.1 1996/03/08 16:51:07 mclareni Exp $
*
* $Log: arthrw.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:07  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arthrw.F,v 1.1.1.1 1996/03/08 16:51:07 mclareni Exp $
 
      REAL FUNCTION ARTHRW(ID,JRAD,I1,I3,IN1,IN2)
 
C...ARiadne subroutine THRoW emission
 
C...Signals emissions not fulfilling certain criteria
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),
     $                ICOLI(MAXDIP),IDIPS
      SAVE /ARDIPS/
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,PT2MAX,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
 
 
      UTHROW=ARUTHR(ID,JRAD,I1,I3,IN1,IN2)
      ARTHRW=UTHROW
      IF (PARA(28).EQ.0.0.OR.UTHROW.LT.0) RETURN
      ARTHRW=-1.0
      GMAX=PARA(40)
      GMIN=0.0
      IF (PARA(28).GT.0.0) THEN
        GMIN=PARA(28)
      ELSE
        GMAX=-PARA(28)
      ENDIF
      DO 100 IP=IN1,IN2
        IF (IFL(IP).EQ.21.AND.(BP(IP,4).GT.GMAX.OR.BP(IP,4).LT.GMIN))
     $       RETURN
 100  CONTINUE
 
      ARTHRW=1.0
 
      RETURN
 
C**** END OF ARTHRW ****************************************************
      END
