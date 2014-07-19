*
* $Id: arcopa.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
*
* $Log: arcopa.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:01  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arcopa.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
 
      SUBROUTINE ARCOPA(IJ,IP,ITYP)
 
C...ARiadne subroutine COpy PArton
 
C...Copies a parton from position IJ in /LUJETS/ common block to
C...Position IP in /ARPART/ common block.
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
 
      DO 100 I=1,5
        BP(IP,I)=P(IJ,I)
 100  CONTINUE
 
      IFL(IP)=K(IJ,2)
      IEX=MOD(K(IJ,4),10)
      IF (IEX.NE.0) THEN
        QEX(IP)=.TRUE.
        IF (PARA(10+IEX).GT.0.0) THEN
          XPMU(IP)=PARA(10+IEX)
        ELSE
          XPMU(IP)=V(IJ,4)
        ENDIF
        XPA(IP)=PARA(10)
      ELSE
        QEX(IP)=.FALSE.
        XPMU(IP)=0.0
        XPA(IP)=0.0
      ENDIF
 
      QQ(IP)=(ITYP.NE.2)
      INO(IP)=0
      INQ(IP)=0
      IDI(IP)=0
      IDO(IP)=0
      IF (MSTA(1).EQ.2) INQ(IP)=-IJ
      PT2GG(IP)=0.0
      K(IJ,4)=-IP
 
      RETURN
 
C**** END OF ARCOPA ****************************************************
      END
