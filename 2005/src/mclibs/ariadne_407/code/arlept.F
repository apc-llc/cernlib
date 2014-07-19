*
* $Id: arlept.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
*
* $Log: arlept.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:05  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arlept.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
 
      SUBROUTINE ARLEPT
 
C...ARiadne subroutine perform cascade on LEPTo event
 
C...Performs a cascade starting on a zero'th order event from LEPTO
 
 
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
      COMMON /ARINT4/ BASS(5),BASSX1,BASSX3,IFLASS
      SAVE /ARINT4/
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,XY,W2,XQ2,U
      SAVE /LEPTOU/
 
 
C...Check that Ariadne was properly initialized
      IF (MSTA(2).EQ.0.OR.MSTA(1).NE.3) CALL ARERRM('ARLEPT',12,0)
      IFLASS=0
      MSAV33=MSTA(33)
 
C...Boost to the hadronic center of mass
      CALL ARBOLE(THEL,PHI1,PHI2,DBXL,DBYL,DBZL)
 
C...Check which quark was struck and try to decide whether it was
C...a sea quark
      IFLSTR=LST(25)
 
C...Copy to Ariadne event record
      CALL ARCOPA(5,1,SIGN(1,IFLSTR))
      IF (MSTA(30).LT.2) THEN
        QEX(1)=.FALSE.
        XPMU(1)=0.0
        XPA(1)=0.0
      ELSE
        QEX(1)=.TRUE.
        IF (PARA(14).GE.0) THEN
          XPMU(1)=SQRT(XQ2)*PARA(14)
        ELSE
          XPMU(1)=ABS(PARA(14))
        ENDIF
        XPA(1)=PARA(15)
      ENDIF
      CALL ARCOPA(6,2,-SIGN(1,IFLSTR))
      CALL ARCRDI(1,1,2,1,.FALSE.)
      CALL ARCOLI(1,-1)
      IPART=2
      IDIPS=1
      ISTRS=1
      IFLOW(1)=SIGN(1,IFLSTR)
      IPF(1)=1
      IPL(1)=2
      IMF=5
      IML=N
      QDUMP=.FALSE.
      NSAV=N+1
 
      IRQ=0
      IRD=2
      IRP=0
      IF (N.GT.6) IRP=7
 
      CALL ARREMN(2,IRQ,IRD,IRP,-1)
 
      IF (PHAR(112).LT.0) THEN
        PHAR(112)=-XPMU(IRD)**2
        IF (MHAR(107).EQ.4) PHAR(112)=-XQ2
      ENDIF
 
      IF (IRP.LE.0.OR.MSTA(32).GT.1) THEN
        LST(24)=1
        PT2LST=PARA(40)
        CALL ARCASC
        GOTO 900
      ENDIF
 
C...Initiate initial g->QQ splitting
      PT2BGF=PARA(40)
      IF (MSTA(9).GT.0) CALL ARCHEM(1)
      IO=0
      LST(24)=1
 
      PT2LST=PARA(40)
      PT2MIN=ARGPT2(1)/PHAR(103)
 210  IF (ABS(MSTA(33)).GT.0) THEN
        CALL ARPTQQ(K(2,2),IFLSTR,SQRT(W2),
     $       PT2BGF,PT2MIN,X,XQ2,XY,XP,ZQ,YQ,PHI)
      ELSE
        CALL ARPTQQ(K(2,2),IFLSTR,SQRT(W2),
     $       PT2BGF,PT2MIN,X,XQ2,1.0,XP,ZQ,YQ,PHI)
      ENDIF
      IF (PT2BGF.GT.PT2MIN) THEN
        CALL ARINQQ(2,IFLSTR,IRP,PT2BGF,YQ,ZQ,PHI,QFAIL)
        IF (QFAIL) GOTO 210
        LST(24)=3
        CALL AREVOL(SQRT(PHAR(103)*PT2BGF),0.0)
      ELSE
        CALL AREVOL(SQRT(PT2LST),0.0)
        IF (IO.GT.0) LST(24)=2
      ENDIF
 
C...Check momentum and dump to /LUJETS/
      IF (.NOT.QDUMP) CALL ARDUMP
      IF (MSTA(9).GT.0) CALL ARCHEM(0)
      GOTO 900
 
C...Include Phi asymmetries for matrix element
 900  IF (IO.GT.0.AND.ABS(MSTA(33)).EQ.1) CALL ARPHAS(NSAV)
 
      MSTA(33)=MSAV33
      CALL LUDBRB(1,N,0.0,PHI2,0.0D0,0.0D0,0.0D0)
      CALL LUDBRB(1,N,THEL,PHI1,DBXL,DBYL,DBZL)
 
      RETURN
 
C**** END OF ARLEPT ****************************************************
      END
