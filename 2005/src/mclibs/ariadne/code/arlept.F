C***********************************************************************
C $Id: arlept.F,v 1.2 1996/04/10 12:33:21 mclareni Exp $

      SUBROUTINE ARLEPT

C...ARiadne subroutine perform cascade on LEPTo event

C...Performs a cascade starting on a zero'th order event from LEPTO


#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arint4.f"
#include "arhide.f"
#include "lujets.f"
#include "leptou.f"


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
