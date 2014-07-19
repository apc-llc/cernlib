C***********************************************************************
C $Id: armtqq.F,v 1.2 1996/04/10 12:33:23 mclareni Exp $

      SUBROUTINE ARMTQQ(KF,KQ,PM,PT2MAX,PT2MIN,X,XQ2,YQ,PHI)

C...ARiadne get PT2 of initial state g->QQ

C...Get kinematical variables describing an initial-state g->qqbar
C...splitting.


#include "arimpl.f"
#include "ardat1.f"
#include "arhide.f"
#include "ludat1.f"


      PHI=0
      T2=XQ2
      PT2MAX=MIN(PT2MAX,0.25*PM**2)
      IF (MHAR(102).LT.0) GOTO 900
      RMQ=ULMASS(KQ)
      PT2CUT=PT2MIN
      PT2CUT=MAX(PT2MIN,PARA(3)**2+RMQ**2)
      IF (PT2MAX.LE.PT2CUT) GOTO 900

      XNUMFL=MAX(ARNOFL(SQRT(T2/X),MAX(5,MSTA(15))),3.0)
      ALPHA0=12.0*PARU(1)/(33.0-2.0*XNUMFL)
      SQ2MIN=PHAR(103)*PT2CUT/PARA(21)
      SQ2MAX=PHAR(103)*PT2MAX/PARA(21)
      IF (MSTA(19).EQ.2) SQ2MIN=MAX(SQ2MIN,XQ2)
      SQ2MIN=MAX(SQ2MIN,4.0*RMQ**2)
      STRA0=ARSTRA(KF,KQ,X,1.0,SQ2MIN)
      STRA0=MAX(STRA0,ARSTRA(KF,KQ,X,1.0,XQ2))
      STRA0=MAX(STRA0,ARSTRA(KF,KQ,X,1.0,SQ2MAX))
      
      C=PHAR(104)*ALPHA0*STRA0/PARU(1)
      ZINT=1.0-X
      CN=1.0/(C*ZINT)
      XLAM2=(PARA(1)**2)/PHAR(103)

 100  IF (PT2MAX.LE.PT2CUT) GOTO 900
      ARG=RLU(IDUM)
      IF (LOG(ARG)*CN.LT.
     $     LOG(LOG(PT2CUT/XLAM2)/LOG(PT2MAX/XLAM2))) GOTO 900
      PT2MAX=XLAM2*(PT2MAX/XLAM2)**(ARG**CN)

      Z=X+RLU(0)*(1.0-X)
      SQ2=PHAR(103)*PT2MAX/PARA(21)

      W=(Z**2+(1.0-Z)**2)*0.25
      IF (MSTA(19).EQ.2) THEN
        W=W*MIN(1.0,LOG(PT2MAX/XLAM2)/LOG(PARA(21)*XQ2/XLAM2))
        SQ2=MAX(SQ2,XQ2)
      ENDIF
      SQ2=MAX(SQ2,SQ2MIN)
      IF (MHAR(113).EQ.1) THEN
        STRA=ARSTRA(KF,KQ,X,Z,SQ2)
        W=W*STRA/STRA0
      ELSE
        BETA=PARA(25)
        IF (MSTA(25).EQ.0) BETA=0.0
        PTIN=SQRT(PHAR(103)*PT2MAX)
        IF (MHAR(113).EQ.2) PTIN=2.0*PTIN
        XMU=PARA(11)
        ALPHA=PARA(10)
        IF (PARA(10).GT.0.0) THEN
          XMU=PARA(11)
          ALPHA=PARA(10)
        ELSEIF (PTIN.GE.ABS(PARA(10))) THEN
          XMU=SQRT(ABS(PARA(10)*PARA(11)))
          ALPHA=2.0
        ELSE
          XMU=PARA(11)
          ALPHA=1.0
        ENDIF
        IF (X/Z.GT.((1.0/RLU(IDUM)-1.0)**BETA)*(XMU/PTIN)**ALPHA)
     $       GOTO 100
      ENDIF

      IF (W.GT.1.0) THEN
        CALL ARERRM('ARPTQQ',22,0)
        RETURN
      ENDIF

      IF (W.LT.RLU(IDUM)) GOTO 100

      IF (MHAR(113).EQ.-1) THEN
        IF (PT2MAX.LT.Z*(1.0-X)*XQ2) GOTO 100
        IF (PT2MAX.LT.(1.0-Z)*(1.0-X)*XQ2) GOTO 100
      ENDIF

      YQ=0.5*LOG(PT2MAX*(Z/((1.0-Z)*X*PM))**2)
      PHI=PARU(2)*RLU(IDUM)

      RETURN

 900  PT2MAX=0.0
      RETURN

C**** END OF ARMTQQ ****************************************************
      END
