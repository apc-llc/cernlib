C***********************************************************************
C $Id: arptqq.F,v 1.2 1996/04/10 12:33:31 mclareni Exp $

      SUBROUTINE ARPTQQ(KF,KQ,PM,PT2MAX,PT2MIN,X,XQ2,XY,XP,ZQ,YQ,PHI)

C...ARiadne get PT2 of initial state g->QQ

C...Get kinematical variables describing an initial-state g->qqbar
C...splitting.


#include "arimpl.f"
#include "ardat1.f"
#include "arhide.f"
#include "ludat1.f"


      XP=X
      ZQ=1.0
      IF (MHAR(104).GT.1.AND.XY.GE.1.0) THEN
        CALL ARMTQQ(KF,KQ,PM,PT2MAX,PT2MIN,X,XQ2,YQ,PHI)
        RETURN
      ENDIF

      PHI=0
      T2=(1.0-X)*XQ2
      IF (MHAR(102).EQ.2) T2=XQ2
      PT2MAX=MIN(PT2MAX,0.25*T2/X)
      IF (MHAR(102).EQ.2) PT2MAX=MIN(PT2MAX,0.25*(1.0-X)*T2/X)
      IF (MHAR(102).LT.0) GOTO 900
      RMQ=ULMASS(KQ)
      PT2CUT=PT2MIN
      IF (MHAR(102).GE.1) PT2CUT=MAX(PT2MIN,PARA(3)**2+RMQ**2)
      IF (PT2MAX.LE.PT2CUT) GOTO 900

      XNUMFL=MAX(ARNOFL(SQRT(T2/X),MAX(5,MSTA(15))),3.0)
      ALPHA0=12.0*PARU(1)/(33.0-2.0*XNUMFL)
      SQ2MIN=PHAR(103)*PT2CUT/PARA(21)
      IF (MSTA(19).EQ.2) SQ2MIN=MAX(SQ2MIN,XQ2)
      SQ2MIN=MAX(SQ2MIN,4.0*RMQ**2)
      STRA0=ARSTRA(KF,KQ,X,1.0,SQ2MIN)
      STRAQ=ARSTRA(KF,KQ,X,1.0,XQ2)
      IF (MHAR(113).EQ.-2.AND.MHAR(102).EQ.2) THEN
        XPMAX=T2*0.25/(PT2CUT+T2*0.25)
        STRAQ=ARSTRA(KF,KQ,X,XPMAX,XQ2)
      ENDIF
      IF (STRA0.LE.0.0) STRA0=STRAQ
      IF (MHAR(113).NE.1) STRA0=STRAQ
      IF (MSTA(19).NE.2) STRA0=MAX(STRAQ,STRA0)

      IF (MHAR(109).GT.0) THEN
        SQ2MIN=PT2CUT/
     $       ((0.5+SQRT(MAX(0.25-PT2CUT*X/(XQ2*(1.0-X)),0.0)))*(1.0-X))
        SQ2MAX=0.5*(XQ2+PT2MAX)
        STRA0=STRAQ
        STRA0=MAX(STRA0,ARSTRA(KF,KQ,X,1.0,SQ2MIN))
        STRA0=MAX(STRA0,ARSTRA(KF,KQ,X,1.0,SQ2MAX))
      ENDIF

      CY=(1.0-XY)/(1.0+(1.0-XY)**2)
      CQ=1.0+CY
      IF (MHAR(102).EQ.2) CQ=0.125+0.25*CY
      C=PHAR(104)*0.25*ALPHA0*STRA0*CQ/PARU(1)
      THEMAX=PT2MAX
      YINT=4.0*LOG(SQRT(THEMAX/PT2CUT)+SQRT(THEMAX/PT2CUT-1.0))
      CN=1.0/(YINT*C)
      IF (MHAR(110).GT.0) CN=1.0/(YINT*C*ALPHA0)
      XLAM2=(PARA(1)**2)/PHAR(103)

 100  IF (PT2MAX.LE.PT2CUT) GOTO 900
      ARG=RLU(IDUM)
      IF (LOG(ARG)*CN.LT.
     $     LOG(LOG(PT2CUT/XLAM2)/LOG(PT2MAX/XLAM2))) GOTO 900
      PT2MAX=XLAM2*(PT2MAX/XLAM2)**(ARG**CN)

      YMAX=2.0*LOG(SQRT(THEMAX/PT2MAX)+SQRT(THEMAX/PT2MAX-1.0))
      Y=(RLU(IDUM)*2.0-1.0)*YMAX

      ZQ=1.0/(1.0+EXP(-Y))
      IF (MHAR(112).LT.0.OR.MHAR(112).EQ.2) ZQ=MIN(ZQ,1.0-ZQ)
      IF (MHAR(102).EQ.2) THEN
        XP=T2*ZQ*(1.0-ZQ)/(PT2MAX+T2*ZQ*(1.0-ZQ))
      ELSE
        XP=ZQ*(1.0-ZQ)*T2/PT2MAX
      ENDIF
      IF (XP.LE.X.OR.XP.GE.1.0) GOTO 100

      SQ2=PHAR(103)*PT2MAX/PARA(21)

      W=2.0*YMAX/YINT
      IF (MSTA(19).EQ.2) THEN
        W=W*MIN(1.0,LOG(PT2MAX/XLAM2)/LOG(PARA(21)*XQ2/XLAM2))
        SQ2=MAX(SQ2,XQ2)
      ENDIF
      SQ2=MAX(SQ2,SQ2MIN)
      IF (MHAR(109).GT.0) SQ2=PT2MAX/
     $     ((0.5+SQRT(MAX(0.25-PT2MAX*XP/(XQ2*(1.0-XP)),0.0)))*
     $     (1.0-XP))
      IF (MHAR(113).EQ.1) THEN
        STRA=ARSTRA(KF,KQ,X,XP,SQ2)
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
        IF (X/XP.GT.((1.0/RLU(IDUM)-1.0)**BETA)*(XMU/PTIN)**ALPHA)
     $       GOTO 100
      ENDIF
      IF (MHAR(102).EQ.2) THEN
        W=W*(XP*(1.0-XP)*(XP**2+(1.0-XP)**2)*(ZQ**2+(1.0-ZQ)**2)+
     $       16.0*((XP*(1.0-XP))**2)*ZQ*(1.0-ZQ)*CY)/CQ
      ELSE
        W=W*((XP**2+(1.0-XP)**2)*(ZQ**2+(1.0-ZQ)**2)+
     $       16.0*XP*(1.0-XP)*ZQ*(1.0-ZQ)*CY)/CQ
      ENDIF

      IF (W.GT.1.0) THEN
        CALL ARERRM('ARPTQQ',22,0)
        RETURN
      ENDIF

      IF (W.LT.RLU(IDUM)) GOTO 100

      IF (MHAR(113).EQ.-1) THEN
        IF (PT2MAX.LT.ZQ*(1.0-X)*XQ2) GOTO 100
        IF (PT2MAX.LT.(1.0-ZQ)*(1.0-X)*XQ2) GOTO 100
      ENDIF

      YQ=0.5*LOG(ZQ*(1.0-X)/((1.0-ZQ)*(X/XP-X)))

      XA=0.125*(1.0+(1.0-XY)**2)*(XP**2+(1.0-XP)**2)*
     $     (ZQ**2+(1.0-ZQ)**2)/(ZQ*(1.0-ZQ))+2.0*(1.0-XY)*XP*(1.0-XP)
      XB=0.5*XY*SQRT((1.0-XY)*XP*(1.0-XP)/(ZQ*(1.0-ZQ)))*(1.0-2.0/XY)*
     $     (1.0-2.0*ZQ)*(1.0-2.0*XP)
      XC=(1.0-XY)*XP*(1.0-XP)

      ABC=ABS(XA)+ABS(XB)+ABS(XC)
 200  PHI=PARU(2)*RLU(IDUM)
      IF (XA+XB*COS(PHI)+XC*COS(2.0*PHI).LT.RLU(IDUM)*ABC) GOTO 200

      RETURN

 900  PT2MAX=0.0
      RETURN

C**** END OF ARPTQQ ****************************************************
      END
