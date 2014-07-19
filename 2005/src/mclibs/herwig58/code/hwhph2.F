*
* $Id: hwhph2.F,v 1.1.1.1 1996/03/08 17:02:15 mclareni Exp $
*
* $Log: hwhph2.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:15  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHPH2
*CMZ :-        -12/01/93  10.12.43  by  Bryan Webber
*-- Author :    Ian Knowles
C------------------------------------------------------------------------
      SUBROUTINE HWHPH2
C     QQD direct photon pair production
C     mean EVWGT = sigma in nb
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER ID,ID1,ID2
      DOUBLE PRECISION HWRGEN,HWRUNI,HWUALF,HWHPPB,EPS,RCS,ET,EJ,KK,KK2,
     & YJ1INF,YJ1SUP,Z1,YJ2INF,YJ2SUP,Z2,FACT,FACTR,RS,S,T,U,CSTU,
     & TQSQ,DSTU,HCS
      SAVE HCS
      PARAMETER (EPS=1.D-9)
      IF (GENEV) THEN
        RCS=HCS*HWRGEN(0)
      ELSE
        EVWGT=0.
        CALL HWRPOW(ET,EJ)
        KK=ET/PHEP(5,3)
        KK2=KK**2
        IF (KK.GE.1.) RETURN
        YJ1INF=MAX( YJMIN , LOG((1.-SQRT(1.-KK2))/KK) )
        YJ1SUP=MIN( YJMAX , LOG((1.+SQRT(1.-KK2))/KK) )
        IF (YJ1INF.GE.YJ1SUP) RETURN
        Z1=EXP(HWRUNI(1,YJ1INF,YJ1SUP))
        YJ2INF=MAX( YJMIN , -LOG(2./KK-1./Z1) )
        YJ2SUP=MIN( YJMAX ,  LOG(2./KK-Z1) )
        IF (YJ2INF.GE.YJ2SUP) RETURN
        Z2=EXP(HWRUNI(2,YJ2INF,YJ2SUP))
        XX(1)=0.5*(Z1+Z2)*KK
        IF (XX(1).GE.1.) RETURN
        XX(2)=XX(1)/(Z1*Z2)
        IF (XX(2).GE.1.) RETURN
        COSTH=(Z1-Z2)/(Z1+Z2)
        S=XX(1)*XX(2)*PHEP(5,3)**2
        RS=0.5*SQRT(S)
        T=-0.5*S*(1.-COSTH)
        U=-S-T
        EMSCA=SQRT(2.*S*T*U/(S*S+T*T+U*U))
        FACT=GEV2NB*PIFAC*0.5*ET*EJ*(YJ1SUP-YJ1INF)*(YJ2SUP-YJ2INF)
     &      *(ALPHEM/S)**2
        CALL HWSGEN(.FALSE.)
        CSTU=2.*(U/T+T/U)/CAFAC
        IF (DISF(13,1).GT.EPS.AND.DISF(13,2).GT.EPS) THEN
           TQSQ=0.
           DO 10 ID=1,6
  10       IF (RMASS(ID).LT.RS) TQSQ=TQSQ+QFCH(ID)**2
           DSTU=DISF(13,1)*DISF(13,2)*FACT*HWHPPB(S,T,U)
     &         /64.*(HWUALF(1,EMSCA)*TQSQ/PIFAC)**2
        ENDIF
      ENDIF
      HCS=0.
      DO 30 ID=1,6
      FACTR=FACT*CSTU*QFCH(ID)**4
C q+qbar ---> gamma+gamma
      ID1=ID
      ID2=ID+6
      IF (DISF(ID1,1).LT.EPS.OR.DISF(ID2,2).LT.EPS) GOTO 20
      HCS=HCS+FACTR*DISF(ID1,1)*DISF(ID2,2)
      IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(59,59,2134,61,*99)
C qbar+q ---> gamma+gamma
  20  ID1=ID+6
      ID2=ID
      IF (DISF(ID1,1).LT.EPS.OR.DISF(ID2,2).LT.EPS) GOTO 30
      HCS=HCS+FACTR*DISF(ID1,1)*DISF(ID2,2)
      IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(59,59,2134,62,*99)
  30  CONTINUE
C g+g ---> gamma+gamma
      ID1=13
      ID2=13
      HCS=HCS+DSTU
      IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(59,59,2134,63,*99)
      EVWGT=HCS
      RETURN
C Generate event
  99  IDN(1)=ID1
      IDN(2)=ID2
      IDCMF=15
      CALL HWETWO
  999 END
