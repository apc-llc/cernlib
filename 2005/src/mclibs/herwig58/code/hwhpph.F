*
* $Id: hwhpph.F,v 1.1.1.1 1996/03/08 17:02:15 mclareni Exp $
*
* $Log: hwhpph.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:15  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHPPH.
*CMZ :-        -12/01/93  10.12.43  by  Bryan Webber
*-- Author :    Ian Knowles
C------------------------------------------------------------------------
      SUBROUTINE HWHPPH
C     Point-like photon/gluon heavy flavour pair production, with
C     exact lightcone massive kinematics, mean EVWGT = sigma in nb.
C     Flavour excitation now appears in HWHPPE, IPROC=5200+IQ
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER IQ1,IHAD1,IHAD2
      DOUBLE PRECISION HWRUNI,HWUALF,EPS,PP1,PP2,QM2,FACTR,ET,EJ,ET2,
     & EXY,EXY2,S,T,U,C
      SAVE PP1,PP2,IQ1,QM2
      PARAMETER (EPS=1.E-9)
      IHAD1=1
      IF (JDAHEP(1,IHAD1).NE.0) IHAD1=JDAHEP(1,IHAD1)
      IHAD2=2
      IF (JDAHEP(1,IHAD2).NE.0) IHAD2=JDAHEP(1,IHAD2)
      IF (FSTWGT.OR.IHAD1.NE.1.OR.IHAD2.NE.2) THEN
         PP1=PHEP(4,IHAD1)+ABS(PHEP(3,IHAD1))
         PP2=PHEP(4,IHAD2)+ABS(PHEP(3,IHAD2))
         XX(1)=1.
         IQ1=MOD(IPROC,100)
         QM2=RMASS(IQ1)**2
         IHPRO=53
         FACTR=-GEV2NB*(YJMAX-YJMIN)*.5*PIFAC*ALPHEM*QFCH(IQ1)**2
      ENDIF
      IF (GENEV) THEN
C Generate event
         IDN(1)=59
         IDN(2)=13
         IDN(3)=IQ1
         IDN(4)=IQ1+6
         ICO(1)=1
         ICO(2)=4
         ICO(3)=2
         ICO(4)=3
         IDCMF=15
         CALL HWETWO
      ELSE
C Select kinematics
         EVWGT=0.
         CALL HWRPOW(ET,EJ)
         ET2=ET**2
         EXY=EXP(HWRUNI(1,YJMIN,YJMAX))
         EXY2=2.*PP1/ET-EXY
         IF (EXY2.LT.EXP(YJMIN).OR.EXY2.GT.EXP(YJMAX)) RETURN
         XX(2)=.5*ET*(1./EXY+1./EXY2)/PP2
         IF (XX(2).LT.0..OR.XX(2).GT.1.) RETURN
         S=XX(2)*PP1*PP2
         IF (S.LT.ET2) RETURN
C define: S=Shat, T=That-M**2, U=Uhat-M**2 (2p.g, -2p.Q, -2p.QBar)
         T=-.5*PP1*ET/EXY
         U=-S-T
         COSTH=(T-U)/(S*SQRT(1.-4.*QM2/S))
         EMSCA=SQRT(2.*S*T*U/(S*S+T*T+U*U))
         CALL HWSFUN(XX(2),EMSCA,IDHW(IHAD2),NSTRU,DISF(1,2),2)
C photon+g ---> Q+Qbar
         IF (DISF(13,2).LT.EPS) THEN
            EVWGT=0.
         ELSE
            C=QM2*S/(U*T)
            EVWGT=FACTR*EJ*ET*HWUALF(1,EMSCA)
     &           *DISF(13,2)*(T/U+U/T+4.*C*(1.-C))/(S*T)
         ENDIF
      ENDIF
  999 END
