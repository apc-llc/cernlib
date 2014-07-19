*
* $Id: hwhppe.F,v 1.1.1.1 1996/03/08 17:02:15 mclareni Exp $
*
* $Log: hwhppe.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:15  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHPPE.
*CMZ :-        -12/01/93  10.12.43  by  Bryan Webber
*-- Author :    Ian Knowles
C------------------------------------------------------------------------
      SUBROUTINE HWHPPE
C     point-like photon/QCD heavy flavour single excitation, using exact
C     massive lightcone kinematics, mean EVWGT = sigma in nb.
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER IQ1,IQ2,ID1,ID2,IHAD1,IHAD2
      DOUBLE PRECISION HWRGEN,HWRUNI,HWUALF,EPS,PP1,PP2,QM2,FACTR,
     & PT,PJ,PT2,PTM,EXY,T,CC,EXY2,S,U,C,SIGE,HCS,RCS
      SAVE PP1,PP2,IQ1,IQ2,QM2,FACTR,SIGE,HCS
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
         IQ2=IQ1+6
         QM2=RMASS(IQ1)**2
         FACTR=GEV2NB*(YJMAX-YJMIN)*4.*PIFAC*CFFAC*PP1
     &        *ALPHEM*QFCH(IQ1)**2
      ENDIF
      IF (GENEV) THEN
         RCS=HCS*HWRGEN(0)
      ELSE
         EVWGT=0.
         CALL HWRPOW(PT,PJ)
         PT2=PT**2
         PTM=SQRT(PT2+QM2)
         EXY=EXP(HWRUNI(1,YJMIN,YJMAX))
         T=-PP1*PT/EXY
         CC=T**2-4.*QM2*(PT2+T)
         IF (CC.LT.0) RETURN
         EXY2=(2.*PT2+T-SQRT(CC))*PP1/(2.*T*PTM)
         IF (EXY2.LT.EXP(YJMIN).OR.EXY2.GT.EXP(YJMAX)) RETURN
         XX(2)=(PT/EXY+PTM/EXY2)/PP2
         IF (XX(2).GT.1.) RETURN
C define: S=Shat-M**2, T=That ,U=Uhat-M**2 (2p.Q, -2p.g, -2p.Q')
         S=XX(2)*PP1*PP2
         U=-S-T
         COSTH=(1.+QM2/S)*(T-U)/S-QM2/S
C Set hard process scale (Approx ET-jet)
         EMSCA=SQRT(2.*S*T*U/(S*S+T*T+U*U))
         C=QM2*T/(U*S)
         SIGE=-FACTR*PT*PJ*HWUALF(1,EMSCA)*(S/U+U/S+4.*C*(1.-C))
     &       /(S**2*EXY2*PTM*(1-QM2/(XX(2)*PP2*EXY2)**2))
         CALL HWSFUN(XX(2),EMSCA,IDHW(IHAD2),NSTRU,DISF(1,2),2)
      ENDIF
      HCS=0.
      ID1=59
C photon+Q ---> g+Q
      ID2=IQ1
      IF (DISF(ID2,2).LT.EPS) GOTO 10
      HCS=HCS+SIGE*DISF(ID2,2)
      IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(13,ID2,1423,51,*99)
C photon+Qbar ---> g+Qbar
  10  ID2=IQ2
      IF (DISF(ID2,2).LT.EPS) GOTO 20
      HCS=HCS+SIGE*DISF(ID2,2)
      IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(13,ID2,1342,52,*99)
  20  EVWGT=HCS
      RETURN
C Generate event
  99  IDN(1)=ID1
      IDN(2)=ID2
      IDCMF=15
      CALL HWETWO
  999 END
