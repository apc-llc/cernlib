*
* $Id: hwhdyp.F,v 1.1.1.1 1996/03/08 17:02:14 mclareni Exp $
*
* $Log: hwhdyp.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:14  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHDYP.
*CMZ :-        -26/04/91  14.55.44  by  Federico Carminati
*-- Author :    Bryan Webber and Ian Knowles
C------------------------------------------------------------------------
      SUBROUTINE HWHDYP
C------------------------------------------------------------------------
C     Drell-Yan Production of lepton pairs via photon, Z and (if ZPRIME)
C     Z' exchange. Lepton universality is assumed for photon and Z, and
C     for Z' if no lepton flavour is specified.
C     MEAN EVWGT = SIGMA IN NB
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER HWRINT,IL,JL,IQ,IDQ,ID1,ID2
      DOUBLE PRECISION HWRGEN,HWRUNI,HWUAEM,EMJAC,SIGMA,FACTR,PRAN,PROB,
     & PMAX,PTHETA
      SAVE IL,FACTR,SIGMA
      IF (GENEV) THEN
C Select and store incoming quarks
        PRAN=SIGMA*HWRGEN(0)
        PROB=0.
        DO 10 IQ=1,MAXFL
        IDQ=MAPQ(IQ)
        ID1=IDQ
        ID2=ID1+6
        PROB=PROB+DISF(ID1,1)*DISF(ID2,2)*CLQ(1,IDQ)
        IF (PROB.GE.PRAN) GOTO 20
        ID1=ID2
        ID2=IDQ
        PROB=PROB+DISF(ID1,1)*DISF(ID2,2)*CLQ(1,IDQ)
        IF (PROB.GE.PRAN) GOTO 20
   10   CONTINUE
   20   IDN(1)=ID1
        IDN(2)=ID2
        ICO(1)=2
        ICO(2)=1
C Select and store outgoing leptons
        IF (IL.EQ.0) JL=2*HWRINT(1,3)-1
        IDN(3)=120+JL
        IDN(4)=IDN(3)+6
        ICO(3)=3
        ICO(4)=4
C Generate polar angle distribution:
C CLQ(1,IDQ)*(1.+COSTH**2)+CLQ(3,IDQ)*COSTH
        PMAX=2.*CLQ(1,IDQ)+ABS(CLQ(3,IDQ))
   30   COSTH=HWRUNI(0,-ONE,ONE)
        PTHETA=CLQ(1,IDQ)*(1.+COSTH**2)+CLQ(3,IDQ)*COSTH
        IF (PTHETA.LT.PMAX*HWRGEN(1)) GOTO 30
        IF (ID1.GT.ID2) COSTH=-COSTH
        IDCMF=200
        CALL HWETWO
      ELSE
        IL=MOD(IPROC,10)
C Select inititial momentum fractions and corresponding weight
        CALL HWRPOW(EMSCA,EMJAC)
        IF (IL.NE.0) THEN
           JL=2*IL-1
           CALL HWUEEC(JL)
        ELSE
           CALL HWUEEC(1)
        ENDIF
        XXMIN=(EMSCA/PHEP(5,3))**2
        XLMIN=LOG(XXMIN)
        CALL HWSGEN(.TRUE.)
C Sum contributions fron initial quark flavours
        SIGMA=0.
        DO 100 IQ=1,MAXFL
        IDQ=MAPQ(IQ)
        ID1=IDQ
        ID2=ID1+6
        SIGMA=SIGMA+(DISF(ID1,1)*DISF(ID2,2)
     &             + DISF(ID2,1)*DISF(ID1,2))*CLQ(1,IDQ)
  100   CONTINUE
        FACTR=-GEV2NB*HWUAEM(EMSCA**2)**2*PIFAC*16.*EMJAC*XLMIN/
     &        (3.*EMSCA)**3
        IF (IL.EQ.0) FACTR=FACTR*3.
        EVWGT=FACTR*SIGMA
      ENDIF
  999 END
