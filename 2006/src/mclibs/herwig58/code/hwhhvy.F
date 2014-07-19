*
* $Id: hwhhvy.F,v 1.1.1.1 1996/03/08 17:02:14 mclareni Exp $
*
* $Log: hwhhvy.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:14  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHHVY.
*CMZ :-        -26/04/91  14.55.44  by  Federico Carminati
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWHHVY
C     QCD HEAVY FLAVOUR PRODUCTION
C     MEAN EVWGT = SIGMA IN NB
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      LOGICAL HQ1,HQ2
      INTEGER IQ1,IQ2,ID1,ID2
      DOUBLE PRECISION HWRGEN,HWRUNI,HWUALF,EPS,RCS,Z1,Z2,ET,EJ,
     & QM2,QPE,FACTR,S,T,U,ST,TU,US,TUS,UST,EN,RN,AF,ASTU,
     & AUST,CF,CN,CS,CSTU,CSUT,CTSU,CTUS,HCS,UT,SU,GT,DIST,KK,KK2,
     & YJ1INF,YJ1SUP,YJ2INF,YJ2SUP
      SAVE HCS
      PARAMETER (EPS=1.D-9)
      IF (GENEV) THEN
        RCS=HCS*HWRGEN(0)
      ELSE
        EVWGT=0.
        CALL HWRPOW(ET,EJ)
        KK = ET/PHEP(5,3)
        KK2=KK**2
        IF (KK.GE.1.) RETURN
        YJ1INF = MAX( YJMIN, LOG((ONE-SQRT(ONE-KK2))/KK) )
        YJ1SUP = MIN( YJMAX, LOG((ONE+SQRT(ONE-KK2))/KK) )
        IF (YJ1INF.GE.YJ1SUP) RETURN
        Z1=EXP(HWRUNI(1,YJ1INF,YJ1SUP))
        YJ2INF = MAX( YJMIN, -LOG(TWO/KK-ONE/Z1) )
        YJ2SUP = MIN( YJMAX, LOG(TWO/KK-Z1) )
        IF (YJ2INF.GE.YJ2SUP) RETURN
        Z2=EXP(HWRUNI(2,YJ2INF,YJ2SUP))
        XX(1)=HALF*(Z1+Z2)*KK
        IF (XX(1).GE.1.) RETURN
        XX(2)=XX(1)/(Z1*Z2)
        IF (XX(2).GE.1.) RETURN
        S=XX(1)*XX(2)*PHEP(5,3)**2
        IQ1=MOD(IPROC,100)
        QM2=RMASS(IQ1)**2
        QPE=S-4.*QM2
        IF (QPE.LT.0.) RETURN
        COSTH=HALF*ET*(Z1-Z2)/SQRT(Z1*Z2*QPE)
        IF (ABS(COSTH).GT.1.) RETURN
C---REDEFINE S, T, U AS P1.P2, -P1.P3, -P1.P4
        S=HALF*S
        T=-HALF*(1.+Z2/Z1)*(HALF*ET)**2
        U=-S-T
C---SET EMSCA TO HEAVY HARD PROCESS SCALE
        EMSCA=SQRT(4.*S*T*U/(S*S+T*T+U*U))
        FACTR = GEV2NB*.125*PIFAC*EJ*ET*(HWUALF(1,EMSCA)/S)**2
     &         *(YJ1SUP-YJ1INF)*(YJ2SUP-YJ2INF)
        CALL HWSGEN(.FALSE.)
C
        ST=S/T
        TU=T/U
        US=U/S
        TUS=US/ST
        UST=ST/TU
C
        EN=CAFAC
        RN=CFFAC/EN
        AF=FACTR*RN
        ASTU=AF*(1.-2.*UST+QM2/T)
        AUST=AF*(1.-2.*TUS+QM2/S)
        CF=FACTR/(2.*CFFAC)
        CN=1./(EN*EN)
        CS=HALF/TU-QM2/T-HALF*(QM2/T)**2
        CSTU=CF*(CS-   US**2-QM2/S - CN*(CS+QM2*QM2/(S*T)))
        CS=HALF*TU-QM2/U-HALF*(QM2/U)**2
        CSUT=CF*(CS-1./ST**2-QM2/S - CN*(CS+QM2*QM2/(S*U)))
        CS=HALF*US-QM2/S-HALF*(QM2/S)**2
        CTSU=-FACTR*(CS-1./TU**2-QM2/T - CN*(CS+QM2*QM2/(S*T)))
        CS=HALF/US-QM2/U-HALF*(QM2/U)**2
        CTUS=-FACTR*(CS-   ST**2-QM2/T - CN*(CS+QM2*QM2/(T*U)))
      ENDIF
C
      HCS=0.
      IQ2=IQ1+6
      DO 6 ID1=1,13
      IF (DISF(ID1,1).LT.EPS) GOTO 6
      HQ1=ID1.EQ.IQ1.OR.ID1.EQ.IQ2
      DO 5 ID2=1,13
      IF (DISF(ID2,2).LT.EPS) GOTO 5
      HQ2=ID2.EQ.IQ1.OR.ID2.EQ.IQ2
      DIST=DISF(ID1,1)*DISF(ID2,2)
      IF (HQ1.OR.HQ2) THEN
C---PROCESSES INVOLVING HEAVY CONSTITUENT
C   N.B. NEGLECT CASE THAT BOTH ARE HEAVY
      IF (HQ1.AND.HQ2) GOTO 5
      IF (ID1.LT.7) THEN
C---QUARK FIRST
       IF (ID2.LT.7) THEN
         HCS=HCS+ASTU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,3421, 3,*9)
       ELSEIF (ID2.NE.13) THEN
         HCS=HCS+ASTU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,3142, 9,*9)
       ELSE
         HCS=HCS+CTSU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,3142,10,*9)
         HCS=HCS+CTUS*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,3421,11,*9)
       ENDIF
      ELSEIF (ID1.NE.13) THEN
C---QBAR FIRST
       IF (ID2.LT.7) THEN
         HCS=HCS+ASTU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,2413,17,*9)
       ELSEIF (ID2.NE.13) THEN
         HCS=HCS+ASTU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,4312,20,*9)
       ELSE
         HCS=HCS+CTSU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,2413,21,*9)
         HCS=HCS+CTUS*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,4312,22,*9)
       ENDIF
      ELSE
C---GLUON FIRST
       IF (ID2.LT.7) THEN
         HCS=HCS+CTSU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,2413,23,*9)
         HCS=HCS+CTUS*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,3421,24,*9)
       ELSEIF (ID2.LT.13) THEN
         HCS=HCS+CTSU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,3142,25,*9)
         HCS=HCS+CTUS*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,4312,26,*9)
       ENDIF
      ENDIF
      ELSEIF (ID2.NE.13.AND.ID2.EQ.ID1+6) THEN
C---LIGHT Q-QBAR ANNIHILATION
         HCS=HCS+AUST*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(IQ1,IQ2,2413, 4,*9)
      ELSEIF (ID1.NE.13.AND.ID1.EQ.ID2+6) THEN
C---LIGHT QBAR-Q ANNIHILATION
         HCS=HCS+AUST*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(IQ2,IQ1,3142,12,*9)
      ELSEIF (ID1.EQ.13.AND.ID2.EQ.13) THEN
C---GLUON FUSION
         HCS=HCS+CSTU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(IQ1,IQ2,2413,27,*9)
         HCS=HCS+CSUT*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(IQ1,IQ2,4123,28,*9)
      ENDIF
    5 CONTINUE
    6 CONTINUE
      EVWGT=HCS
      RETURN
C---GENERATE EVENT
    9 IDN(1)=ID1
      IDN(2)=ID2
      IDCMF=15
      CALL HWETWO
      IF (AZSPIN) THEN
C Calculate coefficients for constructing spin density matrices
         IF (IHPRO.EQ.7 .OR.IHPRO.EQ.8 .OR.
     &       IHPRO.EQ.15.OR.IHPRO.EQ.16) THEN
C qqbar-->gg or qbarq-->gg
            UT=1./TU
            GCOEF(1)=UT+TU
            GCOEF(2)=-2.
            GCOEF(3)=0.
            GCOEF(4)=0.
            GCOEF(5)=GCOEF(1)
            GCOEF(6)=UT-TU
            GCOEF(7)=-GCOEF(6)
         ELSEIF (IHPRO.EQ.10.OR.IHPRO.EQ.11.OR.
     &           IHPRO.EQ.21.OR.IHPRO.EQ.22.OR.
     &           IHPRO.EQ.23.OR.IHPRO.EQ.24.OR.
     &           IHPRO.EQ.25.OR.IHPRO.EQ.26) THEN
C qg-->qg or qbarg-->qbarg or gq-->gq  or gqbar-->gqbar
            SU=1./US
            GCOEF(1)=-(SU+US)
            GCOEF(2)=0.
            GCOEF(3)=2.
            GCOEF(4)=0.
            GCOEF(5)=SU-US
            GCOEF(6)=GCOEF(1)
            GCOEF(7)=-GCOEF(5)
         ELSEIF (IHPRO.EQ.27.OR.IHPRO.EQ.28) THEN
C gg-->qqbar
            UT=1./TU
            GCOEF(1)=TU+UT
            GCOEF(2)=-2.
            GCOEF(3)=0.
            GCOEF(4)=0.
            GCOEF(5)=GCOEF(1)
            GCOEF(6)=TU-UT
            GCOEF(7)=-GCOEF(6)
         ELSEIF (IHPRO.EQ.29.OR.IHPRO.EQ.30.OR.
     &                          IHPRO.EQ.31) THEN
C gg-->gg
            GT=S*S+T*T+U*U
            GCOEF(2)=2.*U*U*T*T
            GCOEF(3)=2.*S*S*U*U
            GCOEF(4)=2.*S*S*T*T
            GCOEF(1)=GT*GT-GCOEF(2)-GCOEF(3)-GCOEF(4)
            GCOEF(5)=GT*(GT-2.*S*S)-GCOEF(2)
            GCOEF(6)=GT*(GT-2.*T*T)-GCOEF(3)
            GCOEF(7)=GT*(GT-2.*U*U)-GCOEF(4)
         ELSE
            CALL HWVZRO(7,GCOEF)
         ENDIF
      ENDIF
  999 END
