CDECK  ID>, HWHQCD.
*CMZ :-        -26/04/91  14.55.45  by  Federico Carminati
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWHQCD
C-----------------------------------------------------------------------
C     QCD HARD 2->2 PROCESSES: MEAN EVWGT = SIGMA IN NB
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,HWRUNI,HWUALF,RS,EPS,HF,RCS,Z1,Z2,ET,EJ,
     & FACTR,S,T,U,ST,TU,US,STU,TUS,UST,EN,RN,GFLA,AF,ASTU,ASUT,AUST,
     & BF,BSTU,BSUT,BUST,BUTS,CF,CSTU,CSUT,CTSU,CTUS,DF,DSTU,DTSU,DUTS,
     & DIST,HCS,UT,SU,GT,KK,KK2,YJ1INF,YJ1SUP,YJ2INF,YJ2SUP
      INTEGER ID1,ID2,I
      EXTERNAL HWRGEN,HWRUNI,HWUALF
      SAVE HCS
      PARAMETER (EPS=1.E-9,HF=0.5)
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
        XX(1)=.5*(Z1+Z2)*KK
        IF (XX(1).GE.1.) RETURN
        XX(2)=XX(1)/(Z1*Z2)
        IF (XX(2).GE.1.) RETURN
        COSTH=(Z1-Z2)/(Z1+Z2)
        S=XX(1)*XX(2)*PHEP(5,3)**2
        RS=HF*SQRT(S)
        DO 3 I=1,NFLAV
        IF (RS.LT.RMASS(I)) GOTO 4
    3   CONTINUE
        I=NFLAV+1
    4   MAXFL=I-1
        IF (MAXFL.EQ.0) CALL HWWARN('HWHQCD',100,*999)
C
        T=-HF*S*(1.-COSTH)
        U=-S-T
C---SET EMSCA TO HARD PROCESS SCALE (APPROX ET-JET)
        EMSCA=SQRT(2.*S*T*U/(S*S+T*T+U*U))
        FACTR = GEV2NB*.5*PIFAC*EJ*ET*(HWUALF(1,EMSCA)/S)**2
     &        * (YJ1SUP-YJ1INF)*(YJ2SUP-YJ2INF)
        CALL HWSGEN(.FALSE.)
C
        ST=S/T
        TU=T/U
        US=U/S
        STU=TU/US
        TUS=US/ST
        UST=ST/TU
C
        EN=CAFAC
        RN=CFFAC/EN
        GFLA=HF*FLOAT(MAXFL)/(EN*RN)**2
        AF=FACTR*RN
        ASTU=AF*(1.-2.*UST)
        ASUT=AF*(1.-2.*STU)
        AUST=AF*(1.-2.*TUS)
        BF=2.*AF/EN
        BSTU=HF*(ASTU+BF*ST)
        BSUT=HF*(ASUT+BF/US)
        BUST=AUST+BF*US
        BUTS=ASTU+BF/TU
        CF=AF*EN
        CSTU=(CF*(RN-TUS))/TU
        CSUT=(CF*(RN-TUS))*TU
        CTSU=(FACTR*(UST-RN))*US
        CTUS=(FACTR*(UST-RN))/US
        DF=HF*FACTR/RN
        DSTU=DF*(1.+1./TUS-STU-UST)
        DTSU=DF*(1.+1./UST-STU-TUS)
        DUTS=DF*(1.+1./STU-UST-TUS)
      ENDIF
C
      HCS=0.
      DO 6 ID1=1,13
      IF (DISF(ID1,1).LT.EPS) GOTO 6
      DO 5 ID2=1,13
      IF (DISF(ID2,2).LT.EPS) GOTO 5
      DIST=DISF(ID1,1)*DISF(ID2,2)
      IF (ID1.LT.7) THEN
C---QUARK FIRST
       IF (ID2.LT.7) THEN
        IF (ID1.NE.ID2) THEN
         HCS=HCS+ASTU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,3421, 3,*9)
        ELSE
         HCS=HCS+BSTU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,3421, 1,*9)
         HCS=HCS+BSUT*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,4312, 2,*9)
        ENDIF
       ELSEIF (ID2.NE.13) THEN
        IF (ID2.NE.ID1+6) THEN
         HCS=HCS+ASTU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,3142, 9,*9)
        ELSE
         HCS=HCS+FLOAT(MAXFL-1)*AUST*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(-ID1, 0,2413, 4,*9)
         HCS=HCS+BUTS*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,3142, 5,*9)
         HCS=HCS+BUST*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,2413, 6,*9)
         HCS=HCS+CSTU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP( 13, 13,2413, 7,*9)
         HCS=HCS+CSUT*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP( 13, 13,2341, 8,*9)
        ENDIF
       ELSE
         HCS=HCS+CTSU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,3142,10,*9)
         HCS=HCS+CTUS*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,3421,11,*9)
       ENDIF
      ELSEIF (ID1.NE.13) THEN
C---QBAR FIRST
       IF (ID2.LT.7) THEN
        IF (ID1.NE.ID2+6) THEN
         HCS=HCS+ASTU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,2413,17,*9)
        ELSE
         HCS=HCS+FLOAT(MAXFL-1)*AUST*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(-ID1, 0,3142,12,*9)
         HCS=HCS+BUTS*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,2413,13,*9)
         HCS=HCS+BUST*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,3142,14,*9)
         HCS=HCS+CSTU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP( 13, 13,3142,15,*9)
         HCS=HCS+CSUT*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP( 13, 13,4123,16,*9)
        ENDIF
       ELSEIF (ID2.NE.13) THEN
        IF (ID1.NE.ID2) THEN
         HCS=HCS+ASTU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,4312,20,*9)
        ELSE
         HCS=HCS+BSTU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,4312,18,*9)
         HCS=HCS+BSUT*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,3421,19,*9)
        ENDIF
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
       ELSE
         HCS=HCS+GFLA*CSTU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(  0,  0,2413,27,*9)
         HCS=HCS+GFLA*CSUT*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(  0,  0,4123,28,*9)
         HCS=HCS+DTSU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,2341,29,*9)
         HCS=HCS+DSTU*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,3421,30,*9)
         HCS=HCS+DUTS*DIST
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,ID2,2413,31,*9)
       ENDIF
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
