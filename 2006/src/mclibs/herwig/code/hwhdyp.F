CDECK  ID>, HWHDYP.
*CMZ :-        -16/07/96  12.41.07  by  Mike Seymour
*-- Author :    Bryan Webber, Ian Knowles and Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWHDYP
C-----------------------------------------------------------------------
C     Drell-Yan Production of fermion pairs via photon, Z0 & (if ZPRIME)
C     Z' exchange. Lepton universality is assumed for photon and Z, and
C     for Z' if no lepton flavour is specified.
C     MEAN EVWGT = SIGMA IN NB
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,HWRUNI,HWUAEM,EPS,C1,C2,C3,EMSQZ,EMGMZ,
     & EMSQZP,EMGMZP,CQF(7,6,16),QPOW,RPOW,A01,A1,A02,A2,A03,A3,CRAN,
     & EMJ1,EMJ2,EMJ3,EMJAC,FACT,QSQ,HCS,FACTR,RCS,EXTRA,PMAX,PTHETA
      INTEGER IMODE,JQMN,JQMX,JQ,JLMN,JLMX,JL,IQ,I,IADD1,IADD2,ID1,ID2,
     & ID3,ID4,JF
      EXTERNAL HWRGEN,HWRUNI,HWUAEM
      SAVE HCS,JQMN,JQMX,JLMN,JLMX,C1,C2,C3,QPOW,RPOW,EMSQZ,EMGMZ,
     & A1,A01,A2,A02,A3,A03,EMSQZP,EMGMZP,FACT,CQF
      PARAMETER (EPS=1.D-9)
      IF (GENEV) THEN
        RCS=HCS*HWRGEN(0)
      ELSE
        IF (FSTWGT) THEN
C Set limits for which particles to include
          JLMN=1
          JLMX=0
          JQMN=1
          JQMX=0
          IMODE=MOD(IPROC,100)
          IF (IMODE.EQ.0) THEN
            JQMN=1
            JQMX=6
          ELSEIF (IMODE.LE.10) THEN
            JQMN=IMODE
            JQMX=IMODE
          ELSEIF (IMODE.EQ.50) THEN
            JLMN=11
            JLMX=16
          ELSEIF (IMODE.GE.50.AND.IMODE.LE.60) THEN
            JLMN=IMODE-40
            JLMX=IMODE-40
          ELSEIF (IMODE.EQ.99) THEN
            JQMN=1
            JQMX=6
            JLMN=11
            JLMX=16
          ELSE
            CALL HWWARN('HWHDYP',500,*999)
          ENDIF
C Set up parameters for importance sampling:
C sum of power law and two Breit-Wigners (relative weights C1,C2,C3)
          C1=ONE
          C2=ONE
          C3=ZERO
          IF (ZPRIME) C3=ONE
          IF (EMPOW.EQ.1) CALL HWWARN('HWHDYP',501,*999)
          IF (C2.EQ.0) CALL HWWARN('HWHDYP',502,*999)
          IF (C3.EQ.0.AND.ZPRIME) CALL HWWARN('HWHDYP',503,*999)
          QPOW=-EMPOW+1
          RPOW=1/QPOW
          EMSQZ=RMASS(200)**2
          EMGMZ=RMASS(200)*GAMZ
          A01=EMMIN**QPOW
          A1=(EMMAX**QPOW-A01)/C1
          A02=ATAN((EMMIN**2-EMSQZ)/EMGMZ)
          A2=(ATAN((EMMAX**2-EMSQZ)/EMGMZ)-A02)/C2
          IF (C3.GT.0) THEN
            EMSQZP=RMASS(202)**2
            EMGMZP=RMASS(202)*GAMZP
            A03=ATAN((EMMIN**2-EMSQZP)/EMGMZP)
            A3=(ATAN((EMMAX**2-EMSQZP)/EMGMZP)-A03)/C3
          ENDIF
        ENDIF
        EVWGT=0.
C Select a mass for the produced pair
        CRAN=(C1+C2+C3)*HWRGEN(1)
        IF (CRAN.LT.C1) THEN
C Use power law
          EMSCA=(A01+A1*CRAN)**RPOW
          QSQ=EMSCA**2
        ELSEIF (CRAN.LT.C1+C2) THEN
C Use Z Breit-Wigner
          CRAN=CRAN-C1
          QSQ=EMSQZ+EMGMZ*TAN(A02+A2*CRAN)
          EMSCA=SQRT(QSQ)
        ELSE
C Use Z' Breit-Wigner
          CRAN=CRAN-C1-C2
          QSQ=EMSQZP+EMGMZP*TAN(A03+A3*CRAN)
          EMSCA=SQRT(QSQ)
        ENDIF
        EMJ1=EMSCA**EMPOW/(1-EMPOW)*A1
        EMJ2=((QSQ-EMSQZ)**2+EMGMZ**2)/(2*EMSCA*EMGMZ)*A2
        IF (C3.GT.0) THEN
          EMJ3=((QSQ-EMSQZP)**2+EMGMZP**2)/(2*EMSCA*EMGMZP)*A3
          EMJAC=(C1+C2+C3)/(1/EMJ1+1/EMJ2+1/EMJ3)
        ELSE
          EMJAC=(C1+C2)/(1/EMJ1+1/EMJ2)
        ENDIF
C Select initial momentum fractions
        XXMIN=QSQ/PHEP(5,3)**2
        XLMIN=LOG(XXMIN)
        CALL HWSGEN(.TRUE.)
        FACT=-GEV2NB*HWUAEM(QSQ)**2*PIFAC*16.*EMJAC*XLMIN/
     &       (3.*EMSCA)**3
C Store cross-section coefficients
        DO 50 IQ=1,6
        DO 30 JQ=JQMN,JQMX
        IF (EMSCA.GT.2.*RMASS(JQ)) THEN
          CALL HWUCFF(IQ,JQ,QSQ,CQF(1,IQ,JQ))
        ELSE
          CALL HWVZRO(7,CQF(1,IQ,JQ))
        ENDIF
  30    CONTINUE
        DO 40 JL=JLMN,JLMX
        IF (EMSCA.GT.2.*RMASS(JQ)) THEN
          CALL HWUCFF(IQ,JL,QSQ,CQF(1,IQ,JL))
        ELSE
          CALL HWVZRO(7,CQF(1,IQ,JL))
        ENDIF
  40    CONTINUE
  50    CONTINUE
      ENDIF
C
      HCS=0.
      DO 90 I=1,2
      IF (I.EQ.1) THEN
C Quark first
        IADD1=0
        IADD2=6
      ELSE
C Anti-quark first
        IADD1=6
        IADD2=0
      ENDIF
      DO 80 IQ=1,6
      ID1=IQ+IADD1
      ID2=IQ+IADD2
      IF (DISF(ID1,1).LT.EPS.OR.DISF(ID2,2).LT.EPS) GOTO 80
      FACTR=FACT*DISF(ID1,1)*DISF(ID2,2)
C Quark final states
      DO 60 JQ=JQMN,JQMX
      ID3=JQ
      ID4=JQ+6
      IF (IQ.EQ.JQ) THEN
        HCS=HCS+FACTR*(CQF(1,IQ,JQ)+2*QFCH(IQ)**4)*FLOAT(NCOLO)
        IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID3,ID4,2143,50,*99)
      ELSE
        HCS=HCS+FACTR*CQF(1,IQ,JQ)*FLOAT(NCOLO)
        IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID3,ID4,2143,50,*99)
      ENDIF
  60  CONTINUE
C Lepton final states
      DO 70 JL=JLMN,JLMX
      ID3=110+JL
      ID4=ID3+6
      HCS=HCS+FACTR*CQF(1,IQ,JL)
      IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID3,ID4,2134,50,*99)
  70  CONTINUE
  80  CONTINUE
  90  CONTINUE
      EVWGT=HCS
      RETURN
C Generate event
  99  IDN(1)=ID1
      IDN(2)=ID2
      IDCMF=200
      IF (ID3.LE.6) THEN
        JF=JQ
      ELSE
        JF=JL
      ENDIF
C Select polar angle from distribution:
C CQF(1,IQ,JF)*(1.+COSTH**2)+CQF(3,IQ,JF)*COSTH+EXTRA*(1-COSTH)
      IF (ID1.EQ.ID3) THEN
        EXTRA=QFCH(ID1)**4
      ELSE
        EXTRA=0
      ENDIF
      PMAX=2.*(CQF(1,IQ,JF)+EXTRA)+ABS(CQF(3,IQ,JF))
  100 COSTH=HWRUNI(0,-ONE,ONE)
      PTHETA=CQF(1,IQ,JF)*(1.+COSTH**2)+CQF(3,IQ,JF)*COSTH
     &      +EXTRA*(1.-COSTH)
      IF (PTHETA.LT.PMAX*HWRGEN(1)) GOTO 100
      IF (ID1.GT.ID2) COSTH=-COSTH
      IDCMF=200
      CALL HWETWO
  999 END
