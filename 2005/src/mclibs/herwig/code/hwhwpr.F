CDECK  ID>, HWHWPR.
*CMZ :-        -24/03/92  14.22.13  by  Mike Seymour
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWHWPR
C-----------------------------------------------------------------------
C     W+/- PRODUCTION AND DECAY VIA DRELL-YAN PROCESS
C     MEAN EVWGT IS SIG(W+/-)*(BRANCHING FRACTION) IN NB
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,HWRUNI,HWUPCM,PRAN,PROB,COEF,CSFAC,EMW,
     & FTQK,PTOP,ETOP,EBOT,PMAX,FHAD,FTOT,BRAF,FLEP,TMIN,HWUAEM
      INTEGER HWRINT,ICH,IC,IL,ID,IDEC,JDEC,IWP(2,16)
      LOGICAL HWRLOG
      EXTERNAL HWRGEN,HWRUNI,HWUPCM,HWRINT,HWRLOG
      SAVE CSFAC,IDEC,FLEP,FTQK,ETOP,PTOP,EBOT,PMAX,PROB
      DATA IWP/2,7,1,8,7,2,8,1,4,9,3,10,9,4,10,3,
     &         2,9,3,8,9,2,8,3,4,7,1,10,7,4,10,1/
      IF (GENEV) THEN
C---GENERATE EVENT (X'S AND STRUCTURE FUNCTIONS ALREADY FOUND)
        PRAN=PROB*HWRGEN(0)
C---LOOP OVER PARTON FLAVOURS
        PROB=0.
        COEF=1.-SCABI
        DO 10 IC=1,16
          IF (IC.EQ.9) COEF=SCABI
          PROB=PROB+DISF(IWP(1,IC),1)*DISF(IWP(2,IC),2)*COEF
          IF (PROB.GE.PRAN) GOTO 20
   10   CONTINUE
C---STORE INCOMING PARTONS
   20   IDN(1)=IWP(1,IC)
        IDN(2)=IWP(2,IC)
        ICO(1)=2
        ICO(2)=1
C---ICH=1/2 FOR W+/-
        ICH=2-MOD(IC,2)
        IF ((IDEC.GT.49.AND.IDEC.LT.54).OR.
     &      (IDEC.EQ.99.AND.HWRLOG(FLEP))) THEN
C---LEPTONIC DECAY
          IL=IDEC-50
          IF (IL.EQ.0.OR.IL.GT.3) IL=HWRINT(1,3)
          IDN(3)=2*IL+121-ICH
          IDN(4)=2*IL+124+ICH
C---W DECAY ANGLE (1+COSTH)**2
          COSTH=2.*HWRGEN(1)**0.3333-1.
        ELSEIF (IDEC.EQ.5.OR.IDEC.EQ.6.OR.
     &        ((IDEC.EQ.0.OR.IDEC.EQ.99).AND.HWRLOG(FTQK))) THEN
C---W -> TOP + BOTTOM DECAY
          IDN(3)=7-ICH
          IDN(4)=10+ICH
   21     COSTH=HWRUNI(1,-ONE, ONE)
          IF ((ETOP+(PTOP*COSTH))*(EBOT+(PTOP*COSTH)).LT.
     &         PMAX*HWRGEN(1)) GOTO 21
        ELSE
C---OTHER HADRONIC DECAY
   25     PROB=0.
          PRAN=2.*HWRGEN(2)
          COEF=1.-SCABI
          DO 30 ID=ICH,16,4
            IF (ID.GT.8) COEF=SCABI
            PROB=PROB+COEF
            IF (PROB.GE.PRAN) THEN
              IDN(3)=IWP(1,ID)
              IDN(4)=IWP(2,ID)
              GOTO 40
            ENDIF
   30     CONTINUE
   40     CONTINUE
          IF (IDEC.GT.0.AND.IDEC.LT.5) THEN
            JDEC=IDEC+6
            IF (IDN(3).NE.IDEC.AND.IDN(4).NE.IDEC
     &     .AND.IDN(3).NE.JDEC.AND.IDN(4).NE.JDEC) GOTO 25
          ENDIF
          COSTH=2.*HWRGEN(1)**0.3333-1.
        ENDIF
        IDCMF=197+ICH
        IF (IDN(1).GT.6) COSTH=-COSTH
        ICO(3)=4
        ICO(4)=3
        CALL HWETWO
      ELSE
        IDEC=MOD(IPROC,100)
        IF (IDEC.EQ.5.OR.IDEC.EQ.6) THEN
          TMIN=ATAN((RMASS(6)**2-RMASS(199)**2)/(GAMW*RMASS(199)))
        ELSE
          TMIN=-ATAN(RMASS(199)/GAMW)
        ENDIF
        EVWGT=0.
        EMW=GAMW*TAN(HWRUNI(0,TMIN,PIFAC/2.))+RMASS(199)
        IF (EMW.LE.QSPAC) RETURN
        EMW=SQRT(EMW*RMASS(199))
        IF (EMW.GE.PHEP(5,3)) RETURN
        IF (EMLST.NE.EMW) THEN
          EMLST=EMW
          EMSCA=EMW
          XXMIN=(EMW/PHEP(5,3))**2
          XLMIN=LOG(XXMIN)
          CSFAC=-GEV2NB*PIFAC**2*HWUAEM(EMSCA**2)
     &          /(3.*SWEIN*EMW**2)*XLMIN
C---COMPUTE TOP AND LEPTONIC FRACTIONS
          FTQK=0.
          IF (NFLAV.GT.5) THEN
            PTOP=HWUPCM(EMW,RMASS(5),RMASS(6))
            IF (PTOP.GT.0.) THEN
              ETOP=SQRT(PTOP**2+RMASS(6)**2)
              EBOT=EMW-ETOP
              FTQK=2.*PTOP*(3.*ETOP*EBOT+PTOP**2)/EMW**3
              PMAX=(ETOP+PTOP)*(EBOT+PTOP)
            ENDIF
          ENDIF
          FHAD=FTQK+2.
          FTOT=FTQK+3.
C---MULTIPLY WEIGHT BY BRANCHING FRACTION
          IF (IDEC.EQ.0) THEN
            BRAF=FHAD
          ELSEIF (IDEC.LT.5.OR.IDEC.EQ.50) THEN
            BRAF=1.
          ELSEIF (IDEC.LT.7) THEN
            BRAF=FTQK
          ELSEIF (IDEC.EQ.99) THEN
            BRAF=FTOT
          ELSE
            BRAF=0.3333
          ENDIF
          CSFAC=CSFAC*BRAF/FTOT*(0.5-TMIN/PIFAC)
          FTQK=FTQK/FHAD
          FLEP=1./FTOT
        ENDIF
        CALL HWSGEN(.TRUE.)
C---LOOP OVER PARTON FLAVOURS
        PROB=0.
        COEF=1.-SCABI
        DO 100 IC=1,16
          IF (IC.EQ.9) COEF=SCABI
          PROB=PROB+DISF(IWP(1,IC),1)*DISF(IWP(2,IC),2)*COEF
  100   CONTINUE
        EVWGT=PROB*CSFAC
      ENDIF
  999 END
