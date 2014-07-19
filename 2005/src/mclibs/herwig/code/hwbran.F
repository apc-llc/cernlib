CDECK  ID>, HWBRAN.
*CMZ :-        -15/07/92  14.08.45  by  Mike Seymour
*-- Author :    Bryan Webber & Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWBRAN(KPAR)
C-----------------------------------------------------------------------
C     BRANCHES TIMELIKE PARTON KPAR INTO TWO, PUTS PRODUCTS
C     INTO NPAR+1 AND NPAR+2, AND INCREASES NPAR BY TWO
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWBVMC,HWRGEN,HWUALF,HWUTAB,HWRUNI,HWULDO,PMOM,
     & QNOW,QLST,QKTHR,RN,QQBAR,DQQ,QGTHR,SNOW,QSUD,ZMIN,ZMAX,ZRAT,WMIN,
     & QLAM,Z1,Z2,ETEST,ZTEST,ENOW,XI,XIPREV,EPREV,QMAX,QGAM,SLST,SFNL,
     & TARG,ALF,BETA0(3:6),BETAP(3:6),SQRK(4:6,5),REJFAC,Z,X1,X2,OTHXI,
     & OTHZ
      INTEGER HWRINT,KPAR,ID,JD,IS,NTRY,N,ID1,ID2,MPAR,ISUD(13),IHEP,
     & JHEP,M,NF,NN,IREJ,NREJ
      EXTERNAL HWBVMC,HWRGEN,HWUALF,HWUTAB,HWRUNI,HWULDO,HWRINT
      SAVE BETA0,BETAP,SQRK
      DATA ISUD,BETA0/2,2,3,4,5,6,2,2,3,4,5,6,1,4*ZERO/
      IF (IERROR.NE.0) RETURN
C---SET SQRK(M,N) TO THE PROBABILITY THAT A GLUON WILL NOT PRODUCE A
C   QUARK-ANTIQUARK PAIR BETWEEN SCALES RMASS(M) AND 2*HWBVMC(N)
      IF (SUDORD.NE.1.AND.BETA0(3).EQ.ZERO) THEN
        DO 100 M=3,6
          BETA0(M)=(11.*CAFAC-2.*M)*0.5
 100      BETAP(M)=(17.*CAFAC**2-(5.*CAFAC+3.*CFFAC)*M)
     &            /BETA0(M)*0.25/PIFAC
        DO 120 N=1,5
          DO 110 M=4,6
            IF (M.LE.N) THEN
              SQRK(M,N)=ONE
            ELSEIF (M.EQ.4.OR.M.EQ.N+1) THEN
              NF=M
              IF (2*HWBVMC(N).GT.RMASS(M)) NF=M+1
              SQRK(M,N)=((BETAP(NF-1)+1/HWUALF(1,2*HWBVMC(N)))/
     $             (BETAP(NF-1)+1/HWUALF(1,RMASS(M))))**(1/BETA0(NF-1))
            ELSE
              SQRK(M,N)=SQRK(M-1,N)*
     $             ((BETAP(M-1)+1/HWUALF(1,RMASS(M-1)))/
     $             (BETAP(M-1)+1/HWUALF(1,RMASS(M))))**(1/BETA0(M-1))
            ENDIF
 110      CONTINUE
 120    CONTINUE
      ENDIF
      ID=IDPAR(KPAR)
C--TEST FOR PARTON TYPE
      IF (ID.LE.13) THEN
        JD=ID
        IS=ISUD(ID)
      ELSEIF (ID.GE.208) THEN
        JD=ID-200
        IS=7
      ELSE
        IS=0
      END IF
      QNOW=-1.
      IF (IS.NE.0) THEN
C--TIMELIKE PARTON BRANCHING
        ENOW=PPAR(4,KPAR)
        XIPREV=PPAR(2,KPAR)
        IF (JMOPAR(1,KPAR).EQ.0) THEN
          EPREV=PPAR(4,KPAR)
        ELSE
          EPREV=PPAR(4,JMOPAR(1,KPAR))
        ENDIF
C--IF THIS IS CHARGED & PHOTONS ARE ALLOWED, ANGLES MIGHT NOT BE ORDERED
        QMAX=0
        QLST=PPAR(1,KPAR)
        IF (ICHRG(ID).NE.0 .AND. VPCUT.LT.PPAR(1,2)) THEN
C--LOOK FOR A PREVIOUS G->QQBAR, IF ANY
          MPAR=KPAR
 1        IF (JMOPAR(1,MPAR).NE.0) THEN
            IF (IDPAR(JMOPAR(1,MPAR)).EQ.ID) THEN
              MPAR=JMOPAR(1,MPAR)
              GOTO 1
            ENDIF
          ENDIF
C--IF CLIMBED TO THE TOP OF THE LIST, FIND QED INTERFERENCE PARTNER
          IF (MPAR.EQ.2) THEN
            IF (ID.LT.7) THEN
              IHEP=JDAHEP(2,JCOPAR(1,1))
              JHEP=JDAHEP(2,IHEP)
            ELSE
              IHEP=JMOHEP(2,JCOPAR(1,1))
              JHEP=JMOHEP(2,IHEP)
            ENDIF
            QMAX=HWULDO(PHEP(1,IHEP),PHEP(1,JHEP))*(ENOW/PPAR(4,2))**2
          ELSE
            QMAX=ENOW**2*PPAR(2,MPAR)
          ENDIF
C--IF PREVIOUS BRANCHING WAS Q->QGAMMA, LOOK FOR A QCD BRANCHING
          MPAR=KPAR
 2        IF (JMOPAR(1,MPAR).NE.0) THEN
            IF (IDPAR(JDAPAR(1,JMOPAR(1,MPAR))).EQ.59 .OR.
     &        IDPAR(JDAPAR(2,JMOPAR(1,MPAR))).EQ.59) THEN
              MPAR=JMOPAR(1,MPAR)
              GOTO 2
            ENDIF
          ENDIF
          QLST=ENOW**2*PPAR(2,MPAR)
          QMAX=SQRT(MAX(ZERO,MIN(
     &         QMAX , EPREV**2*XIPREV , ENOW**2*XIPREV*(2-XIPREV))))
          QLST=SQRT(MIN(
     &         QLST , EPREV**2*XIPREV , ENOW**2*XIPREV*(2-XIPREV)))
        ENDIF
        NTRY=0
    5   NTRY=NTRY+1
        IF (NTRY.GT.NBTRY) CALL HWWARN('HWBRAN',100,*999)
        IF (ID.EQ.13) THEN
C--GLUON -> QUARK+ANTIQUARK OPTION
          IF (QLST.GT.QCDL3) THEN
            DO 8 N=1,NFLAV
            QKTHR=2.*HWBVMC(N)
            IF (QLST.GT.QKTHR) THEN
              RN=HWRGEN(N)
              IF (SUDORD.NE.1) THEN
C---FIND IN WHICH FLAVOUR INTERVAL THE UPPER LIMIT LIES
                NF=3
                DO 200 M=MAX(3,N),NFLAV
 200              IF (QLST.GT.RMASS(M)) NF=M
C---CALCULATE THE FORM FACTOR
                IF (NF.EQ.MAX(3,N)) THEN
                  SFNL=((BETAP(NF)+1/HWUALF(1,QKTHR))/
     $                 (BETAP(NF)+1/HWUALF(1,QLST)))**(1/BETA0(NF))
                  SLST=SFNL
                ELSE
                  SFNL=((BETAP(NF)+1/HWUALF(1,RMASS(NF)))/
     $                 (BETAP(NF)+1/HWUALF(1,QLST)))**(1/BETA0(NF))
                  SLST=SFNL*SQRK(NF,N)
                ENDIF
              ENDIF
              IF (RN.GT.1.E-3) THEN
                QQBAR=QCDL3*(QLST/QCDL3)**(RN**BETAF)
              ELSE
                QQBAR=QCDL3
              ENDIF
              IF (SUDORD.NE.1) THEN
C---FIND IN WHICH FLAVOUR INTERVAL THE SOLUTION LIES
                IF (RN.GE.SFNL) THEN
                  NN=NF
                ELSEIF (RN.GE.SLST) THEN
                  NN=MAX(3,N)
                  DO 210 M=MAX(3,N)+1,NF-1
 210                IF (RN.GE.SLST/SQRK(M,N)) NN=M
                ELSE
                  NN=0
                  QQBAR=QCDL3
                ENDIF
                IF (NN.GT.0) THEN
                  IF (NN.EQ.NF) THEN
                    TARG=HWUALF(1,QLST)
                  ELSE
                    TARG=HWUALF(1,RMASS(NN+1))
                    RN=RN/SLST*SQRK(NN+1,N)
                  ENDIF
                  TARG=1/((BETAP(NN)+1/TARG)*RN**BETA0(NN)-BETAP(NN))
C---NOW SOLVE HWUALF(1,QQBAR)=TARG FOR QQBAR ITERATIVELY
 7                QQBAR=MAX(QQBAR,HALF*QKTHR)
                  ALF=HWUALF(1,QQBAR)
                  IF (ABS(ALF-TARG).GT.ACCUR) THEN
                    NTRY=NTRY+1
                    IF (NTRY.GT.NBTRY) CALL HWWARN('HWBRAN',101,*999)
                    QQBAR=QQBAR*(1+3*PIFAC*(ALF-TARG)
     $                   /(BETA0(NN)*ALF**2*(1+BETAP(NN)*ALF)))
                    GOTO 7
                  ENDIF
                ENDIF
              ENDIF
              IF (QQBAR.GT.QNOW.AND.QQBAR.GT.QKTHR) THEN
                QNOW=QQBAR
                ID2=N
              ENDIF
            ELSE
              GOTO 9
            ENDIF
    8       CONTINUE
          ENDIF
C--GLUON->DIQUARKS OPTION
    9     IF (QLST.LT.QDIQK) THEN
            IF (PDIQK.NE.ZERO) THEN
              RN=HWRGEN(0)
              DQQ=QLST*EXP(-RN/PDIQK)
              IF (DQQ.GT.QNOW) THEN
                IF (DQQ.GT.2.*RMASS(115)) THEN
                  QNOW=DQQ
                  ID2=115
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
C--ENHANCE GLUON AND PHOTON EMISSION BY A FACTOR OF TWO IF THIS BRANCH
C  IS CAPABLE OF BEING THE HARDEST SO FAR
        NREJ=1
        IF (TMPAR(2).AND.0.25*MAX(QLST,QMAX).GT.HARDST) NREJ=2
C--BRANCHING ID->ID+GLUON
        QGTHR=HWBVMC(ID)+HWBVMC(13)
        IF (QLST.GT.QGTHR) THEN
         DO 300 IREJ=1,NREJ
          RN=HWRGEN(1)
          SLST=HWUTAB(SUD(1,IS),QEV(1,IS),NQEV,QLST,INTER)
          IF (RN.EQ.0.) THEN
            SNOW=2.
          ELSE
            SNOW=SLST/RN
          ENDIF
          IF (SNOW.LT.1.) THEN
            QSUD=HWUTAB(QEV(1,IS),SUD(1,IS),NQEV,SNOW,INTER)
C---IF FORM FACTOR DID NOT GET INVERTED CORRECTLY TRY LINEAR INSTEAD
            IF (QSUD.GT.QLST) THEN
              SNOW=HWUTAB(SUD(1,IS),QEV(1,IS),NQEV,QLST,1)/RN
              QSUD=HWUTAB(QEV(1,IS),SUD(1,IS),NQEV,SNOW,1)
              IF (QSUD.GT.QLST) THEN
                CALL HWWARN('HWBRAN',1,*999)
                QSUD=-1
              ENDIF
            ENDIF
            IF (QSUD.GT.QGTHR.AND.QSUD.GT.QNOW) THEN
              ID2=13
              QNOW=QSUD
            ENDIF
          ENDIF
 300     CONTINUE
        ENDIF
C--BRANCHING ID->ID+PHOTON
        IF (ICHRG(ID).NE.0) THEN
          QGTHR=MAX(HWBVMC(ID)+HWBVMC(59),HWBVMC(59)*EXP(0.75))
          IF (QMAX.GT.QGTHR) THEN
           DO 400 IREJ=1,NREJ
            RN=HWRGEN(2)
            IF (RN.EQ.0) THEN
              QGAM=0
            ELSE
              QGAM=(LOG(QMAX/HWBVMC(59))-0.75)**2
     &            +PIFAC*9/(ICHRG(ID)**2*ALPFAC*ALPHEM)*LOG(RN)
              IF (QGAM.GT.0) THEN
                QGAM=HWBVMC(59)*EXP(0.75+SQRT(QGAM))
              ELSE
                QGAM=0
              ENDIF
            ENDIF
            IF (QGAM.GT.QGTHR.AND.QGAM.GT.QNOW) THEN
              ID2=59
              QNOW=QGAM
            ENDIF
 400       CONTINUE
          ENDIF
        ENDIF
        IF (QNOW.GT.0.) THEN
C--BRANCHING HAS OCCURRED
          ZMIN=HWBVMC(ID2)/QNOW
          ZMAX=1.-ZMIN
          IF (ID.EQ.13) THEN
            IF (ID2.EQ.13) THEN
C--GLUON -> GLUON + GLUON
              ID1=13
              WMIN=ZMIN*ZMAX
              ETEST=(1.-WMIN)**2*HWUALF(5-SUDORD*2,QNOW*WMIN)
              ZRAT=(ZMAX*(1-ZMIN))/(ZMIN*(1-ZMAX))
C--CHOOSE Z1 DISTRIBUTED ON (ZMIN,ZMAX)
C  ACCORDING TO GLUON BRANCHING FUNCTION
   10         Z1=ZMAX/(ZMAX+(1-ZMAX)*ZRAT**HWRGEN(0))
              Z2=1.-Z1
              ZTEST=(1.-(Z1*Z2))**2*HWUALF(5-SUDORD*2,QNOW*(Z1*Z2))
              IF (ZTEST.LT.ETEST*HWRGEN(1)) GOTO 10
              Z=Z1
            ELSEIF (ID2.NE.115) THEN
C--GLUON -> QUARKS
              ID1=ID2+6
              ETEST=ZMIN**2+ZMAX**2
   20         Z1=HWRUNI(0,ZMIN,ZMAX)
              Z2=1.-Z1
              ZTEST=Z1*Z1+Z2*Z2
              IF (ZTEST.LT.ETEST*HWRGEN(0)) GOTO 20
            ELSE
C--GLUON -> DIQUARKS
              ID2=HWRINT(115,117)
              ID1=ID2-6
              Z1=HWRUNI(0,ZMIN,ZMAX)
              Z2=1.-Z1
            ENDIF
          ELSE
C--QUARK OR ANTIQUARK BRANCHING
            IF (ID2.EQ.13) THEN
C--TO GLUON
              ZMAX=1.-HWBVMC(ID)/QNOW
              WMIN=MIN(ZMIN*(1.-ZMIN),ZMAX*(1.-ZMAX))
              ETEST=(1.+ZMAX**2)*HWUALF(5-SUDORD*2,QNOW*WMIN)
              ZRAT=ZMAX/ZMIN
   30         Z1=ZMIN*ZRAT**HWRGEN(0)
              Z2=1.-Z1
              ZTEST=(1.+Z2*Z2)*HWUALF(5-SUDORD*2,QNOW*Z1*Z2)
              IF (ZTEST.LT.ETEST*HWRGEN(1)) GOTO 30
            ELSE
C--TO PHOTON
              ZMIN=  HWBVMC(59)/QNOW
              ZMAX=1-HWBVMC(ID)/QNOW
              ZRAT=ZMAX/ZMIN
              ETEST=1+(1-ZMIN)**2
   40         Z1=ZMIN*ZRAT**HWRGEN(0)
              Z2=1-Z1
              ZTEST=1+Z2*Z2
              IF (ZTEST.LT.ETEST*HWRGEN(1)) GOTO 40
            ENDIF
C--QUARKS EMIT ON LOWER SIDE, ANTIQUARKS ON UPPER SIDE
            Z=Z1
            IF (JD.LE.6) THEN
              Z1=Z2
              Z2=1.-Z2
              ID1=ID
            ELSE
              ID1=ID2
              ID2=ID
            ENDIF
          ENDIF
C--UPDATE THIS BRANCH AND CREATE NEW BRANCHES
          XI=(QNOW/ENOW)**2
          IF (ID1.NE.59.AND.ID2.NE.59) THEN
            IF (ID.EQ.13.AND.ID1.NE.13) THEN
              QLAM=QNOW
            ELSE
              QLAM=QNOW*Z1*Z2
            ENDIF
            IF (SUDORD.EQ.1.AND.HWUALF(2,QLAM).LT.HWRGEN(0) .OR.
     &           (2.-XI)*(QNOW*Z1*Z2)**2.GT.EMSCA**2) THEN
C--BRANCHING REJECTED: REDUCE Q AND REPEAT
                QMAX=QNOW
                QLST=QNOW
                QNOW=-1.
                GOTO 5
            ENDIF
          ENDIF
C--IF THIS IS HARDEST EMISSION SO FAR, APPLY MATRIX-ELEMENT CORRECTION
          IF (ID.NE.13.OR.ID1.EQ.13) THEN
            QLAM=QNOW*Z1*Z2
            REJFAC=1
            IF (TMPAR(2).AND.QLAM.GT.HARDST) THEN
              IF (MOD(ISTHEP(JCOPAR(1,1)),10).GE.3) THEN
C---COLOUR PARTNER IS ALSO OUTGOING
                X1=1-Z*(1-Z)*XI
                X2=0.5*(1+Z*(1-Z)*XI +
     $               (1-Z*(1-Z)*XI)*(1-2*Z)/SQRT(1-2*Z*(1-Z)*XI))
                REJFAC=SQRT(2*X1-1)/(X1*Z*(1-Z))
     $               *(1+(1-Z)**2)/(Z*XI)
     $               *(1-X1)*(1-X2)/(X1**2+X2**2)
C---CHECK WHETHER IT IS IN THE OVERLAP REGION
                OTHXI=4*(1-X2)*X2**2/(X2**2-(2*X2-1)*(2*X1+X2-2)**2)
                IF (OTHXI.LT.1) THEN
                  OTHZ=0.5*(1-SQRT(2*X2-1)/X2*(2*X1+X2-2))
                  REJFAC=REJFAC+SQRT(2*X2-1)/(X2*OTHZ*(1-OTHZ))
     $                 *(1+(1-OTHZ)**2)/(OTHZ*OTHXI)
     $                 *(1-X2)*(1-X1)/(X2**2+X1**2)
                ENDIF
              ELSE
C---COLOUR PARTNER IS INCOMING (X1=XP, X2=ZP)
                X1=1/(1+Z*(1-Z)*XI)
                X2=0.5*(1+(1-2*Z)/SQRT(1-2*Z*(1-Z)*XI))
                REJFAC=SQRT(3-2/X1)/(X1**2*Z*(1-Z))
     $               *(1+(1-Z)**2)/(Z*XI)
     $               *(1-X1)*(1-X2)/
     $               (1+(1-X1-X2+2*X1*X2)**2+2*(1-X1)*(1-X2)*X1*X2)
C---CHECK WHETHER IT IS IN THE OVERLAP REGION
                OTHXI=(SQRT(X1+2*(1-X2)*(1-X2+X1*X2))-SQRT(X1))**2/
     $               (1+X1-X2-SQRT(X1*(X1+2*(1-X2)*(1-X2+X1*X2))))
                OTHZ=(SQRT(X1*(X1+2*(1-X2)*(1-X2+X1*X2)))-X1)/(1-X2)
                IF (OTHXI.LT.OTHZ**2) THEN
                  REJFAC=REJFAC+OTHZ**3*(1-X1-X2+2*X1*X2)
     $                 /(X1**2*(1-OTHZ)*(OTHZ+OTHXI*(1-OTHZ)))
     $                 *(1+OTHZ**2)/((1-OTHZ)*OTHXI)
     $                 *(1-X1)*(1-X2)/
     $                 (1+(1-X1-X2+2*X1*X2)**2+2*(1-X1)*(1-X2)*X1*X2)
                ENDIF
              ENDIF
            ENDIF
            IF (NREJ*REJFAC*HWRGEN(NREJ).GT.1) THEN
              QMAX=QNOW
              QLST=QNOW
              QNOW=-1.
              GOTO 5
            ENDIF
            IF (QLAM.GT.HARDST) HARDST=QLAM
          ENDIF
          MPAR=NPAR+1
          IDPAR(MPAR)=ID1
          TMPAR(MPAR)=.TRUE.
          PPAR(1,MPAR)=QNOW*Z1
          PPAR(2,MPAR)=XI
          PPAR(4,MPAR)=ENOW*Z1
          NPAR=NPAR+2
          IDPAR(NPAR)=ID2
          TMPAR(NPAR)=.TRUE.
          PPAR(1,NPAR)=QNOW*Z2
          PPAR(2,NPAR)=XI
          PPAR(4,NPAR)=ENOW*Z2
C---NEW MOTHER-DAUGHTER RELATIONS
          JDAPAR(1,KPAR)=MPAR
          JDAPAR(2,KPAR)=NPAR
          JMOPAR(1,MPAR)=KPAR
          JMOPAR(1,NPAR)=KPAR
C---NEW COLOUR CONNECTIONS
          JCOPAR(3,KPAR)=NPAR
          JCOPAR(4,KPAR)=MPAR
          JCOPAR(1,MPAR)=NPAR
          JCOPAR(2,MPAR)=KPAR
          JCOPAR(1,NPAR)=KPAR
          JCOPAR(2,NPAR)=MPAR
C
        ENDIF
      ENDIF
      IF (QNOW.LT.0.) THEN
C--BRANCHING STOPS
        PPAR(5,KPAR)=RMASS(ID)**2
        PMOM=PPAR(4,KPAR)**2-PPAR(5,KPAR)
        IF (ID.GE.198 .AND. ID.LE.201) THEN
          PPAR(5,KPAR)=PPAR(1,2)**2
          PMOM=0
        ENDIF
        IF (PMOM.LT.-1E-6) CALL HWWARN('HWBRAN',104,*999)
        IF (PMOM.LT.0) PMOM=0
        PPAR(3,KPAR)=SQRT(PMOM)
        JDAPAR(1,KPAR)=0
        JDAPAR(2,KPAR)=0
        JCOPAR(3,KPAR)=0
        JCOPAR(4,KPAR)=0
      ENDIF
  999 END
