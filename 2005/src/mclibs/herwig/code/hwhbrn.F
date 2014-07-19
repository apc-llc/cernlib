CDECK  ID>, HWHBRN.
*CMZ :-        -03/07/95  19.02.12  by  Giovanni Abbiendi
*-- Author :    Giovanni Abbiendi & Luca Stanco
C-----------------------------------------------------------------------
      SUBROUTINE HWHBRN (*)
C----------------------------------------------------------------------
C     Returns a point in the phase space (Y,Q2,SHAT,Z,PHI) and the
C     corresponding Jacobian factor AJACOB
C     Fill the logical vector INSIDE to tag contributing subprocesses
C     to the cross-section
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRUNI,HWRGEN,HWUPCM,LEP,Y,Q2,SHAT,Z,PHI,AJACOB,
     & DSIGMA,ME,MP,ML,MREMIF(18),MFIN1(18),MFIN2(18),RS,SMA,W2,RSHAT,
     & MF1,MF2,YMIN,YMAX,YJAC,Q2INF,Q2SUP,Q2JAC,EMW2,ZMIN,ZMAX,ZJAC,
     & GAMMA2,LAMBDA,PHIJAC,ZINT,ZLMIN,ZL,EMW,TMIN,TMAX,EMLMIN,EMLMAX,
     & SHMIN,EMMIF(18),EMMAF(18),WMIF(18),WMIN,MREMIN,YMIF(18),Q1CM(18),
     & Q2MAF(18),EMMAWF(18),ZMIF(18),ZMAF(18),PLMAX,PINC,SHINF,SHSUP,
     & SHJAC,CTHLIM,Q1,DETDSH,SRY,SRY0,SRY1
      INTEGER IQK,IFLAVU,IFLAVD,I,IMIN,IMAX,IFL,IPROO,IHAD,NTRY,DEBUG
      LOGICAL CHARGD,INCLUD(18),INSIDE(18)
      EXTERNAL HWRUNI,HWRGEN,HWUPCM
      SAVE EMLMIN,EMLMAX,EMMIF,EMMAF,MREMIN,MF1,MF2,YMIF,
     &     YMIN,YMAX,WMIN,WMIF
      COMMON /HWAREA/ LEP,Y,Q2,SHAT,Z,PHI,AJACOB,DSIGMA,ME,MP,ML,MREMIF,
     & MFIN1,MFIN2,RS,SMA,W2,RSHAT,IQK,IFLAVU,IFLAVD,IMIN,IMAX,IFL,
     & IPROO,CHARGD,INCLUD,INSIDE
      EQUIVALENCE (EMW,RMASS(198))
C
      IHAD=2
      IF (JDAHEP(1,IHAD).NE.0) IHAD=JDAHEP(1,IHAD)
C---Initialization
      IF (FSTWGT.OR.IHAD.NE.2) THEN
        ME = RMASS(IDHW(1))
        MP = RMASS(IDHW(IHAD))
        RS = PHEP(5,3)
        SMA = RS**2-ME**2-MP**2
        PINC = HWUPCM(RS,ME,MP)
C---Charged current
        IF (CHARGD) THEN
          ML=RMASS(IDHW(1)+1)
          YMAX = ONE - TWO*ML*MP / SMA
          YMAX = MIN(YMAX,YBMAX)
          MREMIN=MP
          IF (LEP.EQ.ONE) THEN
            MF1=RMASS(IFLAVD)
            MF2=RMASS(IFLAVU)
          ELSE
            MF1=RMASS(IFLAVU)
            MF2=RMASS(IFLAVD)
          ENDIF
          SHMIN = MF1**2+MF2**2 + TWO * PTMIN**2 +
     +            TWO * SQRT(PTMIN**2+MF1**2) * SQRT(PTMIN**2+MF2**2)
          EMLMIN=MAX(EMMIN,SQRT(SHMIN))
          EMLMAX=MIN(EMMAX,RS-ML-MREMIN)
          DEBUG=1
          IF (EMLMIN.GT.EMLMAX) GOTO 888
          WMIN=EMLMIN+MREMIN
          PLMAX=HWUPCM(RS,ML,WMIN)
          YMIN = ONE-TWO*(SQRT(PINC**2+MP**2)*SQRT(PLMAX**2+ML**2)+
     +                    PINC*PLMAX)/SMA
          YMIN = MAX(YMIN,YBMIN)
          DEBUG=2
          IF (YMIN.GT.YMAX) GOTO 888
        ELSE
C---Neutral current
          ML = ME
          YMAX = ONE - TWO*ML*MP / SMA
          YMAX = MIN(YMAX,YBMAX)
          DO I=1,18
            YMIF(I)=ZERO
            EMMIF(I)=ZERO
            EMMAF(I)=ZERO
            WMIF(I)=ZERO
            IF (I.LE.8) THEN
C---Boson-Gluon Fusion (also J/Psi) and QCD Compton with struck u or d
              MREMIF(I)=MP
              IF (I.LE.6) THEN
                MFIN1(I)=RMASS(I)
                MFIN2(I)=RMASS(I+6)
              ELSE
                MFIN1(I)=RMASS(I-6)
                MFIN2(I)=ZERO
              ENDIF
            ELSE
C---QCD Compton with struck non-valence parton
              MREMIF(I)=MP+RMASS(I-6)
              MFIN1(I)=RMASS(I-6)
              MFIN2(I)=ZERO
            ENDIF
          ENDDO
          IF (IFL.EQ.164) THEN
C---J/Psi
            MFIN1(7)=RMASS(164)
            MFIN2(7)=ZERO
          ENDIF
C---y boundaries for different flavours and processes
          DO 100 I=IMIN,IMAX
            IF (INCLUD(I)) THEN
              MF1=MFIN1(I)
              MF2=MFIN2(I)
              MREMIN=MREMIF(I)
              SHMIN = MF1**2+MF2**2 + TWO * PTMIN**2 +
     +              TWO * SQRT(PTMIN**2+MF1**2) * SQRT(PTMIN**2+MF2**2)
              EMMIF(I) = MAX(EMMIN,SQRT(SHMIN))
              EMMAF(I) = MIN(EMMAX,RS-ML-MREMIN)
              IF (EMMIF(I).GT.EMMAF(I)) THEN
                INCLUD(I)=.FALSE.
                CALL HWWARN('HWHBRN',3,*999)
                GOTO 100
              ENDIF
              WMIF(I) = EMMIF(I)+MREMIF(I)
              WMIN = WMIF(I)
              PLMAX = HWUPCM(RS,ML,WMIN)
              YMIF(I)=ONE-TWO*(SQRT(PINC**2+MP**2)*SQRT(PLMAX**2+ML**2)+
     +                         PINC*PLMAX)/SMA
              IF (YMIF(I).GT.YMAX) THEN
                INCLUD(I)=.FALSE.
                CALL HWWARN('HWHBRN',4,*999)
                GOTO 100
              ENDIF
            ENDIF
 100      CONTINUE
C---considering the largest boundaries
          EMLMIN=EMMIF(IMIN)
          EMLMAX=EMMAF(IMIN)
          IF (IPROO.EQ.3) THEN
            EMLMIN=MIN(EMMIF(IMIN),EMMIF(IMIN+6))
            EMLMAX=MAX(EMMAF(IMIN),EMMAF(IMIN+6))
          ENDIF
          DEBUG=3
          IF (EMLMIN.GT.EMLMAX) GOTO 888
          YMIN=YMIF(IMIN)
          IF (IPROO.EQ.3) YMIN=MIN(YMIF(IMIN),YMIF(IMIN+6))
          YMIN = MAX(YMIN,YBMIN)
          DEBUG=4
          IF (YMIN.GT.YMAX) GOTO 888
          WMIN = WMIF(IMIN)
          MREMIN = MREMIF(IMIN)
          MF1=MFIN1(IMIN)
          MF2=MFIN2(IMIN)
          IF (IPROO.EQ.3) THEN
            WMIN = MIN(WMIF(IMIN),WMIF(IMIN+6))
            MREMIN = MIN(MREMIF(IMIN),MREMIF(IMIN+6))
          ENDIF
        ENDIF
      ENDIF
C---Random generation in largest phase space
      Y=ZERO
      Q2=ZERO
      SHAT=ZERO
      Z=ZERO
      PHI=ZERO
      AJACOB=ZERO
C---y generation
      IF (.NOT.CHARGD) THEN
        IF (IFL.LE.5.OR.(IFL.GE.7.AND.IFL.LE.18)) THEN
          SRY0 = SQRT(YMIN)
          SRY1 = SQRT(YMAX)
          SRY = HWRUNI(0,SRY0,SRY1)
          Y = SRY**2
          YJAC = TWO*SRY*(SRY1-SRY0)
        ELSEIF (IFL.EQ.6) THEN
          Y = SQRT(HWRUNI(0,YMIN**2,YMAX**2))
          YJAC = HALF * (YMAX**2-YMIN**2) / Y
        ELSEIF (IFL.EQ.164) THEN
C---in J/psi photoproduction Y and Q2 are given by the Equivalent Photon
C   Approximation
   10     NTRY=0
   20     NTRY=NTRY+1
          IF (NTRY.GT.NETRY) CALL HWWARN('HWHBRN',50,*10)
          Y = (YMIN/YMAX)**HWRGEN(1)*YMAX
          IF (ONE+(ONE-Y)**2.LT.TWO*HWRGEN(2)) GOTO 20
          YJAC=(TWO*LOG(YMAX/YMIN)-TWO*(YMAX-YMIN)
     &                            +HALF*(YMAX**2-YMIN**2))
        ENDIF
      ELSE
        IF (IPRO.EQ.5) THEN
          Y = EXP(HWRUNI(0,LOG(YMIN),LOG(YMAX)))
          YJAC = Y * LOG(YMAX/YMIN)
        ELSE
          Y = HWRUNI(0,YMIN,YMAX)
          YJAC = YMAX - YMIN
        ENDIF
      ENDIF
C---Q**2 generation
      Q2INF = ME**2*Y**2 / (ONE-Y)
      Q2SUP = MP**2 + SMA*Y - WMIN**2
      IF (IFL.EQ.164) THEN
        Q2INF = MAX(Q2INF,Q2WWMN)
        Q2SUP = MIN(Q2SUP,Q2WWMX)
      ELSE
        Q2INF = MAX(Q2INF,Q2MIN)
        Q2SUP = MIN(Q2SUP,Q2MAX)
      ENDIF
      DEBUG=5
      IF (Q2INF .GT. Q2SUP) GOTO 888
C
      IF (.NOT.CHARGD) THEN
        IF (IFL.EQ.164) THEN
          Q2 = EXP(HWRUNI(0,LOG(Q2INF),LOG(Q2SUP)))
          Q2JAC = LOG(Q2SUP/Q2INF)
        ELSEIF (Q2INF.LT.RMASS(4)**2) THEN
          Q2 = EXP(HWRUNI(0,LOG(Q2INF),LOG(Q2SUP)))
          Q2JAC = Q2 * LOG(Q2SUP/Q2INF)
        ELSE
          Q2 = Q2INF*Q2SUP/HWRUNI(0,Q2INF,Q2SUP)
          Q2JAC = Q2**2 * (Q2SUP-Q2INF)/(Q2SUP*Q2INF)
        ENDIF
      ELSE
        EMW2=EMW**2
        Q2=(Q2INF+EMW2)*(Q2SUP+EMW2)/(HWRUNI(0,Q2INF,Q2SUP)+EMW2)-EMW2
        Q2JAC=(Q2+EMW2)**2*(Q2SUP-Q2INF)/((Q2SUP+EMW2)*(Q2INF+EMW2))
      ENDIF
      W2 = MP**2 + SMA*Y - Q2
C---s_hat generation
      SHINF = EMLMIN **2
      SHSUP = (MIN(SQRT(W2)-MREMIN,EMLMAX))**2
      DEBUG=6
      IF (SHINF .GT. SHSUP) GOTO 888
C
      IF (IPRO.EQ.91) THEN
        IF (.NOT.CHARGD) THEN
          SHAT = SHINF*SHSUP/HWRUNI(0,SHINF,SHSUP)
          SHJAC = SHAT**2 * (SHSUP-SHINF)/(SHSUP*SHINF)
        ELSE
          SHAT = EXP(HWRUNI(0,LOG(SHINF),LOG(SHSUP)))
          SHJAC = SHAT*(LOG(SHSUP/SHINF))
        ENDIF
      ELSE
        EMW2=EMW**2
        IF (SHINF.GT.EMW2+10*GAMW*EMW) THEN
          SHAT = SHINF*SHSUP/HWRUNI(0,SHINF,SHSUP)
          SHJAC = SHAT**2 * (SHSUP-SHINF)/(SHSUP*SHINF)
        ELSEIF (SHSUP.LT.EMW2-10*EMW*GAMW) THEN
          SHAT = HWRUNI(0,SHINF,SHSUP)
          SHJAC = SHSUP-SHINF
        ELSE
          TMIN=ATAN((SHINF-EMW2)/(GAMW*EMW))
          TMAX=ATAN((SHSUP-EMW2)/(GAMW*EMW))
          SHAT = GAMW*EMW*TAN(HWRUNI(0,TMIN,TMAX))+EMW2
          SHJAC=((SHAT-EMW2)**2+(GAMW*EMW)**2)/(GAMW*EMW)*(TMAX-TMIN)
        ENDIF
      ENDIF
      DETDSH = ONE/SMA/Y
      SHJAC=SHJAC*DETDSH
      RSHAT = SQRT (SHAT)
C--- z generation
      ZMIN = 10E10
      ZMAX = -ONE
      IF (.NOT.CHARGD) THEN
        DO I=1,18
          Q1CM(I) = ZERO
          ZMIF(I) = ZERO
          ZMAF(I) = ZERO
        ENDDO
        DO 150 I=IMIN,IMAX
          IF (INCLUD(I)) THEN
            Q1CM(I) = HWUPCM( RSHAT, MFIN1(I), MFIN2(I) )
            IF (Q1CM(I) .LT. PTMIN) THEN
              ZMAF(I)=-ONE
              GOTO 150
            ENDIF
            CTHLIM = SQRT(ONE - (PTMIN / Q1CM(I))**2)
            GAMMA2 = SHAT + MFIN1(I)**2 - MFIN2(I)**2
            LAMBDA = (SHAT-MFIN1(I)**2-MFIN2(I)**2)**2 -
     +                4.D0*MFIN1(I)**2*MFIN2(I)**2
            ZMIF(I) = (GAMMA2 - SQRT(LAMBDA)*CTHLIM)/TWO/SHAT
            ZMIF(I) = MAX(ZMIF(I),ZERO)
            ZMAF(I) = (GAMMA2 + SQRT(LAMBDA)*CTHLIM)/TWO/SHAT
            ZMAF(I) = MIN(ZMAF(I),ONE)
            ZMIN = MIN( ZMIN, ZMIF(I) )
            ZMAX = MAX( ZMAX, ZMAF(I) )
          ENDIF
 150    CONTINUE
        IF (IFL.EQ.164) ZMAX=MIN(ZMAX,ZJMAX)
      ELSE
        Q1 = HWUPCM(RSHAT,MF1,MF2)
        DEBUG=7
        IF (Q1.LT.PTMIN) GOTO 888
        CTHLIM = SQRT(ONE-(PTMIN/Q1)**2)
        GAMMA2 = SHAT+MF1**2-MF2**2
        LAMBDA = (SHAT-MF1**2-MF2**2)**2-4.D0*MF1**2*MF2**2
        ZMIN = (GAMMA2-SQRT(LAMBDA)*CTHLIM)/TWO/SHAT
        ZMIN = MAX(ZMIN,1D-6)
        ZMAX = (GAMMA2+SQRT(LAMBDA)*CTHLIM)/TWO/SHAT
        ZMAX = MIN(ZMAX,ONE-1D-6)
      ENDIF
      DEBUG=8
      IF (ZMIN .GT. ZMAX) GOTO 888
      ZLMIN = LOG(ZMIN/(ONE-ZMIN))
      ZINT = LOG(ZMAX/(ONE-ZMAX)) - LOG(ZMIN/(ONE-ZMIN))
      ZL = ZLMIN+HWRGEN(0)*ZINT
      Z = EXP(ZL)/(ONE+EXP(ZL))
      ZJAC = Z*(ONE-Z)*ZINT
C
      DEBUG=9
      IF ((Y.LT.YMIN.OR.Y.GT.YMAX).OR.(Q2.LT.Q2INF.OR.Q2.GT.Q2SUP).OR.
     +   (SHAT.LT.SHINF.OR.SHAT.GT.SHSUP).OR.(Z.LT.ZMIN.OR.Z.GT.ZMAX))
     +     GOTO 888
C---Phi generation
      PHI = HWRUNI(0,ZERO,2*PIFAC)
      PHIJAC = 2 * PIFAC
      IF (IFL.EQ.164) PHIJAC=ONE
C
      AJACOB = YJAC * Q2JAC * SHJAC * ZJAC * PHIJAC
C
      IF (IQK.NE.0.OR.IPRO.EQ.5) GOTO 999
C---contributing subprocesses: filling of logical vector INSIDE
      DO I=1,18
        INSIDE(I)=.FALSE.
        Q2MAF(I)=ZERO
        EMMAWF(I)=ZERO
      ENDDO
      DO 200 I=IMIN,IMAX
      IF (INCLUD(I)) THEN
      IF ( Y.LT.YMIF(I) ) GOTO 200
C
      Q2MAF(I) = MP**2 + SMA*Y - WMIF(I)**2
      Q2MAF(I) = MIN( Q2MAF(I), Q2MAX)
      IF (Q2INF .GT. Q2MAF(I)) GOTO 200
      IF (Q2.LT.Q2INF .OR. Q2.GT.Q2MAF(I)) GOTO 200
C
      EMMAWF(I) = SQRT(W2) - MREMIF(I)
      EMMAWF(I) = MIN( EMMAWF(I), EMLMAX )
C
      IF (EMMIF(I) .GT. EMMAWF(I)) GOTO 200
      IF (SHAT.LT.EMMIF(I)**2.OR.SHAT.GT.EMMAWF(I)**2) GOTO 200
C
      IF (ZMIF(I) .GT. ZMAF(I)) GOTO 200
      IF (Z.LT.ZMIF(I) .OR. Z.GT.ZMAF(I)) GOTO 200
      INSIDE(I)=.TRUE.
      ENDIF
 200  CONTINUE
 999  RETURN
 888  EVWGT=ZERO
C---UNCOMMENT THIS LINE TO GET A DEBUGGING WARNING FOR NO PHASE-SPACE
C      CALL HWWARN('HWHBRN',DEBUG,*777)
 777  RETURN 1
      END
