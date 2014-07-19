CDECK  ID>, HWHBSG.
*CMZ :-        -03/07/95  19.02.12  by  Giovanni Abbiendi
*-- Author :    Giovanni Abbiendi & Luca Stanco
C----------------------------------------------------------------------
      SUBROUTINE HWHBSG
C----------------------------------------------------------------------
C     Returns differential cross section DSIGMA in (Y,Q2,ETA,Z,PHI)
C     Scale for structure functions and alpha_s selected by BGSHAT
C----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWUALF,HWUAEM,LEP,Y,Q2,SHAT,Z,PHI,AJACOB,DSIGMA,
     & ME,MP,ML,MREMIF(18),MFIN1(18),MFIN2(18),RS,SMA,W2,RSHAT,
     & SFUN(13),ALPHA,LDSIG,DLQ(7),SG,XG,MF1,MF2,MSUM,MDIF,MPRO,FFUN,
     & GFUN,H43,H41,H11,H12,H14,H16,H21,H22,G11,G12,G1A,G1B,G21,G22,G3,
     & GC,A11,A12,A44,ALPHAS,PDENS,AFACT,BFACT,CFACT,DFACT,GAMMA,S,T,U,
     & MREMIN,POL,CCOL,ETA
      INTEGER IQK,IFLAVU,IFLAVD,IMIN,IMAX,IFL,IPROO,IHAD,ILEPT,IQ,IS
      LOGICAL CHARGD,INCLUD(18),INSIDE(18)
      EXTERNAL HWUALF,HWUAEM
      COMMON /HWAREA/ LEP,Y,Q2,SHAT,Z,PHI,AJACOB,DSIGMA,ME,MP,ML,MREMIF,
     & MFIN1,MFIN2,RS,SMA,W2,RSHAT,IQK,IFLAVU,IFLAVD,IMIN,IMAX,IFL,
     & IPROO,CHARGD,INCLUD,INSIDE
C
      IHAD=2
      IF (JDAHEP(1,IHAD).NE.0) IHAD=JDAHEP(1,IHAD)
C---set masses
      IF (CHARGD) THEN
        MREMIN=MP
        IF (LEP.EQ.ONE) THEN
          MF1=RMASS(IFLAVD)
          MF2=RMASS(IFLAVU)
        ELSE
          MF1=RMASS(IFLAVU)
          MF2=RMASS(IFLAVD)
        ENDIF
      ELSE
        IS=IFL
        IF (IFL.EQ.164) IS=IQK
        MREMIN = MREMIF(IS)
        MF1 = MFIN1(IS)
        MF2 = MFIN2(IS)
      ENDIF
C---choose subprocess scale
      IF (BGSHAT) THEN
        EMSCA = RSHAT
      ELSE
        S=SHAT+Q2
        IF (IFL.GE.7.AND.IFL.LE.18) S=SHAT+Q2-MF1**2
        T=-S*Z
        U=-S-T
        IF (IFL.GE.7.AND.IFL.LE.18) U=-S-T-2*MF1**2
        EMSCA = SQRT(TWO*S*T*U/(S**2+T**2+U**2))
        IF (IFL.EQ.164) EMSCA=SQRT(-U)
      ENDIF
      ALPHAS = HWUALF(1,EMSCA)
      IF (ALPHAS.GE.ONE.OR.ALPHAS.LE.ZERO) CALL HWWARN('HWHBSG',51,*888)
C---structure functions
      ETA = (SHAT+Q2)/SMA/Y
      IF (ETA.GT.ONE) ETA=ONE
      CALL HWSFUN (ETA,EMSCA,IDHW(IHAD),NSTRU,SFUN,2)
      XG = Q2/(SHAT + Q2)
      SG = ETA*SMA
      IF (SG.LE.(RSHAT+ML)**2.OR.SG.GE.(RS-MREMIN)**2) GOTO 888
C
      IF (IFL.EQ.164) GOTO 200
C
C---Electroweak couplings
      ALPHA=HWUAEM(-Q2)
      IF (CHARGD) THEN
        POL = PPOLN(3) - EPOLN(3)
        DLQ(1)=.0625*VCKM(IFLAVU/2,(IFLAVD+1)/2)/SWEIN**2 *
     +         Q2**2/((Q2+RMASS(198)**2)**2+(RMASS(198)*GAMW)**2) *
     +         (ONE + POL)
        DLQ(2)=ZERO
        DLQ(3)=DLQ(1)
      ELSE
        IQ=MOD(IFL-1,6)+1
        ILEPT=MOD(IDHW(1)-121,6)+11
        CALL HWUCFF(ILEPT,IQ,-Q2,DLQ(1))
      ENDIF
C
      IF (IFL.LE.6) THEN
C---For Boson-Gluon Fusion
        PDENS = SFUN(13)/ETA
        CCOL = HALF
        MSUM = (MF1**2 + MF2**2) / (Y*SG)
        MDIF = (MF1**2 - MF2**2) / (Y*SG)
        MPRO = MF1*MF2 / (Y*SG)
C
        FFUN = (1.D0-XG)*Z*(1.D0-Z) + (MDIF*(2.D0*Z-1.D0)-MSUM)/2.D0
        GFUN = (1.D0-XG)*(1.D0-Z) + XG*Z + MDIF
        IF ( FFUN .LT. ZERO ) FFUN = ZERO
        H43 = (8.D0*(2.D0*Z**2*XG-Z**2-2.D0*Z*XG+2.D0*Z*MDIF+Z-MDIF
     &         -MSUM)) / (Z*(1.D0-Z))**2
C
        H41 = (8.D0*(Z**2-Z*XG+Z*MDIF-MDIF-MSUM)) / (Z**2*(1.D0-Z))
C
        H11 = (4.D0*(2.D0*Z**4-4.D0*Z**3+2.D0*Z**2*MSUM*XG
     &         -2.D0*Z**2*MSUM+2.D0*Z**2*XG**2-2.D0*Z**2*XG+3.D0*Z**2
     &         +2.D0*Z*MDIF*MSUM+2.D0*Z*MDIF*XG-2.D0*Z*MSUM*XG
     &         +2.D0*Z*MSUM-2.D0*Z*XG**2+2.D0*Z*XG-Z-MDIF*MSUM-MDIF*XG
     &         -MSUM**2-MSUM*XG)) / (Z*(1.D0-Z))**2
C
        H12 = (16.D0*(-Z*MDIF+Z*XG+MDIF+MSUM))/(Z**2*(1.D0-Z))
C
        H14 = (16.D0*(-2.D0*Z**2*XG-2.D0*Z*MDIF+2.D0*Z*XG+MDIF+MSUM))
     &        / (Z*(1.D0-Z))**2
C
        H16 = (32.D0*(Z*MDIF-Z*XG-MDIF-MSUM)) / (Z**2*(1.D0-Z))
C
        H21 = (8.D0*MPRO*(-2.D0*Z**2*XG+2.D0*Z**2-2.D0*Z*MDIF+2.D0*Z*XG
     +         -2.D0*Z+MDIF+MSUM)) / (Z*(1.D0-Z))**2
C
        H22 = (-32.D0*MPRO) / (Z*(1.D0-Z))
C
        G11 = -2.D0*H11 + FFUN*H14
        G12 = 2.D0*XG*FFUN*H14 + H12 + GFUN * ( H16+GFUN*H14 )
        G1A = SQRT( XG*FFUN ) * ( H16 + 2.D0*GFUN*H14 )
        G1B = FFUN*H14
        G21 = -2.D0*H21
        G22 = H22
        G3  = H41 - GFUN*H43
        GC  = SQRT( XG*FFUN ) * (-2.D0*XG*H43 )
      ELSE
C---for QCD Compton, massless matrix element
        PDENS = SFUN(IFL-6)/ETA
        CCOL = CFFAC
        FFUN = XG*(ONE-XG)*Z*(ONE-Z)
        GFUN = (ONE-XG)*(ONE-Z)+XG*Z
        G11 = 8.D0*((Z**2+XG**2)/(ONE-XG)/(ONE-Z)+TWO*(XG*Z+ONE))
        G12 = 64.D0*XG**2*Z+TWO*XG*G11
        G1A = 32.D0*XG*GFUN*SQRT(FFUN)/((ONE-XG)*(ONE-Z))
        G1B = 16.D0*XG*Z
        G3  = -16.D0*(ONE-XG)*(ONE-Z)+G11
        GC  = -16.D0*XG*SQRT(FFUN)*(ONE-Z-XG)/((ONE-XG)*(ONE-Z))
        G21 = ZERO
        G22 = ZERO
      ENDIF
C
      A11 = XG * Y**2 * G11  +  (1.D0-Y) * G12
     &      - (2.D0-Y) * SQRT( 1.D0-Y ) * G1A  *  COS( PHI )
     &      + 2.D0 * XG * (1.D0-Y) * G1B  *  COS( 2.D0*PHI )
C
      A12 = XG * Y**2 * G21  +  (1.D0-Y) * G22
C
      A44 = XG * Y * (2.D0-Y) * G3
     &      - 2.D0 * Y * SQRT( 1.D0-Y ) * GC  *  COS( PHI )
C
      IF ( Y*Q2**2 .LT. 1D-38 ) THEN
C---prevent numerical uncertainties in DSIGMA computation
        DSIGMA = PDENS*ALPHA**2*ALPHAS*GEV2NB*CCOL/(16.D0*PIFAC)
     &           *(DLQ(1)*A11 + DLQ(2)*A12 + LEP*DLQ(3)*A44)
        IF ( DSIGMA .LE. ZERO ) GOTO 888
        LDSIG = LOG (DSIGMA) - LOG (Y) - 2.D0 * LOG (Q2)
        DSIGMA = EXP (LDSIG)
      ELSE
        DSIGMA = PDENS*ALPHA**2*ALPHAS*GEV2NB*CCOL
     &         * (DLQ(1)*A11 + DLQ(2)*A12 + LEP*DLQ(3)*A44)
     &         / (16.D0*PIFAC*Y*Q2**2)
      ENDIF
      IF (DSIGMA.LT.ZERO) GOTO 888
      RETURN
C
  200 CONTINUE
C--- J/psi production
      ALPHA = HWUAEM(-Q2)
      GAMMA = 4.8D-6
      PDENS = SFUN(13)/ETA
      AFACT = (8.D0*PIFAC*ALPHAS**2*RMASS(164)**3*GAMMA)/(3.D0*ALPHA)
      BFACT = ONE/(Y*SG*Z**2*((Z-ONE)*Y*SG-RMASS(164)**2)**2)
      CFACT = (RMASS(164)**2-Z*Y*SG)**2/(Y*SG*(ONE-XG)**2*
     &        ((ONE-XG)*Y*SG-RMASS(164)**2)**2*
     &        ((Z-ONE)*Y*SG-RMASS(164)**2)**2)
      DFACT = ((Z-ONE)*Y*SG)**2/(Y*SG*(ONE-XG)**2*
     &          ((ONE-XG)*Y*SG-RMASS(164)**2)**2*(Z*Y*SG)**2)
      DSIGMA = GEV2NB*ALPHA/(TWO*PIFAC)*AFACT*(BFACT+CFACT+DFACT)*PDENS
      IF (DSIGMA.LT.ZERO ) GOTO 888
      RETURN
 888  DSIGMA=ZERO
      END
