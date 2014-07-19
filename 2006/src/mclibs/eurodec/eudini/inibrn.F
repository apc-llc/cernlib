*
* $Id: inibrn.F,v 1.1.1.1 1996/03/08 16:58:51 mclareni Exp $
*
* $Log: inibrn.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:51  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE INIBRN
C.----------------------------------------------------------------------
C.
C.    THIS SUBROUTINE DETERMINES AND PRINTS THE BRANCHING FRACTIONS AND
C.    DECAY MODES OF PARTICLES (H0, H+/-, Z0 W+/-, SIGMA, T, L, H ETC.)
C.    THAT DO NOT HAVE THE DEFAULT SETTINGS. THE PARTICLE MASSES SET IN
C.    THE GENERATOR ARE INTERFACED. PARTICLE MASSES AND WIDTHS ARE
C.    ADJUSTED AS WELL. COUPLINGS, VERTEX AND MIXING PARAMETERS ARE SET.
C.    LAST UPDATE: 12/09/89
C.
C.----------------------------------------------------------------------
#include "eurodec/eudopt.inc"
#include "eurodec/hvyini.inc"
#include "eurodec/wekpar.inc"
#include "eurodec/boscop.inc"
#include "eurodec/convrt.inc"
#include "eurodec/ptable.inc"
#include "eurodec/pwidth.inc"
#include "eurodec/ratmix.inc"
#include "eurodec/inivtx.inc"
#include "eurodec/picons.inc"
      DIMENSION ICOD(NMMAX),AMINI(NMMAX),IMOD(NMMAX),IIMOD(NMMAX,NMMAX)
      DATA ICOD/ 6, 7, 8, 97, 191, 192, 198, 199,1001,1002,1003,1004/
      DATA AMINI/ 60., 100., 160., 40, 9.6, 23., 82., 93., 10., 20, 80,
     &200./
      DATA IIMOD/ 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0,
     &            0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1,
     &            0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1,
     &            0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1,
     &            0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
     &            0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
     &            0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
     &            0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0,
     &            0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
     &            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
     &            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
     &            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1/
      DATA IMOD/ NMMAX*0/
      DATA  CHBAR,AMEPS/ 6.582173E-25, 1.E-5/
C--
C--   CALCULATE VECTOR AND AXIAL Z0 CURRENT CONTRIBUTIONS
      DO 10 I=1,16
        IF ((I.EQ.1).OR.(I.EQ.4).OR.(I.EQ.6).OR.(I.EQ.8)) THEN
          VZBOS(I)=1.-4.*EFERM(I)*XW
          AZBOS(I)=1.
        ELSEIF ((I.EQ.2).OR.(I.EQ.3).OR.(I.EQ.5).OR.(I.EQ.7)) THEN
          VZBOS(I)=-1.-4.*EFERM(I)*XW
          AZBOS(I)=-1.
        ELSE
          IS=I-8
          IF ((IS.EQ.1).OR.(IS.EQ.3).OR.(IS.EQ.5).OR.(IS.EQ.7)) THEN
            VZBOS(I)=-1.-4.*EFERM(I)*XW
            AZBOS(I)=-1.
          ELSE
            VZBOS(I)=1.
            AZBOS(I)=1.
          ENDIF
        ENDIF
   10 CONTINUE
C--
C--   MASS MODIFICATIONS MADE FROM THE GENERATOR OVERRULE THE MASS
C--   MODIFICATIONS FROM THE TITLE FILE/DATA CARDS ONLY WHEN THEY
C--   DIFFER FROM THE DEFAULT SETTINGS.
      DO 20 I=1,NMMAX
        J=ICONV(ICOD(I))
        IF (ABS(AMINIE(I)-AMINI(I)).GT.AMEPS) PM(J)=AMINIE(I)
   20 CONTINUE
C--
C--   INITIALIZE TOPONIUM WIDTHS, WAVE FUNCTIONS ETC.
      DUMMY=AMONI(0)
C--
C--   IF NOT DEFAULT THEN CHECK PARTICLE MASSES AND MODIFY...
      DO 40 I=1,3
        J=ICONV(ICOD(I))
        IF (ABS(PM(J)-AMINI(I)).LE.AMEPS) GOTO 40
C--
C--   MODIFY MASS OF HEAVY FLAVOUR PARTICLES
        DO 30 K=1,NPA
          ICO=ABS(IPC(K))
          ICO1=ICO/1000
          ICO2=(ICO-1000*ICO1)/100
          ICO3=(ICO-1000*ICO1-100*ICO2)/10
          IF ((ICO1.EQ.ICOD(I)).OR.(ICO2.EQ.ICOD(I))) THEN
            IF ((ICO1.EQ.0).AND.(ICO2.EQ.ICO3)) THEN
              IF (ICO2.EQ.6) THEN
C--
C--   TOPONIUM MASSES
                PM(K)=AMONI(K)
              ELSE
C--
C--   FOURTH FAMILY ONIA
                PM(K)=PM(K)+2.*(PM(J)-AMINI(I))
              ENDIF
            ELSE
              PM(K)=PM(K)-AMINI(I)+PM(J)
            ENDIF
          ENDIF
   30   CONTINUE
   40 CONTINUE
      AMW=PM(ICONV(198))
      AMZ=PM(ICONV(199))
C--
C--   KEEP TRACK OF DECAY MODES THAT HAVE TO BE MODIFIED (OPTIONAL)
      IF (IDEFDC.NE.1) THEN
        DO 60 I=1,NMMAX
          J=ICONV(ICOD(I))
          IF (ABS(PM(J)-AMINI(I)).LE.AMEPS) GOTO 60
          DO 50 K=1,NMMAX
   50     IMOD(K)=IMOD(K)+IIMOD(K,I)
   60   CONTINUE
C--
C--   MODIFY BRANCHING FRACTIONS...
        DO 80 I=1,NMMAX
          IF (IMOD(I).EQ.0) GOTO 80
          CALL BRFRAC(ICONV(ICOD(I)))
          IF (I.LE.3) THEN
            DO 70 K=1,NPA
              ICO0=ABS(IPC(K))
              ICO1=ICO0/10
              ICO2=ICO1/10
              ICO3=ICO2/10
              IF ((ICO3.EQ.ICOD(I)).OR.(ICO2.EQ.ICOD(I))) THEN
                IF (ICO2.EQ.(ICO1-10*ICO2)) THEN
C--
C--   TOPONIUM BRANCHING RATIOS
                  IF (ICO2.EQ.6) CALL TOPONI(K)
                ELSE
C--
C--   TOP, LOW OR HIGH HADRONS
                  CALL BRFRAC(K)
                ENDIF
              ENDIF
   70       CONTINUE
          ENDIF
C--
C--   SPECIAL: TOPONIUM ---> H0 X
          IF ((I.EQ.5).AND.(IMOD(1).EQ.0).AND.(PM(ICONV(ICOD(I))).NE.
     &    AMINI(I))) THEN
            CALL TOPONI(ICONV(662))
            CALL TOPONI(ICONV(663))
          ENDIF
   80   CONTINUE
      ENDIF
C--
C--   CALCULATE PARTICLE WIDTH AND MASS LIMITS FOR MASS SMEARING
      DO 90 I=1,NPA
        IF ((PLT(I).LE.0.).OR.(PM(I).LE.1.E-10)) GOTO 90
        GAM(I)=CHBAR/PLT(I)
        ALO2=(PM(I)-1.*GAM(I))**2
        AUP2=(PM(I)+1.*GAM(I))**2
        IF ((AUP2-ALO2).LE.0.) GOTO 90
        VMLO(I)=ATAN((ALO2-PM(I)**2)/PM(I)/GAM(I))
        VMUP(I)=ATAN((AUP2-PM(I)**2)/PM(I)/GAM(I))
   90 CONTINUE
      GAMW=GAM(ICONV(198))
      GAMZ=GAM(ICONV(199))
C--
C--   CALCULATE BOSON COUPLINGS
      GW=(GF*AMW**2/SQRT(2.))**2
      GWH0=8.*GF*AMW**4/SQRT(2.)
      GZ=SQRT(GF*SQRT(2.))*AMZ/2.
      GZH0=8.*GF*AMZ**4/SQRT(2.)
      GG=SQRT(4.*PI*ALPHA)
      DO 100 I=1,16
        GZV(I)=VZBOS(I)*GZ
        GZA(I)=AZBOS(I)*GZ
  100 GGAM(I)=EFERM(I)*GG
C--
C--   CALCULATE X FROM R FOR WEAKLY MIXING PARTICLES
      XD0=SQRT(2.*RD0MIX/MAX(1.E-6,(1-RD0MIX)))
      XB0=SQRT(2.*RBDMIX/MAX(1.E-6,(1-RBDMIX)))
      XBS0=SQRT(2.*RBSMIX/MAX(1.E-6,(1-RBSMIX)))
C--
C--   CHECK WHETHER PRIMARY VERTEX HAS TO BE GENERATED
      IF ((VTXSIG(1).GT.0.).OR.(VTXSIG(2).GT.0.).OR.(VTXSIG(3).GT.0.))
     &IPVTX=1
      RETURN
      END
