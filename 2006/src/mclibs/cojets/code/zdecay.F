*
* $Id: zdecay.F,v 1.1.1.1 1996/01/11 14:14:44 mclareni Exp $
*
* $Log: zdecay.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:44  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE ZDECAY(QP,QM,QK)
C     ***************************
C------------------------------------------------------------
C LEPTONIC DECAY OF Z0 BOSON PRODUCED FROM Q QBAR PAIR
C
C     AUTHORS    F.A.BERENDS AND R.KLEISS
C
C    INFORMATION ON THIS PROGRAM IS FOUND IN
C    F.A. BERENDS AND R. KLEISS, "HARD PHOTON CORRECTIONS IN
C    W AND Z DECAY ", LEIDEN UNIVERSITY PREPRINT (1983).
C    PLEASE REFER TO THIS PAPER WHEN USING THIS PROGRAM |
C
C
C PARAMETERS: S (CM ENERGY OF LEPTON PAIR SQUARED IN GEV**2);
C             XM (LEPTON MASS IN GEV)
C             CVL,CAL (LEPTON VECTOR/AXIAL VECTOR COUPLING)
C             CVQ,CAQ (SAME FOR QUARK)
C             QQ (QUARK ELECTRIC CHARGE)
C             XMZ,XGZ (Z0 MASS AND WIDTH IN GEV)
C THESE PARAMETERS MUST BE SUPPLIED IN THE COMMON 'PARRAM'
C FOR THE CONVENTIONS OF THE COUPLING CONSTANTS, SEE
C F.A.BERENDS,R.KLEISS AND S.JADACH,
C NUCL.PHYS.B202(1982)63-88,TABLE ON PAGE 83.
C THIS GIVES THE CONVENTIONS IN THE GLASHOW-WEINBERG-SALAM
C 'STANDARD MODEL'
C-----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C--
C-- Q0 IS THE ENERGY CUTOFF
C--
#if defined(CERNLIB_SINGLE)
      REAL CJRN,P,PJTOT,W,WL
#endif
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION CJRN,P,PJTOT,W,WL
#endif
#include "cojets/itapes.inc"
#include "cojets/parram.inc"
#include "cojets/photon.inc"
      DIMENSION QP(4),QM(4),QK(4)
      DATA INITI /0/
      IF(INITI.NE.0) GOTO 1000
C----------------------------------- INITIALIZATION STEP ---
      INITI=1
      WCUMUL=0.
      ICUMUL=0
      ONE = 1.
      EB=.5*SQRT(S)
      XM2=(XM/EB)**2
      BARN=3.8938D+05/EB**2
C------------------------------- XK0=ENERGY CUTOFF / BEAM ENERGY
      XK0=Q0/EB
      PIG=4.*ATAN(ONE)
      A0=(CVL**2+CAL**2)*(CVQ**2+CAQ**2)
      B0=8.*CVL*CAL*CVQ*CAQ
      FCMAX=2.*A0+ABS(B0)
      Z2=16.*((1.-XMZ**2/S)**2 + (XMZ*XGZ/S)**2)
C TOTAL LOWEST ORDER CROSS SECTION
      SIG0=16.*ALFA**2*PIG*QQ**2*A0/Z2*BARN/3.D0
C TOTAL WITH SOFT + VIRTUAL CORRECTIONS
      XLS=LOG(4./XM2)
      SIGS=SIG0*( 1. + 2.*ALFA/PIG*
     . ( LOG(XK0)*(XLS-1.) + 3./4.*XLS + PIG**2/6. - 1.) )
C TOTAL APPROXIMATE HARD CROSS SECTION
      SIGH=SIG0* 2.*ALFA/PIG*( -LOG(XK0)*XLS - PIG**2/6. )
C CORRECTIONS
      DELS=(SIGS/SIG0-1.)*100.
      DELH=(SIGH/SIG0   )*100.
      DELT=DELS+DELH
C FRACTION OF HARD EVENTS IN TRIAL SAMPLE
      FRHRD=DELH/(100.+DELT)
      WMAX=1.05D0
C     WRITE(ITLIS,1) EB,XM2,BARN,XK0,A0,B0,FCMAX,Z2,FRHRD,WMAX,
C    . S,XM,CVL,CAL,CVQ,CAQ,QQ,XMZ,XGZ,
C    . SIG0,SIGS,SIGH,DELS,DELH,DELT
    1 FORMAT(1H0,96(1H-),/,
     . 12X,30H THIS IS ROUTINE ''ZDECAY'' | ,/,
     . 1X,8D12.4,/,1X,2F12.4,/,
     . 1X,27HCENTRE OF MASS ENERGY       ,D12.4,7H GEV**2 ,/,
     . 1X,27HMASS OF LEPTON              ,D12.4,4H GEV ,/,
     . 1X,27HLEPTON COUPLING: VECTOR     ,D12.4,/,
     . 1X,27H                 AXIAL V.   ,D12.4,/,
     . 1X,27HQUARK COUPLING : VECTOR     ,D12.4,/,
     . 1X,27H                 AXIAL V.   ,D12.4,/,
     . 1X,27HQUARK E.M. CHARGE           ,D12.4,/,
     . 1X,27HZ0 BOSON : MASS             ,D12.4,4H GEV ,/,
     . 1X,27H           WIDTH            ,D12.4,4H GEV ,/,
     . 1X,27HLOWEST ORDER CROSS SECTION  ,D12.4,9H NANOBARN ,/,
     . 1X,27HWITH SOFT + VIRTUAL CORR.   ,D12.4,9H NANOBARN ,/,
     . 1X,27HAPPROXIMATE HARD CONTRIB.   ,D12.4,9H NANOBARN ,/,
     . 1X,27HSOFT PHOTON CORRECTION      ,F12.4,8H PERCENT ,/,
     . 1X,27HHARD APPROX.CORRECTION      ,F12.4,8H PERCENT ,/,
     . 1X,27HTOTAL CORRECTION (APPR.)    ,F12.4,8H PERCENT )
C--------------------------------------------------------------
 1000 CONTINUE
C------------------------ START OF EVENT GENERATION STEP ------
C GENERATE VALUE OF QP SOLID ANGLE (BOTH HARD OR SOFT)
    2 C= -1. + 2.*CJRN(1.)
      WC= (A0*(1.+C*C)+B0*C)/FCMAX
      IF(WC.LT.CJRN(2.)) GOTO 2
      SC=SQRT(1.-C*C)
      FC=2.*PIG*CJRN(3.)
      CFC=COS(FC)
      SFC=SIN(FC)
C------------------------ CHOSE BETWEEN HARD AND SOFT PART ----
      IF(CJRN(4.).GT.FRHRD) GOTO 2000
C------------------------ HARD PART CHOSEN -----------------------
C ADD TO EVENT COUNTER FOR FINDING REAL CROSS SECTION
      ICUMUL=ICUMUL+1
C GENERATE VALUE FOR K
    3 XK=XK0**CJRN(5.)
      WK=1. + LOG(1.-XK)/XLS
      IF(WK.LT.CJRN(6.)) GOTO 3
C GENERATE VALUE FOR FG
      FG=2.*PIG*CJRN(7.)
      CFG=COS(FG)
      SFG=SIN(FG)
C GENERATE VALUE FOR Y1
      YMIN=XM2*XK/(2.*(1.-XK))
      YMAX=2.*XK+YMIN
      Y1=YMIN* (YMAX/YMIN)**CJRN(8.)
      IF(CJRN(9.).GT.0.5D0) Y1=YMAX-Y1
C CONSTRUCT MOMENTA AND CHECK ON KINEMATICS BY MASS SHELL
      X=1.-XK+.5*(Y1-YMIN)
      CG=(2.*(1.-XK)/X-2.+XK)/XK
      SG=(1.-CG*CG)
      IF(SG.LT.0.D0) CG=CG/ABS(CG)
      SG=SQRT(1.-CG*CG)
      SG=DSQRT(1.-CG*CG)
C QK VECTOR
      QK(4)=XK
      QK(3)=XK*(C*CG-SC*SG*CFG)
      QK(2)=XK*((C*SG*CFG + SC*CG)*SFC + SG*SFG*CFC)
      QK(1)=XK*((C*SG*CFG + SC*CG)*CFC - SG*SFG*SFC)
C QP VECTOR
      QP(4)=X
      QP(3)=X*C
      QP(2)=X*SC*SFC
      QP(1)=X*SC*CFC
C QM VECTOR
      DO 4 I=1,4
    4 QM(I)=-QP(I)-QK(I)
      QM(4)=2.+QM(4)
C CHECK ON MASSES
      XMK=QK(4)**2-QK(3)**2-QK(2)**2-QK(1)**2
      XMP=QP(4)**2-QP(3)**2-QP(2)**2-QP(1)**2
      XMM=QM(4)**2-QM(3)**2-QM(2)**2-QM(1)**2
C----------------------------------- CALCULATE WEIGHT ----
      C1=-QM(3)/QM(4)
      Y2=YMAX-Y1
      WW=(QM(4)**2*(A0*(1.+C1*C1) + B0*C1)*(1. - XM2*Y2/(2.*Y1) )+
     .    QP(4)**2*(A0*(1.+C*C  ) + B0*C )*(1. - XM2*Y1/(2.*Y2) ))
     . /(2.*(A0*(1.+C*C)+B0*C) )
C----------------------------------- REJECTION POINT -----
C CUMULATE WEIGHTS
      WCUMUL=WCUMUL+WW
      WSOFAR=WCUMUL/ICUMUL
      DSOFAR=WSOFAR*DELH
      IF(CJRN(10.).GT.(WW/WMAX)) GOTO 1000
      GOTO 3000
C------------------------------------------------- SOFT PART CHOSEN ----
C APPLY IMMEDIATE REJECTION
 2000 IF(CJRN(11.).GT.(1./WMAX)) GOTO 1000
      QP(4)=1.
      QP(3)=C
      QP(2)=SC*SFC
      QP(1)=SC*CFC
      QM(4)=1.
      DO 5 I=1,3
    5 QM(I)=-QP(I)
      DO 6 I=1,4
    6 QK(I)=0.D0
C----------------------------------- SCALE MOMENTA -------
 3000 DO 7 I=1,4
      QP(I)=EB*QP(I)
      QM(I)=EB*QM(I)
    7 QK(I)=EB*QK(I)
      RETURN
      END
