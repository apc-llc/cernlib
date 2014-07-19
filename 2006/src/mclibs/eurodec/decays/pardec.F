*
* $Id: pardec.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: pardec.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE PARDEC(IP,IPPAR,IPDEC)
C.----------------------------------------------------------------------
C.
C.    ROUTINES TREATS DECAY OF IP-TH PARTICLE IN /RESULT/ AT IPPAR
C.    LOCATION IN PARTICLE TABLE AND AT IPDEC LOCATION IN DECAY TABLE.
C.    DECAY PRODUCTS ARE ADDED TO RESULT AT LOCATION NTEIL+1 ETC.
C.    LAST UPDATE: 14/08/89
C.
C.----------------------------------------------------------------------
#include "eurodec/eudopt.inc"
#include "eurodec/result.inc"
#include "eurodec/ptable.inc"
#include "eurodec/dtable.inc"
#include "eurodec/convrt.inc"
#include "eurodec/ratmix.inc"
#include "eurodec/picons.inc"
#include "eurodec/dforce.inc"
#include "eurodec/inivtx.inc"
#include "eurodec/mixing.inc"
      DIMENSION PHAD(5,6),PSFAC(6),RORD(6),XMUL(3),BET(3),SECVER(3)
      DATA PSFAC/ 1., 1., 2., 5.,15.,60./
      DATA VLIGHT/ 2.99792458E10/
      NCUT=0
C--
C--   SEE WHICH DECAYS (POS. AND/OR NEG. PART. CODE) HAVE TO BE FORCED
      IF (INDEXT(IP).GT.0) THEN
        IFORCE=IFRCP(IPPAR)
      ELSE
        IFORCE=IFRCM(IPPAR)
      ENDIF
      IF (IFORCE.NE.0) THEN
        IPDEC=IFORCE
      ELSE
C--
C--   BRANCH ON DECAY MODES
        BRANCH=EURRAN(IBR)
   10   IF (BRANCH.LE.DBR(IPDEC)) GOTO 20
        IPDEC=IPDEC+1
        GOTO 10
      ENDIF
C--
C--   CHECK ON LENGTH OF /RESULT/
   20 INIT=0
      NHAD=NDP(IPDEC)
      NTPNH=NTEIL+NHAD
      IF (NTPNH.GT.NTMAX) CALL ERRORD(61,' ',FLOAT(NTMAX))
C--
C--   IF SINGLE PARTICLE DECAY, COPY DIRECTLY TO RESULT
      IF (NHAD.EQ.1) THEN
        INDEXT(NTPNH)=IDC(1,IPDEC)
        DO 30 I=1,5
   30   PTEIL(I,NTPNH)=PTEIL(I,IP)
        ITHEL(NTPNH)=ITHEL(IP)
C--
C--   FILL HISTORY INFORMATION: JET=IABS(IORIGT(IP))/10000
        IOR=10000*(IABS(IORIGT(IP))/10000)+IP
        IORIGT(NTPNH)=IOR
        IDCAYT(NTPNH)=0
        IDCAYT(IP)=(NTEIL+1)*10000+NTPNH
C--
C--   CALCULATE 'DECAY' VERTEX
        IF (ISVTX.EQ.1) THEN
          PVEC=(PTEIL(4,IP)-PTEIL(5,IP))*(PTEIL(4,IP)+PTEIL(5,IP))
          IF (PVEC.LE.0.) THEN
            DO 40 J=1,3
   40       SECVTX(J,NTPNH)=SECVTX(J,IP)
          ELSE
            CT=VLIGHT*REALLT(IP)
            DO 50 J=1,3
   50       SECVTX(J,NTPNH)=SECVTX(J,IP)+CT*PTEIL(J,IP)/SQRT(PVEC)
          ENDIF
        ELSE
C--
C--   COPY PARENT VERTEX
          DO 60 J=1,3
   60     SECVTX(J,NTPNH)=SECVTX(J,IP)
        ENDIF
      ELSE
C--
C--   GET CODES OF THE DECAY PRODUCTS, THEIR MASS AND MASS-SUM
C--   FUSE PARTONS IN SOME CASES, IF ANY...
        MATRIX=ME(IPDEC)
        NTRY=0
        IF (MATRIX.GE.30) THEN
          NHA=NHAD
   70     IF (NTRY.GE.20) CALL ERRORD(84,PNA(IPPAR),FLOAT(IPDEC))
          NTRY=NTRY+1
          NHAD=0
          SUMM=0.
          I=1
   80     NHAD=NHAD+1
          NTPNH=NTEIL+NHAD
          ID=IDC(I,IPDEC)
          IQ1=IABS(ID)
          IF (IQ1.LT.9) THEN
            I=I+1
            IQ2=IABS(IDC(I,IPDEC))
            INDEXT(NTPNH)=MESON(IQ1,IQ2)
            IF (ID.LT.0) INDEXT(NTPNH)=-INDEXT(NTPNH)
          ELSE
            INDEXT(NTPNH)=ID
          ENDIF
          I=I+1
C--
C--     MASS SMEARING ACCORDING TO TRUNCATED BREIT-WIGNER (OPTIONAL)
          IPPOIN=ICONV(IABS(INDEXT(NTPNH)))
          IF ((ABS(INDEXT(NTPNH)).GT.99).AND.(MSMEAR.EQ.1)) THEN
            PTEIL(5,NTPNH)=BWMASS(IPPOIN)
          ELSE
            PTEIL(5,NTPNH)=PM(IPPOIN)
          ENDIF
          SUMM=SUMM+PTEIL(5,NTPNH)
          IF (I.LE.NHA) GOTO 80
          IF (SUMM.GE.(PTEIL(5,IP)-0.00001)) GOTO 70
C--
C--   ALL PARTONS REMOVED, SCALE MATRIX
          MATRIX=MATRIX-30
        ELSE
   90     IF (NTRY.GE.20) CALL ERRORD(85,PNA(IPPAR),FLOAT(IPDEC))
          NTRY=NTRY+1
          SUMM=0.
          DO 100 I=1,NHAD
            NTPI=NTEIL+I
            INDEXT(NTPI)=IDC(I,IPDEC)
            IPPOIN=ICONV(IABS(INDEXT(NTPI)))
C--
C--   MASS SMEARING (OPTONAL)
            IF ((ABS(INDEXT(NTPI)).GT.99).AND.(MSMEAR.EQ.1)) THEN
              PTEIL(5,NTPI)=BWMASS(IPPOIN)
            ELSE
              PTEIL(5,NTPI)=PM(IPPOIN)
            ENDIF
  100     SUMM=SUMM+PTEIL(5,NTPI)
          IF (SUMM.GE.(PTEIL(5,IP)-0.00001)) GOTO 90
        ENDIF
        NHADM1=NHAD-1
C--
C--   COPY PARENT KINEMATICS
        DO 110 J=1,5
  110   PHAD(J,1)=PTEIL(J,IP)
        PHAD(5,NHAD)=PTEIL(5,NTPNH)
C--
C--   SEPARATE TWO-BODY DECAYS
        IF (NHAD.EQ.2) GOTO 180
        IF (MATRIX.NE. 0) THEN
          IF (MATRIX.EQ. 1) THEN
C--
C--   KROLL-WADA DISTRIBUTION FOR THE DALITZ DECAYS OF PI0 AND ETA
            PHAD(5,2)=WADKRO(IP)
            GOTO 180
          ELSEIF (MATRIX.EQ. 6) THEN
C--
C--   GENERATE (INTERMEDIATE) RHO MASS IN A1 DECAY...
            IF (ABS(INDEXT(NTEIL+1)).EQ.110) THEN
              IPRHO=ICONV(211)
            ELSE
              IPRHO=ICONV(111)
            ENDIF
            IF (MSMEAR.EQ.1) THEN
  120         PHAD(5,2)=BWMASS(IPRHO)
              IF ((PHAD(5,2)+PTEIL(5,NTEIL+1)).GT.PTEIL(5,IP)) GOTO 120
            ELSE
              PHAD(5,2)=PM(IPRHO)
            ENDIF
            GOTO 180
          ELSEIF (MATRIX.EQ.24) THEN
C--
C--   GENERATE MASSES VIRTUAL HEAVY QUARK AND W IN ONIUM DECAY...
            PHAD(5,2)=VIRHVY(IP,VWMAS)
            PHAD(5,3)=VWMAS
            GOTO 180
          ENDIF
        ENDIF
C--
C--   MAXIMUM PHASE-SPACE WEIGHT
        WTMAX=1./PSFAC(NHAD)
        SUMM1=PHAD(5,1)
        SUMM2=SUMM-PTEIL(5,NTEIL+1)
        DO 130 I=1,NHADM1
          NTPI=NTEIL+I
          WTMAX=WTMAX*TRIANG(SUMM1,SUMM2,PTEIL(5,NTPI))
          SUMM1=SUMM1-PTEIL(5,NTPI)
  130   SUMM2=SUMM2-PTEIL(5,NTPI+1)
C--
C--   NEXT WE GENERATE THE UNIFORM NHAD-BODY PHASE-SPACE
C--   FIRST SORT NHAD-1 RANDOM NUMBERS IN DECREASING ORDER
  140   RORD(1)=1.
        DO 160 I=2,NHADM1
          RNEW=EURRAN(I)
          IM1=I-1
          DO 150 J=1,IM1
            IMJ=I-J
            IMJP1=IMJ+1
            IF (RNEW.LE.RORD(IMJ)) GOTO 160
  150     RORD(IMJP1)=RORD(IMJ)
  160   RORD(IMJP1)=RNEW
        RORD(NHAD)=0.
        WT=1.
        SUMM1=SUMM
        DO 170 I=2,NHAD
          SUMM1=SUMM1-PTEIL(5,NTEIL+I-1)
          PHAD(5,I)=SUMM1+RORD(I)*(PHAD(5,1)-SUMM)
  170   WT=WT*TRIANG(PHAD(5,I-1),PHAD(5,I),PTEIL(5,NTEIL+I-1))
C--
C--   CHECK VERSUS MAXIMUM WEIGHT...
        IF (WT.LT.(EURRAN(IWT)*WTMAX)) GOTO 140
C--
C--   TWO-BODY DECAY IN RESTFRAME OF DECAYING (INTERMEDIATE) PARTICLE
  180   DO 200 I=1,NHADM1
          NTPI=NTEIL+I
          TWOCM=TRIANG(PHAD(5,I),PHAD(5,I+1),PTEIL(5,NTPI))
          XMUL(3)=2.*EURRAN(I)-1.
          XMULT=SQRT(1.-XMUL(3)**2)
          PHI=TWOPI*EURRAN(IPHI)
          XMUL(1)=XMULT*SIN(PHI)
          XMUL(2)=XMULT*COS(PHI)
          DO 190 J=1,3
            PTEIL(J,NTPI)=TWOCM*XMUL(J)
  190     PHAD(J,I+1)=-PTEIL(J,NTPI)
          PTEIL(4,NTPI)=SQRT(TWOCM**2+PTEIL(5,NTPI)**2)
  200   PHAD(4,I+1)=SQRT(TWOCM**2+PHAD(5,I+1)**2)
        DO 210 J=1,4
  210   PTEIL(J,NTPNH)=PHAD(J,NHAD)
C--
C--   BOOST THE DECAY PARTICLES TO THE RESTFRAME OF THE PARENT
C--   WE CALCULATE MATRIX ELEMENTS IN THIS RESTFRAME...
        NHADM2=NHAD-2
        IF (NHADM2.GT.0) THEN
          DO 250 I=1,NHADM2
            NHADMI=NHAD-I
            DO 220 J=1,3
  220       BET(J)=PHAD(J,NHADMI)/PHAD(5,NHADMI)
            GAM=PHAD(4,NHADMI)/PHAD(5,NHADMI)
            DO 240 J=NHADMI,NHAD
              NTPJ=NTEIL+J
              PB=BET(1)*PTEIL(1,NTPJ)+BET(2)*PTEIL(2,NTPJ)+BET(3)*PTEIL
     &        (3,NTPJ)
              DO 230 K=1,3
  230         PTEIL(K,NTPJ)=PTEIL(K,NTPJ)+BET(K)*(PTEIL(4,NTPJ)+PB
     &        /(GAM+1.))
  240       PTEIL(4,NTPJ)=GAM*PTEIL(4,NTPJ)+PB
  250     CONTINUE
C--
C--   MULTI-BODY DECAY MATRIX ELEMENTS
          DO 260 I=NTEIL+1,NTPNH
  260     ITHEL(I)=0
          IF (MATRIX.GE.1) THEN
            IF (MATRIX.LE.10) THEN
              IF (MATRIX.EQ.2) THEN
C--
C--   OMEGA, PHI ---> PI+ PI- PI0
                WT=OMEPHI(IP)
                IF (WT.LT.EURRAN(IOPDUM)) GOTO 140
              ELSE
                IF (MATRIX.EQ.1) THEN
C--
C--   PI0, ETA ---> GAMMA E+ E-...
                  WT=DALITZ(IP)
                ELSEIF (MATRIX.EQ.6) THEN
C--
C--   A1 ---> RHO, PI ---> PI PI PI
                  WT=A1VEC(IP,INIT)
                ENDIF
                IF (WT.LT.EURRAN(IPADUM)) GOTO 180
              ENDIF
            ELSE
              IF (MATRIX.EQ.24) THEN
C--
C--   ONIUM SINGLE QUARK DECAYS
                WT=ONISQD(IP,PHAD(5,2))
                IF (WT.LT.EURRAN(ISQDUM)) GOTO 180
              ELSE
                IF (MATRIX.EQ.13) THEN
C--
C--   QUARKONIUM ---> G G G
                  WT=ONI3G(IP)
                ELSEIF (MATRIX.EQ.14) THEN
C--
C--   QUARKONIUM ---> G G GAMMA
                  WT=ON2GGA(IP)
                ELSEIF (MATRIX.EQ.15) THEN
C--
C--   QUARKONIUM ---> G QBAR Q
                  WT=ONGQBQ(IP)
                ELSEIF (MATRIX.EQ.22) THEN
C--
C--   WEAK DECAYS, V-A COUPLING...
                  WT=VMINA(IP)
                ELSEIF (MATRIX.EQ.23) THEN
C--
C--   WEAK DECAYS, VECTOR COUPLING...
                  WT=AMPVEC(IP)
                ENDIF
                IF (WT.LT.EURRAN(IDADUM)) GOTO 140
              ENDIF
            ENDIF
          ENDIF
        ELSE
C--
C--   TWO-BODY DECAYS ONLY
          ITHEL(NTEIL+1)=0
          ITHEL(NTPNH)=0
          IF (MATRIX.EQ.12) THEN
C--
C--   QUARKONIUM ---> G G
            WT=ONI2G(IP)
            IF (WT.LT.0.) GOTO 180
          ELSEIF (MATRIX.EQ.15) THEN
C--
C--   QUARKONIUM ---> HIGGS0 GAMMA
            WT=ONH0GA(IP)
            IF (WT.LT.EURRAN(IHGDUM)) GOTO 180
          ELSE
            IF (MATRIX.EQ.3) THEN
C--
C--   VECTOR ---> SCALAR1 SCALAR2
              WT=VECPAR(IP,INIT)
              IF (WT.LT.EURRAN(IWVDUM)) GOTO 180
            ELSEIF (MATRIX.EQ.4) THEN
              IF (ITHEL(IP).NE.0) THEN
C--
C--   POLARIZED TAU ---> K, PI
                WT=TAUSCA(IP)
                IF (WT.LT.EURRAN(ITPDUM)) GOTO 180
              ENDIF
            ELSEIF (MATRIX.EQ.5) THEN
              IF (ITHEL(IP).NE.0) THEN
C--
C--   POLARIZED TAU ---> K*, RHO, A1
                WT=TAUVEC(IP,INIT)
                IF (WT.LT.EURRAN(ITKDUM)) GOTO 180
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        ID=IABS(INDEXT(IP))
C--
C--   CALCULATE SECONDARY VERTICES
        IF (ISVTX.EQ.1) THEN
          PVEC=(PTEIL(4,IP)-PTEIL(5,IP))*(PTEIL(4,IP)+PTEIL(5,IP))
          IF (PVEC.LE.0.) THEN
            DO 270 J=1,3
  270       SECVER(J)=SECVTX(J,IP)
          ELSE
            CT=VLIGHT*REALLT(IP)
            DO 280 J=1,3
  280       SECVER(J)=SECVTX(J,IP)+CT*PTEIL(J,IP)/SQRT(PVEC)
          ENDIF
        ELSE
C--
C--   COPY PARENT VERTEX
          DO 290 J=1,3
  290     SECVER(J)=SECVTX(J,IP)
        ENDIF
C--
C--   INCLUDE MIXING IN D0-D0BAR, BD-BDBAR AND BS-BSBAR:
C--   TAU*DP(PAR ---> PARBAR)/DT = EXP(-T/TAU) *
C--   (SIN((X*T)/(2*TAU)))**2, X = SQRT(2*R/(1-R)), 0 < R < 1!
        IF (ID.EQ.410) THEN
          PROMIX(IP)=(SIN(.5*XD0*REALLT(IP)/PLT(IPPAR)))**2
          RD0MIX=EURRAN(ID0DUM)
          IF (RD0MIX.LE.PROMIX(IP)) INDEXT(IP)=-INDEXT(IP)
        ELSEIF (ID.EQ.520) THEN
          PROMIX(IP)=(SIN(.5*XB0*REALLT(IP)/PLT(IPPAR)))**2
          RB0MIX=EURRAN(IB0DUM)
          IF (RB0MIX.LE.PROMIX(IP)) INDEXT(IP)=-INDEXT(IP)
        ELSEIF (ID.EQ.530) THEN
          PROMIX(IP)=(SIN(.5*XBS0*REALLT(IP)/PLT(IPPAR)))**2
          RBSMIX=EURRAN(IDSDUM)
          IF (RBSMIX.LE.PROMIX(IP)) INDEXT(IP)=-INDEXT(IP)
        ENDIF
C--
C--   FLIP SIGN OF PARTICLE CODE FOR DECAY OF (ANTI) PARTICLE
        IF (INDEXT(IP).NE.IPC(IPPAR)) THEN
          DO 300 I=NTEIL+1,NTPNH
            ID=IABS(INDEXT(I))
            IF ((ID.GE.9).AND.(ID.LT.1000)) THEN
              IQ1=ID/100
              IQ100=IQ1*100
              IQ2=(ID-IQ100)/10
              IF (IQ1.EQ.IQ2) GOTO 300
              ISPIN=ID-IQ100-IQ2*10
              IF ((ID.EQ.328).OR.(ISPIN.EQ.9)) GOTO 300
            ENDIF
            INDEXT(I)=-INDEXT(I)
  300     CONTINUE
        ENDIF
C--
C--   FILL HISTORY INFORMATION: JET=IABS(IORIGT(IP))/10000
        IOR=10000*(IABS(IORIGT(IP))/10000)+IP
        DO 310 I=NTEIL+1,NTPNH
          IORIGT(I)=IOR
  310   IDCAYT(I)=0
        IDCAYT(IP)=(NTEIL+1)*10000+NTPNH
C--
C--   EVOLVE, FRAGMENT PARTONS AND ADD HADRONS TO /RESULT/
        NJETS=0
        DO 320 I=NTEIL+1,NTPNH
  320   IF (ABS(INDEXT(I)).LT.90) NJETS=NJETS+1
        ID=ABS(INDEXT(IP))/100
        IF ((NJETS.GE.2).OR.(ID/10.GE.6).OR.((ID.GE.6).AND.(ID.LT.9)))
     &  THEN
          CALL JETS(IP,NTEIL+1,NTPNH,NJETS,IADCUT)
          IF (IADCUT.EQ.1) THEN
            NCUT=NCUT+1
            IF (NCUT.GE.100) CALL ERRORD(43,' ',FLOAT(NCUT))
            GOTO 20
          ENDIF
        ENDIF
C--
C--   BOOST THE DECAY PARTICLES TO THE LAB-FRAME
        DO 330 J=1,3
  330   BET(J)=PHAD(J,1)/PHAD(5,1)
        GAM=PHAD(4,1)/PHAD(5,1)
        DO 350 I=NTEIL+1,NTPNH
          PB=BET(1)*PTEIL(1,I)+BET(2)*PTEIL(2,I)+BET(3)*PTEIL(3,I)
          DO 340 J=1,3
  340     PTEIL(J,I)=PTEIL(J,I)+BET(J)*(PTEIL(4,I)+PB/(GAM+1.))
  350   PTEIL(4,I)=GAM*PTEIL(4,I)+PB
C--
C--   INCLUDE SECONDARY VERTEX IN /RESULT/
        DO 360 I=NTEIL+1,NTPNH
          DO 360 J=1,3
  360   SECVTX(J,I)=SECVER(J)
      ENDIF
      NTEIL=NTPNH
      RETURN
      END
