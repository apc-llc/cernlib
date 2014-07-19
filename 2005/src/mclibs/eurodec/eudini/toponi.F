*
* $Id: toponi.F,v 1.1.1.1 1996/03/08 16:58:51 mclareni Exp $
*
* $Log: toponi.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:51  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE TOPONI(IP)
C.----------------------------------------------------------------------
C.
C.    CALCULATE BRANCHING FRACTIONS FOR IP-TH TOPONIUM IN PARTICLE
C.    TABLE. PRINT NEW BRANCHING FRACTIONS AND MODES (OPTIONAL).
C.    LAST UPDATE: 14/08/89
C.
C.----------------------------------------------------------------------
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION SQD,DGAUSS
#endif
#include "eurodec/eudopt.inc"
#include "eurodec/inpout.inc"
#include "eurodec/wekpar.inc"
#include "eurodec/convrt.inc"
#include "eurodec/ptable.inc"
#include "eurodec/pwidth.inc"
#include "eurodec/dtable.inc"
#include "eurodec/picons.inc"
#include "eurodec/onipot.inc"
#include "eurodec/sqdpar.inc"
#include "eurodec/boscop.inc"
      CHARACTER*8 PNAM(6)
      DIMENSION AM(6)
      EXTERNAL SQD
      SAVE INIT,WSQD
      DATA INIT/ 0/
      DATA WSQD,GAMEPS,CHBAR,RTEPS/ 0., 1.E-5, 6.582173E-25, 1.E-6/
C--
C--   STRONG COUPLING CONSTANT FOR BOUND STATES, LAMBDA = 0.2 GEV
      ONIALP(X)=6.*PI/23./LOG(X/.2)
      BREITW(X,Y,Z)=(X-Y)**2+Y*Z
C--
C--   INITIALIZE COMMON FOR SINGLE QUARK TOPONIUM DECAYS...
      IF (INIT.EQ.0) THEN
        INIT=1
        XRH=(PM(ICONV(6))/AMW)**2
        XMU0=PM(ICONV(5))/PM(ICONV(6))
        XMU=XMU0**2
        XGW=(CHBAR/PLT(ICONV(198))/AMW)**2
        OMXRH2=(1.-XMU)**2
C--
C--   ...AND CALCULATE TOTAL SQD WIDTH
        WSQD=36.*GF**2*PM(ICONV(6))**5*
     &  DGAUSS(SQD,0.,(1.-XMU0)**2,RTEPS)/192./PI**3
      ENDIF
C--
C--   CALCULATE INDIVIDUAL DECAY WIDTHS AND CUMULATIVE DISTRIBUTION
      IDONI=ABS(IPC(IP))-659
      AM2=PM(IP)**2
      AM3=PM(IP)*AM2
      AM4=PM(IP)*AM3
      IFIRST=ABS(IDP(IP))
      IF (IFIRST.EQ.0) RETURN
      I=IFIRST
      WIDTH=0.
   10 AMSUM=0.
      WIDINI=DBR(I)
      DO 20 J=1,NDP(I)
        ID=ABS(IDC(J,I))
        AM(J)=PM(ICONV(ID))
   20 AMSUM=AMSUM+AM(J)
C--
C--   PHASE SPACE ALLOWED DECAY MODE ?
      IF (AMSUM.LT.PM(IP)) THEN
        COLOUR=1.
        IF (ID.LE.8) COLOUR=3.
C--
C--   SEPARATE 2, 3 AND 4-BODY ONIUM DECAYS
        IF (NDP(I).EQ.2) THEN
          IF ((ID.NE.9).AND.(ID.NE.99)) THEN
C--
C--   FERMION ANTI-FERMION PAIR
            X=AM(1)**2/AM2
            IDD=ID
            IF (ID.GT.90) IDD=ID-90+8
            V=VZBOS(IDD)
            A=AZBOS(IDD)
            EF=EFERM(IDD)
            EF2=EF**2
            AMW2=AMW**2
            AMZ2=AMZ**2
            GAMZ2=(CHBAR/PLT(ICONV(199)))**2
            BW=BREITW(AM2,AMZ2,GAMZ2)
            IF (IDONI.LT.8) THEN
              DBR(I)=4./9.*EF2+(1.-8.*XW/3.)**2*(A**2+V**2)*AM4/BW/
     &        (16.*XW*(1.-XW))**2+4./3.*EF*(1.-8.*XW/3.)*V*(AM2-AMZ2)
     &        /BW/(16.*XW*(1.-XW))*AM2
              IF (ID.EQ.5) THEN
                EXCH=AM2*(AMW2+AM2/8.)/(AMW2+AM2/4.)/AMW2/24./XW
                EXCH2=EXCH**2
                DBR(I)=DBR(I)-EF*4.*EXCH/3.+2.*EXCH2-2.*EXCH*(1.-
     &          8.*XW/3.)*(V+A)*(AM2-AMZ2)/BW/(16.*XW*(1.-XW))*AM2
              ENDIF
              DBR(I)=9.*COLOUR*ONIG0(IDONI,3)*DBR(I)/4.
            ELSE
              DBR(I)=(A**2+V**2)/BW/(16.*XW*(1.-XW))**2
              IF (ID.EQ.5) THEN
                EXCH=(AMW2-AM2/8.)/(AMW2+AM2/4.)/AMW2/24./XW
                EXCH2=EXCH**2
                DBR(I)=DBR(I)+2.*EXCH2-2.*EXCH*(A+V)*(AM2-AMZ2)
     &          /BW/16./XW/(1.-XW)
              ENDIF
              DBR(I)=96.*COLOUR*ALPHA**2*ONIRP0(IDONI,3)*DBR(I)
            ENDIF
          ELSEIF (ID.EQ.9) THEN
C--
C--   GLUON PAIR (ETA(T) AND CHI(T))
            IF (IDONI.LE.2) THEN
              DBR(I)=8.*(ONIALP(PM(IP)))**2*ONIR0(1,3)/AM4/3.
            ELSEIF (IDONI.EQ.8) THEN
              DBR(I)=96.*(ONIALP(PM(IP)))**2*ONIRP0(8,3)/AM4
            ELSEIF (IDONI.EQ.10) THEN
              DBR(I)=128.*(ONIALP(PM(IP)))**2*ONIRP0(10,3)/AM4/5.
            ENDIF
          ELSEIF (ID.EQ.99) THEN
            IF (ABS(IDC(1,I)).EQ.99) THEN
C--
C--   PHOTON PAIR (ETA(T) AND CHI(T))
              IF (IDONI.LE.2) THEN
                DBR(I)=64.*ALPHA**2*ONIR0(IDONI,3)/AM4/27.
              ELSEIF (IDONI.EQ.8) THEN
                DBR(I)=256.*ALPHA**2*ONIRP0(8,3)/AM4/3.
              ELSEIF (IDONI.EQ.10) THEN
                DBR(I)=1024.*ALPHA**2*ONIRP0(10,3)/AM4/45.
              ENDIF
            ELSEIF (ABS(IDC(1,I)).EQ.191) THEN
C--
C--   NEUTRAL HIGGS AND PHOTON
              DBR(I)=GF*AM2*(1.-PM(ICONV(191))**2/AM2)
     &        *ONIG0(IDONI,3)/4./PI/ALPHA/SQRT(2.)
            ELSEIF (ABS(IDC(1,I)).GE.660) THEN
C--
C--   ONIUM AND PHOTON
              DBR(I)=GAMDIP(IDONI)
            ENDIF
          ENDIF
        ELSEIF (NDP(I).EQ.3) THEN
C--
C--   THREE-BODY DECAYS
          IF (ID.EQ.9) THEN
C--
C--   3 GLUONS
            DBR(I)=5.*(ONIALP(PM(IP)))**3*ONIG0(IDONI,3)
     &      *(PI**2-9.)/PI/ALPHA**2/18.
          ELSEIF (ID.EQ.99) THEN
C--
C--   GLUON GLUON PHOTON
            DBR(I)=8.*(ONIALP(PM(IP)))**2*ONIG0(IDONI,3)*(PI**2-9.)/PI
     &      /ALPHA/9.
          ELSEIF ((ID.EQ.210).OR.(ID.EQ.110)) THEN
C--
C--   THET1S PI PI
            DBR(I)=3.E-7
          ELSE
C--
C--   GLUON QBAR Q
            DBR(I)=640.*(ONIALP(PM(IP)))**3*ONIRP0(9,3)
     &      *LOG(PM(IP)/0.6)/AM4/PI/9./5.
          ENDIF
        ELSE
C--
C--   (FOUR-BODY) SINGLE QUARK DECAYS
          DBR(I)=COLOUR*WSQD/18.
        ENDIF
      ELSE
        DBR(I)=0.
      ENDIF
      WIDTH=WIDTH+DBR(I)
      ILAST=I
      I=I+1
      IF (DBR(I).GE.(WIDINI-GAMEPS)) GOTO 10
C--
C--   WRITE INFO
      IF (IBRDMP.EQ.1) THEN
        WRITE(LUN2,9000)
        WRITE(LUN2,9010)
        WRITE(LUN2,9020) PNA(IP),PM(IP)
      ENDIF
      GAM(IP)=WIDTH
      IF (WIDTH.NE.0.) PLT(IP)=CHBAR/WIDTH
      IF (IBRDMP.EQ.1) THEN
        WRITE(LUN2,9070) PNA(IP),WIDTH,PLT(IP)
        WRITE(LUN2,9030)
        WRITE(LUN2,9000)
      ENDIF
      IF (WIDTH.GT.0.) THEN
        DO 40 I=IFIRST,ILAST
          IF (WIDTH.NE.0.) DBR(I)=DBR(I)/WIDTH
          DO 30 J=1,NDP(I)
   30     CALL INDNAM(IDC(J,I),PNAM(J))
          IF (IBRDMP.EQ.1) THEN
            IF (NDP(I).EQ.2) THEN
              WRITE(LUN2,9040) PNA(IP),(PNAM(J),J=1,2),DBR(I)*100.
            ELSEIF (NDP(I).EQ.3) THEN
              WRITE(LUN2,9050) PNA(IP),(PNAM(J),J=1,3),DBR(I)*100.
            ELSE
              WRITE(LUN2,9060) PNA(IP),(PNAM(J),J=1,4),DBR(I)*100.
            ENDIF
          ENDIF
   40   CONTINUE
      ELSE
        IF (IBRDMP.EQ.1) WRITE(LUN2,9080)
        IDP(IP)=0
      ENDIF
      IF (IBRDMP.EQ.1) THEN
        WRITE(LUN2,9000)
        WRITE(LUN2,9010)
      ENDIF
C--
C--   CALCULATE CUM. DISTRIBUTION
      IF (WIDTH.GT.0.) THEN
        DO 50 I=IFIRST+1,ILAST
   50   DBR(I)=DBR(I-1)+DBR(I)
        IF ((DBR(ILAST).LT.0.99999).OR.(DBR(ILAST).GE.1.00001))
     &  CALL ERRORD(57,PNA(IP),DBR(ILAST))
      ENDIF
      RETURN
 9000 FORMAT(1H )
 9010 FORMAT(1H ,80('*'))
 9020 FORMAT(1H0,5X,'You have modified the ',A8,
     &' (or decay particle) mass: ',F10.5,' GeV.')
 9030 FORMAT(1H0,13X,
     &'The new branching modes and branching fractions read:')
 9040 FORMAT(1H ,13X,A8,' ---> ',A8,1X,A8,19X,': ',F8.4,' %')
 9050 FORMAT(1H ,13X,A8,' ---> ',A8,1X,A8,1X,A8,10X,': ',F8.4,' %')
 9060 FORMAT(1H ,13X,A8,' ---> ',A8,1X,A8,1X,A8,1X,A8,1X,': ',F8.4,' %')
 9070 FORMAT(1H0,6X,'The ',A8,' width reads: ',F7.5,' GeV. (Lifetime = '
     &,E10.4,' S.).')
 9080 FORMAT(1H ,9X,
     &'Beware, NO phase space allowed branching modes in decay table!')
      END
