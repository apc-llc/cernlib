*
* $Id: hwhigw.F,v 1.1.1.1 1996/03/08 17:02:15 mclareni Exp $
*
* $Log: hwhigw.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:15  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHIGW.
*CMZ :-        -26/04/91  14.55.44  by  Federico Carminati
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWHIGW
C     HIGGS PRODUCTION VIA W BOSON FUSION
C     MEAN EVWGT = HIGGS PRODN C-S * BRANCHING FRACTION IN NB
C-----------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER HWRINT,IDEC,I,ID1,ID2,IHAD
      LOGICAL EE,EP
      DOUBLE PRECISION K1MAX2,K1MIN2,K12,K2MAX2,K2MIN2,K22,EMW2,EMW,
     & ROOTS,EMH2,EMH,ROOTS2,P1,PHI1,PHI2,COSPHI,COSTH1,SINTH1,
     & COSTH2,SINTH2,P2,WEIGHT,TAU,TAULN,CSFAC,PSUM,PROB,Q1(5),Q2(5),
     & H(5),A,B,C,TERM2,BRHIGQ,G1WW,G2WW,G1ZZ(6),G2ZZ(6),AWW,AZZ(6),
     & PWW,PZZ(6),EMZ,EMZ2,RSUM,GLUSQ,GRUSQ,GLDSQ,GRDSQ,GLESQ,GRESQ,
     & CW,CZ,EMFAC,CV,CA,BR,HWULDO,HWRUNI,HWRGEN,X2,ETA,P1JAC,FACTR,
     & EH2,HWUAEM
      SAVE EMW2,EMZ2,EE,GLUSQ,GRUSQ,GLDSQ,GRDSQ,GLESQ,GRESQ,G1ZZ,G2ZZ,
     & G1WW,G2WW,CW,CZ,PSUM,AWW,PWW,AZZ,PZZ,ROOTS,Q1,Q2,H,FACTR
      EQUIVALENCE (EMW,RMASS(198))
      EQUIVALENCE (EMZ,RMASS(200))
      IHAD=2
      IF (JDAHEP(1,IHAD).NE.0) IHAD=JDAHEP(1,IHAD)
      IF (FSTWGT) THEN
        EMW2=EMW**2
        EMZ2=EMZ**2
        GLUSQ=(VFCH(2,1)+AFCH(2,1))**2
        GRUSQ=(VFCH(2,1)-AFCH(2,1))**2
        GLDSQ=(VFCH(1,1)+AFCH(1,1))**2
        GRDSQ=(VFCH(1,1)-AFCH(1,1))**2
        GLESQ=(VFCH(11,1)+AFCH(11,1))**2
        GRESQ=(VFCH(11,1)-AFCH(11,1))**2
        G1ZZ(1)=GLUSQ*GLUSQ+GRUSQ*GRUSQ
        G2ZZ(1)=GLUSQ*GRUSQ+GRUSQ*GLUSQ
        G1ZZ(2)=GLUSQ*GLDSQ+GRUSQ*GRDSQ
        G2ZZ(2)=GLUSQ*GRDSQ+GRUSQ*GLDSQ
        G1ZZ(3)=GLDSQ*GLDSQ+GRDSQ*GRDSQ
        G2ZZ(3)=GLDSQ*GRDSQ+GRDSQ*GLDSQ
        G1ZZ(4)=GLESQ*GLESQ+GRESQ*GRESQ
        G2ZZ(4)=GLESQ*GRESQ+GRESQ*GLESQ
        G1ZZ(5)=GLESQ*GLUSQ+GRESQ*GRUSQ
        G2ZZ(5)=GLESQ*GRUSQ+GRESQ*GLUSQ
        G1ZZ(6)=GLESQ*GLDSQ+GRESQ*GRDSQ
        G2ZZ(6)=GLESQ*GRDSQ+GRESQ*GLDSQ
        G1WW=0.25
        G2WW=0
        FACTR=GEV2NB/(128.*PIFAC**3)
        EH2=RMASS(201)**2
        CW=256*(PIFAC*HWUAEM(EH2)/SWEIN)**3*EMW2
        CZ=256.*(PIFAC*HWUAEM(EH2))**3*EMZ2/(SWEIN*(1.-SWEIN))
      ENDIF
      EE=IPRO.LT.10
      EP=IPRO.GE.90
      IF (.NOT.GENEV) THEN
C---CHOOSE PARAMETERS
        EVWGT=0.
        CALL HWHIGM(EMH,EMFAC)
        IF (EMH.LE.0 .OR. EMH.GE.PHEP(5,3)) RETURN
        EMSCA=EMH
        IF (EE) THEN
          ROOTS=PHEP(5,3)
        ELSE
          TAU=(EMH/PHEP(5,3))**2
          TAULN=LOG(TAU)
          ROOTS=PHEP(5,3)*SQRT(EXP(HWRUNI(0,-1D-10,TAULN)))
        ENDIF
        EMH2=EMH**2
        ROOTS2=ROOTS**2
C---CHOOSE P1 ACCORDING TO (1-ETA)*(ETA-X2)/ETA**2
C   WHERE ETA=1-2P1/ROOTS AND X2=EMH**2/S
        X2=EMH2/ROOTS2
 1      ETA=X2**HWRGEN(0)
        IF (HWRGEN(0)*(1-EMH/ROOTS)**2*ETA.GT.(1-ETA)*(ETA-X2))GOTO 1
        P1JAC=0.5*ROOTS*ETA**2/((1-ETA)*(ETA-X2))
     &    *(-LOG(X2)*(1+X2)-2*(1-X2))
        P1=0.5*ROOTS*(1-ETA)
C---CHOOSE PHI1,2 UNIFORMLY
        PHI1=2*PIFAC*HWRGEN(0)
        PHI2=2*PIFAC*HWRGEN(0)
        COSPHI=COS(PHI2-PHI1)
C---CHOOSE K1^2, ON PROPAGATOR FACTOR
        K1MAX2=2*P1*ROOTS
        K1MIN2=0
        K12=EMW2-(EMW2+K1MAX2)*(EMW2+K1MIN2)/
     &           ((K1MAX2-K1MIN2)*HWRGEN(0)+(EMW2+K1MIN2))
C---CALCULATE COSTH1 FROM K1^2
        COSTH1=1+K12/(P1*ROOTS)
        SINTH1=SQRT(1-COSTH1**2)
C---CHOOSE K2^2
        K2MAX2=ROOTS*(ROOTS2-EMH2-2*ROOTS*P1)*(ROOTS-P1-P1*COSTH1)
     &        /((ROOTS-P1)**2-(P1*COSTH1)**2-(P1*SINTH1*COSPHI)**2)
        K2MIN2=0
        K22=EMW2-(EMW2+K2MAX2)*(EMW2+K2MIN2)/
     &           ((K2MAX2-K2MIN2)*HWRGEN(0)+(EMW2+K2MIN2))
C---CALCULATE A,B,C FACTORS, AND...
        A=-2*K22*P1*COSTH1 - ROOTS*(ROOTS2-EMH2-2*ROOTS*P1)
        B=-2*K22*P1*SINTH1*COSPHI
        C=+2*K22*P1 - 2*ROOTS*K22 - ROOTS*(ROOTS2-EMH2-2*ROOTS*P1)
C---SOLVE A*COSTH2 + B*SINTH2 + C = 0 FOR COSTH2
        TERM2=B**2 + A**2 - C**2
        IF (TERM2.LT.0) RETURN
        TERM2=B*SQRT(TERM2)
        IF (A.GE.0) RETURN
        COSTH2=(-A*C + TERM2)/(A**2+B**2)
        SINTH2=SQRT(1-COSTH2**2)
C---FINALLY, GET P2
        IF (COSTH2.EQ.-1) RETURN
        P2=-K22/(ROOTS*(1+COSTH2))
C---LOAD UP CMF MOMENTA
        Q1(1)=P1*SINTH1*COS(PHI1)
        Q1(2)=P1*SINTH1*SIN(PHI1)
        Q1(3)=P1*COSTH1
        Q1(4)=P1
        Q1(5)=0
        Q2(1)=P2*SINTH2*COS(PHI2)
        Q2(2)=P2*SINTH2*SIN(PHI2)
        Q2(3)=P2*COSTH2
        Q2(4)=P2
        Q2(5)=0
        H(1)=-Q1(1)-Q2(1)
        H(2)=-Q1(2)-Q2(2)
        H(3)=-Q1(3)-Q2(3)
        H(4)=-Q1(4)-Q2(4)+ROOTS
        CALL HWUMAS(H)
C---CALCULATE MATRIX ELEMENTS SQUARED
        AWW=ENHANC(10)**2 * CW*(ROOTS2/2*HWULDO(Q1,Q2)*G1WW
     &         +ROOTS2/4*P1*P2*(1+COSTH1)*(1-COSTH2)*G2WW)
        DO 10 I=1,6
          AZZ(I)=ENHANC(11)**2 * CZ*(ROOTS2/2*HWULDO(Q1,Q2)*G1ZZ(I)
     &               +ROOTS2/4*P1*P2*(1+COSTH1)*(1-COSTH2)*G2ZZ(I))
     &          *((K12-EMW2)/(K12-EMZ2)*(K22-EMW2)/(K22-EMZ2))**2
 10     CONTINUE
C---CALCULATE WEIGHT IN INTEGRAL
        WEIGHT=FACTR*P2*P1JAC/(ROOTS2**2*HWULDO(H,Q2))
     &              *(K1MAX2-K1MIN2)/((K1MAX2+EMW2)*(K1MIN2+EMW2))
     &              *(K2MAX2-K2MIN2)/((K2MAX2+EMW2)*(K2MIN2+EMW2))
     &              * EMFAC
        EMSCA=EMW
        XXMIN=(ROOTS/PHEP(5,3))**2
        XLMIN=LOG(XXMIN)
C---INCLUDE BRANCHING RATIO OF HIGGS
        IDEC=MOD(IPROC,100)
        IF (IDEC.GT.0.AND.IDEC.LE.12) WEIGHT=WEIGHT*BRHIG(IDEC)
        IF (IDEC.EQ.0) THEN
          BRHIGQ=0
          DO 20 I=1,6
 20         BRHIGQ=BRHIGQ+BRHIG(I)
          WEIGHT=WEIGHT*BRHIGQ
        ENDIF
        IF (IDEC.EQ.10) THEN
          CALL HWDBOZ(198,ID1,ID2,CV,CA,BR,1)
          CALL HWDBOZ(199,ID1,ID2,CV,CA,BR,1)
          WEIGHT=WEIGHT*BR
        ELSEIF (IDEC.EQ.11) THEN
          CALL HWDBOZ(200,ID1,ID2,CV,CA,BR,1)
          CALL HWDBOZ(200,ID1,ID2,CV,CA,BR,1)
          WEIGHT=WEIGHT*BR
        ENDIF
        IF (EE) THEN
          CSFAC=WEIGHT
          PSUM=AWW+AZZ(4)
          EVWGT=CSFAC*PSUM
        ELSEIF (EP) THEN
          CSFAC=-WEIGHT*TAULN
          XX(1)=ONE
          XX(2)=XXMIN
          CALL HWSFUN(XX(2),EMSCA,IDHW(IHAD),NSTRU,DISF(1,2),2)
          IF (IDHW(1).LE.126) THEN
            PWW=(DISF(2,2)+DISF(4,2)+DISF(7,2)+DISF( 9,2))*AWW
          ELSE
            PWW=(DISF(1,2)+DISF(3,2)+DISF(8,2)+DISF(10,2))*AWW
          ENDIF
          PZZ(5)=(DISF(2,2)+DISF(4,2)+DISF(8,2)+DISF(10,2))*AZZ(5)
          PZZ(6)=(DISF(1,2)+DISF(3,2)+DISF(7,2)+DISF( 9,2))*AZZ(6)
          PSUM=PWW+PZZ(5)+PZZ(6)
          EVWGT=CSFAC*PSUM
        ELSE
          CSFAC=WEIGHT*TAULN*XLMIN
          CALL HWSGEN(.TRUE.)
          PWW=((DISF(2,1)+DISF(4, 1)+DISF(7,1)+DISF(9,1))
     &        *(DISF(8,2)+DISF(10,2)+DISF(1,2)+DISF(3,2))
     &        +(DISF(8,1)+DISF(10,1)+DISF(1,1)+DISF(3,1))
     &        *(DISF(2,2)+DISF(4, 2)+DISF(7,2)+DISF(9,2)))
     &        *AWW
          PZZ(1)=((DISF(2,1)+DISF(4,1)+DISF(8,1)+DISF(10,1))
     &           *(DISF(2,2)+DISF(4,2)+DISF(8,2)+DISF(10,2)))
     &           *AZZ(1)
          PZZ(2)=((DISF(2,1)+DISF(4,1)+DISF(8,1)+DISF(10,1))
     &           *(DISF(1,2)+DISF(3,2)+DISF(7,2)+DISF(9, 2))
     &           +(DISF(1,1)+DISF(3,1)+DISF(7,1)+DISF(9, 1))
     &           *(DISF(2,2)+DISF(4,2)+DISF(8,2)+DISF(10,2)))
     &           *AZZ(2)
          PZZ(3)=((DISF(1,1)+DISF(3,1)+DISF(7,1)+DISF(9,1))
     &           *(DISF(1,2)+DISF(3,2)+DISF(7,2)+DISF(9,2)))
     &           *AZZ(3)
          PSUM=PWW+PZZ(1)+PZZ(2)+PZZ(3)
C---EVENT WEIGHT IS SUM OVER ALL COMBINATIONS
          EVWGT=CSFAC*PSUM
        ENDIF
      ELSE
C---GENERATE EVENT
C---CHOOSE EVENT TYPE
        RSUM=PSUM*HWRGEN(0)
C---ELECTRON BEAMS?
        IF (EE) THEN
          IDN(1)=IDHW(1)
          IDN(2)=IDHW(2)
C---WW FUSION?
          IF (RSUM.LT.AWW) THEN
            IDN(3)=IDN(1)+1
            IDN(4)=IDN(2)+1
C---ZZ FUSION?
          ELSE
            IDN(3)=IDN(1)
            IDN(4)=IDN(2)
          ENDIF
C---LEPTON-HADRON COLISION?
        ELSEIF (EP) THEN
C---WW FUSION?
          IDN(1)=IDHW(1)
          IF (RSUM.LT.PWW) THEN
 24         IDN(2)=HWRINT(1,8)
            IF (IDN(2).GE.5) IDN(2)=IDN(2)+2
            IF (ICHRG(IDN(1))*ICHRG(IDN(2)).GT.0) GOTO 24
            PROB=DISF(IDN(2),2)*AWW/PWW
            IF (HWRGEN(0).GT.PROB) GOTO 24
            IDN(3)=IDN(1)+1
            IF (HWRGEN(0).GT.SCABI) THEN
              IDN(4)= 4*INT((IDN(2)-1)/2)-IDN(2)+3
            ELSE
              IDN(4)=12*INT((IDN(2)-1)/6)-IDN(2)+5
            ENDIF
C---ZZ FUSION FROM U-TYPE QUARK?
          ELSEIF (RSUM.LT.PWW+PZZ(5)) THEN
 26         IDN(2)=2*HWRINT(1,4)
            IF (IDN(2).GE.5) IDN(2)=IDN(2)+2
            PROB=DISF(IDN(2),2)*AZZ(5)/PZZ(5)
            IF (HWRGEN(0).GT.PROB) GOTO 26
            IDN(3)=IDN(1)
            IDN(4)=IDN(2)
C---ZZ FUSION FROM D-TYPE QUARK?
          ELSE
 28         IDN(2)=2*HWRINT(1,4)-1
            IF (IDN(2).GE.5) IDN(2)=IDN(2)+2
            PROB=DISF(IDN(2),2)*AZZ(6)/PZZ(6)
            IF (HWRGEN(0).GT.PROB) GOTO 28
            IDN(3)=IDN(1)
            IDN(4)=IDN(2)
          ENDIF
C---HADRON BEAMS?
        ELSE
C---WW FUSION?
          IF (RSUM.LT.PWW) THEN
 31         DO 32 I=1,2
              IDN(I)=HWRINT(1,8)
              IF (IDN(I).GE.5) IDN(I)=IDN(I)+2
 32         CONTINUE
            IF (ICHRG(IDN(1))*ICHRG(IDN(2)).GT.0) GOTO 31
            PROB=DISF(IDN(1),1)*DISF(IDN(2),2)*AWW/PWW
            IF (HWRGEN(0).GT.PROB) GOTO 31
C---CHOOSE OUTGOING QUARKS
            DO 33 I=1,2
              IF (HWRGEN(0).GT.SCABI) THEN
                IDN(I+2)=4*INT((IDN(I)-1)/2)-IDN(I)+3
              ELSE
                IDN(I+2)=12*INT((IDN(I)-1)/6)-IDN(I)+5
              ENDIF
 33         CONTINUE
C---ZZ FUSION FROM U-TYPE QUARKS?
          ELSEIF (RSUM.LT.PWW+PZZ(1)) THEN
 41         DO 42 I=1,2
              IDN(I)=2*HWRINT(1,4)
              IF (IDN(I).GE.5) IDN(I)=IDN(I)+2
 42         CONTINUE
            PROB=DISF(IDN(1),1)*DISF(IDN(2),2)*AZZ(1)/PZZ(1)
            IF (HWRGEN(0).GT.PROB) GOTO 41
            IDN(3)=IDN(1)
            IDN(4)=IDN(2)
C---ZZ FUSION FROM D-TYPE QUARKS?
          ELSEIF (RSUM.LT.PWW+PZZ(1)+PZZ(3)) THEN
 51         DO 52 I=1,2
              IDN(I)=2*HWRINT(1,4)-1
              IF (IDN(I).GE.5) IDN(I)=IDN(I)+2
 52         CONTINUE
            PROB=DISF(IDN(1),1)*DISF(IDN(2),2)*AZZ(3)/PZZ(3)
            IF (HWRGEN(0).GT.PROB) GOTO 51
            IDN(3)=IDN(1)
            IDN(4)=IDN(2)
C---ZZ FUSION FROM UD-TYPE PAIRS?
          ELSE
 61         IF (HWRGEN(0).GT.0.5) THEN
              IDN(1)=2*HWRINT(1,4)-1
              IDN(2)=2*HWRINT(1,4)
            ELSE
              IDN(1)=2*HWRINT(1,4)
              IDN(2)=2*HWRINT(1,4)-1
            ENDIF
            DO 62 I=1,2
 62           IF (IDN(I).GE.5) IDN(I)=IDN(I)+2
            PROB=DISF(IDN(1),1)*DISF(IDN(2),2)*AZZ(2)/PZZ(2)
            IF (HWRGEN(0).GT.PROB) GOTO 61
            IDN(3)=IDN(1)
            IDN(4)=IDN(2)
          ENDIF
        ENDIF
C---NOW BOOST TO LAB, AND SET UP STATUS CODES etc
        IDCMF=15
C---INCOMING
        IF (.NOT.EE) CALL HWEONE
C---CMF POINTERS
        JDAHEP(1,NHEP)=NHEP+1
        JDAHEP(2,NHEP)=NHEP+3
        JMOHEP(1,NHEP+1)=NHEP
        JMOHEP(1,NHEP+2)=NHEP
        JMOHEP(1,NHEP+3)=NHEP
C---OUTGOING MOMENTA (GIVE QUARKS MASS NON-COVARIANTLY!)
        Q1(5)=RMASS(IDN(1))
        Q1(4)=SQRT(Q1(4)**2+Q1(5)**2)
        Q2(5)=RMASS(IDN(2))
        Q2(4)=SQRT(Q2(4)**2+Q2(5)**2)
        H(4)=-Q1(4)-Q2(4)+PHEP(5,NHEP)
        CALL HWUMAS(H)
        CALL HWULOB(PHEP(1,NHEP),Q1,PHEP(1,NHEP+1))
        CALL HWULOB(PHEP(1,NHEP),Q2,PHEP(1,NHEP+2))
        CALL HWULOB(PHEP(1,NHEP),H,PHEP(1,NHEP+3))
C---STATUS AND IDs
        ISTHEP(NHEP+1)=113
        ISTHEP(NHEP+2)=114
        ISTHEP(NHEP+3)=114
        IDHW(NHEP+1)=IDN(3)
        IDHEP(NHEP+1)=IDPDG(IDN(3))
        IDHW(NHEP+2)=IDN(4)
        IDHEP(NHEP+2)=IDPDG(IDN(4))
        IDHW(NHEP+3)=201
        IDHEP(NHEP+3)=IDPDG(201)
C---COLOUR LABELS
        JMOHEP(2,NHEP+1)=NHEP-2
        JMOHEP(2,NHEP+2)=NHEP-1
        JMOHEP(2,NHEP-1)=NHEP+2
        JMOHEP(2,NHEP-2)=NHEP+1
        JMOHEP(2,NHEP+3)=NHEP+3
        JDAHEP(2,NHEP+1)=NHEP-2
        JDAHEP(2,NHEP+2)=NHEP-1
        JDAHEP(2,NHEP-1)=NHEP+2
        JDAHEP(2,NHEP-2)=NHEP+1
        JDAHEP(2,NHEP+3)=NHEP+3
        NHEP=NHEP+3
      ENDIF
  999 END
