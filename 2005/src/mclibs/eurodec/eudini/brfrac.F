*
* $Id: brfrac.F,v 1.1.1.1 1996/03/08 16:58:51 mclareni Exp $
*
* $Log: brfrac.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:51  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE BRFRAC(IP)
C.----------------------------------------------------------------------
C.
C.    CALCULATE BRANCHING FRACTIONS FOR IP-TH PARTICLE IN PARTICLE
C.    TABLE. PRINT NEW BRANCHING FRACTIONS AND MODES (OPTIONAL).
C.    LAST UPDATE: 14/08/89
C.
C.----------------------------------------------------------------------
#include "eurodec/eudopt.inc"
#include "eurodec/inpout.inc"
#include "eurodec/wekpar.inc"
#include "eurodec/boscop.inc"
#include "eurodec/convrt.inc"
#include "eurodec/ptable.inc"
#include "eurodec/pwidth.inc"
#include "eurodec/dtable.inc"
#include "eurodec/picons.inc"
      CHARACTER*8 PNAM(6),EPSNAM(9)
      DIMENSION AM(6),AMEPS(9)
      SAVE INIT,AMEPS
      DATA GAMEPS,CHBAR/ 1.E-5, 6.582173E-25/
      DATA INIT/ 0/
      DATA EPSNAM/'PI0     ','PI0     ','K      -','D0      ',
     & 'B      -','T0      ','L      -','H0      ','PI0     '/
      DATA AMEPS/ 9*0./
C--
C--   WIDTH FUNCTIONS
      W2BODY(X,Y)=SQRT((1.-X-Y)**2-4.*X*Y)
      WHIG0F(X)=GF*AM3*X*SQRT(1.-X)*(1.-X)/16/PI/SQRT(2.)
      WHIG0W(X)=GF*AM3*X*SQRT(1.-X)*(X*(3.*X-4.)+4)/32./PI/SQRT(2.)
      WHIG0Z(X)=GF*AM3*X*SQRT(1.-X)*(X*(3.*X-4.)+4)/64./PI/SQRT(2.)
      WHIGPF(X,Y)=GF*PM(IP)*(X**2+Y**2)*(1.-(X+Y)**2/AM2)/4./PI/SQRT(2.)
      WWBOSF(X,Y)=GF*AM3*(1.-.5*(X+Y)-.5*(X-Y)**2)/6./PI/SQRT(2.)
      WZBOSF(A,X)=GF*AM3*(1.-X+3.*X*(A-1.)/(A+1.))/24./PI/SQRT(2.)
      WLQF(X,Y)=(X**2+Y**2)*SQRT(1.-(X+Y)**2/AM2)*(1.-(X+Y)**2/AM2)
C--
C--   INITIALIZE AMEPS
      IF (INIT.EQ.0) THEN
        INIT=1
        DO 10 I=1,9
          IPOIN=NAMPOI(EPSNAM(I))
          AMEPS(I)=MAX(PM(IPOIN)-PM(I)+GAMEPS,0.)
   10   IF ((I.GT.3).AND.(I.LT.9)) AMEPS(I)=AMEPS(I)+FLOAT(I-3)*0.15
        AMEPS(9)=2.*AMEPS(9)
      ENDIF
C--
C--   CALCULATE THE INDIVIDUAL DECAY WIDTHS AND CUMULATIVE DISTRIBUTION
      AM2=PM(IP)**2
      AM3=PM(IP)*AM2
      IFIRST=ABS(IDP(IP))
      IF (IFIRST.EQ.0) RETURN
      I=IFIRST
      WIDTH=0.
   20 AMSUM=0.
      WIDINI=DBR(I)
      DO 30 J=1,NDP(I)
        ID=IABS(IDC(J,I))
        AM(J)=PM(ICONV(ID))
        IF (ID.LE.9) AMSUM=AMSUM+AMEPS(ID)
   30 AMSUM=AMSUM+AM(J)
      IF (AMSUM.LT.PM(IP)) THEN
        COLOUR=1.
        IF (ID.LE.8) COLOUR=3.
        IF (IPC(IP).EQ.191) THEN
          IF (ID.EQ.198) THEN
            DBR(I)=WHIG0W(4.*AM(1)**2/AM2)
          ELSEIF (ID.EQ.199) THEN
            DBR(I)=WHIG0Z(4.*AM(1)**2/AM2)
          ELSE
            DBR(I)=COLOUR*WHIG0F(4.*AM(1)**2/AM2)
          ENDIF
        ELSEIF (IPC(IP).EQ.192) THEN
          DBR(I)=COLOUR*WHIGPF(AM(1),AM(2))*W2BODY(AM(1)**2/AM2,AM(2)**2
     &    /AM2)
        ELSEIF (IPC(IP).EQ.198) THEN
          X=AM(1)**2/AM2
          Y=AM(2)**2/AM2
          DBR(I)=COLOUR*WWBOSF(X,Y)*W2BODY(X,Y)
        ELSEIF (IPC(IP).EQ.199) THEN
          X=AM(1)**2/AM2
          IDD=ID
          IF (ID.GT.90) IDD=ID-90+8
          V=VZBOS(IDD)
          A=AZBOS(IDD)
          DBR(I)=COLOUR*(V**2+A**2)*WZBOSF((V/A)**2,X)*W2BODY(X,X)
        ELSEIF ((IPC(IP).GT.1000).AND.(IPC(IP).LT.1010)) THEN
          DBR(I)=WLQF(AM(1),AM(2))
        ELSE
          IF (NDP(I).NE.3) THEN
            DBR(I)=0.
          ELSE
            IC=IABS(IPC(IP))
            W=AM(1)**2/AM2
            X=(AM(2)+AM(3))**2/AM2
            XI=(MIN((PM(IP)-AM(1)-0.01),AMW))**2
            Y=AM(2)**2/XI
            Z=AM(3)**2/XI
            IF (IC/1000.GE.6.OR.((IC/100.GE.6).AND.(IC/100.LE.8))) THEN
              DBR(I)=COLOUR*W2BODY(W,X)*WWBOSF(Y,Z)*W2BODY(Y,Z)
            ELSEIF (IC.EQ.97) THEN
              DBR(I)=COLOUR*W2BODY(W,X)*WWBOSF(Y,Z)*W2BODY(Y,Z)
            ENDIF
          ENDIF
        ENDIF
      ELSE
        DBR(I)=0.
      ENDIF
      WIDTH=WIDTH+DBR(I)
      ILAST=I
      I=I+1
      IF (DBR(I).GE.(WIDINI-GAMEPS)) GOTO 20
C--
C--   WRITE INFO
      IF (IBRDMP.EQ.1) THEN
        WRITE(LUN2,9000)
        WRITE(LUN2,9010)
        WRITE(LUN2,9020) PNA(IP),PM(IP)
      ENDIF
      IF ((IPC(IP).GT.190).AND.(IPC(IP).LT.200)) THEN
        GAM(IP)=WIDTH
        IF (WIDTH.NE.0.) PLT(IP)=CHBAR/WIDTH
        IF (IBRDMP.EQ.1) WRITE(LUN2,9060) PNA(IP),WIDTH,PLT(IP)
      ENDIF
      IF (IBRDMP.EQ.1) THEN
        WRITE(LUN2,9030)
        WRITE(LUN2,9000)
      ENDIF
      IF (WIDTH.GT.0.) THEN
        DO 50 I=IFIRST,ILAST
          IF (WIDTH.NE.0.) DBR(I)=DBR(I)/WIDTH
          DO 40 J=1,NDP(I)
   40     CALL INDNAM(IDC(J,I),PNAM(J))
          IF (IBRDMP.EQ.1) THEN
            IF (NDP(I).EQ.2) THEN
              WRITE(LUN2,9040) PNA(IP),(PNAM(J),J=1,2),DBR(I)*100.
            ELSE
              WRITE(LUN2,9050) PNA(IP),(PNAM(J),J=1,3),DBR(I)*100.
            ENDIF
          ENDIF
   50   CONTINUE
      ELSE
        IF (IBRDMP.EQ.1) WRITE(LUN2,9070)
        IDP(IP)=0
      ENDIF
      IF (IBRDMP.EQ.1) THEN
        WRITE(LUN2,9000)
        WRITE(LUN2,9010)
      ENDIF
C--
C--   CALCULATE CUM. DISTRIBUTION
      IF (WIDTH.GT.0.) THEN
        DO 60 I=IFIRST+1,ILAST
   60   DBR(I)=DBR(I-1)+DBR(I)
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
 9040 FORMAT(1H ,13X,A8,' ---> ',A8,1X,A8,10X,': ',F8.4,' %')
 9050 FORMAT(1H ,13X,A8,' ---> ',A8,1X,A8,1X,A8,1X,': ',F8.4,' %')
 9060 FORMAT(1H0,6X,'The ',A8,' width reads: ',F7.5,' GeV. (Lifetime = '
     &,E10.4,' S.).')
 9070 FORMAT(1H ,9X,
     &'Beware, NO phase space allowed branching modes in decay table!')
      END
