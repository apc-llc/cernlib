*
* $Id: c318m.F,v 1.1.1.1 1996/04/01 15:01:15 mclareni Exp $
*
* $Log: c318m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:15  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C318M
C     This program tests the operation of MATHLIB subprograms
C     ELFUN and DELFUN

#include "imp64r.inc"
c     REAL RELI1C, RELIKC
      REAL         RELIKC
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2)

#include "gen/def64.inc"
     + Z0, Z1, Z2, X, K, XK, SN,CN,DN,SN0,CN0,DN0,SN1,CN1,DN1,
     + SNC,CNC,DNC, SNR, CNR,DNR
      PARAMETER (Z0 = 0, Z1 = 1, Z2 = 2)
      LOGICAL LTEST
#include "iorc.inc"
      DATA LTEST/.TRUE./
      DATA TOL/1D-6, 5D-14/

      CALL HEADER('C318',0)

#if defined(CERNLIB_DOUBLE)
      NF=2
#endif
#if !defined(CERNLIB_DOUBLE)
      NF=1
#endif
      DO 1000 JF=1,NF
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TEST FOR ELFUN'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TEST FOR DELFUN'')')
         ERRMAX= 0.0D0
C---  Test 1 -----------
      WRITE(LOUT,'(/10X,''---  Test 1 ---'')')
         WRITE(LOUT,'(/4X,''X'',5X,''K'',13X,''SN'',18X,''CN'',22X,
     1         ''DN'',14X,''SC'',6X,''SD'',15X,''Error'')')
      DO 1 IX = -4,4
      DO 2 IK = -4,4,4

                 CALL C318S1(IX,IK,JF,ERRMAX)
    2 CONTINUE
    1 CONTINUE
#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
      ERRMAX=0D0

C---  Test 2 -----------
      WRITE(LOUT,'(/10X,''---  Test 2 ---'')')
      WRITE(LOUT,'(/4X,''X'',5X,''K'',13X,''SN'',18X,''CN'',22X,
     1''DN'',14X,''SC'',6X,''SD'',15X,''Error'')')
      DO 3 IK = -4,4
      DO 4 IX = -4,4,4

                 CALL C318S1(IX,IK,JF,ERRMAX)

    4 CONTINUE
    3 CONTINUE
#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
C1000  CONTINUE
      ERRMAX=0D0

C---  Test 3 -----------
      WRITE(LOUT,'(/10X,''---  Test 3 ---'')')

      K=0.9D0
#if !defined(CERNLIB_DOUBLE)
      P=RELIKC(K)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1) THEN
                  RK=0.9E0
                  P=RELIKC(RK)
      ENDIF
      IF(JF.EQ.2) P=DELIKC(K)
#endif
      WRITE(LOUT,'(/5X,''X'',20X,''SN'',24X,''CN'',26X,
     1''DN'',13X,''Error'')')

                 CALL C318S2(P,K,JF,ERRMAX)

      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX

      ERRMAX=0D0

C---  Test 4 -----------
      WRITE(LOUT,'(/10X,''---  Test 4 ---'')')
      K=1/0.9D0
#if !defined(CERNLIB_DOUBLE)
      P=RELIKC(1/K)/K
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1) THEN
                  RK=1/0.9E0
                  P=RELIKC(1/RK)/RK
      ENDIF
      IF(JF.EQ.2) P=DELIKC(1/K)/K
#endif
      WRITE(LOUT,'(/5X,''X'',20X,''SN'',24X,''CN'',26X,
     1''DN'',13X,''Error'')')

                 CALL C318S2(P,K,JF,ERRMAX)

      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX

C---  Test 5 -----------
      WRITE(LOUT,'(/10X,''---  Test 5 ---'')')
      K=0
#if !defined(CERNLIB_DOUBLE)
      P=RELIKC(K)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1) THEN
                  RK=0E0
                  P=RELIKC(RK)
      ENDIF
      IF(JF.EQ.2) P=DELIKC(K)
      WRITE(LOUT,'(/4X,''X'',14X,''Error'')')
      DO 7 IX=-205,195,100
      X=P*IX/50


#endif
#if !defined(CERNLIB_DOUBLE)
                  CALL ELFUN(X,Z0,SN0,CN0,DN0)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1) THEN
                  RX=X
                  RZ0=Z0
                  CALL ELFUN(RX,RZ0,RSN0,RCN0,RDN0)
                  SN0=RSN0
                  CN0=RCN0
                  DN0=RDN0
      ENDIF
      IF(JF.EQ.2) CALL DELFUN(X,Z0,SN0,CN0,DN0)

       ES=SN0-SIN(X)
       EC=CN0-COS(X)
       ED=DN0-1D0
      ERRMAX=MAX(ABS(ES),ABS(EC),ABS(ED),ERRMAX)
      WRITE(LOUT,'(1X,F6.2,3(1P,D9.1))') X,ABS(ES),ABS(EC),ABS(ED)
    7 CONTINUE
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX

C---  Test 6 -----------
      WRITE(LOUT,'(/10X,''---  Test 6 ---'')')
      K=0
#endif
#if !defined(CERNLIB_DOUBLE)
      P=RELIKC(K)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1) THEN
                  RK=0E0
                  P=RELIKC(RK)
      ENDIF
      IF(JF.EQ.2) P=DELIKC(K)
      WRITE(LOUT,'(/4X,''X'',14X,''Error'')')
      DO 8 IX=-205,195,100
      X=P*IX/50

#endif
#if !defined(CERNLIB_DOUBLE)
                  CALL ELFUN(X,Z1,SN1,CN1,DN1)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1) THEN
                  RX=X
                  RZ1=Z1
                  CALL ELFUN(RX,RZ1,RSN1,RCN1,RDN1)
                  SN1=RSN1
                  CN1=RCN1
                  DN1=RDN1
      ENDIF
      IF(JF.EQ.2) CALL DELFUN(X,Z1,SN1,CN1,DN1)
#endif
       ES=SN1-TANH(X)
       EC=CN1-(1D0/COSH(X))
       ED=DN1-(1D0/COSH(X))
      ERRMAX=MAX(ABS(ES),ABS(EC),ABS(ED),ERRMAX)
      WRITE(LOUT,'(1X,F6.2,3(1P,D9.1))') X,ABS(ES),ABS(EC),ABS(ED)
    8 CONTINUE
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX

1000  CONTINUE


C     Check if the test was successful
       IRC=ITEST('C318',LTEST)
      CALL PAGEND('C318')

      END

      SUBROUTINE C318S1(IX,IK,JF,ERRMAX)

#include "imp64r.inc"
c     REAL RELI1C, RELIKC
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2)

cSEQ,DEF64.
c    + Z0, Z1, Z2, X, K, XK, SN,CN,DN,SN0,CN0,DN0,SN1,CN1,DN1,
c    + SNC,CNC,DNC, SNR, CNR,DNR
      PARAMETER (Z0 = 0, Z1 = 1, Z2 = 2)
      LOGICAL LTEST
#include "iorc.inc"
      DATA LTEST/.TRUE./
C     Set maximum error allowed for test to be considered successful
      DATA TOL/1D-6, 1D-15/

      X=IX/Z2
      K=IK/Z2
      XK=K
#if !defined(CERNLIB_DOUBLE)
                  CALL  ELFUN(X,XK**2,SN,CN,DN)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1) THEN
                  RX=X
                  RXK=XK
                  CALL  ELFUN(RX,RXK**2,RSN,RCN,RDN)
                  SN=RSN
                  CN=RCN
                  DN=RDN
                  XK=RXK
      ENDIF
      IF(JF.EQ.2) CALL DELFUN(X,XK**2,SN,CN,DN)
#endif
      SC=SN**2+CN**2-1
      SD=(XK*SN)**2+DN**2-1
      ERRMAX=MAX(ABS(SC),ABS(SD),ERRMAX)
      DXX=1-XK**2
      IF(DXX.GE.0) THEN
        DA1=X
        RA1=RX
        DA2=1-XK**2
        RA2=1-RXK**2
      ELSE
        DA1=XK*X
        RA1=RXK*RX
        DA2=1-1/XK**2
        RA2=1-1/RXK**2
      ENDIF
#if !defined(CERNLIB_DOUBLE)
                   CALL DSCDN(DA1,DA2,SNC,CNC,DNC)
#endif
#if defined(CERNLIB_DOUBLE)
       IF(JF.EQ.1) THEN
                   CALL RSCDN(RA1,RA2,RSNC,RCNC,RDNC)
                   SNC=RSNC
                   CNC=RCNC
                   DNC=RDNC
       ENDIF
       IF(JF.EQ.2) CALL DSCDN(DA1,DA2,SNC,CNC,DNC)
#endif
       IF(DXX.LT.0) THEN
         SNC=SNC/XK
         CNT=DNC
         DNC=CNC
         CNC=CNT
       ENDIF
       ES=SN-SNC
       EC=CN-CNC
       ED=DN-DNC
       ERRMAX=MAX(ABS(ES),ABS(EC),ABS(ED),ERRMAX)
      WRITE(LOUT,'(1X,2F6.2,3F22.16,5(1P,D9.1))')
     +    X,XK,SN,CN,DN,ABS(SC),ABS(SD),ABS(ES),ABS(EC),ABS(ED)
      RETURN
       END



      SUBROUTINE C318S2(P,K,JF,ERRMAX)

#include "imp64r.inc"
c     REAL RELI1C, RELIKC
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2)

#include "gen/def64.inc"
     + Z0, Z1, Z2, X, K, XK, SN,CN,DN,SN0,CN0,DN0,SN1,CN1,DN1,
     + SNC,CNC,DNC, SNR, CNR,DNR
      PARAMETER (Z0 = 0, Z1 = 1, Z2 = 2)
      LOGICAL LTEST
#include "iorc.inc"
      DATA TOL/1D-6, 1D-15/
      DATA LTEST/.TRUE./

      DO 55 IX=-205,195,100
      X=P*IX/50
#if !defined(CERNLIB_DOUBLE)
                  CALL ELFUN(X,K**2,SN,CN,DN)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1) THEN
                  RX=X
                  RK=K
                  CALL ELFUN(RX,RK**2,RSN,RCN,RDN)
                  SN=RSN
                  CN=RCN
                  DN=RDN
      ENDIF
      IF(JF.EQ.2) CALL DELFUN(X,K**2,SN,CN,DN)
#endif
      SC=SN**2+CN**2-1
      SD=(K*SN)**2+DN**2-1
      ERRMAX=MAX(ABS(SC),ABS(SD),ERRMAX)
      WRITE(LOUT,'(1X,F12.8,3F25.16,2(1P,D9.1))') X,SN,CN,DN,
     +                                            ABS(SC),ABS(SD)
   55 CONTINUE
      RETURN
      END

      SUBROUTINE DSCDN(X,AKP2,SN,CN,DN)

C     BASED ON ALGOL PROCEDURE SNCNDN(X,MC,SN,CN,DN) IN
C      R. BULIRSCH, Numerical Calculation of Elliptic Integrals and
C      Elliptic Functions,  Numer. Math. 7 (1965) 78-90

#include "gen/imp64.inc"
      LOGICAL LBO

      DIMENSION XM(0:12),XN(0:12)

      PARAMETER (ID = 16)
      PARAMETER (Z1 = 1, Z10 = 10, HF = Z1/2)
      PARAMETER (CA = Z10**(-ID/2))

#if defined(CERNLIB_VAXVMS)
      DATA AMC0 /-1D20/
#endif
#if !defined(CERNLIB_VAXVMS)
      DATA AMC0 /-1D74/
#endif

      SAVE AMC0,XM,XN,C,L

      IF(AKP2 .EQ. 0) THEN
       SN=TANH(X)
       CN=1/COSH(X)
       DN=CN
      ELSEIF(AKP2 .EQ. 1) THEN
       SN=SIN(X)
       CN=COS(X)
       DN=1
      ELSE
       XX=X
       YMC=AKP2
       LBO=YMC .LT. 0
       IF(LBO) THEN
        D=1-YMC
        YMC=-YMC/D
        D=SQRT(D)
        XX=D*XX
       ENDIF
       IF(AKP2 .EQ. AMC0) GO TO 2
       AMC0=AKP2
       A=1
       DO 1 I = 0,12
       L=I
       XM(I)=A
       YMC=SQRT(YMC)
       XN(I)=YMC
       C=HF*(A+YMC)
       IF(ABS(A-YMC) .LE. CA*A) GO TO 2
       YMC=A*YMC
       A=C
    1  CONTINUE
    2  XX=C*XX
       SN=SIN(XX)
       CN=COS(XX)
       DN=1
       IF(SN .NE. 0) THEN
        A=CN/SN
        C1=A*C
        DO 3 I = L,0,-1
        A=C1*A
        C1=DN*C1
        DN=(XN(I)+A)/(XM(I)+A)
        A=C1/XM(I)
    3   CONTINUE
        SN=SIGN(1/SQRT(C1**2+1),SN)
        CN=C1*SN
       ENDIF
       IF(LBO) THEN
        A=DN
        DN=CN
        CN=A
        SN=SN/D
       ENDIF
      ENDIF
      RETURN
      END
#if defined(CERNLIB_DOUBLE)
      SUBROUTINE RSCDN(X,AKP2,SN,CN,DN)
#include "gen/def64.inc"
     +        DSN,DCN,DDN,D

      SROUND(D)=D+(D-DBLE(SNGL(D)))
      CALL DSCDN(DBLE(X),DBLE(AKP2),DSN,DCN,DDN)
      SN=SROUND(DSN)
      CN=SROUND(DCN)
      DN=SROUND(DDN)
      RETURN
      END
#endif
