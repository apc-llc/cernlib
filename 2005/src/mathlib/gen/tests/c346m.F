*
* $Id: c346m.F,v 1.1.1.1 1996/04/01 15:01:19 mclareni Exp $
*
* $Log: c346m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:19  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C346M
C     This program tests the operation of MATHLIB subprograms
C     RELI1, RELI2, RELI3 and
C     DELI1, DELI2, DELI3 (C346)

#include "imp64r.inc"
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2)
      REAL GAUSS
      EXTERNAL F1EL1,F1EL2
#if defined(CERNLIB_DOUBLE)
      EXTERNAL R1EL1,R1EL2
      COMMON /RPARAMT/ RAKP,RA,RB,RP,RFMODE
#endif
      COMMON /PARAMT/ AKP,A,B,P,FMODE
      LOGICAL LTEST
      PARAMETER (Z0 = 0, Z1 = 1, EPS=1D-14, REPS=1E-8)
      DIMENSION X3(20),AKP3(20),P3(20),EL3(20)
#if defined(CERNLIB_DOUBLE)
      REAL      RX3(20),RAKP3(20),RP3(20),REL3(20)
#endif
#include "iorc.inc"

      DATA TOL/1D-5, 5D-13/
      DATA LTEST/.TRUE./
C     Input parameters for individual tests
      DATA (X3(I),AKP3(I),P3(I),EL3(I),I=1,10)
     1/ 1.3D+00, 0.11D+00, 4.21     D+00, 6.6220 7858 4701 5254D-01,
     2  1.3D+00, 0.11D+00, 0.82     D+00, 1.1307 0464 4207 4609D+00,
     3  1.3D+00, 0.92D+00, 0.71     D+00, 1.0058 2862 6697 7115D+00,
     4  1.3D+00, 0.92D+00, 0.23     D+00, 1.1884 0708 2334 5123D+00,
     5  1.3D+00, 0.12D+00,-0.11     D+00, 1.7259 6503 5534 8878D+00,
     6  1.3D+00, 0.12D+00,-2.11     D+00, 2.4416 8145 2072 1179D-01,
     7  1.3D+00, 0.40D+00, 0.1600001D+00, 1.4004 1652 5836 6944D+00,
     8  1.3D+00, 1.00D-10, 0.82     D+00, 1.1341 5053 9528 2723D+00,
     9  1.3D-10, 1.00D-10, 1.00     D-10, 1.3000 0000 0000 0000D-10,
     A  1.6D+00, 1.90D+00, 9.81     D+00, 3.8572 3243 7996 7252D-01/
      DATA (X3(I),AKP3(I),P3(I),EL3(I),I=11,20)
     B/ 1.6D+00, 1.90D+00, 1.22     D+00, 7.6656 1793 1195 6402D-01,
     C  1.6D+00, 1.90D+00, 0.87     D+00, 8.3210 5911 1261 8096D-01,
     D  1.6D+00, 1.90D+00, 0.21     D+00, 1.0521 2722 2190 6806D+00,
     E  1.6D+00, 1.90D+00,-0.21     D+00, 1.4730 4398 1995 4361D+00,
     F  1.6D+00, 1.90D+00,-4.30     D+00, 2.5476 9513 9719 3611D-01,
     G  1.6D+00, 1.01D+01,-1.00     D-05, 3.9501 7098 2164 9139D-01,
     H  1.6D+00, 1.50D+00, 2.24999  D+00, 7.0057 4316 8835 7934D-01,
     I  1.6D+00, 1.00D+10, 1.20     D+00, 2.3734 7746 6977 2208D-09,
     J -1.6D+00, 1.00D+10, 1.20     D+00,-2.3734 7746 6977 2208D-09,
     K  1.0D+00, 0.31D+00, 9.90     D-02, 1.0903 5779 2177 7398D+00/


      CALL HEADER('C346',0)
#if defined(CERNLIB_DOUBLE)
      NF=2
#endif
#if !defined(CERNLIB_DOUBLE)
      NF=1
#endif
      DO 1000 JF=1,NF
C---  Test 1 -----------
         ERRMAX= 0.0D0
      WRITE(LOUT,'(1X)')
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR RELI1'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TEST FOR RELI1'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TEST FOR DELI1'')')
#endif
      WRITE(LOUT,'(/7X,''X'',6X,''AKP'',10X,''CALCULATED'',
     + 20X,''TEST'',8X,''Rel. Error'')')
      WRITE(LOUT,'(1X)')
      DO 1 J = 1,101,20
      X=J/10D0
      DO 1 K = 0,101,20
      AKP=K/10D0
#if !defined(CERNLIB_DOUBLE)
      DR=RELI1(X,AKP)
      T=GAUSS(F1EL1,Z0,X,EPS)
         ERRMAX= MAX(ERRMAX,ABS((DR-T)/DR))
      WRITE(LOUT,'(1X,2F8.1,2F23.18,1P,D10.1)') X,AKP,DR,T,
     +                                          ABS((DR-T)/DR)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF .EQ. 1) THEN
                    RX=X
                    RAKP=AKP
                    RZ0=Z0
                    RDR=RELI1(RX,RAKP)
                    RT=GAUSS(R1EL1,RZ0,RX,REPS)
         ERRMAX= MAX(SNGL(ERRMAX),ABS((RDR-RT)/RDR))
      WRITE(LOUT,'(1X,2F8.1,2F23.9,1P,E10.1)') RX,RAKP,RDR,RT,
     +                                          ABS((RDR-RT)/RDR)
                    X=RX
                    AKP=RAKP
      ENDIF
      IF(JF .EQ. 2) THEN
      DR=DELI1(X,AKP)
      T=DGAUSS(F1EL1,Z0,X,EPS)
         ERRMAX= MAX(ERRMAX,ABS((DR-T)/DR))
      WRITE(LOUT,'(1X,2F8.1,2F23.18,1P,D10.1)') X,AKP,DR,T,
     +                                          ABS((DR-T)/DR)
      ENDIF
#endif
    1 CONTINUE
#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif

      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0.0D0

C---  Test 2 -----------
      WRITE(LOUT,'(1X)')
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR RELI2'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TEST FOR RELI2'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TEST FOR DELI2'')')
#endif
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
#if !defined(CERNLIB_DOUBLE)
      DR=RELI2(Z1,Z0,Z1,Z1,1)
c     DR=RELI2(Z1,Z1,Z1,Z1,-1)
      DR=RELI2(Z1,2*Z1,Z1,Z1,0)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1) THEN
                   RZ1=Z1
                   RZ0=Z0
                   DR=RELI2(RZ1,RZ0,RZ1,RZ1,1)
c                  DR=RELI2(RZ1,RZ1,RZ1,RZ1,-1)
                   DR=RELI2(RZ1,2*RZ1,RZ1,RZ1,0)
      ENDIF
      IF(JF.EQ.2) THEN
      DR=DELI2(Z1,Z0,Z1,Z1,1)
c     DR=DELI2(Z1,Z1,Z1,Z1,-1)
      DR=DELI2(Z1,2*Z1,Z1,Z1,0)
      ENDIF
#endif
      WRITE(LOUT,'(/4X,''IFM'',2X,''X'',5X,''AKP'',
     + 4X,''A'',5X,''B'',5X,''CALCULATED'',
     + 10X,''TEST'',9X,''Rel. Error'')')
      DO 2 IFM = 1,-1,-2
      WRITE(LOUT,'(1X)')
      FMODE=IFM
      RFMODE=IFM
      DO 2 IA = 2,3
      A=-IA
      DO 2 IB = 0,2
      B=IB
      DO 2 J = 5,50,45
      X=J/10D0
      DO 2 K = 5,50,45
      AKP=K/10D0
      IF(1+FMODE*(AKP*X)**2 .LE. 0) GO TO 2
#if !defined(CERNLIB_DOUBLE)
      DR=RELI2(X,AKP,A,B,IFM)
                 T= GAUSS(F1EL2,Z0,X,EPS)
         ERRMAX= MAX(ERRMAX,ABS((DR-T)/DR))
      WRITE(LOUT,'(1X,I5,4F6.1,2F20.15,1P,D10.1)')
     1                           IFM,X,AKP,A,B,DR,T,ABS((DR-T)/DR)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RX=X
                 RAKP=AKP
                 RA=A
                 RB=B
                 RZ0=Z0
                 RDR=RELI2(RX,RAKP,RA,RB,IFM)
                 RT= GAUSS(R1EL2,RZ0,RX,REPS)
         ERRMAX= MAX(SNGL(ERRMAX),ABS((RDR-RT)/RDR))
      WRITE(LOUT,'(1X,I5,4F6.1,2F20.7,1P,E10.1)')
     1         IFM,RX,RAKP,RA,RB,RDR,RT,ABS((RDR-RT)/RDR)
                 X=RX
                 AKP=RAKP
                 A=RA
                 B=RB
      ENDIF
      IF(JF.EQ.2)THEN
      DR=DELI2(X,AKP,A,B,IFM)
      T=DGAUSS(F1EL2,Z0,X,EPS)
         ERRMAX= MAX(ERRMAX,ABS((DR-T)/DR))
      WRITE(LOUT,'(1X,I5,4F6.1,2F20.15,1P,D10.1)')
     1                           IFM,X,AKP,A,B,DR,T,ABS((DR-T)/DR)
      ENDIF
#endif
    2 CONTINUE
#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D10.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0.0D0

C---  Test 3 -----------
      WRITE(LOUT,'(1X)')
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR RELI3'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TEST FOR RELI3'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TEST FOR DELI3'')')
#endif
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
#if !defined(CERNLIB_DOUBLE)
      DR=RELI3(Z1,Z1,-Z1)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1) THEN
                  RZ1=Z1
                 RDR=RELI3(RZ1,RZ1,-RZ1)
      ENDIF
      IF(JF.EQ.2)DR=DELI3(Z1,Z1,-Z1)
#endif
      WRITE(LOUT,'(/7X,''X'',7X,''AKP'',8X,''P'',7X,''CALCULATED'',
     + 10X,''TEST'',8X,''Rel. Error''/)')
      DO 3 I = 1,20
#if !defined(CERNLIB_DOUBLE)
      DR=RELI3(X3(I),AKP3(I),P3(I))
      ERRMAX= MAX(ERRMAX,ABS((DR-EL3(I))/EL3(I)))
      WRITE(LOUT,'(1X,1P,D10.1,D10.2,D10.2,2D18.10,
     +1P,D10.1)') X3(I),
     +AKP3(I),P3(I),DR,EL3(I),ABS((DR-EL3(I))/EL3(I))
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1 .AND. I .NE. 9) THEN
                  RX3(I)=X3(I)
                  RAKP3(I)=AKP3(I)
                  RP3(I)=P3(I)
                  REL3(I)=EL3(I)
                 RDR=RELI3(RX3(I),RAKP3(I),RP3(I))
         ERRMAX= MAX(SNGL(ERRMAX),ABS((RDR-REL3(I))/REL3(I)))
      WRITE(LOUT,'(1X,1P,E10.1,E10.2,E10.2,2E18.7,
     +1P,E10.1)') RX3(I),
     +RAKP3(I),RP3(I),RDR,REL3(I),ABS((RDR-REL3(I))/REL3(I))
      ENDIF
      IF(JF.EQ.2 .AND. I .NE. 9)THEN
      DR=DELI3(X3(I),AKP3(I),P3(I))
      ERRMAX= MAX(ERRMAX,ABS((DR-EL3(I))/EL3(I)))
      WRITE(LOUT,'(1X,1P,D10.1,D10.2,D10.2,2D18.10,
     +1P,D10.1)') X3(I),
     +AKP3(I),P3(I),DR,EL3(I),ABS((DR-EL3(I))/EL3(I))
      ENDIF
#endif
    3 CONTINUE
#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/''Largest Relative Error was'',
     +1P,D10.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0.0D0
1000  CONTINUE
C     Check if the test was successful
      IRC=ITEST('C346',LTEST)
      CALL PAGEND('C346')
      END
