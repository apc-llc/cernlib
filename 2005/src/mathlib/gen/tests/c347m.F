*
* $Id: c347m.F,v 1.1.1.1 1996/04/01 15:01:19 mclareni Exp $
*
* $Log: c347m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:19  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C347M
C     This program tests the operation of GENLIB subprograms
C     RELI1C, RELI2C, RELI3C ,
C     DELI1C, DELI2C, DELI3C ,
C     RELIGC, DELIGC, RELIKC, RELIEC, DELIKC, and DELIEC  (C347)

#include "imp64r.inc"
      REAL RELI1C, RELI2C, RELI3C
      REAL RELIGC, RELIKC, RELIEC,GAUSS
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2)

#include "gen/def64.inc"
     + Z0, Z1, Z2, EPS,EPS2,EPS3,AKP,A,B
      EXTERNAL FF1EL1,FF2EL1,FF1EL2,FF2EL2,FF1EL3,FF2EL3
      EXTERNAL GAUSS,DGAUSS
      COMMON /PARAMT/ AKP,A,B,P,FMODE
      LOGICAL LTEST
      PARAMETER (Z0=0, Z1=1, Z2=2, EPS=1D-14, EPS2=1E-10, EPS3=1D-11)
#include "iorc.inc"
      DATA LTEST/.TRUE./
      DATA TOL/1D-5, 8D-12/

      CALL HEADER('C347',0)
         ERRMAX= 0.0D0
#if defined(CERNLIB_DOUBLE)
      NF=2
#endif
#if !defined(CERNLIB_DOUBLE)
      NF=1
#endif
      DO 1000 JF=1,NF

C---  Test 1 -----------
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR RELI1C'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TEST FOR RELI1C'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TEST FOR DELI1C'')')
#endif
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
#if !defined(CERNLIB_DOUBLE)
      DR=RELI1C(Z0)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RZ0=Z0
                 DR=RELI1C(RZ0)
      ENDIF
      IF(JF.EQ.2)DR=DELI1C(Z0)
#endif
      WRITE(LOUT,'(1X)')
      WRITE(LOUT,'(/5X,''AKP'',6X,''CALCULATED'',
     + 10X,''TEST'',9X,''Rel. Error'')')
      DO 11 K = 1,101,20
      AKP=K/10D0
#if !defined(CERNLIB_DOUBLE)
      DR=RELI1C(AKP)
      T=GAUSS(FF1EL1,Z0,Z1,EPS)+GAUSS(FF2EL1,Z0,Z1,EPS)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                  RAKP=AKP
                  DR=RELI1C(RAKP)
                  AKP=RAKP
      ENDIF
      IF(JF.EQ.2) DR=DELI1C(AKP)
      T=DGAUSS(FF1EL1,Z0,Z1,EPS)+DGAUSS(FF2EL1,Z0,Z1,EPS)
#endif
         ERRMAX= MAX(ERRMAX,ABS((DR-T)/DR))
      WRITE(LOUT,'(1X,F6.1,2F20.15,1P,D10.1)') AKP,DR,T,ABS((DR-T)/DR)
   11 CONTINUE
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

#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR RELI2C'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TEST FOR RELI2C'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TEST FOR DELI2C'')')
#endif
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
      FMODE=1
#if !defined(CERNLIB_DOUBLE)
      DR=RELI2C(Z0,Z1,Z1)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RZ0=Z0
                 RZ1=Z1
                 DR=RELI2C(RZ0,RZ1,RZ1)
      ENDIF
      IF(JF.EQ.2)DR=DELI2C(Z0,Z1,Z1)
#endif
      WRITE(LOUT,'(1X)')
      WRITE(LOUT,'(/4X,''AKP'',
     + 4X,''A'',5X,''B'',5X,''CALCULATED'',
     + 10X,''TEST'',10X,''Rel. Error'')')
      DO 12 IA = 2,3
      A=IA
      DO 12 IB = 0,2
      B=IB
      DO 12 K = 0,100,25
      AKP=K/10D0
      IF(AKP .EQ. 0 .AND. B .NE. 0) GO TO 12
#if !defined(CERNLIB_DOUBLE)
      DR=RELI2C(AKP,A,B)
      T=GAUSS(FF1EL2,Z0,Z1,EPS)+GAUSS(FF2EL2,Z0,Z1,EPS)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RAKP=AKP
                 RA=A
                 RB=B
                 DR=RELI2C(RAKP,RA,RB)
      ENDIF
      IF(JF.EQ.2) DR=DELI2C(AKP,A,B)
      T=DGAUSS(FF1EL2,Z0,Z1,EPS)+DGAUSS(FF2EL2,Z0,Z1,EPS)
#endif
         ERRMAX= MAX(ERRMAX,ABS((DR-T)/DR))
      WRITE(LOUT,'(1X,3F6.1,2F20.15,1P,D10.1)')AKP,A,B,DR,T,
     +                                         ABS((DR-T)/DR)
   12 CONTINUE

#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0.0D0

C---  Test 3 -----------
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR RELI3C'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TEST FOR RELI3C'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TEST FOR DELI3C'')')
#endif
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
#if !defined(CERNLIB_DOUBLE)
      DR=RELI3C(Z0,Z1,Z1)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RZ0=Z0
                 RZ1=Z1
                 DR=RELI3C(RZ0,RZ1,RZ1)
      ENDIF
      IF(JF.EQ.2)DR=DELI3C(Z0,Z1,Z1)
#endif
      WRITE(LOUT,'(1X)')
      WRITE(LOUT,'(/4X,''AKP'',
     + 4X,''P'',6X,''CALCULATED'',
     + 12X,''TEST'',7X,''Rel. Error'')')
#if defined(CERNLIB_DOUBLE)
      DO 13 IP = -3,3,6
#endif
#if !defined(CERNLIB_DOUBLE)
      DO 13 IP = -2,3,6
#endif
      P=IP
      DO 13 K = 1,101,25
      AKP=K/10D0
      AM=1-AKP**2
#if !defined(CERNLIB_DOUBLE)
      DR=RELI3C(AKP,AM,P)
      IF(P .GT. 0) THEN
       T=GAUSS(FF1EL3,Z0,Z1,EPS)+GAUSS(FF2EL3,Z0,Z1,EPS)
      ELSE
       S=1/SQRT(-P)
       T=CAUCHY(FF1EL3,Z0,Z1,S,EPS2)+GAUSS(FF2EL3,Z0,Z1,EPS)
      ENDIF
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RAKP=AKP
                 RAM=AM
                 RP=P
                 DR=RELI3C(RAKP,RAM,RP)
                 AKP=RAKP
                 AM=RAM
                 P=RP
      ENDIF
      IF(JF.EQ.2) DR=DELI3C(AKP,AM,P)
      IF(P .GT. 0) THEN
       T=DGAUSS(FF1EL3,Z0,Z1,EPS)+DGAUSS(FF2EL3,Z0,Z1,EPS)
      ELSE
       S=1/SQRT(-P)
#endif
#if (!defined(CERNLIB_UNIX))&&(defined(CERNLIB_DOUBLE))
       T=DCAUCH(FF1EL3,Z0,Z1,S,EPS)+DGAUSS(FF2EL3,Z0,Z1,EPS)
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_DOUBLE))
       T=DCAUCH(FF1EL3,Z0,Z1,S,EPS2)+DGAUSS(FF2EL3,Z0,Z1,EPS)
#endif
#if defined(CERNLIB_DOUBLE)
      ENDIF
#endif
         ERRMAX= MAX(ERRMAX,ABS((DR-T)/DR))
      WRITE(LOUT,'(1X,2F6.1,2F20.15,1P,D10.1)')AKP,P,DR,T,
     +                                         ABS((DR-T)/DR)
   13 CONTINUE


#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0.0D0

C---  Test 4 -----------
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR RELIGC'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TEST FOR RELIGC'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TEST FOR DELIGC'')')
#endif
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
#if !defined(CERNLIB_DOUBLE)
      DR=RELIGC(Z0,Z1,Z1,Z1)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RZ0=Z0
                 RZ1=Z1
                 DR=RELIGC(RZ0,RZ1,RZ1,RZ1)
      ENDIF
      IF(JF.EQ.2)DR=DELIGC(Z0,Z1,Z1,Z1)
#endif
      WRITE(LOUT,'(1X)')
      WRITE(LOUT,'(/10X,
     + ''CALCULATED'',
     + 18X,''TEST'',9x,''Rel. Error'')')
      T1= 1.5464442694017956D0
      T2=-6.7687378198360556D-1
#if !defined(CERNLIB_DOUBLE)
      DR1=RELIGC(0.1D0, 4.1D0,1.2D0,1.1D0)
      DR2=RELIGC(0.1D0,-4.1D0,1.2D0,1.1D0)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
      DR1=RELIGC(0.1E0, 4.1E0,1.2E0,1.1E0)
      DR2=RELIGC(0.1E0,-4.1E0,1.2E0,1.1E0)
      ENDIF
      IF(JF.EQ.2)THEN
      DR1=DELIGC(0.1D0, 4.1D0,1.2D0,1.1D0)
      DR2=DELIGC(0.1D0,-4.1D0,1.2D0,1.1D0)
      ENDIF
#endif
         ERRMAX = MAX(ERRMAX,ABS((DR1-T1)/DR1),ABS((DR2-T2)/DR2))
      WRITE(LOUT,'(1X,2F25.15,1P,D10.1)')DR1,T1,ABS((DR1-T1)/DR1)
      WRITE(LOUT,'(1X,2F25.15,1P,D10.1)')DR2,T2,ABS((DR2-T2)/DR2)

      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0.0D0

C---  Test 5 -----------
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TESTS FOR RELIKC and RELIEC'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TESTS FOR RELIKC and RELIEC'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TESTS FOR DELIKC and DELIEC'')')
#endif
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
#if !defined(CERNLIB_DOUBLE)
      DR=RELIKC(Z1)
      DR=RELIKC(Z2)
      DR=RELIEC(Z2)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RZ1=Z1
                 RZ2=Z2
      R=RELIKC(RZ1)
      R=RELIKC(RZ2)
      R=RELIEC(RZ2)
      ENDIF
      IF(JF.EQ.2)THEN
      DR=DELIKC(Z1)
      DR=DELIKC(Z2)
      DR=DELIEC(Z2)
      ENDIF
#endif
      WRITE(LOUT,'(/13X,
     + ''CALCULATED'',
     + 21X,''TEST'',10X,''Rel. Error'')')
      T1=1.6520896063577719D0
      T2=1.3948418087767893D0
#if !defined(CERNLIB_DOUBLE)
      DR1=RELIKC(0.43D0)
      DR2=RELIEC(0.64D0)
      ERRMAX= MAX(ERRMAX,ABS((DR1-T1)/DR1),ABS((DR2-T2)/DR2))
      WRITE(LOUT,'(1X,2F25.15,1P,D10.1)') DR1,T1,ABS((DR1-T1)/DR1)
      WRITE(LOUT,'(1X,2F25.15,1P,D10.1)') DR2,T2,ABS((DR2-T2)/DR2)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
      RT1=T1
      RT2=T2
      DR1=RELIKC(0.43E0)
      DR2=RELIEC(0.64E0)
      ERRMAX= MAX(ERRMAX,ABS((DR1-RT1)/DR1),ABS((DR2-RT2)/DR2))
      WRITE(LOUT,'(1X,2F25.15,1P,E10.1)') DR1,RT1,ABS((DR1-RT1)/DR1)
      WRITE(LOUT,'(1X,2F25.15,1P,E10.1)') DR2,RT2,ABS((DR2-RT2)/DR2)
      ENDIF
      IF(JF.EQ.2)THEN
      DR1=DELIKC(0.43D0)
      DR2=DELIEC(0.64D0)
      ERRMAX= MAX(ERRMAX,ABS((DR1-T1)/DR1),ABS((DR2-T2)/DR2))
      WRITE(LOUT,'(1X,2F25.15,1P,D10.1)') DR1,T1,ABS((DR1-T1)/DR1)
      WRITE(LOUT,'(1X,2F25.15,1P,D10.1)') DR2,T2,ABS((DR2-T2)/DR2)
      ENDIF

#endif
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0.0D0
 1000 CONTINUE
C     Check if the test was successful
      IRC=ITEST('C347',LTEST)
      CALL PAGEND('C347')
      END

      FUNCTION FC347(T)
#include "gen/imp64.inc"
      COMMON /PARAMT/ AKP,A,B,P,FMODE

      ENTRY FF1EL1(T)
      FF1EL1=1/SQRT((1+T**2)*(1+(AKP*T)**2))
      RETURN

      ENTRY FF2EL1(T)
      FF2EL1=1/SQRT((1+T**2)*(AKP**2+T**2))
      RETURN

      ENTRY FF1EL2(T)
      FF1EL2=(A+B*T**2)/((1+T**2)*SQRT((1+T**2)*(1+FMODE*(AKP*T)**2)))
      RETURN

      ENTRY FF2EL2(T)
      FF2EL2=(B+A*T**2)/((T**2+1)*SQRT((1+T**2)*(FMODE*AKP**2+T**2)))
      RETURN

      ENTRY FF1EL3(T)
      FF1EL3=(1+T**2)/((1+P*T**2)*SQRT((1+T**2)*(1+(AKP*T)**2)))
      RETURN

      ENTRY FF2EL3(T)
      FF2EL3=(1+T**2)/((T**2+P)*SQRT((1+T**2)*(AKP**2+T**2)))
      RETURN
      END
