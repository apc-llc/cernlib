*
* $Id: c333m.F,v 1.1.1.1 1996/04/01 15:01:17 mclareni Exp $
*
* $Log: c333m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:17  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C333M
C     This program tests the operation of MATHLIB subprograms
C     CLOGAM and WLOGAM (C333)

#include "gen/defc64.inc"
     +  WLOGAM
#include "gen/defc64.inc"
     +  Z,I,R,T
      COMPLEX CLOGAM,ZS
#include "gen/def64.inc"
     +      X,Y,HALF,PI,ERRMAX
      CHARACTER NAME*6
      DIMENSION MU(3)
      DIMENSION X(3),Y(3)

C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2)
      LOGICAL LTEST
#include "iorc.inc"
      DATA LTEST/.TRUE./
      DATA TOL/5D-14, 5D-6/

      DATA I /(0  ,1  )/, HALF /0.5D0/
      DATA MU /1,30,100/
      DATA X /0.1D0,0.1D0,1D0/, Y /1D0,1D0,0.2D0/

      CALL HEADER('C333',0)
      ERRMAX=0D0
      PI = 3.14159 26535 89793D0
C---  Number of functions to test
#if !defined(CERNLIB_DOUBLE)
      NF=2
#endif
#if defined(CERNLIB_DOUBLE)
      NF=1
#endif
C
      DO 9 IDS = NF,2
      IF(IDS .EQ. 1) NAME='WLOGAM'
      IF(IDS .EQ. 2) NAME='CLOGAM'
c     DO 1 M = 1,3   ********* M=2 and M=3 ====>underflow on VM
      DO 1 M = 1,1
      WRITE(LOUT,100) NAME
      DO 1 J = 1,50
c     The following condition isjust to avoid underflows on VM
      if (m .eq. 2 .and. ids .eq. 2 .and. j .ge. 16)go to 1
      Z=MU(M)*((RANF()-HALF)+I*(RANF()-HALF))
      ZS=Z
      IF(NAME .EQ. 'WLOGAM') THEN
       R=WLOGAM(2*Z)
       T=WLOGAM(Z)+WLOGAM(Z+HALF)+(2*Z-1)*LOG(2D0)-LOG(PI)/2
      WRITE(LOUT,'(1X,2F10.4,2D25.16,1P,2D12.1)') ZS,R,ABS((T-R)/T)
      ELSE
       R=CLOGAM(2*ZS)
       T=CLOGAM(ZS)+CLOGAM(ZS+0.5)+(2*ZS-1)*LOG(2D0)-LOG(PI)/2
      WRITE(LOUT,'(1X,2F10.4,2D25.7,1P,2D12.1)') ZS,R,ABS((T-R)/T)
      ENDIF
      ERRMAX=MAX( ERRMAX, ABS((T-R)/T) )
    1 CONTINUE

      WRITE(LOUT,100) NAME
      DO 2 M = 1,2
      DO 2 L = 0,1
      DO 2 J = 0,1
      DO 2 K = 1,3
c     The following 3 conditions are just to avoid underflows on VM
      if(m .eq. 2 .and. l .eq. 0 .and. j .eq.1 )go to 2
      if(m .eq. 2 .and. l .eq. 1 .and. j .eq.0 )go to 2
      if(m .eq. 2 .and. l .eq. 1 .and. j .eq.1 )go to 2
      Z=((-1)**J*X(K)+I*(-1)**L*Y(K))*10**M/2
      ZS=Z
      IF(NAME .EQ. 'WLOGAM') THEN
       R=WLOGAM(2*Z)
       T=WLOGAM(Z)+WLOGAM(Z+HALF)+(2*Z-1)*LOG(2D0)-LOG(PI)/2
       WRITE(LOUT,'(1X,2F10.2,2D25.16,1P,2D12.1)')
     1 ZS,R,ABS((T-R)/T)
c      WRITE(LOUT,'(1X,2F10.2,2D25.16,1P,2D12.1,2X,4I1)')
c    1 ZS,R,ABS((T-R)/T),M,L,J,K
      ELSE
       R=CLOGAM(2*ZS)
       T=CLOGAM(ZS)+CLOGAM(ZS+0.5)+(2*ZS-1)*LOG(2D0)-LOG(PI)/2
       WRITE(LOUT,'(1X,2F10.2,2D25.7,1P,2D12.1)')
     1 ZS,R,ABS((T-R)/T)
c      WRITE(LOUT,'(1X,2F10.2,2D25.7,1P,2D12.1,2X,4I1)')
c    1 ZS,R,ABS((T-R)/T),M,L,J,K
      ENDIF
    2 CONTINUE

      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE. TOL(IDS))
C     WRITE(LOUT,'(1X)')
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
      IF(NAME .EQ. 'WLOGAM') THEN
       R=WLOGAM(0D0+0*I)
       R=WLOGAM(-3D0+0*I)
      ELSE
       R=CLOGAM((0.,0.))
       R=CLOGAM((-3.,0.))
      ENDIF
    9 CONTINUE
  100 FORMAT('1'/1X,10X,'Z',9X,20X,A6,'(2*Z)'/)

      WRITE(LOUT,'(1X)')
C     Check if the test was successful
      IRC=ITEST('C333',LTEST)
      CALL PAGEND('C333')
      RETURN
      END
