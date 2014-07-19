*
* $Id: c345m.F,v 1.4 1997/10/24 14:46:49 mclareni Exp $
*
* $Log: c345m.F,v $
* Revision 1.4  1997/10/24 14:46:49  mclareni
* Revert do loop change for NT at least
*
* Revision 1.3  1997/09/02 16:09:41  mclareni
* WINNT corrections
*
* Revision 1.2  1997/04/07 10:01:04  mclareni
* Mods for winnt
*
* Revision 1.1.1.1.2.1  1997/01/21 11:26:21  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/04/01 15:01:19  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C345M
C     This program tests the operation of MATHLIB subprograms
C     BZEJY and DBZEJY (C345)
#include "gen/imp64.inc"

      DIMENSION Z(40,4),Z0(40,4),BP(0:50),BM(0:50)

C     Specify the largest relative error permitted in a successful test
#if defined(CERNLIB_LINUX) || (defined(CERNLIB_WINNT)&&!defined(_ALPHA_))
      PARAMETER ( TSTERR= 5D-11 )
#else
      PARAMETER ( TSTERR= 5D-12 )
#endif
      PARAMETER (Z1 = 1)

#include "iorc.inc"
      CALL HEADER('C345',0)
#if (defined(CERNLIB_QIEEE))&&(!defined(CERNLIB_LINUX))
      IEPS=11
#endif
#if (defined(CERNLIB_QIEEE))&&(defined(CERNLIB_LINUX))
      IEPS=10
#endif
#if !defined(CERNLIB_QIEEE)
      IEPS=12
#endif

      ERMAX=0D0
      PI = 3.14159 26535 89793D0
      EPS=(10*Z1)**(-IEPS)
      NMAX=40
      DO 1 JA = 0,25,25
#if defined(CERNLIB_QIEEE)
*GF      DO 1 IA = 0,8,2   is the step 2 needed? 
*                          (Yes, othewise this doesn't work, under NT at least VF)
*        Given by L.Garren
      DO 1 IA = 0,8,2
#endif
#if defined(CERNLIB_VAX)
      DO 1 IA = 0,6
      IF(IA.GT.3 .AND. JA.EQ.0) GO TO 1
      IF(IA.LT.1 .AND. JA.EQ.25) GO TO 1
#endif
#if (!defined(CERNLIB_QIEEE))&&(!defined(CERNLIB_VAX))
      DO 1 IA = 0,10
#endif
#if !defined(CERNLIB_DOUBLE)
      IF(IA .EQ. 4 .OR. ((IA .EQ. 8 .OR. IA .EQ. 10) .AND.
     +                   JA .EQ. 25)) GO TO 1
#endif
      A=IA+JA/100D0
      NA=A
      C=COS(A*PI)
      S=SIN(A*PI)
      C1=COS((A+1)*PI)
      S1=SIN((A+1)*PI)
      WRITE(LOUT,100) A,EPS
      DO 2 MODE = 1,4
#if !defined(CERNLIB_DOUBLE)
    2 CALL  BZEJY(A,NMAX,MODE,EPS,Z(1,MODE))
#endif
#if defined(CERNLIB_DOUBLE)
    2 CALL DBZEJY(A,NMAX,MODE,EPS,Z(1,MODE))
#endif
      WRITE(LOUT,101)
      DO 9 N = 1,NMAX
      IF(A .EQ. 0) THEN
#if !defined(CERNLIB_DOUBLE)
       Z0(N,1)= BESJ0(Z(N,1))
       Z0(N,2)= BESY0(Z(N,2))
       Z0(1,3)=0
       IF(N .GT. 1) Z0(N,3)=- BESJ1(Z(N-1,3))
       Z0(N,4)=- BESY1(Z(N,4))
      ELSEIF(A .EQ. 1) THEN
       Z0(N,1)= BESJ1(Z(N,1))
       Z0(N,2)= BESY1(Z(N,2))
       Z0(N,3)= BESJ0(Z(N,3))- BESJ1(Z(N,3))/Z(N,3)
       Z0(N,4)= BESY0(Z(N,4))- BESY1(Z(N,4))/Z(N,4)
      ELSEIF(A .NE. INT(A)) THEN
       CALL  BSJA(Z(N,1),A-NA,NA,IEPS,BP)
       Z0(N,1)=BP(NA)
       CALL  BSJA(Z(N,2),A-NA,NA,IEPS,BP)
       CALL  BSJA(Z(N,2),NA+1-A,-(NA+1),IEPS,BM)
       Z0(N,2)=(BP(NA)*C-BM(NA+1))/S
       CALL  BSJA(Z(N,3),A-NA,NA+1,IEPS,BP)
       Z0(N,3)=BP(NA)*A/Z(N,3)-BP(NA+1)
       CALL  BSJA(Z(N,4),A-NA,NA+1,IEPS,BP)
       CALL  BSJA(Z(N,4),NA+1-A,-(NA+2),IEPS,BM)
       Z0(N,4)=((BP(NA)*C-BM(NA+1))/S)*A/Z(N,4)-
     1          (BP(NA+1)*C1-BM(NA+2))/S1
      ELSE
       CALL  BSJA(Z(N,1),A-NA,NA,IEPS,BP)
       Z0(N,1)=BP(NA)
       BP(0)= BESY0(Z(N,2))
       BP(1)= BESY1(Z(N,2))
       DO 3 M = 1,NA-1
    3  BP(M+1)=2*M*BP(M)/Z(N,2)-BP(M-1)
       Z0(N,2)=BP(NA)
       Z0(N,3)=0
       BP(0)= BESY0(Z(N,4))
       BP(1)= BESY1(Z(N,4))
#endif
#if defined(CERNLIB_DOUBLE)
       Z0(N,1)=DBESJ0(Z(N,1))
       Z0(N,2)=DBESY0(Z(N,2))
       Z0(1,3)=0
       IF(N .GT. 1) Z0(N,3)=-DBESJ1(Z(N-1,3))
       Z0(N,4)=-DBESY1(Z(N,4))
      ELSEIF(A .EQ. 1) THEN
       Z0(N,1)=DBESJ1(Z(N,1))
       Z0(N,2)=DBESY1(Z(N,2))
       Z0(N,3)=DBESJ0(Z(N,3))-DBESJ1(Z(N,3))/Z(N,3)
       Z0(N,4)=DBESY0(Z(N,4))-DBESY1(Z(N,4))/Z(N,4)

      ELSEIF(A .NE. INT(A)) THEN
       CALL DBSJA(Z(N,1),A-NA,NA,IEPS,BP)
       Z0(N,1)=BP(NA)
       CALL DBSJA(Z(N,2),A-NA,NA,IEPS,BP)
       CALL DBSJA(Z(N,2),NA+1-A,-(NA+1),IEPS,BM)
       Z0(N,2)=(BP(NA)*C-BM(NA+1))/S
       CALL DBSJA(Z(N,3),A-NA,NA+1,IEPS,BP)
       Z0(N,3)=BP(NA)*A/Z(N,3)-BP(NA+1)
       CALL DBSJA(Z(N,4),A-NA,NA+1,IEPS,BP)
       CALL DBSJA(Z(N,4),NA+1-A,-(NA+2),IEPS,BM)
       Z0(N,4)=((BP(NA)*C-BM(NA+1))/S)*A/Z(N,4)-
     1          (BP(NA+1)*C1-BM(NA+2))/S1
      ELSE
       CALL DBSJA(Z(N,1),A-NA,NA,IEPS,BP)
       Z0(N,1)=BP(NA)
       BP(0)=DBESY0(Z(N,2))
       BP(1)=DBESY1(Z(N,2))
       DO 3 M = 1,NA-1
    3  BP(M+1)=2*M*BP(M)/Z(N,2)-BP(M-1)
       Z0(N,2)=BP(NA)
       Z0(N,3)=0
       BP(0)=DBESY0(Z(N,4))
       BP(1)=DBESY1(Z(N,4))
#endif
       DO 4 M = 1,NA-1
    4  BP(M+1)=2*M*BP(M)/Z(N,4)-BP(M-1)
       Z0(N,4)=BP(NA-1)-NA*BP(NA)/Z(N,4)
      ENDIF
      DO 1111 JJ=1,4
      ERMAX=MAX(ERMAX,Z0(N,JJ))
 1111 CONTINUE
    9 CONTINUE
      WRITE(LOUT,'(1X,I3,4F21.15,1P,4D8.1)')
     1 (N,(Z(N,J),J=1,4),(ABS(Z0(N,J)),J=1,4),N=1,NMAX)
    1 CONTINUE
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
*      Mode no longer is set. Thanks to L.Garren
      MODE=4
#if !defined(CERNLIB_DOUBLE)
      CALL  BZEJY(-Z1,NMAX,1,EPS,Z(1,MODE))
#endif
#if defined(CERNLIB_DOUBLE)
      CALL DBZEJY(-Z1,NMAX,1,EPS,Z(1,MODE))
#endif
  100 FORMAT('1'/1X,'a = ',F10.6,5X,'EPS = ',1PE8.1/)
  101 FORMAT(1X,'  N',16X,'Ja(X)',16X,'Ya(X)',15X,'Ja''(X)',15X,
     1 'Ya''(x)',10X,'Error'/)
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D10.1)') ERMAX
      IRC= ITEST('C345',ERMAX .LE. TSTERR)
      CALL PAGEND('C345')
      RETURN
      END
