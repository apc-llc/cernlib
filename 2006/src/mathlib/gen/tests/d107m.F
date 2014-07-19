*
* $Id: d107m.F,v 1.1.1.1 1996/04/01 15:01:22 mclareni Exp $
*
* $Log: d107m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:22  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D107M
C     This program tests the operation of MATHLIB subprograms
C     RGQUAD and DGQUAD

#include "gen/imp64.inc"
C     Set maximum error allowed for test to be considered successful
      PARAMETER ( TSTERR =1D-12 )

      EXTERNAL FF1,FF2
      COMMON /NNNNNN/ N2
      DIMENSION NG(23),X(96),W(96)
#include "iorc.inc"


      DATA NG
     1 /2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,20,24,32,40,48,64,80,96/

      CALL HEADER('D107',0)
      ERRMAX=0D0
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/5X,''N'',11X,''DGQUAD'',
     + 17X,''Test Value'',15X,''Error'')')
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/5X,''N'',11X,''RGQUAD'',
     + 17X,''Test Value'',15X,''Error'')')
#endif
      A=0
      B=1
      DO 1 I = 1,23
      N=NG(I)
#if defined(CERNLIB_DOUBLE)
      R=DGQUAD(FF1,A,B,N)
#endif
#if !defined(CERNLIB_DOUBLE)
      R=RGQUAD(FF1,A,B,N)
#endif
      T=1
      E=ABS(R-1)
         ERRMAX= MAX(ERRMAX,E)
      WRITE(LOUT,'(1X,I5,1P,D25.15,D25.15,D15.1)') N,R,T,E
    1 CONTINUE

      WRITE(LOUT,'(1X)')
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/5X,''N'',11X,''DGQUAD'',
     + 17X,''Test Value'',15X,''Error'')')
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/5X,''N'',11X,''RGQUAD'',
     + 17X,''Test Value'',15X,''Error'')')
#endif
      A=1.1D0
      B=2
#if !defined(CERNLIB_VAX)
      DO 2 I = 1,23
#endif
#if defined(CERNLIB_VAX)
C      Since the last 3 values are too big for the VAX
      DO 2 I = 1,20
#endif
      N=NG(I)
      N2=2*N-1
#if defined(CERNLIB_DOUBLE)
      R=DGQUAD(FF2,A,B,N)
#endif
#if !defined(CERNLIB_DOUBLE)
      R=RGQUAD(FF2,A,B,N)
#endif
      T=(B**(N2+1)-A**(N2+1))/(N2+1)
      E=ABS((R-T)/T)
         ERRMAX= MAX(ERRMAX,E)
      WRITE(LOUT,'(1X,I5,2D25.15,1P,D15.1)') N,R,T,E
    2 CONTINUE

      WRITE(LOUT,'(1X)')
      WRITE(LOUT,'(/5X,''N'',9X,''Sum(w*f)'',
     + 17X,''Test Value'',15X,''Error'')')
      A=1.1D0
      B=2
#if !defined(CERNLIB_VAX)
      DO 3 I = 1,23
#endif
#if defined(CERNLIB_VAX)
C      Since the last 3 values are too big for the VAX, L.Garren
      DO 3 I = 1,20
#endif
      N=NG(I)
      N2=2*N-1
#if defined(CERNLIB_DOUBLE)
      CALL DGSET(A,B,N,X,W)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL RGSET(A,B,N,X,W)
#endif
      R=0
      DO 4 J = 1,N
    4 R=R+W(J)*FF2(X(J))
      T=(B**(N2+1)-A**(N2+1))/(N2+1)
      E=ABS((R-T)/T)
         ERRMAX= MAX(ERRMAX,E)
      WRITE(LOUT,'(1X,I5,2D25.15,1P,D15.1)') N,R,T,E
    3 CONTINUE
      WRITE(LOUT,'(1X)')
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
#if defined(CERNLIB_DOUBLE)
      R=DGQUAD(FF2,A,B,100)
      CALL DGSET(A,B,100,X,W)
#endif
#if !defined(CERNLIB_DOUBLE)
      R=RGQUAD(FF2,A,B,100)
      CALL RGSET(A,B,100,X,W)
#endif

      WRITE(LOUT,'(/'' Largest Relative Error was'',
     +D10.1)') ERRMAX
C     Check if the test was successful
      IRC=ITEST('D107',ERRMAX.LE. TSTERR)
      CALL PAGEND('D107')
      RETURN
      END

      FUNCTION FD107(X)
#include "gen/imp64.inc"
      COMMON /NNNNNN/ N2

      ENTRY FF1(X)
      FF1=1
      RETURN

      ENTRY FF2(X)
      FF2=X**N2
      RETURN
      END
