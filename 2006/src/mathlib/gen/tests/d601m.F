*
* $Id: d601m.F,v 1.1.1.1 1996/04/01 15:01:26 mclareni Exp $
*
* $Log: d601m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:26  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D601M
C     This program tests the operation of GENLIB subroutine subprograms
C     RFRDH1, DFRDH1 and function subprograms
C     RFRDH2, DFRDH2 and RFRDH3, DFRDH3.

#include "gen/imp64.inc"
C     Set the total number of tests
      PARAMETER ( NT=11)
C     Set maximum error allowed for test to be considered successful
      PARAMETER ( TSTERR=1D-11 )

      EXTERNAL FD601,G,H
      PARAMETER (IDIM = 96)
      DIMENSION WS(IDIM,50),TV(0:10),NGTV(10)
#include "iorc.inc"

      CALL HEADER('D601',0)
      ERRMAX=0D0
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/8X,''X'',14X,''DFRDH2'',
     + 21X,''TEST'',9X,''Rel. Error'')')
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/8X,''X'',14X,''RFRDH2'',
     + 21X,''TEST'',9X,''Rel. Error'')')
#endif

      EX=1.777777777777774D0
      M=4
      TV(0)=-1
      TV(1)=-0.25D0
      TV(2)=0.1D0
      TV(3)=0.5D0
      TV(4)=1
      NGTV(1)=8
      NGTV(2)=10
      NGTV(3)=9
      NGTV(4)=7
#if defined(CERNLIB_DOUBLE)
      CALL DFRDH1(FD601,G,M,TV,NGTV,WS,IDIM,N)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL RFRDH1(FD601,G,M,TV,NGTV,WS,IDIM,N)
#endif
      DO 1 I = -100,100
      X=I/100D0
#if defined(CERNLIB_DOUBLE)
      PHI=DFRDH2(FD601,G,X,WS,IDIM,N)
#endif
#if !defined(CERNLIB_DOUBLE)
      PHI=RFRDH2(FD601,G,X,WS,IDIM,N)
#endif
      TST=25*X**2/9+6*X+1
      E=ABS((PHI-TST)/TST)
         ERRMAX= MAX(ERRMAX,E)
    1 WRITE(LOUT,'(1X,F10.2,2F25.15,1P,D10.1)') X,PHI,TST,E
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/12X,''DFRDH3'',10X,''Rel. Error'')')
      DF3=DFRDH3(H,WS,IDIM,N)
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/12X,''RFRDH3'',10X,''Rel. Error'')')
      DF3=RFRDH3(H,WS,IDIM,N)
#endif
      E=ABS((DF3-EX)/EX)
         ERRMAX= MAX(ERRMAX,E)
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/1X,F25.15,1P,D10.1)') DFRDH3(H,WS,IDIM,N),E
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/1X,F25.15,1P,D10.1)') RFRDH3(H,WS,IDIM,N),E
#endif
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D10.1)') ERRMAX
C     Check if the test was successful
      IRC=ITEST('D601',ERRMAX .LE. TSTERR)
      CALL PAGEND('D601')
      RETURN
      END

      FUNCTION FD601(X)
#include "gen/imp64.inc"
      FD601=(1+X)**2
      RETURN

      ENTRY G(X,Y)
      G=X*Y+(X*Y)**2
      RETURN

      ENTRY H(T)
      H=T**2
      RETURN
      END
