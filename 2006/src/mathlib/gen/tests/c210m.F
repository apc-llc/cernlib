*
* $Id: c210m.F,v 1.1.1.1 1996/04/01 15:01:13 mclareni Exp $
*
* $Log: c210m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C210M
#include "gen/imp64.inc"
C     This program tests the MATHLIB routine NZERFZ by calculating the
C     number of zeros of complex functions.

#include "gen/defc64.inc"
     +          I,Z0,Z(4),F1,F2,F3,F4
      DIMENSION NK(50),NEX(4,50),NERROR(4,50)
C     Specify the largest absolute error allowed for a successful test
      PARAMETER (NTSTERR=1D-13 )
      EXTERNAL F1,F2,F3,F4
#include "iorc.inc"
      COMMON /NDNDND/ N

        DATA NEX(1,1)/6/
        DATA NEX(2,1)/2/
        DATA NEX(3,1)/16/
        DATA (NEX(4,J),J=1,10)/
     +    1 ,  2 ,  3 ,  4 ,  5 ,  6 ,  7 ,  8 ,  9 , 10/
        DATA (NEX(4,J),J=11,20)/
     +   11 , 12 , 13 , 14 , 15 , 16 , 17 , 18 , 19 , 20/
        DATA (NEX(4,J),J=21,30)/
     +   21 , 22 , 23 , 24 , 25 , 26 , 27 , 28 , 29 , 30/
        DATA (NEX(4,J),J=31,40)/
     +   31 , 32 , 33 , 34 , 35 , 36 , 37 , 38 , 39 , 40/
        DATA (NEX(4,J),J=41,50)/
     +   41 , 42 , 43 , 44 , 45 , 46 , 47 , 48 , 49 , 50/
         NERRMAX= 0
      CALL HEADER('C210',0)
C     N denotes the test number
      Numb=1
         WRITE(LOUT,'(/'' Test Number'',I3)') Numb
      I=(0  ,1  )
      Z(1)=-6-6*I
      Z(2)=+6-6*I
      Z(3)=+6+6*I
      Z(4)=-6+6*I
      NZS=NZERFZ(F1,Z,4)
         NERROR(1,1)=ABS( NEX(1,1)-NZS )
         NERRMAX= MAX( NERRMAX,NERROR(1,1) )
      WRITE(LOUT,101) NZS

      Numb=2
         WRITE(LOUT,'(/'' Test Number'',I3)') Numb
      Z(1)=0-1*I
      Z(2)=3-1*I
      Z(3)=3-2.6*I
      NZS=NZERFZ(F2,Z,3)
         NERROR(2,1)=ABS( NEX(2,1)-NZS )
         NERRMAX= MAX( NERRMAX,NERROR(2,1) )
      WRITE(LOUT,102) NZS

      Numb=3
         WRITE(LOUT,'(/'' Test Number'',I3)') Numb
      Z(1)=-15.1D0+0.1D0*I
      Z(2)=-15.1D0-0.1D0*I
      Z(3)=+1-0.1D0*I
      Z(4)=+1+0.1D0*I
      NZS=NZERFZ(F3,Z,4)
         NERROR(3,1)=ABS( NEX(3,1)-NZS )
         NERRMAX= MAX( NERRMAX,NERROR(3,1) )
      WRITE(LOUT,103) NZS

      Numb=4
         WRITE(LOUT,'(/'' Test Number'',I3)') Numb
      WRITE(LOUT,104)
      Z0=1.01D0
      Z(1)=-Z0-Z0*I
      Z(2)=+Z0-Z0*I
      Z(3)=+Z0+Z0*I
      Z(4)=-Z0+Z0*I
      DO 1 N = 1,50
      NK(N)=NZERFZ(F4,Z,4)
         NERROR(4,N)=ABS( NEX(4,N)-NK(N) )
    1    NERRMAX= MAX( NERRMAX,NERROR(4,N) )
      WRITE(LOUT,'(1X,5X,10I5)') (NK(N),N=1,50)

      Numb=5
         WRITE(LOUT,'(/'' Test Number'',I3)') Numb
      WRITE(LOUT,'(1X)')
         WRITE(LOUT,'(/'' TESTING ERROR MESSAGES:''/)')
      Z(1)=+0.1D0*I
      Z(2)=-0.2D0*I
      Z(3)=+1-0.1D0*I
      Z(4)=+1+0.1D0*I
      NZS=NZERFZ(F3,Z,4)
      WRITE(LOUT,'(1X,I5)') NZS

  101 FORMAT(1X,I5,5X,'Zeros of J1(z)**2-J0(z)*J2(z) in '
     1/7X,'(-6,6),(+6,-6),(+6,+6),(-6,+6)'//)
  102 FORMAT(1X,I5,5X,'Zeros of the complex error function w(z) in '
     1/7X,'(0,-1),(3,-1),(3,-2.6)'//)
  103 FORMAT(1X,I5,5X,'Zeros of the reciprocal gamma function in '
     1/7X,'(-15.1,+0.1),(-15.1,-0.1),(1,-0.1),(1,+0.1)'//)
  104 FORMAT(1X,5X,5X,'Zeros of z**n - 1 in '
     1/7X,'(-Z0,-Z0),(+Z0,-Z0),(+Z0,+Z10),(-Z0,+Z0)',
     2                '   Z0 = 1.01'/)
      WRITE(LOUT,'(/'' Largest Absolute Error was'',I5)') NERRMAX
      IRC=ITEST('C210',NERRMAX .LE. NTSTERR)
      CALL PAGEND('C210')
      RETURN
      END
      FUNCTION F(Z)
#include "gen/impc64.inc"
#include "gen/def64.inc"
     +  R0
      COMMON /NDNDND/ N
      DIMENSION CB(0:2)

      PARAMETER (R0 = 0)

      ENTRY F1(Z)
#if defined(CERNLIB_DOUBLE)
      CALL WBSJA(Z,R0,2,10,CB)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL CBSJA(Z,R0,2,10,CB)
#endif
      F1=CB(1)**2-CB(0)*CB(2)
      RETURN

      ENTRY F2(Z)
#if defined(CERNLIB_DOUBLE)
      F2=WWERF(Z)
#endif
#if !defined(CERNLIB_DOUBLE)
      F2=CWERF(Z)
#endif
      RETURN

      ENTRY F3(Z)
#if defined(CERNLIB_DOUBLE)
      F3=1/WGAMMA(Z)
#endif
#if !defined(CERNLIB_DOUBLE)
      F3=1/CGAMMA(Z)
#endif
      RETURN

      ENTRY F4(Z)
      F4=Z**N-1
      RETURN
      END
