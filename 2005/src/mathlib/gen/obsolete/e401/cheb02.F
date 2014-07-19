*
* $Id: cheb02.F,v 1.1.1.1 1996/04/01 15:03:15 mclareni Exp $
*
* $Log: cheb02.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:15  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE CHEB02(CK,K,IDIM)
C
C     CERN PROGRAM LIBRARY NO. E401
C
C     BASIC VERSION OF AUGUST 1970.
C
C     PURPOSE = GENERATION OF CHEBYSHEV POLYNOMIALS OF ORDER K.
C               DISTINCTION IS MADE BETWEEN EVEN AND ODD TERMS.CALCULAT-
C               ION PROCEEDS BY BUILDING UP EACH TERM FROM THE TERM BUT
C               ONE PREVIOUSLY.
C
C     PARAMETERS
C
C     INPUT
C
C     K       = ORDER OF POLYNOMIAL REQUIRED.
C     IDIM    = MAXIMUM ORDER OF K.
C
C     OUTPUT
C
C     CK      = ARRAY HOLDING THE REQUIRED COEFFICIENTS.
C
      DOUBLE PRECISION CK(IDIM),AK,AM
C
         DO 4 NN=1,IDIM
    4 CK(NN)=0.D0
      AK=DBLE(K)
      IF((K/2)*2.EQ.K)GOTO2
      CK(2)=AK*(-1.0)**((K-1)/2)
      IF(K.EQ.1)GOTO3
         DO 5 M=3,K,2
      AM=DBLE(M)
    5 CK(M+1)=CK(M-1)*(AK**2-AM**2+4.D0*AM-4.D0)/(AM*(1.D0-AM))
      GOTO3
    2 CK(1)=(-1.D0)**(K/2)
      CK(3)=(-1.D0)*CK(1)*(AK**2)/2.D0
      IF(K.EQ.2)GOTO3
         DO 6 M=4,K,2
      AM=DBLE(M)
    6 CK(M+1)=CK(M-1)*(AK**2-AM**2+4.D0*AM-4.D0)/(AM*(1.D0-AM))
    3 RETURN
C
      END
