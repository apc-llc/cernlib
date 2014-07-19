*
* $Id: f106m.F,v 1.1.1.1 1996/04/01 15:01:26 mclareni Exp $
*
* $Log: f106m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:26  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE F106M
#include "iorc.inc"
C     This program tests the GENLIB routine SPXINV (F106) which inverts
C     a packed real symmetric matrix. We check the validity of the
C     inverse by checking that the inverse multiplied by the original
C     matrix gives the identity matrix.
C                        T Hepworth April 1990

C     Define the order of the matrix
      PARAMETER ( N=8 )
C     Set the numerical tolerance for comparisons in computed identity
      PARAMETER (TSTERR=1E-4)
C     Define the packed matrices
      REAL A( (N*(N+1))/2 ),B( (N*(N+1))/2 )
C     Define the unpacked matrices
      REAL C(N,N),D(N,N),E(N,N),ERROR(N,N)

C     A and B are going to hold a packed symmetric matrix of order N
      DATA A/                                  9, 2, 2, 1, 0, 0, 2, 6,
     +                                            0,-2, 5,-9, 1, 3,-7,
     +                                              -3, 7,-7, 2, 4, 1,
     +                                                 -5, 5, 1, 2, 9,
     +                                                     2,-1, 4, 0,
     +                                                        9,-8, 9,
     +                                                           7, 4,
     +                                                              3/
C     Set B the same as A, B is to hold inverse of A
      DATA B/                                  9, 2, 2, 1, 0, 0, 2, 6,
     +                                            0,-2, 5,-9, 1, 3,-7,
     +                                              -3, 7,-7, 2, 4, 1,
     +                                                 -5, 5, 1, 2, 9,
     +                                                     2,-1, 4, 0,
     +                                                        9,-8, 9,
     +                                                           7, 4,
     +                                                              3/
C     Open up input file
      CALL SPXINV(B,N,IFAIL)
C     Unpack the initial matrix A in C and unpack B in D
      K=1
      DO 200 I=1,N
         DO 100 J=I,N
            C(I,J)=A(K)
            D(I,J)=B(K)
            K=K+1
            C(J,I)=C(I,J)
            D(J,I)=D(I,J)
 100     CONTINUE
 200  CONTINUE
      WRITE(LOUT,*) 'PRINTING C'
      CALL F106S1(N,C)
      WRITE(LOUT,*) 'PRINTING COMPUTED INVERSE'
      CALL F106S1(N,D)
      WRITE(LOUT,'('' IFAIL RETURNED AS '',I2)') IFAIL

C     Compute A (unpacked in c) multiplied by its inverse (unpacked in D
C     This should give the identity matrix (unpacked in E)
      CALL F106S2(N,C,D,E)
C     Check accuracy of computed identity
      ERRMAX=ABS(E(1,1)-1)
      DO 400 I=1,N
         DO 300 J=1,N
C           Check for diagonal term
            IF (I .EQ. J) THEN
               ERROR(I,J)=ABS( E(I,J)-1 )
            ELSE
               ERROR(I,J)=ABS( E(I,J) )
            ENDIF
            ERRMAX=MAX(ERRMAX,ERROR(I,J))
300      CONTINUE
400   CONTINUE
      WRITE(LOUT,'('' PRINTING MATRIX OF RESIDUES'')')
      CALL F106S1(N,ERROR)
      WRITE(LOUT,'('' LARGEST ERROR WAS'',F16.8)') ERRMAX
C     Check if the test was successful
      IRC=ITEST('F106',ERRMAX .LE. TSTERR)
      CALL PAGEND('F106')
      RETURN
      END

