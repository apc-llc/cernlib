*
* $Id: c207m.F,v 1.1.1.1 1996/04/01 15:01:13 mclareni Exp $
*
* $Log: c207m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C207M
#include "iorc.inc"
C     This program tests the MATHLIB routines RTEQ3 and DRTEQ3 (C207)
C     by prespecifying the roots of a cubic polynomial, and then
C     constructing the polynomial from these roots. The polynomial is
C     then solved using the library routine and the computed solutions
C     are compared with the analytic solutions.
#include "gen/defc64.inc"
     +     X(9,3),CALC(9,3),J
#include "gen/def64.inc"
     +     RES,RESMAX,TSTERR,R,S,T,SOL(3),D
C     Set maximum residue permitted for the test to still be successful
      PARAMETER ( TSTERR=1D-9,J=(0,1) )
C     Three real roots
      DATA X(1,1),X(1,2),X(1,3) /( 0, 0),( 0, 0),( 0, 0)/
      DATA X(2,1),X(2,2),X(2,3) /( 1, 0),( 1, 0),( 0, 0)/
      DATA X(3,1),X(3,2),X(3,3) /(-1, 0),(-2, 0),( 2, 0)/
      DATA X(4,1),X(4,2),X(4,3) /(10, 0),(30, 0),( 6, 0)/
      DATA X(5,1),X(5,2),X(5,3) /( 7, 0),( 7, 0),( 0, 0)/
C     One real,and a complex conjugate pair of roots
      DATA X(6,1),X(6,2),X(6,3) /( 1, 0),( 0, 1),( 0,-1)/
      DATA X(7,1),X(7,2),X(7,3) /( 0, 0),( 2, 3),( 2,-3)/
      DATA X(8,1),X(8,2),X(8,3) /(-4, 0),( 1, 6),( 1,-6)/
      DATA X(9,1),X(9,2),X(9,3) /(-9, 0),(-9, 5),(-9,-5)/

      CALL HEADER('C207',0)
C     Initialise maximum error term
      RESMAX=0

      DO 100 I=1,9
C        Construct the polynomial to be solved
C        R= -sum of roots
         R= -( X(I,1)+X(I,2)+X(I,3) )
C        S= sum of products of pairs of roots
         S= (( X(I,1)*X(I,2) ) + ( X(I,1)*X(I,3) ) + ( X(I,2)*X(I,3) ))
C        T= -product of roots
         T= -( X(I,1)*X(I,2)*X(I,3) )

C        Solve the polynomial
#if defined(CERNLIB_DOUBLE)
         CALL DRTEQ3(R,S,T,SOL,D)
#endif
#if !defined(CERNLIB_DOUBLE)
         CALL RTEQ3(R,S,T,SOL,D)
#endif
C        Construct calculated solution in same form as original roots
         IF (D .LE. 0) THEN
C           Three real roots
            CALC(I,1)=SOL(1)
            CALC(I,2)=SOL(2)
            CALC(I,3)=SOL(3)
         ELSE
C           One real and a complex conjugate pair of roots
            CALC(I,1)=SOL(1)
            CALC(I,2)=SOL(2)+(J*SOL(3))
            CALC(I,3)=SOL(2)-(J*SOL(3))
         ENDIF
         WRITE(LOUT,'(/'' Test Number'',I4)') I
         WRITE(LOUT,'('' Descriminant'',F25.16)') D
         DO 50 K=1,3
C          Calculate the residue for the calculated root
           RES=ABS( (CALC(I,K)**3)+(R*CALC(I,K)**2)+(S*CALC(I,K))+T )
           WRITE(LOUT,'('' X('',I2,'' )'',2F20.16,3X,''Residue'',
     +                              F20.16)') K,CALC(I,K),RES
           RESMAX=MAX(RESMAX,RES)
50       CONTINUE
100   CONTINUE
      WRITE(LOUT,'('' Largest Residue was'',F20.16)') RESMAX
      IRC=ITEST('C207',RESMAX .LE. TSTERR)
      CALL PAGEND('C207')
      RETURN
      END
