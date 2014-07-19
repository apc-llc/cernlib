*
* $Id: c208m.F,v 1.1.1.1 1996/04/01 15:01:13 mclareni Exp $
*
* $Log: c208m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C208M
#include "iorc.inc"
C     This program tests the MATHLIB routines RTEQ4, DRETQ4 (C208)
C     by solving specially constructed polynomials.
C     Specify the total number of tests
      PARAMETER ( NT=6 )
#include "gen/def64.inc"
     +  A,B,C,D,DC,RESMAX,ERROR(4),ERRMAX,RESTOL,TSTERR,RESABS
#include "gen/defc64.inc"
     +  ROOT(NT,4),SOL(NT,4),Z1,Z2,Z3,Z4,Z(4),RES
C     Set maximum residue and maximum absolute error allowed for the
C     test to still be considered successful
      PARAMETER        ( RESTOL=1D-10,TSTERR=1D-12 )

C     The test roots, the root order matches with order of calculation
      DATA (ROOT(1,J),J=1,4) /(3,0),(2,0),(0,1),(0,-1)/
      DATA (ROOT(2,J),J=1,4) /(8,0),(6,0),(1,1),(1,-1)/
      DATA (ROOT(3,J),J=1,4) /(1,0),(0, 0),(0,0),(0,0)/
      DATA (ROOT(4,J),J=1,4) /(1,1),(-1,-1),(-1,1),(1,-1)/
      DATA (ROOT(5,J),J=1,4) /(2,0),(-2,0),(0,2),(0,-2)/
#if defined(CERNLIB_CMPXDOUB)
      DATA ROOT(6,1),ROOT(6,2)/ (1.54368901269207637D0,0D0),(-1D0,0D0)/
      DATA ROOT(6,3) /(0.228155493653961816D0, 1.11514250803993731D0)/
      DATA ROOT(6,4) /(0.228155493653961816D0,-1.11514250803993731D0)/
#endif
#if !defined(CERNLIB_CMPXDOUB)
      DATA ROOT(6,1),ROOT(6,2)/ (1.54368901269207637,0),(-1,0)/
      DATA ROOT(6,3) /(0.228155493653961816, 1.11514250803993731)/
      DATA ROOT(6,4) /(0.228155493653961816,-1.11514250803993731)/
#endif
C     Declare residue calculating function
      RES(Z1)= (Z1**4)+(A*Z1**3)+(B*Z1**2)+(C*Z1)+D

      CALL HEADER('C208',0)
C     Initialise maximum residue and maximum absolute error
      RESMAX=0D0
      ERRMAX=0D0
      DO 100 I=1,NT
         WRITE(LOUT,'(/'' Test Number'',I3)') I
C        Use shorter names for roots (easier typing)
         Z1=ROOT(I,1)
         Z2=ROOT(I,2)
         Z3=ROOT(I,3)
         Z4=ROOT(I,4)
C        Calculate polynomial coefficients
         A= -(Z1+Z2+Z3+Z4)
         B= (Z1*Z2)+(Z1*Z3)+(Z1*Z4)+(Z2*Z3)+(Z2*Z4)+(Z3*Z4)
         C= -( (Z1*Z2*Z3)+(Z1*Z2*Z4)+(Z1*Z3*Z4)+(Z2*Z3*Z4) )
         D= Z1*Z2*Z3*Z4
C        Special case for test 6 we force values for A,B,C,D
C        Solution still contained in ROOT(6,J) but we require B=0,C=0
         IF (I .EQ. 6) THEN
            A=-1D0
            B=0D0
            C=0D0
            D=-2D0
         ENDIF
         WRITE(LOUT,'('' Roots '',8F8.1)') Z1,Z2,Z3,Z4
         WRITE(LOUT,'('' Coeffs'',4F8.1)') A,B,C,D
C        Solve the polynomials numerically
#if defined(CERNLIB_DOUBLE)
         CALL DRTEQ4(A,B,C,D,Z,DC,MT)
#endif
#if !defined(CERNLIB_DOUBLE)
         CALL RTEQ4(A,B,C,D,Z,DC,MT)
#endif
C        Write the results
         DO 90 J=1,4
            SOL(I,J)=Z(J)
            WRITE(LOUT,'(/'' Calculated root'',2F25.16)') SOL(I,J)
C           Calculate absolute error
            ERROR(J)= ABS( SOL(I,J)-ROOT(I,J) )
            RESABS  = ABS( RES(SOL(I,J)) )
            WRITE(LOUT,1000) ERROR(J),RESABS
            RESMAX=MAX( RESMAX,RESABS )
90       CONTINUE
         ERRMAX=MAX( ERRMAX,ERROR(1),ERROR(2),ERROR(3),ERROR(4) )
100   CONTINUE
      WRITE(LOUT,1001) ERRMAX,RESMAX
C     Check if the whole test of the routine was successful
      IRC=ITEST('C208',ERRMAX .LE. TSTERR .AND. RESMAX .LE. RESTOL)
      CALL PAGEND('C208')
      RETURN
1000  FORMAT(' Absolute Error ',F25.16/' Residue',F33.16)
1001  FORMAT(/' Largest Absolute Error was',F25.16/
     +        ' Largest Residue was       ',F25.16)
      END
