*
* $Id: c206m.F,v 1.1.1.1 1996/04/01 15:01:13 mclareni Exp $
*
* $Log: c206m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C206M
#include "iorc.inc"
      PARAMETER (NT=4)
C     This program tests the GENLIB routine POLY2 by calculating the
C     roots of specially constructed complex polynomials.
      COMPLEX   TEST(NT,NT),A(NT+1),ROOT(NT),SUM
C     R is the radius vector H in library manual
      REAL      R(NT),RES(NT)
      LOGICAL   INRING(NT),ALLIN
      PARAMETER (MAXFUN=50000)
C     Set different precisions as no double precision routine on IBM
#if defined(CERNLIB_DOUBLE)
      PARAMETER (TSTERR=1E-3)
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (TSTERR=1E-6)
#endif
C     The roots of the test equations, second index=test number
      DATA (TEST(J,1),J=1,4) / ( 1, 0),(-1, 1),( 3,-2),( 5, 2) /
      DATA (TEST(J,2),J=1,4) / ( 3, 0),( 0, 0),( 0,-3),( 0,-3) /
      DATA (TEST(J,3),J=1,4) / ( 2, 1),( 2, 1),( 2, 1),( 6, 3) /
      DATA (TEST(J,4),J=1,4) / (-1,-3),( 1, 3),(-1, 3),( 3, 1) /

      CALL HEADER('C206',0)
C     Initialise maximum residues as zero
      RESMAX=0.0
C     N denotes the test number
      DO 100 N=1,NT
         WRITE(LOUT,'(/'' Test Number'',I3)') N
C        Get data for test number N in A vector
         CALL C206S1(TEST(1,N),NT,A)
         WRITE(LOUT,'('' After call to C206S1'')')
         WRITE(LOUT,'('' A:'',2X,4(F7.1,'','',F7.1))') A
         MODE=0
         CALL POLY2(A,NT,ROOT,R,MAXFUN,MODE)
         WRITE(LOUT,'('' Calculated Roots'',2(/1X,2F16.10,6X,2F16.10))')
     +                                                  (ROOT(J),J=1,NT)
         WRITE(LOUT,'('' Error Radii'',2X,4F16.10)')    (R(J),J=1,NT)
         WRITE(LOUT,'('' Exact Roots'',2(/1X,2F16.10,6X,2F16.10))')
     +                                                (TEST(J,N),J=1,NT)
C        Calculate the residues for the calculated root
         DO 40 I=1,NT
            SUM= A(NT+1)
            DO 30 K=1,NT
               SUM=SUM+( A(K)*(ROOT(I)**(5-K)) )
30          CONTINUE
            RES(I)=MIN(999.0,ABS(SUM))
C        Calculate the maximum (absolute) residue
            IF(RES(I).GT.RESMAX) RESMAX=RES(I)
40       CONTINUE
         WRITE(LOUT,'('' Residues'',3X,4F17.10)') (RES(I),I=1,NT)
C        Check if the disc centred on the calculated root encloses an
C        exact root when radius R is used
            ALLIN    = .TRUE.
         DO 75 I=1,NT
            INRING(I)= .FALSE.
            DO 50 J=1,NT
               INRING(I)=INRING(I).OR.(ABS(ROOT(I)-TEST(J,N)).LE.R(I))
50          CONTINUE
C           Check if Root I is contained in a disc
            IF (INRING(I)) THEN
               WRITE(LOUT,'('' Root'',I3,'' is contained in a disc'')')I
            ELSE
               WRITE(LOUT,'('' Root'',I3,'' is not in a disc'')') I
            ENDIF
            ALLIN = ALLIN.AND.INRING(I)
75       CONTINUE
C        Check if all of roots for test N were inside a disc
C
         IF (ALLIN) THEN
          WRITE(LOUT,'('' For test'',I3,'' all roots were in discs'')')N
         ELSE
          WRITE(LOUT,'('' For test'',I3,'' at least one root was'',
     +                     '' not contained in a disc'')') N
         ENDIF
100   CONTINUE
      WRITE(LOUT,'('' Largest Residue was'',F17.9)') RESMAX
C     Check if all the residues for all the tests were OK
      IRC=ITEST('C206',RESMAX .LE. TSTERR)
      CALL PAGEND('C206')
      RETURN
      END

