*
* $Id: c209m.F,v 1.1.1.1 1996/04/01 15:01:13 mclareni Exp $
*
* $Log: c209m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C209M
C     This program tests the MATHLIB routines CPOLYZ and WPOLYZ (C209)
C     by calculating the roots of specially constructed complex
C     polynomials.
      PARAMETER (NT=4)
#include "gen/imp64.inc"
#include "gen/defc64.inc"
     +   TEST(NT,NT),A(0:NT),ROOT(NT),SUM
C     R is the estimated radius of a circle centered at a root
      DIMENSION    R(NT),RES(NT)
      LOGICAL   INR2(NT,NT)
      PARAMETER (MAXFUN=50000)
      PARAMETER (TSTERR=5D-8)
#include "iorc.inc"

C     The roots of the test equations; second index=test number
      DATA (TEST(J,1),J=1,4) / ( 1  , 0  ),(-1  , 1  ),
     + ( 3  ,-2  ),( 5  , 2  ) /
      DATA (TEST(J,2),J=1,4) / ( 3  , 0  ),( 0  , 0  ),
     + ( 0  ,-3  ),( 0  ,-3  ) /
      DATA (TEST(J,3),J=1,4) / ( 2  , 1  ),( 2  , 1  ),
     + ( 2  , 1  ),( 6  , 3  ) /
      DATA (TEST(J,4),J=1,4) / (-1  ,-3  ),( 1  , 3  ),
     + (-1  , 3  ),( 3  , 1  ) /

      CALL HEADER('C209',0)
C     Initialise maximum residues as zero
      RESMAX=0.0E0
C     N denotes the test number
      DO 100 N=1,NT
         WRITE(LOUT,'(/'' Test Number'',I3)') N
C        Get data for test number N in A vector
#if defined(CERNLIB_DOUBLE)
         CALL C209S (TEST(1,N),NT,A)
         WRITE(LOUT,'('' After call to C209S '')')
#endif
#if !defined(CERNLIB_DOUBLE)
         CALL C209S (TEST(1,N),NT,A)
         WRITE(LOUT,'('' After call to C209S '')')
#endif
         WRITE(LOUT,'('' A:'',2X,4(F7.1,'','',F7.1))') A
           DO 10 I=1,NT
           ROOT(I)=(0  ,0  )
10         CONTINUE
#if defined(CERNLIB_DOUBLE)
         CALL WPOLYZ(A,NT,MAXFUN,ROOT,R)
#endif
#if !defined(CERNLIB_DOUBLE)
         CALL CPOLYZ(A,NT,MAXFUN,ROOT,R)
#endif
         WRITE(LOUT,'('' Calculated Roots'',2(/1X,2F16.10,6X,2F16.10))')
     +                                                  (ROOT(J),J=1,NT)
         WRITE(LOUT,'('' Error Radii'',2X,4F16.10)')    (R(J),J=1,NT)
         WRITE(LOUT,'('' Exact Roots'',2(/1X,2F16.10,6X,2F16.10))')
     +                                                (TEST(J,N),J=1,NT)
C        Calculate the residues for the calculated root
         DO 40 I=1,NT
            SUM= A(NT)
            DO 30 K=1,NT
               SUM=SUM+( A(K-1)*(ROOT(I)**(NT+1-K)) )
30          CONTINUE
#if defined(CERNLIB_CMPXDOUB)
            RES(I)=MIN(999.0D0,ABS(SUM))
#endif
#if !defined(CERNLIB_CMPXDOUB)
            RES(I)=MIN(999.0E0,ABS(SUM))
#endif
C        Calculate the maximum (absolute) residue
            IF(RES(I).GT.RESMAX) RESMAX=RES(I)
40       CONTINUE
         WRITE(LOUT,'('' Residues'',3X,4(D10.2))') (RES(I),I=1,NT)
C        Check if the disc centred on the calculated root encloses an
C        exact root when radius R is used
         DO 75 I=1,NT
            INR2(I,I)= .FALSE.
            DO 50 J=1,NT
               INR2(I,J)=INR2(I,J).OR.(ABS(ROOT(I)-TEST(J,N)).LE.R(I))
C           Check if Root I is contained in a disc
            IF (INR2(I,J)) THEN
          WRITE(LOUT,'('' Exact Root'',I3,'' is contained'',
     +   '' in a disc of calculated root'',I3)')J,I
            ELSE
          WRITE(LOUT,'('' Exact Root'',I3,'' is not'',
     +    '' in a disc of calculated root'',I3)') J,I
            ENDIF
50          CONTINUE
75       CONTINUE
100   CONTINUE
      WRITE(LOUT,'(//'' Largest Residue was'',D10.2)') RESMAX
C     Check if all the residues for all the tests were OK
      IRC=ITEST('C209',RESMAX .LE. TSTERR)
      CALL PAGEND('C209')
      RETURN
      END

