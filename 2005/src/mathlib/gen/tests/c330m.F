*
* $Id: c330m.F,v 1.1.1.1 1996/04/01 15:01:17 mclareni Exp $
*
* $Log: c330m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:17  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C330M
C     Routine to test the MATHLIB routines ASLGF and DASLGF (C330)
C     Specify the total number of tests
      PARAMETER ( NT=5 )
#include "gen/def64.inc"
     +     X(NT),P(0:30),SOL(NT),
     +     EXACT(NT),ERROR(NT),ERRMAX,TSTERR
      REAL SX(NT),SP(0:30),SSOL(NT)
      REAL SEXACT(NT),SERROR(NT),SERRMAX,STSTERR
      INTEGER M(NT),N(NT),MODE(NT)
C     Set largest error permitted for a valid test
      PARAMETER ( TSTERR=1D-14 )
      PARAMETER (STSTERR=1D-6  )
      LOGICAL LTEST
#include "iorc.inc"
C     The test parameter values
      DATA X(1),M(1),N(1),MODE(1) / 0.5D0,  0, 6, 1 /
      DATA X(2),M(2),N(2),MODE(2) /   0D0,  6, 8, 2 /
      DATA X(3),M(3),N(3),MODE(3) /-0.2D0, -2, 3, 1 /
      DATA X(4),M(4),N(4),MODE(4) /-0.8D0,  0, 0, 2 /
      DATA X(5),M(5),N(5),MODE(5) / 0.8D0, -6, 5, 2/

C     The exact values to be compared with
      DATA EXACT(1) /   0.3232421875000000D+00 /
      DATA EXACT(2) /  -0.9435352611654214D+00 /
      DATA EXACT(3) /  -0.2400000000000000D-01 /
      DATA EXACT(4) /   0.7071067811865475D+00 /
      DATA EXACT(5) /   0.0000000000000000D+00 /

      ERRMAX=0D0
      SERRMAX=0E0
      LTEST=.TRUE.
      CALL HEADER('C330',0)
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/''  M  N'',6X,''X'',9X,''Calculated'',14X,
     +                        ''Exact'',10X,''Error'')')
#endif
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/''  M  N'',5X,''X'',9X,''DASLGF'',14X,
     + ''Exact'',16X,''ASLGF'',13X,''Error'')')
#endif
      DO 100 I=1,NT
C        Calculate the numerical legendre function evaluations
#if defined(CERNLIB_DOUBLE)
         SX(I)=X(I)
         SEXACT(I)=EXACT(I)
         CALL DASLGF(MODE(I),X(I),M(I),N(I),P)
         CALL  ASLGF(MODE(I),SX(I),M(I),N(I),SP)
         SOL(I)=P(N(I))
         ERROR(I)= ABS( SOL(I)-EXACT(I) )
         SSOL(I)=SP(N(I))
         SERROR(I)= ABS( SSOL(I)-SEXACT(I) )
         WRITE(LOUT,'(1X,I2,I3,F7.2,2F21.16,F16.8,1P,2D10.1)')
     +   M(I),N(I),X(I),SOL(I),
     +   EXACT(I),SSOL(I),ERROR(I),SERROR(I)
         SERRMAX=MAX( SERRMAX,SERROR(I) )
         ERRMAX=MAX( ERRMAX,ERROR(I) )
         LTEST=LTEST .AND. ERRMAX .LE. TSTERR .AND. SERRMAX .LE.
     +   STSTERR
#endif
#if !defined(CERNLIB_DOUBLE)
         CALL ASLGF(MODE(I),X(I),M(I),N(I),P)
         SOL(I)=P(N(I))
         ERROR(I)= ABS( SOL(I)-EXACT(I) )
         WRITE(LOUT,'(1X,I2,I3,F7.2,2F21.16,1P,D10.1)')
     +    M(I),N(I),X(I),SOL(I),
     +   EXACT(I),ERROR(I)
         ERRMAX=MAX( ERRMAX,ERROR(I) )
         LTEST=LTEST .AND. ERRMAX .LE. TSTERR
#endif
100   CONTINUE

#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/'' Largest Error was'',1P,D10.1)') ERRMAX
#endif
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/''DASLGF Largest Error was'',1P,D10.1)')ERRMAX
      WRITE(LOUT,'('' ASLGF Largest Error was'',1P,D10.1/)')SERRMAX
#endif
      IRC=ITEST('C330',LTEST)
      CALL PAGEND('C330')
      RETURN
      END
