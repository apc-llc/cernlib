*
* $Id: c323m.F,v 1.1.1.1 1996/04/01 15:01:16 mclareni Exp $
*
* $Log: c323m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:16  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C323M
C     This program tests the MATHLIB routines FERDR,DFERDR (C323)
#include "gen/def64.inc"
     +       DFERDR
#include "gen/def64.inc"
     +       X(4),EXACT(4,0:2),RESULT(4,0:2),
     +       TSTERR,ERRMAX,ERROR(4,0:2)
c     REAL FERDR, FRCOS, FRSIN,SX(4),SEXACT(4,0:2),SRESULT(4,0:2),
      REAL FERDR, SX(4),SEXACT(4,0:2),SRESULT(4,0:2),
     +       STSTERR,SERRMAX,SERROR(4,0:2)
      INTEGER K(0:2)
C     Set the maximum error allowed for the test to still be considered
C     successful
      PARAMETER ( TSTERR=1D-12)
      PARAMETER (STSTERR=5D-6 )
      LOGICAL LTEST
#include "iorc.inc"
C     Set up test data and theoretical solutions
      DATA K(0),K(1),K(2) / -1, 1, 3 /
      DATA X / 0.0D0,2.0D0,4.0D0,10.0D0/

      DATA (EXACT(I,0),EXACT(I,1),EXACT(I,2),I=1,4)/
     + 1.07215492998544293D0,0.678093895181781731D0,
     + 1.15280383710969403D0,
     + 2.59539458607676621D0,2.50245782805757155D0,
     + 5.53725368941236828D0,
     + 3.87435314445380663D0,5.77072652243627493D0,
     + 17.6277024547494925D0,
     + 6.29713709574403624D0,21.3444715741730811D0,
     + 134.270159991638756D0/
      CALL HEADER('C323',0)
C     Initialise maximum error
      ERRMAX=0D0
      SERRMAX=0E0
      LTEST=.TRUE.
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/5X,''X'',12X,''DFERDR'',14X,
     +        ''Exact Value'',13X,''FERDR'',8X,
     +        ''Exact '',7X,''DError'',4X,''SError'')')
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/5X,''X'',9X,''FERDR'',13X,
     +        ''Exact Value'',9X,''Error'')')
#endif
      DO 100 I=1,4
      SX(I)=X(I)
      SEXACT(I,0)=EXACT(I,0)
      SEXACT(I,1)=EXACT(I,1)
      SEXACT(I,2)=EXACT(I,2)
#if defined(CERNLIB_DOUBLE)
         RESULT(I,0)=DFERDR( X(I),K(0) )
         RESULT(I,1)=DFERDR( X(I),K(1) )
         RESULT(I,2)=DFERDR( X(I),K(2) )
         SRESULT(I,0)=FERDR(SX(I),K(0) )
         SRESULT(I,1)=FERDR(SX(I),K(1) )
         SRESULT(I,2)=FERDR(SX(I),K(2) )
#endif
#if !defined(CERNLIB_DOUBLE)
         RESULT(I,0)=FERDR( X(I),K(0) )
         RESULT(I,1)=FERDR( X(I),K(1) )
         RESULT(I,2)=FERDR( X(I),K(2) )
#endif
C        Calculate absolute errors
         ERROR(I,0)= ABS ( RESULT(I,0)-EXACT(I,0) )
         ERROR(I,1)= ABS ( RESULT(I,1)-EXACT(I,1) )
         ERROR(I,2)= ABS ( RESULT(I,2)-EXACT(I,2) )
         ERRMAX=MAX ( ERRMAX,ERROR(I,0),ERROR(I,1),ERROR(I,2) )
         LTEST=LTEST .AND. ERRMAX .LE. TSTERR
#if defined(CERNLIB_DOUBLE)
        SERROR(I,0)= ABS (SRESULT(I,0)-SEXACT(I,0) )
        SERROR(I,1)= ABS (SRESULT(I,1)-SEXACT(I,1) )
        SERROR(I,2)= ABS (SRESULT(I,2)-SEXACT(I,2) )
        SERRMAX=MAX ( SERRMAX,SERROR(I,0),SERROR(I,1),SERROR(I,2) )
         LTEST=LTEST .AND. SERRMAX .LE. STSTERR
#endif
#if !defined(CERNLIB_DOUBLE)
         WRITE(LOUT,'(F6.1,2F24.16,1P,D10.1)') X(I),RESULT(I,0),
     +   EXACT(I,0), ERROR(I,0)
         WRITE(LOUT,'(6X,2F24.16,1P,D10.1)') RESULT(I,1),EXACT(I,1),
     +   ERROR(I,1)
         WRITE(LOUT,'(6X,2F24.16,1P,D10.1)') RESULT(I,2),EXACT(I,2),
     +   ERROR(I,2)
#endif
#if defined(CERNLIB_DOUBLE)
         WRITE(LOUT,'(F6.1,2F24.16,2F15.8,1P,2D10.1)') X(I),
     +   RESULT(I,0),EXACT(I,0),
     +   SRESULT(I,0),SEXACT(I,0),ERROR(I,0),SERROR(I,0)
         WRITE(LOUT,'(6X,2F24.16,2F15.8,1P,2D10.1)') RESULT(I,1),
     +   EXACT(I,1),
     +   SRESULT(I,1),SEXACT(I,1),ERROR(I,1),SERROR(I,1)
         WRITE(LOUT,'(6X,2F24.16,2F15.8,1P,2D10.1)') RESULT(I,2),
     +   EXACT(I,2),
     +   SRESULT(I,2),SEXACT(I,2),ERROR(I,2),SERROR(I,2)
#endif
C        WRITE(LOUT,*)  RESULT(I,0), RESULT(I,1), RESULT(I,2)
100   CONTINUE
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/'' Largest Error'',1P,D10.1)') ERRMAX
#endif
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/''Double Precision largest Error'',1P,D10.1)')ERRMAX
      WRITE(LOUT,'(''Single Precision largest Error'',1P,D10.1)')SERRMAX
#endif

      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
#if defined(CERNLIB_DOUBLE)
      R=DFERDR(1D0,0)
#endif
      R= FERDR(1E0,0)
      WRITE(LOUT,'(1X)')
C     Check if the test was successful
      IRC=ITEST('C323',LTEST)
      CALL PAGEND('C323')
      RETURN
      END
