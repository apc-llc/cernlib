*
* $Id: c322m.F,v 1.1.1.1 1996/04/01 15:01:16 mclareni Exp $
*
* $Log: c322m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:16  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C322M
C     This program tests the GENLIB routines DFRSIN,DFRCOS,FRSIN,FRCOS,
C     (C322) by comparing computed results against those obtained from
C     another source of numerical integration.
C     Written by T Hepworth, Brunel University, England, 23.4.90
C     Revised by B. Damgaard Sept. 1992
#include "gen/imp64.inc"
      REAL FRCOS, FRSIN,SX(7),SEXACT(7,0:1),SRESULT(7,0:1)
      REAL STSTERR,SERRMAX,SERROR(7,0:1)
#include "gen/def64.inc"
     +     DFRCOS,DFRSIN
#include "gen/def64.inc"
     +      TSTERR,ERRMAX
      DIMENSION  X(7),EXACT(7,0:1),RESULT(7,0:1),ERROR(7,0:1)
C     Set the maximum error allowed for the test to still be considered
C     successful
      PARAMETER ( TSTERR=1D-13 )
#if defined(CERNLIB_DOUBLE)
      PARAMETER (STSTERR=1D-6  )
#endif
      LOGICAL LTEST
#include "iorc.inc"

C     Set up test data and theoretical solutions
C     DFRSIN (or FRSIN) test data
      DATA X(1),EXACT(1,0) /  0.0D0,  0D0                  /
      DATA X(2),EXACT(2,0) / -0.4D0, -0.1667371102944582D0 /
      DATA X(3),EXACT(3,0) /  0.4D0,  0.1667371102944582D0 /
      DATA X(4),EXACT(4,0) /  2.0D0,  1.4108529827013918D0 /
      DATA X(5),EXACT(5,0) /  7.6D0,  1.1413499262090459D0 /
      DATA X(6),EXACT(6,0) /  8.0D0,  1.2834177865335759D0 /
      DATA X(7),EXACT(7,0) / 12.4D0,  0.9764064367978704D0 /
C     DFRCOS (or FRCOS) test data
      DATA EXACT(1,1) /  0D0                     /
      DATA EXACT(2,1) / -1.2448218501015764D0    /
      DATA EXACT(3,1) /  1.2448218501015764D0    /
      DATA EXACT(4,1) /  1.8882490336945132D0    /
      DATA EXACT(5,1) /  1.5946919390901555D0    /
      DATA EXACT(6,1) /  1.6024905840697365D0    /
      DATA EXACT(7,1) /  1.1954629800381893D0    /

      CALL HEADER('C322',0)
      LTEST= .TRUE.
C     Initialise maximum error
      ERRMAX=0D0
      SERRMAX=0E0
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/5X,''X'',7X,''DFRSIN/DFRCOS'',11X,
     +        ''Exact Value'',10X,''FRSIN/FRCOS'',5X,
     +        ''Exact '',7X,''DError'',4X,''SError'')')
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/5X,''X'',9X,''FRSIN/FRCOS'',13X,
     +        ''Exact Value'',9X,''Error'')')
#endif
      DO 100 I=1,7
      SX(I)=X(I)
      SEXACT(I,0)=EXACT(I,0)
      SEXACT(I,1)=EXACT(I,1)
#if defined(CERNLIB_DOUBLE)
         RESULT(I,0)=DFRSIN(X(I))
         RESULT(I,1)=DFRCOS(X(I))
        SRESULT(I,0)= FRSIN(SX(I))
        SRESULT(I,1)= FRCOS(SX(I))
#endif
#if !defined(CERNLIB_DOUBLE)
         RESULT(I,0)=FRSIN(X(I))
         RESULT(I,1)=FRCOS(X(I))
#endif
C        Calculate absolute errors
         ERROR(I,0)= ABS ( RESULT(I,0)-EXACT(I,0) )
         ERROR(I,1)= ABS ( RESULT(I,1)-EXACT(I,1) )
#if defined(CERNLIB_DOUBLE)
        SERROR(I,0)= ABS (SRESULT(I,0)-SEXACT(I,0) )
        SERROR(I,1)= ABS (SRESULT(I,1)-SEXACT(I,1) )
        SERRMAX=MAX (SERRMAX,SERROR(I,0),SERROR(I,1) )
         ERRMAX=MAX ( ERRMAX,ERROR(I,0),ERROR(I,1) )
         WRITE(LOUT,'(F6.1,2F24.18,2F15.9,1P,2D10.1)')X(I),RESULT(I,0),
     +   EXACT(I,0),SRESULT(I,0),SEXACT(I,0),
     +   ERROR(I,0),SERROR(I,0)
         WRITE(LOUT,'(6X,2F24.18,2F15.9,1P,2D10.1)') RESULT(I,1),
     +   EXACT(I,1),
     +   SRESULT(I,1),SEXACT(I,1),ERROR(I,1),SERROR(I,1)
#endif
#if !defined(CERNLIB_DOUBLE)
         ERRMAX=MAX ( ERRMAX,ERROR(I,0),ERROR(I,1) )
         WRITE(LOUT,'(F6.1,3F24.16)') X(I),RESULT(I,0),EXACT(I,0),
     +         ERROR(I,0)
         WRITE(LOUT,'(6X,3F24.16)') RESULT(I,1),EXACT(I,1),ERROR(I,1)
#endif

100   CONTINUE
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/'' Largest Error'',1P,D10.1)') ERRMAX
#endif
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/''Double Precision largest Error'',1P,D10.1)')ERRMAX
      WRITE(LOUT,'(''Single Precision largest Error'',1P,D10.1)')SERRMAX
#endif

      LTEST=LTEST .AND. (ERRMAX .LE. TSTERR)
#if defined(CERNLIB_DOUBLE)
      LTEST=LTEST .AND. (SERRMAX .LE. STSTERR)
#endif
      WRITE(LOUT,'(1X)')
C     Check if the test was successful
      IRC=ITEST('C322',LTEST)
      CALL PAGEND('C322')
      RETURN
      END
