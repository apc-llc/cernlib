*
* $Id: c343s.F,v 1.1.1.1 1996/04/01 15:01:19 mclareni Exp $
*
* $Log: c343s.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:19  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C343S
C     Test BSJA/BSIA
      DIMENSION TBJ(24),TBI(24)
      DIMENSION BJ(0:101),BI(0:101)
      LOGICAL LTEST1
      COMMON /C343LT1/LTEST1
#include "iorc.inc"
#include "c343codd.inc"
      WRITE(LOUT,'(/5X,''A'',4X,''N'',7X,''X'',7X,''BJ/BI'',
     + 10X,''ERR'')')
#include "c343cod1.inc"
      ERMAX=0
#if defined(CERNLIB_DOUBLE)
      ND=5
#endif
#if !defined(CERNLIB_DOUBLE)
      ND=12
#endif
      CALL BSJA(X,A,N,ND,BJ)
      ERR=ABS((TBJ(IC)-BJ(ABS(N)))/TBJ(IC))
      ERMAX=MAX(ERMAX,ERR)
      WRITE(LOUT,'(1X,F6.1,I4,F8.1,1P,E15.6,E10.1/)') A,N,X,
     +BJ(ABS(N)),ERR
      LTEST1=LTEST1 .AND. (ERMAX .LE. TSTERS)
#include "c343cod2.inc"
#if defined(CERNLIB_DOUBLE)
      ND=5
#endif
#if !defined(CERNLIB_DOUBLE)
      ND=12
#endif
      CALL  BSIA(X,A,N,ND,BI)
      ERR=ABS((TBI(IC)-BI(ABS(N)))/TBI(IC))
      ERMAX=MAX(ERMAX,ERR)
      WRITE(LOUT,'(1X,F6.1,I4,F8.1,1P,E15.6,E10.1/)') A,N,X,
     +BI(ABS(N)),ERR
      LTEST1=LTEST1 .AND. (ERMAX .LE. TSTERS)
   13 CONTINUE
   12 CONTINUE
   11 CONTINUE
      WRITE(LOUT,'(/''BSJA/BSIA: Largest Error'',1P,E10.1)') ERMAX
      WRITE(LOUT,'(1X)')
      CALL  BSJA(-R1,R0,1,ND,BJ)
      CALL  BSJA(R1,2*R1,1,ND,BJ)
      CALL  BSJA(R1,HF,101,ND,BJ)
      WRITE(LOUT,'(1X)')
      CALL  BSIA(-R1,R0,1,ND,BI)
      CALL  BSIA(R1,2*R1,1,ND,BI)
      CALL  BSIA(R1,HF,-101,ND,BI)
      END
