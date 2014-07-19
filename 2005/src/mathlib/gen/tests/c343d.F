*
* $Id: c343d.F,v 1.1.1.1 1996/04/01 15:01:19 mclareni Exp $
*
* $Log: c343d.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:19  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C343D
C     Test DBSJA/DBSIA

#if defined(CERNLIB_DOUBLE)
#include "gen/imp64.inc"
#endif
#if !defined(CERNLIB_DOUBLE)
#include "gen/imp128.inc"
#endif
      DIMENSION TBJ(24),TBI(24)
      DIMENSION BJ(0:101),BI(0:101)
      LOGICAL LTEST2
      COMMON /C343LT2/LTEST2
#include "iorc.inc"
#include "c343codd.inc"
      WRITE(LOUT,'(/5X,''A'',4X,''N'',5X,''X'',14X,''BJ/BI'',
     + 13X,''ERR'')')
#include "c343cod1.inc"
      ERMAXD=0

#if defined(CERNLIB_DOUBLE)
      ND=15
#endif
#if !defined(CERNLIB_DOUBLE)
      ND=26
#endif
      CALL DBSJA(X,A,N,ND,BJ)
      ERR=ABS((TBJ(IC)-BJ(ABS(N)))/TBJ(IC))
      ERMAXD=MAX(ERMAXD,ERR)
      WRITE(LOUT,'(1X,F6.1,I4,F8.1,1P,D28.16,D10.1/)') A,N,X,
     +BJ(ABS(N)),ERR
      LTEST2=LTEST2 .AND. (ERMAXD .LE. TSTERD)
#include "c343cod2.inc"
#if defined(CERNLIB_DOUBLE)
      ND=15
#endif
#if !defined(CERNLIB_DOUBLE)
      ND=26
#endif
      CALL DBSIA(X,A,N,ND,BI)
      ERR=ABS((TBI(IC)-BI(ABS(N)))/TBI(IC))
      ERMAXD=MAX(ERMAXD,ERR)
      WRITE(LOUT,'(1X,F6.1,I4,F8.1,1P,D28.16,D10.1/)') A,N,X,
     +BI(ABS(N)),ERR
      LTEST2=LTEST2 .AND. (ERMAXD .LE. TSTERD)
   13 CONTINUE
   12 CONTINUE
   11 CONTINUE
      WRITE(LOUT,'(/''DBSJA/DBSIA: Largest Error'',1P,D10.1)') ERMAXD
      WRITE(LOUT,'(1X)')
      CALL DBSJA(-R1,R0,1,ND,BJ)
      CALL DBSJA(R1,2*R1,1,ND,BJ)
      CALL DBSJA(R1,HF,101,ND,BJ)
      WRITE(LOUT,'(1X)')
      CALL DBSIA(-R1,R0,1,ND,BI)
      CALL DBSIA(R1,2*R1,1,ND,BI)
      CALL DBSIA(R1,HF,-101,ND,BI)
      END
