*
* $Id: c343q.F,v 1.1.1.1 1996/04/01 15:01:19 mclareni Exp $
*
* $Log: c343q.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:19  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if (defined(CERNLIB_QUAD))&&(defined(CERNLIB_DOUBLE))
      SUBROUTINE C343Q
C     Test QBSJA/QBSIA
#include "gen/imp128.inc"

      DIMENSION TBJ(24),TBI(24)
      DIMENSION BJ(0:101),BI(0:101)
      LOGICAL LTEST3
      COMMON /C343LT3/LTEST3
#include "iorc.inc"
#include "c343codd.inc"
      WRITE(LOUT,'(/5X,''A'',4X,''N'',5X,''X'',20X,''BJ/BI'',
     + 22X,''ERR'')')
#include "c343cod1.inc"
      ERMAXQ=0

      ND=31
      CALL QBSJA(X,A,N,ND,BJ)
      ERR=ABS((TBJ(IC)-BJ(ABS(N)))/TBJ(IC))
      ERMAXQ=MAX(ERMAXQ,ERR)
      WRITE(LOUT,'(1X,F6.1,I4,F8.1,1P,E42.32,E10.1/)') A,N,X,
     +BJ(ABS(N)),ERR
      LTEST3=LTEST3 .AND. (ERMAXQ .LE. TSTERQ)
#include "c343cod2.inc"

      ND=31
      CALL QBSIA(X,A,N,ND,BI)
      ERR=ABS((TBI(IC)-BI(ABS(N)))/TBI(IC))
      ERMAXQ=MAX(ERMAXQ,ERR)
      WRITE(LOUT,'(1X,F6.1,I4,F8.1,1P,E42.32,E10.1/)') A,N,X,
     +BI(ABS(N)),ERR
      LTEST3=LTEST3 .AND. (ERMAXQ .LE. TSTERQ)
   13 CONTINUE
   12 CONTINUE
   11 CONTINUE
      WRITE(LOUT,'(/''QBSJA/QBSIA: Largest Error'',1P,D10.1)') ERMAXQ
      WRITE(LOUT,'(1X)')
      CALL QBSJA(-R1,R0,1,ND,BJ)
      CALL QBSJA(R1,2*R1,1,ND,BJ)
      CALL QBSJA(R1,HF,101,ND,BJ)
      WRITE(LOUT,'(1X)')
      CALL QBSIA(-R1,R0,1,ND,BI)
      CALL QBSIA(R1,2*R1,1,ND,BI)
      CALL QBSIA(R1,HF,-101,ND,BI)
      END
#endif
