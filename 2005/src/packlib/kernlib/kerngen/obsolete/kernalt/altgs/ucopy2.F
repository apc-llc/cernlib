*
* $Id: ucopy2.F,v 1.1.1.1 1996/02/15 17:50:55 mclareni Exp $
*
* $Log: ucopy2.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:55  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      SUBROUTINE UCOPY2 (A,B,N)
C
C CERN PROGLIB# V301    UCOPY2          .VERSION KERNALT  1.01  880222
C ORIG. 22/02/88 JZ
C
C--   Vectors A and B may be overlapping
 
      INTEGER      A(99), B(99)
 
      IA = LOC (A(1))
      IB = LOC (B(1))
      IF (N.EQ.0)            RETURN
      IF (IA-IB)             41, 99, 21
 
C----         IB < IA : the beginning of A may overlap the end of B
 
   21 IF (IB+4*N .LE. IA)  THEN
          B(1:N) = A(1:N)
          RETURN
        ENDIF
 
CVD$  NOCONCUR
      DO 24 I=1,N
   24 B(I) = A(I)
CVD$  CONCUR
      RETURN
 
C----         IA < IB : the beginning of B may overlap the end of A
 
   41 IF (IA+4*N .LE. IB)  THEN
          B(1:N) = A(1:N)
          RETURN
        ENDIF
 
CVD$  NOCONCUR
      DO 44 I=N,1,-1
   44 B(I) = A(I)
CVD$  CONCUR
   99 RETURN
      END
#ifdef CERNLIB_TCGEN_UCOPY2
#undef CERNLIB_TCGEN_UCOPY2
#endif
