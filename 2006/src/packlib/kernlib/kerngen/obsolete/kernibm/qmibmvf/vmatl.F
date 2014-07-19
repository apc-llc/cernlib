*
* $Id: vmatl.F,v 1.1.1.1 1996/02/15 17:53:06 mclareni Exp $
*
* $Log: vmatl.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:06  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      SUBROUTINE VMATL (H,A,X,K,N)
C
C CERN PROGLIB# F121    VMATL           .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 24/05/89  M.Roethlisberger/IBM        Optimized/Vectorized
C
      DIMENSION A(99),X(99),H(N,K)
      PARAMETER (LVMIN   =    7,   NS  =    96)
      REAL*8     TMPI, TMPJ

      IF (MAX0(N,K).GE.LVMIN) THEN

        IF (N.GT.K.OR.N.GT.NS) THEN
C
C Vectorize over J
C
          DO 71 I=1,K
            X(I)=0.
   71     CONTINUE

          DO 79 J = 1,N
            TEMP = A(J)
            DO 75 I= 1,K
              X(I)= X(I) + TEMP*H(J,I)
   75       CONTINUE
   79     CONTINUE

          RETURN
        ENDIF

C
C Vectorize over I
C
C*VDIR: PREFER VECTOR
        DO 89 I= 1,K
          TMPI=0.
          DO 85 J= 1,N
            TMPJ= A(J)
            TMPI= TMPI + TMPJ*H(J,I)
   85     CONTINUE
          X(I)=TMPI
   89   CONTINUE
        RETURN
        ENDIF

C*VDIR: PREFER SCALAR ON

       DO 99 I= 1,K
         TMPI= 0.

         DO 95 J= 1,N
           TMPJ= A(J)
           TMPI= TMPI + TMPJ*H(J,I)
   95    CONTINUE
         X(I)=TMPI
   99  CONTINUE
      END
#ifdef CERNLIB_TCGEN_VMATL
#undef CERNLIB_TCGEN_VMATL
#endif
