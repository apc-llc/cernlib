*
* $Id: vmatr.F,v 1.1.1.1 1996/02/15 17:54:33 mclareni Exp $
*
* $Log: vmatr.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:33  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      SUBROUTINE VMATR (A,G,X,N,K)
C
C CERN PROGLIB# F121    VMATR           .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 24/05/89  M.Roethlisberger/IBM        Optimized/Vectorized
C
      DIMENSION A(99),X(99),G(K,N)
      PARAMETER (LVMIN   =   7, KS    =   128)
      REAL*8     TMPI, TMPJ

      IF (MAX0(K,N).GE.LVMIN) THEN

        IF (K.GT.N.OR.K.GT.KS) THEN
C
C Vectorize over I
C
          DO 79 I= 1,K
          TMPI=0
            DO 75 J= 1,N
              TMPJ= A(J)
              TMPI= TMPI + TMPJ*G(I,J)
   75       CONTINUE
          X(I)=TMPI
   79     CONTINUE

          RETURN
        ENDIF
C
C Vectorize over J
C
        DO 81 I=1,K
          X(I)=0.
   81   CONTINUE

C*VDIR: PREFER VECTOR
        DO 89 J= 1,N
          TEMP = A(J)
C*VDIR: PREFER SCALAR
          DO 85 I= 1,K
            X(I) = X(I)+TEMP*G(I,J)
   85     CONTINUE
   89   CONTINUE

        RETURN
      ENDIF

C*VDIR: PREFER SCALAR ON

       DO 99 I= 1,K
         TMPI= 0.
         DO 95 J= 1,N
           TMPJ=A(J)
           TMPI= TMPI + TMPJ*G(I,J)
   95    CONTINUE
       X(I)=TMPI
   99  CONTINUE

      END
#ifdef CERNLIB_TCGEN_VMATR
#undef CERNLIB_TCGEN_VMATR
#endif
