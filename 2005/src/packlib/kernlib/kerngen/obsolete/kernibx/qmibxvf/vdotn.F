*
* $Id: vdotn.F,v 1.1.1.1 1996/02/15 17:54:34 mclareni Exp $
*
* $Log: vdotn.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:34  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      FUNCTION VDOTN (X,Y,N)
C
C CERN PROGLIB# F121    VDOTN           .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 28/02/89  M.Roethlisberger/IBM        Optimization/Vectorization
C
      DIMENSION X(*), Y(*)
      REAL*8    XX8, XY8, YY8
      PARAMETER (LVMIN   =  25)
C
      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         XX= 0.
         XY= 0.
         YY= 0.
C
         DO 90 J=1,N
            XX = XX + X(J)*X(J)
            XY = XY + X(J)*Y(J)
   90       YY = YY + Y(J)*Y(J)
C
         VDOTN= XY / SQRT(XX*YY)
         IF (ABS (VDOTN).LT.1.) RETURN
         VDOTN= SIGN (1.,VDOTN)

      ELSE IF (N.GT.0) THEN
C
C*VDIR: PREFER SCALAR

         XX8 = 0.
         XY8 = 0.
         YY8 = 0.
C
         DO 95 J=1,N
            XX8 = XX8 + DBLE(X(J))*DBLE(X(J))
            XY8 = XY8 + DBLE(X(J))*DBLE(Y(J))
   95       YY8 = YY8 + DBLE(Y(J))*DBLE(Y(J))
C
         XX = XX8
         XY = XY8
         YY = YY8

         VDOTN= XY / SQRT(XX*YY)
         IF (ABS (VDOTN).LT.1.) RETURN
         VDOTN= SIGN (1.,VDOTN)

      ELSE

         VDOTN = 0.0

      ENDIF
      END
#ifdef CERNLIB_TCGEN_VDOTN
#undef CERNLIB_TCGEN_VDOTN
#endif
