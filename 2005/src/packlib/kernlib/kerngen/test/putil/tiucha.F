*
* $Id: tiucha.F,v 1.1.1.1 1996/02/15 17:54:57 mclareni Exp $
*
* $Log: tiucha.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:57  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TIUCHA

#include "kerngent/mkcde.inc"

      INTEGER TINF(4),TINF2,TINF22

      DIMENSION    MCHA(600), MBIN(400), MHIS(200)
      EQUIVALENCE (IA(401), MCHA(401), MBIN(201), MHIS(1))

      DIMENSION    PBIN(3), PHIS(3)
      EQUIVALENCE (NX   ,PBIN(1)), (DX   ,PBIN(2)), (XL   ,PBIN(3))
      EQUIVALENCE (NXHIS,PHIS(1)), (DXHIS,PHIS(2)), (XLHIS,PHIS(3))

      DATA         NX   /20/,  DX   /2./,  XL   /11./
      DATA         NXHIS/20/,  DXHIS/.5/,  XLHIS/11./

      DATA  NEXP   / 3 /
      DATA  BIAS   / .001 /

      DATA TINF /10000,4HIUCH,0,4H    /, TINF2/4HIUBI/, TINF22/4HIUHI/

C-    DATA GROUPS   1 - 20  INSIDE, LOWER BIN EDGE EXACT
C-                 21 - 40  INSIDE, JUST BELOW UPPER BIN EDGE
C-                 41 - 50  UNDERFLOWS
C-                 51 - 60  OVERFLOWS

      CALL NEWGUY ('IUCHAN - IUBIN - IUHIST.','TIUCHA  ')
      CALL VZERO (A,2000)

C----              RUN INSIDE LIMITS

      DO 29  J=1,20
      X = XL + DX*FLOAT(J-1)
      MCHA(J)    = IUCHAN (X,XL,DX,NX)
      MBIN(J)    = IUBIN  (X,PBIN(1),MBIN(J+60))
      MHIS(J)    = IUHIST (X,PHIS(1),MHIS(J+60))
      IB  (J)    = J

      X  =  X + DX - BIAS
      MCHA(J+20) = IUCHAN (X,XL,DX,NX)
      MBIN(J+20) = IUBIN  (X,PBIN(1),MBIN(J+80))
      MHIS(J+20) = IUHIST (X,PHIS(1),MHIS(J+80))
   29 IB  (J+20) = J
      CALL VFILL (IB(61),40,.FALSE.)

C----              RUN UNDERFLOW

      FACT = 2 ** NEXP
      FACT = FACT * 1.007

      X  = XL - BIAS
      MCHA(41)   = IUCHAN (X,XL,DX,NX)
      MBIN(41)   = IUBIN  (X,PBIN(1),MBIN(101))
      MHIS(41)   = IUHIST (X,PHIS(1),MHIS(101))

      X  = -1.
      DO 39  J=1,9
      X  = X * FACT
      MCHA(J+41) = IUCHAN (X,XL,DX,NX)
      MBIN(J+41) = IUBIN  (X,PBIN(1),MBIN(J+101))
      MHIS(J+41) = IUHIST (X,PHIS(1),MHIS(J+101))
   39 CONTINUE

      CALL VZERO (IB(41),10)
      CALL VFILL (IB(101),20,.TRUE.)

C----              RUN OVERFLOW

      XH  =  XL + DX*FLOAT(NX)
      X   =  XH + BIAS
      MCHA(51)   = IUCHAN (X,XL,DX,NX)
      MBIN(51)   = IUBIN  (X,PBIN(1),MBIN(111))
      MHIS(51)   = IUHIST (X,PHIS(1),MHIS(111))

      Y  =  1.
      DO 49  J=1,9
      Y  = Y * FACT
      X  = XH + Y
      MCHA(J+51) = IUCHAN (X,XL,DX,NX)
      MBIN(J+51) = IUBIN  (X,PBIN(1),MBIN(J+111))
      MHIS(J+51) = IUHIST (X,PHIS(1),MHIS(J+111))
   49 CONTINUE
      CALL VFILL (IB(51),10,NX+1)

C----              CHECK

      CALL MVERII (1,MCHA,IB,60)
      CALL MVERII (2,MBIN,IB,120)
      CALL MVERII (3,MHIS,IB,120)

C----              TIMING

      IF (ITIMES.EQ.0) RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      CALL TIMED (TIMERD)
      AX=15.

         DO 80 J=1,NTIMES
   80 IA(1)=IUCHAN (AX,XL,DX,NX)

      CALL TIMING (TINF)
      TINF(2)=TINF2
      CALL TIMED (TIMERD)

         DO 82 J=1,NTIMES
   82 IA(1) = IUBIN (AX,PBIN,SPILL)

      CALL TIMING (TINF)
      TINF(2)=TINF22
      CALL TIMED (TIMERD)

         DO 84 J=1,NTIMES
   84 IA(1) = IUHIST (AX,PHIS,SPILL)

      CALL TIMING (TINF)
      RETURN

      END
