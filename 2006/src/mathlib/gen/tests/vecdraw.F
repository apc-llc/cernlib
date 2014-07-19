*
* $Id: vecdraw.F,v 1.1.1.1 1996/04/01 15:01:31 mclareni Exp $
*
* $Log: vecdraw.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:31  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE VECDRAW(V,N)
C
C CERN PROGLIB#         VECDRAW         .VERSION GENT     1.02  920318
C Author: M.Marquina 92/03/18
C
C-    Simple vector drawing routine
C
      DIMENSION V(*)
      CHARACTER*78 LINE,REPEAT
C
      VMAX=0.
      VMIN=0.
      DO 10 I=1,N
         IF(V(I).GT.VMAX) VMAX=V(I)
         IF(V(I).LT.VMIN) VMIN=V(I)
10    CONTINUE
      LWIDTH=LEN(LINE)
      LINE  =REPEAT('-',LWIDTH)
C
      VWIDTH=VMAX-VMIN
      I0    =-VMIN/VWIDTH *LWIDTH + 1
      IF(I0.GT.LWIDTH) I0=LWIDTH
      LINE(I0:I0)='|'
      PRINT 1000,N,VMIN,VMAX
      PRINT 1001,LINE
C
      DO 20 I=1,N
      LINE=' '
      I1=(V(I)-VMIN)/VWIDTH *LWIDTH +1
      IF(I1.GT.LWIDTH) I1=LWIDTH
      IF(I1.LT.1     ) I1=1
      IS=SIGN(1.,V(I))
         J=I1
      DO WHILE(J.NE.I0)
         LINE(J:J)='*'
         J=J-IS
      ENDDO
      LINE(I0:I0)='|'
      PRINT 1001,LINE
20    CONTINUE
      PRINT 1001
      RETURN
1000  FORMAT(/' Vector drawing:',I4,' components; range:',2F8.2/)
1001  FORMAT(1X,A)
      END
