*
* $Id: cumpy.F,v 1.1.1.1 1996/02/15 17:49:00 mclareni Exp $
*
* $Log: cumpy.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:00  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CUMPY(N,U,U12,U22,Y,Y2,Z,Z2)
          COMPLEX             U(*),U12(*),U22(*),Y(*),Y2(*),Z(*),Z2(*)
          COMPLEX   A, B
#if !defined(CERNLIB_NUMCDBLE)
          COMPLEX             SUM, ZERO, F
          F(A,B,SUM)  =  A*B + SUM
          DATA      ZERO      /  (0.,0.)  /
#endif
#if defined(CERNLIB_NUMCDBLE)
          COMPLEX*16          SUM, ZERO, DC, F
          DC(A)       =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
          F(A,B,SUM)  =  DC(A)*DC(B) + SUM
          DATA      ZERO      /  (0.D0,0.D0)  /
#endif
          IF(N .LE. 0)  RETURN
#include "cujk.inc"
#include "cyj.inc"
#include "czi.inc"
#include "umpy.inc"
