*
* $Id: cmbil.F,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: cmbil.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          COMPLEX FUNCTION CMBIL(N,X,X2,Y,Y12,Y21,Z,Z2)
          COMPLEX X(*),X2(*),Y(*),Y12(*),Y21(*),Z(*),Z2(*)
          COMPLEX   A, B
#if !defined(CERNLIB_NUMCDBLE)
          COMPLEX           SUM, ZERO, F, G, SXYZ, SYZ
          F(A,B,SUM)  =  A*B + SUM
          G(SYZ,B,SUM)=  SYZ*B + SUM
          DATA      ZERO      /  (0.,0.)  /
#endif
#if defined(CERNLIB_NUMCDBLE)
          COMPLEX*16          SUM, ZERO, DC, F, G, SXYZ, SYZ
          DC(A)       =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
          F(A,B,SUM)  =  DC(A)*DC(B) + SUM
          G(SYZ,B,SUM)=  SYZ*DC(B) + SUM
          DATA      ZERO      /  (0.D0,0.D0)  /
#endif
          SXYZ  =  ZERO
          IF(N .LE. 0)  GOTO 20
#include "cxi.inc"
#include "cyij.inc"
#include "czj.inc"
#include "mbil.inc"
  20      CMBIL  =  SXYZ
          RETURN
          END
