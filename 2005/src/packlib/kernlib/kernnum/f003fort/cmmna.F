*
* $Id: cmmna.F,v 1.1.1.1 1996/02/15 17:48:57 mclareni Exp $
*
* $Log: cmmna.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:57  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CMMNA(M,N,X,X12,X21,Y,Y2,Z,Z2)
          COMPLEX             X(*),X12(*),X21(*),Y(*),Y2(*),Z(*),Z2(*)
          COMPLEX             A, B, SIGNF
#if !defined(CERNLIB_NUMCDBLE)
          COMPLEX             SUM, F
          F(A,B,SUM)  =  -A*B + SUM
#endif
#if defined(CERNLIB_NUMCDBLE)
          COMPLEX*16          SUM, DC, F
          DC(A)       =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
          F(A,B,SUM)  =  -DC(A)*DC(B) + SUM
#endif
#if defined(CERNLIB_IBMAIX)
          SIGNF(A)    =  A*1
#else
          SIGNF(A)    =  A
#endif
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "cxij.inc"
#include "cyj.inc"
#include "czi.inc"
#include "mmpa.inc"
