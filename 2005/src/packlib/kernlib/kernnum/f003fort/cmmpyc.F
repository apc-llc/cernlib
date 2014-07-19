*
* $Id: cmmpyc.F,v 1.1.1.1 1996/02/15 17:48:58 mclareni Exp $
*
* $Log: cmmpyc.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:58  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CMMPYC(M,N,X,X12,X21,Y,Y2,Z,Z2)
          COMPLEX             X(*),X12(*),X21(*),Y(*),Y2(*),Z(*),Z2(*)
          COMPLEX             A, B
#if !defined(CERNLIB_NUMCDBLE)
          COMPLEX             SUM, ZERO, F
          F(A,B,SUM)  =  A*CONJG(B) + SUM
          DATA      ZERO      /  (0.,0.)  /
#endif
#if (defined(CERNLIB_NUMCDBLE))&&(!defined(CERNLIB_NUMND))
          COMPLEX*16          SUM, ZERO, DC, F
          DC(A)       =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
          F(A,B,SUM)  =  DC(A)*DCONJG(DC(B)) + SUM
          DATA      ZERO      /  (0.D0,0.D0)  /
#endif
#if (defined(CERNLIB_NUMCDBLE))&&(defined(CERNLIB_NUMND))
          COMPLEX*16          SUM, ZERO, DC, F
          DC(A)       =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
          F(A,B,SUM)  =  DC(A)*CDCONJG(DC(B)) + SUM
          DATA      ZERO      /  (0.D0,0.D0)  /
#endif
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "cxij.inc"
#include "cyj.inc"
#include "czi.inc"
#include "mmpy.inc"
