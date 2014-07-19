*
* $Id: cvmpyc.F,v 1.1.1.1 1996/02/15 17:48:52 mclareni Exp $
*
* $Log: cvmpyc.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:52  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          COMPLEX FUNCTION CVMPYC(N,X,X2,Y,Y2)
          COMPLEX             X(*), X2(*), Y(*), Y2(*), A, B
#if (defined(CERNLIB_NUMCDBLE))&&(!defined(CERNLIB_NUMND))
          COMPLEX*16          SUM,  MPA,  DC
          DC(A)  =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
          MPA(A,B,SUM)  =  DC(A)*DCONJG(DC(B)) + SUM
#endif
#if (defined(CERNLIB_NUMCDBLE))&&(defined(CERNLIB_NUMND))
          COMPLEX*16          SUM,  MPA,  DC
          DC(A)  =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
          MPA(A,B,SUM)  =  DC(A)*CDCONJG(DC(B)) + SUM
#endif
#if !defined(CERNLIB_NUMCDBLE)
          COMPLEX             SUM,  MPA
          MPA(A,B,SUM)  =  A*CONJG(B) + SUM
#endif
          SUM  =  (0.,0.)
          IF(N .LE. 0)  GOTO 20
#include "dxj.inc"
#include "dyj.inc"
#include "vmpa.inc"
  20      CVMPYC  =  SUM
          RETURN
          END
