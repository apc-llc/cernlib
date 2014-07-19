*
* $Id: cvmpac.F,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: cvmpac.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          COMPLEX FUNCTION CVMPAC(N,X,X2,Y,Y2,S)
          COMPLEX             X(*), X2(*), Y(*), Y2(*), S, A, B
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
          SUM  =  S
          IF(N .LE. 0)  GOTO 20
#include "dxj.inc"
#include "dyj.inc"
#include "vmpa.inc"
  20      CVMPAC  =  SUM
          RETURN
          END
