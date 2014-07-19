*
* $Id: cmmlt.F,v 1.1.1.1 1996/02/15 17:49:01 mclareni Exp $
*
* $Log: cmmlt.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:01  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE        CMMLT(M,N,K,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21,T)
          COMPLEX             X(*), X12(*), X21(*), Y(*), Y12(*), Y21(*)
          COMPLEX             Z(*), Z12(*), Z21(*), T(*), A, B, CNJF
#if !defined(CERNLIB_NUMCDBLE)
          COMPLEX             ZERO, SUM, DOTF, SQRF
          COMPLEX             S11, S21, S22, S31, S41, S51, S52
          DATA      ZERO      /  (0.,0.)  /
          DOTF(A,B,SUM)  =  A*B + SUM
          SQRF(A,SUM)    =  A*A + SUM
          CNJF(A)        =  A
#endif
#if defined(CERNLIB_NUMCDBLE)
          COMPLEX*16          ZERO, SUM, DC, DOTF, SQRF
          COMPLEX*16          S11, S21, S22, S31, S41, S51, S52
          DATA      ZERO      /  (0.D0,0.D0)  /
          DC(A)          =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
          DOTF(A,B,SUM)  =  DC(A)*DC(B) + SUM
          SQRF(A,SUM)    =  DC(A)**2 + SUM
          CNJF(A)        =  A
#endif
#include "dlocf.inc"
#include "mmlt.inc"
          END
