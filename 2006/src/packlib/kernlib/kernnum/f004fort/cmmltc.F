*
* $Id: cmmltc.F,v 1.1.1.1 1996/02/15 17:49:01 mclareni Exp $
*
* $Log: cmmltc.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:01  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE       CMMLTC(M,N,K,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21,T)
          COMPLEX             X(*), X12(*), X21(*), Y(*), Y12(*), Y21(*)
          COMPLEX             Z(*), Z12(*), Z21(*), T(*), A, B, CNJF
#if !defined(CERNLIB_NUMCDBLE)
          COMPLEX             ZERO, SUM, DOTF, SQRF
          COMPLEX             S11, S21, S22, S31, S41, S51, S52
          DATA      ZERO      /  (0.,0.)  /
          DOTF(A,B,SUM)  =  A*CONJG(B) + SUM
          SQRF(A,SUM)    =  REAL(A)**2 +AIMAG(A)**2 + SUM
          CNJF(A)        =  CONJG(A)
#endif
#if (defined(CERNLIB_NUMCDBLE))&&(!defined(CERNLIB_NUMND))
          COMPLEX*16          ZERO, SUM, DC, DOTF, SQRF
          COMPLEX*16          S11, S21, S22, S31, S41, S51, S52
          DATA      ZERO      /  (0.D0,0.D0)  /
          DC(A)          =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
          DOTF(A,B,SUM)  =  DC(A)*DCONJG(DC(B)) + SUM
          SQRF(A,SUM)    =  DBLE(REAL(A))**2 + DBLE(AIMAG(A))**2 + SUM
          CNJF(A)        =  CONJG(A)
#endif
#if (defined(CERNLIB_NUMCDBLE))&&(defined(CERNLIB_NUMND))
          COMPLEX*16          ZERO, SUM, DC, DOTF, SQRF
          COMPLEX*16          S11, S21, S22, S31, S41, S51, S52
          DATA      ZERO      /  (0.D0,0.D0)  /
          DC(A)          =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
          DOTF(A,B,SUM)  =  DC(A)*CDCONJG(DC(B)) + SUM
          SQRF(A,SUM)    =  DBLE(REAL(A))**2 + DBLE(AIMAG(A))**2 + SUM
          CNJF(A)        =  CONJG(A)
#endif
#include "dlocf.inc"
#include "mmltc.inc"
          END
