*
* $Id: rmmlt.F,v 1.1.1.1 1996/02/15 17:49:01 mclareni Exp $
*
* $Log: rmmlt.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:01  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE        RMMLT(M,N,K,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21,T)
          REAL                X(*), X12(*), X21(*), Y(*), Y12(*), Y21(*)
          REAL                Z(*), Z12(*), Z21(*), T(*), A, B, CNJF
#if !defined(CERNLIB_NUMRDBLE)
          REAL                ZERO, SUM, DOTF, SQRF
          REAL                S11, S21, S22, S31, S41, S51, S52
          DATA      ZERO      /  0.  /
          DOTF(A,B,SUM)  =  A*B + SUM
          SQRF(A,SUM)    =  A*A + SUM
          CNJF(A)        =  A
#endif
#if defined(CERNLIB_NUMRDBLE)
          DOUBLE PRECISION    ZERO, SUM, DOTF, SQRF
          DOUBLE PRECISION    S11, S21, S22, S31, S41, S51, S52
          DATA      ZERO      /  0.D0  /
          DOTF(A,B,SUM)  =  DBLE(A)*DBLE(B) + SUM
          SQRF(A,SUM)    =  DBLE(A)**2 + SUM
          CNJF(A)        =  A
#endif
#include "rlocf.inc"
#include "mmlt.inc"
          END
