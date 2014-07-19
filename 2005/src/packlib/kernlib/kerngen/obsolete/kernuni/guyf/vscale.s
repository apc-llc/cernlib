*
* $Id: vscale.s,v 1.1.1.1 1996/02/15 17:54:07 mclareni Exp $
*
* $Log: vscale.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:07  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     SUBROUTINE  VSCALE( A,ALPHA,X,N )          *** F 121 ***
.     SETS  X(I) = ALPHA*A(I)  FOR I=1,N
.     DO NOTHING IF N.EQ.0
. *   ERROR EXIT GERRB$ IF N.LT.0
. *   ANY OVERLAP
. *   SINGLE PRECISION A,ALPHA,X
.
. $$$*****$$$*****$$$*****$$$*****
.
          AXR$
          FORT$
$(1),FIRS01
          +         'VSCALE'
VSCALE*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,2,XARG          . LOC(X), LOC(N)
          DL        A0,0,XARG          . LOC(A), LOC(ALPHA)
          L         A3,0,A3            . N
          L         A1,0,A1            . ALPHA
          JZ        A3,RETUR4          . DO NOTHING IF N=0
          JGD       A3,$+2             . N-1
LMJER1    SLJ       GERR$              . IF  N.LT.0
          TLE       A0,A2
          J         VSC2
          LXI,U     A0,1
          LXI,U     A2,1
LOOP      LA        A4,0,*A0 .
          FM        A4,A1
          S         A4,0,*A2
          JGD       A3,LOOP .
          J         RETUR4
VSC2                                   . LOC(B).GT.LOC(A)
          A         A0,A3              . LOC(A(N))
          A         A2,A3
          LXI,U     A0,-1              .
          LXI,U     A2,-1
          J         LOOP
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,VSCALE
          +         0,DYN$
          +         'VSCALE'
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VSCALE( ,,,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_VSCALE
#undef CERNLIB_TCGEN_VSCALE
#endif
