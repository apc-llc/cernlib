*
* $Id: vsum.s,v 1.1.1.1 1996/02/15 17:54:07 mclareni Exp $
*
* $Log: vsum.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:07  mclareni
* Kernlib
*
*
. *************************************
.
.     FUNCTION VSUM( A,N )                       *** F 121 ***
.     Y = A(1)+   +A(N)
.     RETURNS VSUM=0.  IF N.EQ.0
. *   ERROR EXIT GERRB$ IF N.LT.0
.
. *************************************
 
          AXR$
          FORT$
$(1),FIRS01
          +         'VSUM  '
VSUM*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(A), LOC(N)
          SZ        A0
          L         A4,0,A3            . N
          LXI,U     A2,1
          JZ        A4,RETUR2          . VSUM=0. IF N.EQ.0
          JGD       A4,$+2             . N-1
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LT.0
LOOP
          FA        A0,0,*A2
VSUM1     JGD       A4,LOOP
          J         RETUR2
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,VSUM
          +         0,DYN$
          +         'VSUM  '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VSUM( A,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_VSUM
#undef CERNLIB_TCGEN_VSUM
#endif
