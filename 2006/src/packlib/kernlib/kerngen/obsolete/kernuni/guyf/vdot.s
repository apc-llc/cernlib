*
* $Id: vdot.s,v 1.1.1.1 1996/02/15 17:54:06 mclareni Exp $
*
* $Log: vdot.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:06  mclareni
* Kernlib
*
*
. *************************************
.
.     FUNCTION VDOT( A, B, N )                   *** F 121 ***
.     Y = A.B = A(1)*B(1)+   +A(N)*B(N)
.     A AND B ARE FLOATING POINT SINGLE PRECISION VECTORS
.     RETURNS VDOT=0. IF N.EQ.0
. *   ERROR EXIT GERRB$ IF N.LT.0
. *   NO CHECK ON FLOATING-POINT UNDERFLOW AND OVERFLOW
.
. *************************************
 
          AXR$
          FORT$
$(1),FIRS01
          +         'VDOT  '
VDOT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A1,2,XARG          . LOC(N)
          DL        A2,0,XARG          . LOC(A), LOC(B)
          L         A0,0,A1            . N
          LXI,U     A2,1
          JZ        A0,RETUR3          . RETURNS VDOT=0. IF N=0
          S         A0,R1
          LXI,U     A3,1
 .        SZ        A0                 . NOT NEEDED
          JGD       R1,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LT.0
VDOT1     L         A4,0,*A2      . A(I)
          FM        A4,0,*A3      . A(I)*B(I)
          FA        A0,A4
          JGD       R1,VDOT1
          J         RETUR3
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.         DIAGNOSTIC PACKETS FOR FORTRAN T.
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO  010  03
$(3)
DIAG1     F         077,050,3,VDOT
          +         0,DYN$
          +         'VDOT  '
          ON    GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VDOT( A,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF . GERMAX
          ON    GERMAX=0
DIAG1     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF .
#endif
          END
#ifdef CERNLIB_TCGEN_VDOT
#undef CERNLIB_TCGEN_VDOT
#endif
