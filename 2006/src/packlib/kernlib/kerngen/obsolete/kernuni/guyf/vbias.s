*
* $Id: vbias.s,v 1.1.1.1 1996/02/15 17:54:06 mclareni Exp $
*
* $Log: vbias.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:06  mclareni
* Kernlib
*
*
.  ************************************
.
.     SUBROUTINE VBIAS ( A, ALPHA, X, N)         *** F 121 ***
.     X(I) = A(I) +ALPHA
.     ALPHA IS A FLOATING POINT SINGLE PRECISION VARIABLE OR CONSTANT
.     A,X ARE FLOATING POINT, SINGLE PRECISION VECTORS
. *   DO NOTHING IF N.EQ.O
. *   ERROR EXIT GERRB$ IF N.LT.0
.
.  ************************************
 
          AXR$
          FORT$
$(1),FIRS01
          +         'VBIAS '
VBIAS*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,2,XARG          . LOC(X), LOC(N)
          DL        A0,0,XARG          . LOC(A), LOC(ALPHA)
          L         A3,0,A3            . N
          LXI,U     A2,1
          LXI,U     A0,1
          JZ        A3,RETUR4          . DO NOTHING IF N.EQ.0
          L         A1,0,A1            . ALPHA
          JGD       A3,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LT.0
VBS10                                  . LOOP
          L         A4,A1              . ALPHA
          FA        A4,0,*A0           . ALPHA+A(I)
          S         A4,0,*A2
          JGD       A3,VBS10
          J         RETUR4
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,VBIAS
          +         0,DYN$
          +         'VBIAS '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VBIAS( A,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_VBIAS
#undef CERNLIB_TCGEN_VBIAS
#endif
