*
* $Id: vline.s,v 1.1.1.1 1996/02/15 17:54:06 mclareni Exp $
*
* $Log: vline.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:06  mclareni
* Kernlib
*
*
. *************************************
.
.     SUBROUTINE VLINE( A, ALPHA, B, BETA, X, N )*** F 121 ***
.     X(I) = ALPHA*A(I) + BETA*B(I)    I = 1,   N
.     A,ALPHA,B,BETA,X ARE FLOATING-POINT
.     SINGLE PRECISION VARIABLES AND CONSTANTS OR ARRAYS
.     DO NOTHING IF N.EQ.0
. *   ERROR EXIT GERRB$ IF N.LT.0
. *   NO CHECK IS DONE TO DETECT OVERLAPPING ARRAYS
.
. *************************************
 
          AXR$
          FORT$
$(1),FIRS01
          +         'VLINE '
VLINE*
#include "spyuse.inc"
* Ignoring t=pass
#if defined(CERNLIB_GUYFORE)||defined(CERNLIB_GUYFORT)
#include "ftnors1.inc"
          DL        A0,0,X11           . LOC(A), LOC(ALPHA)
          L         R1,0,A1            . ALPHA
          DL        A1,2,X11           . LOC(B), LOC(BETA)
          L         R2,0,A2            . BETA
          DL        A2,4,X11           . LOC(X), LOC(N)
          LXI,U     A0,1
          L         A3,0,A3            . N
          LXI,U     A1,1
          LXI,U     A2,1
          JZ        A3,RETUR6          . DO NOTHING IF N=0
          JGD       A3,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LT.0
          DS        A6,A15+1           . SAVE A6, A7
VLIN20
          L         A4,0,*A0
          L         A6,0,*A1
          FM        A4,R1
          FM        A6,R2
          FA        A4,A6
          S         A4,0,*A2
          JGD       A3,VLIN20
          DL        A6,A15+1
#endif
#if defined(CERNLIB_GUYFTN)
          DL        A2,0,A0            . LOC(A), LOC(ALPHA)
          L         R1,0,A3            . ALPHA
          L         A3,A2
          DL        A1,2,A0            . LOC(B), LOC(BETA)
          L         R2,0,A2            . BETA
          L         A2,A1
          DL        A0,4,A0            . LOC(X), LOC(N)
          LXI,U     A3,1
          L         A1,0,A1            . N
          LXI,U     A2,1
          LXI,U     A0,1
          JZ        A1,RETUR6          . DO NOTHING IF N=0
          JGD       A1,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF N<0
          DS        A6,A15+1
VLIN20
          L         A4,0,*A3
          L         A6,0,*A2
          FM        A4,R1
          FM        A6,R2
          FA        A4,A6
          S         A4,0,*A0
          JGD       A1,VLIN20
          DL        A6,A15+1
#endif
          J         RETUR6
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,VLINE
          +         0,DYN$
          +         'VLINE '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VLINE( ,,,,,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_VLINE
#undef CERNLIB_TCGEN_VLINE
#endif
