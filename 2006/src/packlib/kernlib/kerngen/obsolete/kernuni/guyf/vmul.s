*
* $Id: vmul.s,v 1.1.1.1 1996/02/15 17:54:07 mclareni Exp $
*
* $Log: vmul.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:07  mclareni
* Kernlib
*
*
. *************************************
.
.     SUBROUTINE VMUL ( A,B,X,N )                *** F 121 ***
.     X( I ) = A(I)*B(I)          I = 1,   N
.     DO NOTHING IF N.EQ.0
. *   ERROR EXIT  IF N.LT.0
.
. *************************************
 
          AXR$
          FORT$
$(1),FIRS01
          +         'VMUL  '
VMUL*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,2,XARG          . LOC(X), LOC(N)
          DL        A0,0,XARG          . LOC(A), LOC(B)
          L         A3,0,A3
          LXI,U     A2,1
          LXI,U     A0,1
          LXI,U     A1,1
          JZ        A3,RETUR4          . DO NOTHING IF N.EQ.0
          JGD       A3,$+2             . N-1
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LT.0
VMLOOP
          L         A4,0,*A0
          FM        A4,0,*A1
          S         A4,0,*A2
VML1      JGD       A3,VMLOOP
          J         RETUR4
 
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,VMUL
          +         0,DYN$
          +         'VMUL  '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VMUL( ,,,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
 END
#ifdef CERNLIB_TCGEN_VMUL
#undef CERNLIB_TCGEN_VMUL
#endif
