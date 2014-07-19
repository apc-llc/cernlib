*
* $Id: vmatl.s,v 1.1.1.1 1996/02/15 17:54:06 mclareni Exp $
*
* $Log: vmatl.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:06  mclareni
* Kernlib
*
*
. *************************************
.
.         SUBROUTINE VMATL( G, C, X, N, NJ )     *** F 121 ***
.         X = G * C          MATRIX * VECTOR PRODUCT
.         G  IS A  N*NJ - MATRIX   (N ROWS, NJ COLUMNS )
.         C  IS A  NJ - VECTOR
.         X  IS A  N - VECTOR
.         N IS THE NUMBER OF ROWS FOR MATRIX G
.         NJ IS THE NUMBER OF COLUMNS FOR MATRIX G
.         MATRIX STORED USING CERN CONVENTION: GIJ STORED IN G(J,I)
. *   ERROR EXIT IF N.LE.0 .OR. NJ.LE.0
.
. *************************************
 
          AXR$
          FORT$
$(1),FIRS01
          +         'VMATL '
VMATL*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,3,XARG          . LOC(N), LOC(NJ)
          L         R1,0,A2            . N
          L         R3,0,A3            . NJ
          DL        A1,1,XARG          . LOC(C), LOC(X)
          L         A0,0,XARG          . LOC(G)
          JGD       R1,$+2
          J         $+2                . ERROR EXIT IF N.LE.0
          JGD       R3,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF NJ.LE.0
          LXI,U     A2,1
          LXI,U     A1,1
          LXI,U     A0,1
          S         A1,A15+1
VMAL20
          L         A1,A15+1           . LOC(C(1))
          L         R2,R3
          SZ        A3
VMAL24
          L         A4,0,*A0
          FM        A4,0,*A1
          FA        A3,A4
          JGD       R2,VMAL24
          S         A3,0,*A2
          JGD       R1,VMAL20
          J         RETUR5
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,VMATL
          +         0,DYN$
          +         'VMATL '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VMATL:CHECK MATRIX DIMENSIONS'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_VMATL
#undef CERNLIB_TCGEN_VMATL
#endif
