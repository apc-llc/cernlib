*
* $Id: vmatr.s,v 1.1.1.1 1996/02/15 17:54:06 mclareni Exp $
*
* $Log: vmatr.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:06  mclareni
* Kernlib
*
*
. *************************************
.
.         SUBROUTINE VMATR( A, G, V, N, NJ )     *** F 121 ***
.         V = A * G          VECTOR * MATRIX PRODUCT
.         A  IS A  N - VECTOR
.         G  IS A  N*NJ - MATRIX   (N ROWS, NJ COLUMNS )
.         V  IS A  NJ - VECTOR
.         N  NUMBER OF ROWS OF MATRIX G
.         NJ   NUMBER OF COLUMNS OF MATRIX G
.         MATRIX STORED USING CERN CONVENTION: GIJ STORED IN G(J,I)
. *   ERROR EXIT IF N.LE.0 .OR. NJ.LE.0
.
. *************************************
 
          AXR$
          FORT$
VRTEMP    EQU       A15+2
 
$(1),FIRS01
          +         'VMATR '
VMATR*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,3,XARG          . LOC(N), LOC(NJ)
          L         A5,0,A2            . N
          L         R3,0,A3            . NJ
          DL        A1,1,XARG          . LOC(G), LOC(V)
          S         A5,R2
          L         A0,0,XARG
          MSI       A5,R3              . N*NJ
          LXI,U     A2,1
          LXI       A1,R3
          LXI,U     A0,1
          JGD       R2,$+2
          J         LMJER1             . ERROR EXIT IF N.LE.0
          S         A6,VRTEMP
          JGD       R3,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF NJ.LE.0
          S         A0,A15+1
          ANU,U     A5,1               . N*NJ-1
VMAR20
          L         A0,A15+1           . LOC( A(1) )
          L         R1,R2              . N-1
          SZ        A3
VMAR24
          L         A4,0,*A1           . G
          FM        A4,0,*A0           . G*A
          FA        A3,A4
          JGD       R1,VMAR24
          S         A3,0,*A2           . V
          AN        A1,A6
          JGD       R3,VMAR20
          L         A6,VRTEMP
          J         RETUR5
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,VMATR
          +         0,DYN$
          +         'VMATR '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VMATR:CHECK MATRIX DIMENSIONS'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_VMATR
#undef CERNLIB_TCGEN_VMATR
#endif
