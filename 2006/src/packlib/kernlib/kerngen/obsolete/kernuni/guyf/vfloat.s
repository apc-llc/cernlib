*
* $Id: vfloat.s,v 1.1.1.1 1996/02/15 17:54:06 mclareni Exp $
*
* $Log: vfloat.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:06  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     SUBROUTINE  VFLOAT( IV,V,N )               *** F 121 ***
.     IF N.LE.0 DO NOTHING
.     DO 1 I=1,N
.   1 V(I) = IV(I)
.     DO NOTHING IF N=0
. *   ERROR EXIT GERRB$ IF N.LT.0
.     WORKS WITH OVERLAPPING ARRAYS
.
. $$$*****$$$*****$$$*****$$$*****
.
          AXR$
          FORT$
$(1),FIRS01
          +         'VFLOAT'
VFLOAT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A2,2,XARG          . LOC(N)
          DL        A0,0,XARG          . LOC(IV), LOC(V)
          L         A2,0,A2            . N
          L,U       A4,0233
          JZ        A2,RETUR3          . DO NOTHING IF N.EQ.0
          JGD       A2,$+2             . N-1
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LT.0
          TLE       A0,A1
          J         VFL2
         LXI,U  A0,1 .
         LXI,U  A1,1 .
LOOP     LCF    A4,0,*A0 .
         SA     A5,0,*A1 .
         JGD    A2,LOOP .
          J         RETUR3
VFL2
          A         A0,A2              . LOC (IV(N))
          A         A1,A2              . LOC (IV(N))
          LXI,XU    A0,-1
          LXI,XU    A1,-1
          J         LOOP
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,VFLOAT
          +         0,DYN$
          +         'VFLOAT'
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VFLOAT( IV,V,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
 END
#ifdef CERNLIB_TCGEN_VFLOAT
#undef CERNLIB_TCGEN_VFLOAT
#endif
