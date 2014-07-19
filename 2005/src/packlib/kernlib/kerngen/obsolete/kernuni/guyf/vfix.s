*
* $Id: vfix.s,v 1.1.1.1 1996/02/15 17:54:06 mclareni Exp $
*
* $Log: vfix.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:06  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     SUBROUTINE  VFIX( V,IV,N )                 *** F 121 ***
.     DO NOTHING IF N.EQ.0
.     DO 1 I=1,N
.   1 IV(I) = V(I)
. *   ERROR EXIT GERRB$ IF N.LT.0
. *   IF( ABS(V(I)).LT.1. ) IV(I)=0
. *   IF( ABS(V(I)).GE.2**35 ) IV(I)=SIGN(1.,V(I))*(2**35-1)
. *   WORKS IF V AND IV ARE OVERLAPPING ARRAYS
. *   DOESN'T CARE FOR UNNORMALISED VALUES (SEE FLAGGED CARDS)
. *   SINGLE PRECISION VARIABLES
.
. $$$*****$$$*****$$$*****$$$*****
.
          AXR$
          FORT$
$(0)
. UNNORM  +         0233000,0
LARGST    +         0377777,-0         . LARGEST POSITIVE INTEGER
 
$(1),FIRS01
          +         'VFIX  '
VFIX*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A2,2,XARG          . LOC(N)
          DL        A0,0,XARG          . LOC(V), LOC(IV)
          L         A2,0,A2            . N
.         L         R2,UNNORM
          JZ        A2,RETUR3          . DO NOTHING IF N.EQ.0
          JGD       A2,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LT.0
          TLE       A0,A1              . ADJUST SCANNING DIRECTION
          J         VFI6               . ACCORDING TO RELATIVE POSITIONS VANDIV
          LXI,U     A0,1
          LXI,U     A1,1
VFI1                                   . LOOP ENTRY
.         L         A4,0,*A0
.         FA        A4,R2
.         LUF       A4,A4
          LUF       A4,0,*A0
          TG,U      A4,0244
          J         LARGE              . ABS(V(I)).GE.2**35
          TLE,U     A4,0201
          L,U       A5,0
          LN        A3,A4
          LSSC      A5,8
          SSA       A5,0243,A3
          S         A5,0,*A1
VFI2      JGD       A2,VFI1
          J         RETUR3
LARGE
          L         A4,LARGST
          TP        A5
          LN        A4,A4
          S         A4,0,*A1
          J         VFI2
VFI6
          A         A0,A2
          A         A1,A2
          LXI,XU    A0,-1
          LXI,XU    A1,-1
          J         VFI1
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,VFIX
          +         0,DYN$
          +         'VFIX  '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VFIX( V,IV,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
 END
#ifdef CERNLIB_TCGEN_VFIX
#undef CERNLIB_TCGEN_VFIX
#endif
