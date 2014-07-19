*
* $Id: lvmin.s,v 1.1.1.1 1996/02/15 17:54:04 mclareni Exp $
*
* $Log: lvmin.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:04  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     FUNCTION LVMIN( A,N )                      *** F 121 ***
.     RETURNS  IVMIN= ADDRESS OF THE WORD WHICH CONTAINS THE MINIMUM VALUE
.     IN ARRAY A(1) THRU A(N)
.     IF SEVERAL WORDS ARE EQUAL TO AMIN RETURNS THE ADDRESS OF THE FIRST
. *   ERROR EXIT IF  N.LE.0   (==>LVMIN=N)
. *   ARRAY A MAY BE INTEGER OR REAL, NOT DOUBLE PRECISION
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1),FIRS01
          +         'LVMIN '
LVMIN*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(A), LOC(N)
          L         A1,0,A3            . N
          L         A0,A1
          JGD       A1,$+2             . N-1
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LE.0
          A         A2,A1
          AN,U      A2,1
          LXI,XU    A2,-1
          S         A1,R1
LVMN1     L         A4,1,A2            . NEW CANDIDATE
          L         A0,R1
          SLE       A4,0,*A2           . LOOP
          J         $+2
          J         LVMN1
          A,U       A0,1
          J         RETUR2
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,LVMIN
          +         0,DYN$
          +         'LVMIN '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'LVMIN(A,N) ERROR: N.LE.0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_LVMIN
#undef CERNLIB_TCGEN_LVMIN
#endif
