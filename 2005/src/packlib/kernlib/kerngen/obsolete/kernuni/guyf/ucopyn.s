*
* $Id: ucopyn.s,v 1.1.1.1 1996/02/15 17:54:05 mclareni Exp $
*
* $Log: ucopyn.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:05  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     SUBROUTINE  UCOPYN( A,B,N )                *** V 301 ***
.     COPY NEGATIVE
.     COPY -A(1)...-A(N) INTO B(1)...B(N)
.     ANY OVERLAP
. *   DO NOTHING IF N.LE.0
. *   SINGLE PRECISION VARIABLE
. *   VCOPYN IS ANOTHER NAME FOR THE SAME ROUTINE ADDED FOR COMPATIBILITY
.     WITH THE CERN LIBRARY
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1),FIRS01
          +         'UCOPYN'
VCOPYN*
UCOPYN*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A2,2,XARG          . LOC(N)
          DL        A0,0,XARG          . LOC(A), LOC(B)
          L         A2,0,A2            . N
          JZ        A2,RETUR3          . DO NOTHING IF N.EQ.0
          JGD       A2,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LT.0
          TLE       A0,A1
          J         UCOPN2
          LXI,XU    A0,1
          LXI,XU    A1,1
UCOPN1    LN        A4,0,*A0
          S         A4,0,*A1
          JGD       A2,UCOPN1
          J         RETUR3
UCOPN2
          A         A0,A2
          A         A1,A2
          LXI,XU    A0,-1
          LXI,XU    A1,-1
          J         UCOPN1
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,UCOPYN
          +         0,DYN$
          +         'UCOPYN'
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'UCOPYN( A,B,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_UCOPYN
#undef CERNLIB_TCGEN_UCOPYN
#endif
#ifdef CERNLIB_TCGEN_VCOPYN
#undef CERNLIB_TCGEN_VCOPYN
#endif
