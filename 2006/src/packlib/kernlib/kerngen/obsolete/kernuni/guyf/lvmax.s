*
* $Id: lvmax.s,v 1.1.1.1 1996/02/15 17:54:04 mclareni Exp $
*
* $Log: lvmax.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:04  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     FUNCTION LVMAX( A,N )                      *** F 121 ***
.     RETURNS  IVMAX= ADDRESS OF THE WORD WHICH CONTAINS THE MAXIMUM VALUE
.     IN ARRAY A(1) THRU A(N)
.     IF SEVERAL WORDS ARE EQUAL TO AMAX RETURNS THE ADDRESS OF THE FIRST
. *   ERROR EXIT IF  N.LE.0   (==>LVMAX=N)
. *   ARRAY A MAY BE INTEGER OR REAL, NOT DOUBLE PRECISION
.
. $$$*****$$$*****$$$*****$$$*****
.
          AXR$
          FORT$
$(1),FIRS01
          +         'LVMAX '
LVMAX*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(A), LOC(N)
          L         A0,0,A3            . N
          ANU,U     A0,1
          LXI,U     A2,1
          TP        A1
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LE.0
          S         A1,R1
LVMX1     L         A3,0,A2            . ANEW CANDIDATE
          L         A1,R1
          SG        A3,1,*A2           . LOOP
          J         $+2
          J         LVMX1
          AN        A0,A1
          J         RETUR2
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,LVMAX
          +         0,DYN$
          +         'LVMAX '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'LVMAX(A,N) ERROR: N.LE.0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_LVMAX
#undef CERNLIB_TCGEN_LVMAX
#endif
