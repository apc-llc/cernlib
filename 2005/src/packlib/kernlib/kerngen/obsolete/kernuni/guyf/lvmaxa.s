*
* $Id: lvmaxa.s,v 1.1.1.1 1996/02/15 17:54:04 mclareni Exp $
*
* $Log: lvmaxa.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:04  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     FUNCTION LVMAXA( A,N )                     *** F 121 ***
.     RETURNS LVMAXA = LOCATION OF THE ELEMENT WITH THE MAXIMUM ABSOLUTE VALUE
.     IN VECTOR A(1) THROUGH A(N)
.     IF SEVERAL WORDS ARE EQUAL TO AMAXA RETURNS THE ADDRESS OF THE FIRST
. *   ERROR EXIT IF  N.LE.0   (==>LVMAXA=N)
. *   ARRAY A MAY BE INTEGER OR REAL, NOT DOUBLE PRECISION
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1),FIRS01
          +         'LVMAXA'
LVMAXA*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(A), LOC(N)
          L         A0,0,A3            . N
          ANU,U     A0,1
          LXI,U     A2,1
          TP        A1
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LE.0
          LMA       A4,0,*A2
LVMXA1    L         A3,A1              . A NEW CANDIDATE
          L         A5,A4
          J         LVMXA3
LVMXA2    LMA       A4,0,*A2           . LOOP
          TLE       A5,A4
          J         LVMXA1
LVMXA3    JGD       A1,LVMXA2
          AN        A0,A3
          J         RETUR2
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,LVMAXA
          +         0,DYN$
          +         'LVMAXA'
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
                    'LVMAXA( A,N )  ERROR IF N.LE.0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_LVMAXA
#undef CERNLIB_TCGEN_LVMAXA
#endif
