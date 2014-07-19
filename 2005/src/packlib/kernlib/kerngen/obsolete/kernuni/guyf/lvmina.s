*
* $Id: lvmina.s,v 1.1.1.1 1996/02/15 17:54:04 mclareni Exp $
*
* $Log: lvmina.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:04  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     FUNCTION LVMINA( A,N )                     *** F 121 ***
.     RETURNS LVMINA= LOCATION OF ELEMENT WITH THE MINIMUM ABSOLUTE VALUE
.     IN VECTOR A(1) THROUGH A(N)
.     IF SEVERAL WORDS ARE EQUAL TO AMINA RETURNS THE ADDRESS OF THE FIRST
. *   ERROR EXIT IF  N.LE.0   (==>LVMINA=N)
. *   ARRAY A MAY BE INTEGER OR REAL, NOT DOUBLE PRECISION
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1),FIRS01
          +         'LVMINA'
LVMINA*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(A), LOC(N)
          L         A0,0,A3            . N
          ANU,U     A0,1
          LXI,U     A2,1
          TP        A1
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LE.0
          LMA       A4,0,A2
LVMNA1    L         A3,A1              . NEW CANDIDATE
          L         A5,A4
          J         LVMNA3
LVMNA2    LMA       A4,1,*A2
          TLE       A4,A5
          J         LVMNA1
LVMNA3    JGD       A1,LVMNA2
          AN        A0,A3
          J         RETUR2
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,LVMINA
          +         0,DYN$
          +         'LVMINA'
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
                    'LVMINA( A,N )  ERROR IF N.LE.0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_LVMINA
#undef CERNLIB_TCGEN_LVMINA
#endif
