*
* $Id: uswop.s,v 1.1.1.1 1996/02/15 17:54:02 mclareni Exp $
*
* $Log: uswop.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:02  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     SUBROUTINE USWOP( A,B,N )                  *** V 303 ***
.     EXCHANGES THE CONTENTS OF ARRAYS A AND B (N FIRST WORDS)
.     DO NOTHING IF N.EQ.0
. *   ERROR EXIT GERR$ IF OVERLAP OR N.LT.0
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
 
$(1),FIRS01
          +         'USWOP '
USWOP*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A3,2,XARG          . LOC(N)
          DL        A0,0,XARG          . LOC(A), LOC(B)
          L         A3,0,A3            . N
          ANU       A1,A0              .
          JZ        A3,RETUR3          . DO NOTHING IF N.EQ.0
          LMA       A2,A2              . (A2)=IABS(B-A)
          LXI,U     A0,1
          TG        A2,A3
          JGD       A3,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF OVERLAP OR N.LT.0
          LXI,U     A1,1
USW1     LA     A4,0,A0                . LOOP
         LA     A5,0,A1 .
         SA     A4,0,*A1 .
         SA     A5,0,*A0 .
          JGD       A3,USW1
          J         RETUR3
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,USWOP
          +         0,DYN$
          +         'USWOP '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'USWOP( A,B,N ) ERROR: OVERLAP OR N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_USWOP
#undef CERNLIB_TCGEN_USWOP
#endif
