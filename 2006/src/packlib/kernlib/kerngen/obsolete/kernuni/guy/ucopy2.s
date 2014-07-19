*
* $Id: ucopy2.s,v 1.1.1.1 1996/02/15 17:54:02 mclareni Exp $
*
* $Log: ucopy2.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:02  mclareni
* Kernlib
*
*
. **************************************
.
.         SUBROUTINE UCOPY2( A, B, N )           *** V 301 ***
.         COPY A(1)...A(N) INTO B(1)...B(N)
.         ANY OVERLAP
. *   ERROR EXIT GERR$ IF N.LT.0 (UCOPY,UCOPY2 )
.     DO NOTHING IF N.EQ.0
. *   ONLY ONE ROUTINE WITH ENTRY POINTS UCOPY, UCOPY2
. *   OBSOLETE ENTRY POINT MOVE TRANSFERED ELSEWHERE
.
. **************************************
 
          AXR$
          FORT$
$(1),FIRS01
          +         'UCOPY '
UCOPY*
UCOPY2*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A2,2,XARG          . LOC(N)
          DL        A0,0,XARG          . LOC(A), LOC(B)
          L         A2,0,A2            . N
UCOP1                                  . *** ENTRY POINT FOR MOVE ***
          JZ        A2,RETUR3          . DO NOTHING IF N.EQ.0
          S         A2,R1              .
          JGD       A2,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LT.0
          TLE       A0,A1
          J         UCOP3              . IF LOC(B).GT.LOC(A)
          LXI,U     A0,1               . LOC(B).LE.LOC(A)
          LXI,U     A1,1
UCOP2     BT        A1,0,*A0
          J         RETUR3
UCOP3                                  . LOC(B).GT.LOC(A)
          A         A0,A2
          A         A1,A2
          LXI,U     A0,-1
          LXI,U     A1,-1
          J         UCOP2
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,UCOPY
          +         0,DYN$
          +         'UCOPY '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'UCOPY,UCOPY2( A,B,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
 END
#ifdef CERNLIB_TCGEN_UCOPY
#undef CERNLIB_TCGEN_UCOPY
#endif
#ifdef CERNLIB_TCGEN_UCOPY2
#undef CERNLIB_TCGEN_UCOPY2
#endif
