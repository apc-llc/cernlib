*
* $Id: mxuty.s,v 1.1.1.1 1996/02/15 17:54:05 mclareni Exp $
*
* $Log: mxuty.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:05  mclareni
* Kernlib
*
*
. **************************************
.
.     SUBROUTINE MXUTY( A, N )
.     DIMENSION A(N,N)
.     BUILDS UNIT MATRIX
.     DO NOTHING IF N=0
. *   ERROR EXIT IF N.LT.0
.
. **************************************
 
          AXR$
          FORT$
$(1),FIRS01
          +         'MXUTY '
MXUTY*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(A), LOC(N)
          L,U       A1,0
          L,U       A0,A1
          LN        A3,0,A3           . -N
          LXI,U     A2,1
          JZ        A3,RETUR2         . DO NOTHING IF N=0
          TG,U      A3,1
LMJER1    SLJ       GERR$             . ERROR EXIT IF  N<0
          ANU,U     A3,1              . -N-1
          MSI       A3,A3              . N*N
          AN,U      A3,1              . N**2-1
          S         A3,R1
          BT        A2,0,*A0          . VZERO( A, N*N )
          SMA       A4,R1
          LXI,U     A1,0201400        . (A1) = +1.0
          LXI       A2,A4             . -N-1, LOC(A(N**2))
          JGD       R1,$+1
          BT        A2,0,*A0           . VFILL(A,N,1.0) EVERY N+1 WORDS
          J         RETUR2
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS
#include "gerr_c.inc"
          INFO  010  03
$(3),DIAG1  +       0775003,MXUTY
          +         DYN$
          +         'MXUTY '
          ON    GERMAX
F         FORM      6,6,6,18
DIAG2     +         0776702,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'MXUTY(A,N) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF . GERMAX
          ON    GERMAX=0
DIAG2     +         0776202,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF . GERMAX=0
#endif
          END
#ifdef CERNLIB_TCGEN_MXUTY
#undef CERNLIB_TCGEN_MXUTY
#endif
