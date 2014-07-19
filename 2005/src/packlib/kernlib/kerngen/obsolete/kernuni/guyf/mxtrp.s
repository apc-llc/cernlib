*
* $Id: mxtrp.s,v 1.1.1.1 1996/02/15 17:54:05 mclareni Exp $
*
* $Log: mxtrp.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:05  mclareni
* Kernlib
*
*
. **************************************
.
.     SUBROUTINE MXTRP( A, X, NI, NJ )           *** F 110 ***
.     DIMENSION A(NJ,NI), X(NI,NJ)
. *   ERROR EXIT IF NI.LE.0 .OR. NJ.LE.0
.
. **************************************
 
          AXR$
          FORT$
$(1),FIRS01
          +         'MXTRP '
MXTRP*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,2,XARG          . LOC(NI), LOC(NJ)
          DL        A0,0,XARG          . LOC(A), LOC(X)
          L         A4,0,A2            . NI
          L         A5,0,A3            . NJ
          LXI       A1,A4
          LXI,U     A0,1
          TG,U      A5,1
          JGD       A4,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF NI.LT.1 .OR. NJ.LT.1
          A         A1,A4
MXTR10
          ANU       A1,A4
          S         A5,R1
          BT        A2,0,*A0
          JGD       A4,MXTR10
          J         RETUR4
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS
#include "gerr_c.inc"
          INFO  010  03
$(3),DIAG1 +        0775003,MXTRP
          +         DYN$
          +         'MXTRP '
          ON    GERMAX
F         FORM      6,6,6,18
DIAG2     +         0776702,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'MXTRP(,,NI,NJ) ERROR: NI OR NJ.LE.0'
LDIAG3    EQU       $-DIAG3
          OFF . GERMAX
          ON    GERMAX=0
DIAG2     +         0776202,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF . GERMAX=0
#endif
          END
#ifdef CERNLIB_TCGEN_MXTRP
#undef CERNLIB_TCGEN_MXTRP
#endif
