*
* $Id: vmina.s,v 1.1.1.1 1996/02/15 17:54:06 mclareni Exp $
*
* $Log: vmina.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:06  mclareni
* Kernlib
*
*
. *************************************
.
.     FUNCTION VMINA( A,N )                      *** F 121 ***
.     FUNCTION IVMINA( IA,N )
.     Y = AMIN1( ABS(A(1)),   ABS(A(N)) )
.     OR IY = MINO( ABS(IA(1)),   ABS(IA(N)) )
. *   ERROR EXIT IF  N.LE.0   (==>VMINA=0.)
.
. *************************************
 
          AXR$
          FORT$
$(1),FIRS01
          +         'VMINA '
VMINA*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(A), LOC(N)
 DO FOR , SZ        A0                . (FTN: H1(A0)=0)
          L         A3,0,A3            . N
          LXI,U     A2,1
          JGD       A3,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LE.0
          LMA       A0,0,A2
VMIN10
          LMA       A4,0,*A2
          TLE       A4,A0
          L         A0,A4
          JGD       A3,VMIN10
          J         RETUR2
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,VMINA
          +         0,DYN$
          +         'VMINA '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VMINA( A,N ) ERROR: N.LE.0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_VMINA
#undef CERNLIB_TCGEN_VMINA
#endif
