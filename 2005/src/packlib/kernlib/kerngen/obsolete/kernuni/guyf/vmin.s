*
* $Id: vmin.s,v 1.1.1.1 1996/02/15 17:54:06 mclareni Exp $
*
* $Log: vmin.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:06  mclareni
* Kernlib
*
*
. *************************************
.
.     FUNCTION VMIN( A,N )                       *** F 121 ***
.     FUNCTION IVMIN( IA,N )
.     Y = AMIN1( A(1),   A(N) )
.     OR IY = MIN0( IA(1),   IA(N) )
. *   ERROR EXIT IF  N.LE.0   (==>VMIN=0.)
.
. *************************************
 
          AXR$
          FORT$
$(1),FIRS01
          +         'VMIN  '
VMIN*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(A), LOC(N)
 DO FOR , SZ        A0                . (FTN: H1(A0)=0)
          L         R1,0,A3            . N
          LXI,U     A2,1
          JGD       R1,$+2
LMJER1    SLJ       GERR$              . ERROR IXIT IF N.LE.0
VMN1      L         A0,0,A2
          SLE       A0,1,*A2
          J         RETUR2
          J         VMN1
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,VMIN
          +         0,DYN$
          +         'VMIN  '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VMIN( A,N ) ERROR: N.LE.0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_VMIN
#undef CERNLIB_TCGEN_VMIN
#endif
