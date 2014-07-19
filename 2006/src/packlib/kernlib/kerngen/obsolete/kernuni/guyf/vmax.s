*
* $Id: vmax.s,v 1.1.1.1 1996/02/15 17:54:06 mclareni Exp $
*
* $Log: vmax.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:06  mclareni
* Kernlib
*
*
. *************************************
.
.     FUNCTION VMAX( A,N )                       *** F 121 ***
.     FUNCTION IVMAX( IA,N)
.     Y = AMAX1( A(1),   A(N))
.     OR IY = MAX0( IA(1),   IA(N) )
. *   ERROR EXIT IF  N.LE.0   (==>VMAX=0.)
. *   ALL VALUES IN ARRAY A ARE SUPPOSED TO BE NORMALIZED
.
. *************************************
 
          AXR$
          FORT$
$(1),FIRS01
          +         'VMAX  '
IVMAX*
VMAX*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(A), LOC(N)
 DO FOR , SZ        A0                . (FTN: H1(A0)=0)
          L         R1,0,A3            . N
          LXI,U     A2,1
          JGD       R1,$+2             . N-1
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LE.0
VMX1      L         A0,0,A2            . LOOP
          SG        A0,1,*A2
          J         RETUR2
          J         VMX1
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,VMAX
          +         0,DYN$
          +         'VMAX  '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VMAX( A,N ) ERROR: N.LE.0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_VMAX
#undef CERNLIB_TCGEN_VMAX
#endif
