*
* $Id: vasum.s,v 1.1.1.1 1996/02/15 17:54:06 mclareni Exp $
*
* $Log: vasum.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:06  mclareni
* Kernlib
*
*
. *************************************
.
.     FUNCTION VASUM( A,N )                      *** F 121 ***
.     Y = ABS( A(1))+   +ABS( A(N))
.     RETURNS VASUM=.0 IF N.EQ.0
. *   ERROR EXIT GERRB$ IF N.LT.0
.
. *************************************
 
          AXR$
          FORT$
$(1),FIRS01
          +         'VASUM '
VASUM*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(X), LOC(N)
          SZ        A0
          L         A4,0,A3            . N
          LXI,U     A2,1
          JZ        A4,RETUR2          . DO NOTHING IF N.EQ.0
          JGD       A4,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LT.0
VASM1     LMA       A5,0,*A2
          FA        A0,A5
          JGD       A4,VASM1
          J         RETUR2
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,VASUM
          +         0,DYN$
          +         'VASUM '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VASUM( A,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_VASUM
#undef CERNLIB_TCGEN_VASUM
#endif
